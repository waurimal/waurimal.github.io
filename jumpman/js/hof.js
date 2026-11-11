/**
 * 점프맨 게임 - 명예의 전당 및 학생 성적 관리 (Hall of Fame)
 * Firebase Firestore 연동 & LocalStorage 하이브리드 캐시
 * Waurimal Firebase Sharding System (tournament-267ab 프로젝트)
 */

class JumpmanHallOfFame {
    constructor() {
        this.storageKey = 'waurimal_jumpman_hof_v3';
        this.profileKey = 'waurimal_jumpman_profile';
        this.defaultRecords = []; // 기존 더미/샘플 기록 전면 삭제 (순수 학생 기록만 보관)

        // 이전 버전 및 더미 데이터 로컬스토리지 정리
        try {
            localStorage.removeItem('waurimal_jumpman_hof');
            localStorage.removeItem('waurimal_jumpman_hof_v2');
        } catch (e) {}
    }

    // 학생 프로필 저장 / 불러오기 (와우리말 SSO 및 로컬스토리지 연동)
    getProfile() {
        // 1. 와우리말 통합 학생 SSO 세션 우선
        if (typeof window !== 'undefined' && window.WaurimalStudent && typeof window.WaurimalStudent.get === 'function') {
            const sso = window.WaurimalStudent.get();
            if (sso && sso.name && sso.name.trim()) {
                return {
                    grade: Number(sso.grade) || 6,
                    classNum: Number(sso.classNum) || 1,
                    studentNum: Number(sso.studentNum) || 1,
                    name: sso.name.trim()
                };
            }
        }

        // 2. 통합 학생 프로필 로컬스토리지
        try {
            const rawStudent = localStorage.getItem('waurimal_student_profile');
            if (rawStudent) {
                const p = JSON.parse(rawStudent);
                if (p && p.name && p.name.trim()) {
                    return {
                        grade: Number(p.grade) || 6,
                        classNum: Number(p.classNum) || 1,
                        studentNum: Number(p.studentNum) || 1,
                        name: p.name.trim()
                    };
                }
            }
        } catch (e) {}

        // 3. 점프맨 전용 프로필 로컬스토리지
        try {
            const raw = localStorage.getItem(this.profileKey);
            if (raw) {
                const parsed = JSON.parse(raw);
                if (parsed && typeof parsed === 'object') {
                    return {
                        grade: Number(parsed.grade) || 6,
                        classNum: Number(parsed.classNum) || 1,
                        studentNum: Number(parsed.studentNum) || 1,
                        name: String(parsed.name || '').trim()
                    };
                }
            }
        } catch (e) {}

        return { grade: 6, classNum: 1, studentNum: 1, name: '' };
    }

    saveProfile(profile) {
        const cleanProfile = {
            grade: Number(profile.grade) || 6,
            classNum: Number(profile.classNum) || 1,
            studentNum: Number(profile.studentNum) || 1,
            name: String(profile.name || '').trim()
        };

        try {
            localStorage.setItem(this.profileKey, JSON.stringify(cleanProfile));
        } catch (e) {}

        // 와우리말 통합 SSO 프로필에도 동기화
        if (typeof window !== 'undefined' && window.WaurimalStudent && typeof window.WaurimalStudent.save === 'function') {
            window.WaurimalStudent.save({
                school: "이의초등학교",
                grade: String(cleanProfile.grade),
                classNum: String(cleanProfile.classNum),
                studentNum: String(cleanProfile.studentNum),
                name: cleanProfile.name
            });
        }
    }

    // 로컬 기록 불러오기 (더미 제외)
    getLocalRecords() {
        try {
            const raw = localStorage.getItem(this.storageKey);
            if (raw) {
                const parsed = JSON.parse(raw);
                if (Array.isArray(parsed) && parsed.length > 0) {
                    return parsed.filter(r => r && !String(r.id || '').startsWith('hof_'));
                }
            }
        } catch (e) {}
        return [];
    }

    // 모든 로컬 기록 완전 삭제
    clearAllRecords() {
        try {
            localStorage.removeItem(this.storageKey);
            localStorage.removeItem('waurimal_jumpman_hof');
            localStorage.removeItem('waurimal_jumpman_hof_v2');
        } catch (e) {}
        return [];
    }

    // Firebase Firestore 전용 인스턴스 획득 (tournament-267ab 샤딩)
    getFirestoreDb() {
        if (typeof firebase === 'undefined' || typeof firebase.firestore !== 'function') {
            return null;
        }

        try {
            const cfg = (window.WaurimalFirebase && typeof window.WaurimalFirebase.getConfig === 'function')
                ? window.WaurimalFirebase.getConfig('jumpman')
                : {
                    apiKey: "AIzaSyAaRTZlJ_fQAKl8j0lCJfaOdJyx8wMP5Zo",
                    authDomain: "tournament-267ab.firebaseapp.com",
                    projectId: "tournament-267ab",
                    storageBucket: "tournament-267ab.firebasestorage.app",
                    messagingSenderId: "401796751337",
                    appId: "1:401796751337:web:6f3ba64fde25adf1a8f578"
                };

            if (window.WaurimalFirebase && window.WaurimalFirebase.isCircuitBroken && window.WaurimalFirebase.isCircuitBroken(cfg.projectId)) {
                console.warn("[HOF] Circuit breaker active for " + cfg.projectId);
                return null;
            }

            let app = null;
            try {
                app = firebase.app('jumpman');
            } catch (e) {
                try {
                    app = firebase.initializeApp(cfg, 'jumpman');
                } catch (initErr) {
                    if (!firebase.apps.length) {
                        app = firebase.initializeApp(cfg);
                    } else {
                        app = firebase.app();
                    }
                }
            }

            return app ? app.firestore() : firebase.firestore();
        } catch (err) {
            console.warn("[HOF] Firestore get db error:", err);
            return null;
        }
    }

    // 신규 기록 저장 (LocalStorage + Firestore 비동기 전송)
    async saveScore(record) {
        const fullRecord = {
            id: 'jump_' + Date.now() + '_' + Math.floor(Math.random() * 1000),
            grade: Number(record.grade) || 6,
            classNum: Number(record.classNum) || 1,
            studentNum: Number(record.studentNum) || 1,
            name: String(record.name || '학생').trim() || '학생',
            score: Number(record.score) || 0,
            distance: Number(record.distance) || 0,
            survivalTime: Number(record.survivalTime) || 0,
            coins: Number(record.coins) || 0,
            quizzesSolved: Number(record.quizzesSolved) || 0,
            date: new Date().toISOString().split('T')[0],
            timestamp: Date.now()
        };

        // 1. LocalStorage 즉시 저장
        try {
            const records = this.getLocalRecords();
            records.push(fullRecord);
            records.sort((a, b) => b.score - a.score);
            localStorage.setItem(this.storageKey, JSON.stringify(records.slice(0, 100)));
        } catch (e) {
            console.warn("[HOF] LocalStorage save error:", e);
        }

        // 2. Firebase Firestore 비동기 전송
        try {
            await this.syncToFirestore(fullRecord);
        } catch (e) {
            console.warn("[HOF] syncToFirestore error:", e);
        }

        return fullRecord;
    }

    async syncToFirestore(record) {
        try {
            const db = this.getFirestoreDb();
            if (db) {
                const docRef = await db.collection('games').doc('jumpman').collection('scores').add({
                    ...record,
                    createdAt: (typeof firebase !== 'undefined' && firebase.firestore && firebase.firestore.FieldValue)
                        ? firebase.firestore.FieldValue.serverTimestamp()
                        : new Date()
                });
                console.log("[HOF] Firestore sync success, doc id:", docRef.id);
                return docRef.id;
            }
        } catch (e) {
            if (window.WaurimalFirebase && window.WaurimalFirebase.tripCircuitBreaker && e && (e.code === 'resource-exhausted' || String(e).includes('429'))) {
                window.WaurimalFirebase.tripCircuitBreaker('tournament-267ab');
            }
            console.warn("[HOF] Firestore sync fallback:", e);
        }
        return null;
    }

    async fetchCloudRecords() {
        try {
            const db = this.getFirestoreDb();
            if (db) {
                const snap = await db.collection('games').doc('jumpman').collection('scores')
                    .orderBy('score', 'desc')
                    .limit(50)
                    .get();

                const cloudRecords = [];
                if (snap && !snap.empty) {
                    snap.forEach(doc => {
                        const d = doc.data();
                        cloudRecords.push({ id: doc.id, ...d });
                    });
                }

                const local = this.getLocalRecords();
                const mergedMap = new Map();
                [...cloudRecords, ...local].forEach(item => {
                    if (!item || String(item.id || '').startsWith('hof_')) return;
                    const key = `${item.grade}_${item.classNum}_${item.studentNum}_${item.name}_${item.score}`;
                    if (!mergedMap.has(key)) {
                        mergedMap.set(key, item);
                    }
                });

                const mergedList = Array.from(mergedMap.values());
                mergedList.sort((a, b) => b.score - a.score);

                try {
                    localStorage.setItem(this.storageKey, JSON.stringify(mergedList.slice(0, 100)));
                } catch (e) {}

                return mergedList;
            }
        } catch (e) {
            if (window.WaurimalFirebase && window.WaurimalFirebase.tripCircuitBreaker && e && (e.code === 'resource-exhausted' || String(e).includes('429'))) {
                window.WaurimalFirebase.tripCircuitBreaker('tournament-267ab');
            }
            console.warn("[HOF] fetchCloudRecords fallback:", e);
        }
        return this.getLocalRecords();
    }

    // 명예의 전당 필터링 목록
    getFilteredRecords({ period = 'all', grade = 'all', classNum = 'all', keyword = '' } = {}) {
        let records = this.getLocalRecords();
        const now = new Date();

        // 기간 필터
        if (period === 'month') {
            const curMonth = now.toISOString().slice(0, 7);
            records = records.filter(r => (r.date || '').startsWith(curMonth));
        } else if (period === 'week') {
            const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
            records = records.filter(r => (r.date || '') >= oneWeekAgo);
        }

        // 학년 필터
        if (grade !== 'all') {
            records = records.filter(r => String(r.grade) === String(grade));
        }

        // 반 필터
        if (classNum !== 'all') {
            records = records.filter(r => String(r.classNum) === String(classNum));
        }

        // 검색어 필터
        if (keyword && keyword.trim()) {
            const kw = keyword.trim().toLowerCase();
            records = records.filter(r => (r.name || '').toLowerCase().includes(kw));
        }

        return records.sort((a, b) => b.score - a.score);
    }
}

window.JumpmanHOF = new JumpmanHallOfFame();
