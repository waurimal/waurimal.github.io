/**
 * 점프맨 게임 - 명예의 전당 및 학생 성적 관리 (Hall of Fame)
 * Firebase Firestore 연동 & LocalStorage 하이브리드 캐시
 */

class JumpmanHallOfFame {
    constructor() {
        this.storageKey = 'waurimal_jumpman_hof';
        this.profileKey = 'waurimal_jumpman_profile';
        this.defaultRecords = [
            { id: 'hof_1', grade: 6, classNum: 1, studentNum: 1, name: '슈퍼점퍼', score: 9850, distance: 1820, survivalTime: 145, coins: 48, quizzesSolved: 3, date: '2026-11-10' },
            { id: 'hof_2', grade: 6, classNum: 2, studentNum: 3, name: '날쌘돌이', score: 8400, distance: 1540, survivalTime: 128, coins: 39, quizzesSolved: 2, date: '2026-11-09' },
            { id: 'hof_3', grade: 5, classNum: 1, studentNum: 5, name: '바람의아이', score: 7200, distance: 1320, survivalTime: 110, coins: 34, quizzesSolved: 2, date: '2026-11-08' },
            { id: 'hof_4', grade: 5, classNum: 3, studentNum: 7, name: '구름점프', score: 6150, distance: 1150, survivalTime: 95, coins: 28, quizzesSolved: 3, date: '2026-11-07' },
            { id: 'hof_5', grade: 4, classNum: 1, studentNum: 2, name: '번개스니커즈', score: 5300, distance: 980, survivalTime: 82, coins: 24, quizzesSolved: 1, date: '2026-11-06' },
            { id: 'hof_6', grade: 4, classNum: 2, studentNum: 4, name: '점프대장', score: 4500, distance: 820, survivalTime: 70, coins: 20, quizzesSolved: 2, date: '2026-11-05' },
            { id: 'hof_7', grade: 3, classNum: 1, studentNum: 6, name: '새싹러너', score: 3800, distance: 680, survivalTime: 58, coins: 16, quizzesSolved: 1, date: '2026-11-04' }
        ];
    }

    // 학생 프로필 저장 / 불러오기
    getProfile() {
        try {
            const raw = localStorage.getItem(this.profileKey);
            if (raw) return JSON.parse(raw);
        } catch (e) {}
        return { grade: 6, classNum: 1, studentNum: 1, name: '' };
    }

    saveProfile(profile) {
        try {
            localStorage.setItem(this.profileKey, JSON.stringify(profile));
        } catch (e) {}
    }

    // 로컬 기록 불러오기
    getLocalRecords() {
        try {
            const raw = localStorage.getItem(this.storageKey);
            if (raw) {
                const parsed = JSON.parse(raw);
                if (Array.isArray(parsed) && parsed.length > 0) {
                    return parsed;
                }
            }
        } catch (e) {}
        return [...this.defaultRecords];
    }

    // 신규 기록 저장 (Firestore 비동기 동기화 포함)
    async saveScore(record) {
        const fullRecord = {
            id: 'jump_' + Date.now() + '_' + Math.floor(Math.random() * 1000),
            grade: Number(record.grade) || 6,
            classNum: Number(record.classNum) || 1,
            studentNum: Number(record.studentNum) || 1,
            name: String(record.name || '학생').trim(),
            score: Number(record.score) || 0,
            distance: Number(record.distance) || 0,
            survivalTime: Number(record.survivalTime) || 0,
            coins: Number(record.coins) || 0,
            quizzesSolved: Number(record.quizzesSolved) || 0,
            date: new Date().toISOString().split('T')[0],
            timestamp: Date.now()
        };

        // 1. LocalStorage 즉시 저장
        const records = this.getLocalRecords();
        records.push(fullRecord);
        records.sort((a, b) => b.score - a.score);
        try {
            localStorage.setItem(this.storageKey, JSON.stringify(records.slice(0, 100)));
        } catch (e) {}

        // 2. Firebase Firestore 비동기 전송
        this.syncToFirestore(fullRecord);

        return fullRecord;
    }

    async syncToFirestore(record) {
        try {
            if (typeof firebase !== 'undefined') {
                let db = null;
                if (window.FirebaseConfig && window.FirebaseConfig.getFirebaseConfig) {
                    const cfg = window.FirebaseConfig.getFirebaseConfig('jumpman');
                    if (!firebase.apps.length) {
                        firebase.initializeApp(cfg);
                    }
                    db = firebase.firestore();
                } else if (firebase.apps.length > 0) {
                    db = firebase.firestore();
                }
                if (db) {
                    await db.collection('games').doc('jumpman').collection('scores').add(record);
                }
            }
        } catch (e) {
            console.warn("[HOF] Firestore sync fallback:", e);
        }
    }

    async fetchCloudRecords() {
        try {
            if (typeof firebase !== 'undefined') {
                let db = null;
                if (window.FirebaseConfig && window.FirebaseConfig.getFirebaseConfig) {
                    const cfg = window.FirebaseConfig.getFirebaseConfig('jumpman');
                    if (!firebase.apps.length) {
                        firebase.initializeApp(cfg);
                    }
                    db = firebase.firestore();
                } else if (firebase.apps.length > 0) {
                    db = firebase.firestore();
                }
                if (db) {
                    const snap = await db.collection('games').doc('jumpman').collection('scores')
                        .orderBy('score', 'desc')
                        .limit(50)
                        .get();
                    if (snap && !snap.empty) {
                        const cloudRecords = [];
                        snap.forEach(doc => {
                            const d = doc.data();
                            cloudRecords.push({ id: doc.id, ...d });
                        });
                        const local = this.getLocalRecords();
                        const mergedMap = new Map();
                        [...cloudRecords, ...local].forEach(item => {
                            const key = `${item.grade}_${item.classNum}_${item.name}_${item.score}`;
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
                }
            }
        } catch (e) {
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
