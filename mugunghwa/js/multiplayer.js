/**
 * 무궁화 꽃이 피었습니다: 영어 런 - 전용 실시간 멀티플레이어 엔진 (Firebase Firestore)
 * 2026 Eui Elementary School - Waurimal Project
 *
 * 지원 기능:
 * 1. 4자리 방 코드 기반 방 생성(교사용 대형 화면/TV) 및 전교생/반 학생 실시간 입장
 * 2. 제한 시간(들어오는 시간) 실시간 동기화 (60초, 90초, 120초, 180초 등)
 * 3. 술래의 상태 머신(CHANTING, TURNING, SCANNING) 호스트 브로드캐스팅
 * 4. 학생별 100m 달리기 트랙 위치, 체크포인트, 획득 아이템, 골인 순위 실시간 동기화
 * 5. 오답 시 직전 체크포인트/원위치 강제 후퇴 동기화
 */

class MugunghwaMultiplayer {
    constructor() {
        this.db = null;
        this.currentRoomId = null;
        this.roomData = null;
        this.roomUnsubscribe = null;
        this.isHost = false;
        this.myPlayerId = this.getOrCreatePlayerId();
        this.onStateChangeCallbacks = [];

        this.initFirebase();
    }

    getOrCreatePlayerId() {
        let pid = '';
        try {
            pid = localStorage.getItem('mugunghwa_player_id') || sessionStorage.getItem('mugunghwa_player_id');
        } catch (e) {}
        if (!pid) {
            pid = 'p_' + Math.random().toString(36).substring(2, 9);
        }
        try {
            localStorage.setItem('mugunghwa_player_id', pid);
            sessionStorage.setItem('mugunghwa_player_id', pid);
        } catch (e) {}
        return pid;
    }

    initFirebase() {
        if (typeof window !== 'undefined' && window.firebase) {
            try {
                if (!window.firebase.apps.length) {
                    const cfg = (window.WaurimalFirebase && window.WaurimalFirebase.getConfig)
                        ? window.WaurimalFirebase.getConfig('mugunghwa')
                        : {
                            apiKey: "AIzaSyDHCnRrVE2dVuJpf77P-1oftGpczR5WDd4",
                            authDomain: "eng-activity.firebaseapp.com",
                            projectId: "eng-activity"
                        };
                    window.firebase.initializeApp(cfg);
                }
                this.db = window.firebase.firestore();
            } catch (e) {
                console.warn("[MugunghwaMultiplayer] Firebase init warning:", e);
            }
        }
    }

    // 4자리 랜덤 숫자 방 코드 생성
    generateRoomId() {
        return Math.floor(1000 + Math.random() * 9000).toString();
    }

    // 1. 교사/호스트: 방 생성
    async createRoom(settings = {}) {
        if (!this.db) this.initFirebase();

        const roomId = settings.roomId || this.generateRoomId();
        const timeLimit = Number(settings.timeLimit) || 90; // 기본 90초
        const grade = settings.grade || 'all';
        const isHostParticipating = (settings.hostParticipating === true || settings.hostParticipating === 'player');
        const hostName = settings.hostName || '선생님';

        // 100m 트랙 상에 배치될 초기 필드 아이템 5개
        const initialFieldItems = [
            { id: 'item_1', m: 18, type: 'shoes', name: '부스터 신발', icon: '⚡' },
            { id: 'item_2', m: 38, type: 'shield', name: '방어막 쉴드', icon: '🛡️' },
            { id: 'item_3', m: 58, type: 'time', name: '타임 캡슐 (+10초)', icon: '⏱️' },
            { id: 'item_4', m: 76, type: 'hint', name: '50:50 힌트 패스', icon: '🎯' },
            { id: 'item_5', m: 90, type: 'star', name: '무적 스타 대시', icon: '⭐' }
        ];

        const playersMap = {};
        if (isHostParticipating) {
            playersMap[this.myPlayerId] = {
                id: this.myPlayerId,
                name: hostName,
                isHost: true,
                grade: '선생님',
                classNum: 0,
                studentNum: 0,
                avatarSkin: 'runner_teacher',
                m: 0,
                checkpoint: 0,
                combo: 0,
                items: [],
                activeBuffs: { boostShoes: false, shield: false, starDash: false },
                isFinished: false,
                finishRank: 0,
                isOut: false,
                joinedAt: Date.now()
            };
        }

        const roomPayload = {
            roomId: roomId,
            hostId: this.myPlayerId,
            hostParticipating: isHostParticipating,
            status: 'waiting', // waiting | playing | ended
            timeLimit: timeLimit,
            remainingTime: timeLimit,
            grade: grade,
            createdAt: Date.now(),
            taggerState: 'CHANTING', // CHANTING | TURNING | SCANNING
            taggerSpeed: 1.0,
            taggerTurnCount: 0,
            players: playersMap,
            fieldItems: initialFieldItems,
            finishers: [] // 골인 순서 기록 [{ id, name, rank, time }]
        };

        if (!this.db) {
            console.warn("[MugunghwaMultiplayer] Firebase not initialized. Using offline local mock room.");
            this.isMock = true;
            this.currentRoomId = roomId;
            this.isHost = true;
            this.roomData = roomPayload;
            setTimeout(() => this.notifyStateChange(this.roomData), 100);
            return roomId;
        }

        const roomRef = this.db.collection('mugunghwa_rooms').doc(roomId);
        await roomRef.set(roomPayload);

        this.currentRoomId = roomId;
        this.isHost = true;
        this.listenRoom(roomId);

        return roomId;
    }

    // 2. 학생: 방 입장
    async joinRoom(roomId, playerInfo) {
        if (!this.db) this.initFirebase();

        const cleanId = String(roomId).trim();
        const playerData = {
            id: this.myPlayerId,
            name: playerInfo.name || '학생',
            grade: playerInfo.grade || 6,
            classNum: playerInfo.classNum || 1,
            studentNum: playerInfo.studentNum || 1,
            avatarSkin: playerInfo.avatarSkin || 'runner_1',
            m: 0, // 0m 출발선에서 시작
            checkpoint: 0, // 0m 원위치
            combo: 0,
            items: [], // 보유 중인 버프 아이템
            activeBuffs: {
                boostShoes: false,
                shield: false,
                starDash: false
            },
            isFinished: false,
            finishRank: 0,
            isOut: false,
            joinedAt: Date.now()
        };

        if (!this.db) {
            this.isMock = true;
            this.currentRoomId = cleanId;
            const mockRoom = {
                roomId: cleanId,
                status: 'waiting',
                timeLimit: 90,
                grade: 6,
                players: {}
            };
            mockRoom.players[this.myPlayerId] = playerData;
            this.roomData = mockRoom;
            setTimeout(() => this.notifyStateChange(this.roomData), 100);
            return mockRoom;
        }

        const roomRef = this.db.collection('mugunghwa_rooms').doc(cleanId);
        const snap = await roomRef.get();

        if (!snap.exists) {
            throw new Error(`방 번호 [${cleanId}]를 찾을 수 없습니다. 다시 확인해 주세요.`);
        }

        const room = snap.data();
        if (room.status === 'ended') {
            throw new Error("이미 종료된 게임 방입니다.");
        }

        const updateObj = {};
        updateObj[`players.${this.myPlayerId}`] = playerData;
        await roomRef.update(updateObj);

        this.currentRoomId = cleanId;
        this.isHost = (room.hostId === this.myPlayerId);
        this.listenRoom(cleanId);

        return room;
    }

    // 3. 실시간 방 구독 (onSnapshot)
    listenRoom(roomId) {
        if (this.roomUnsubscribe) {
            this.roomUnsubscribe();
            this.roomUnsubscribe = null;
        }

        const roomRef = this.db.collection('mugunghwa_rooms').doc(roomId);
        let lastKnownTurn = 0;
        this.roomUnsubscribe = roomRef.onSnapshot(docSnap => {
            if (!docSnap.exists) {
                this.roomData = null;
                this.notifyStateChange({ deleted: true });
                return;
            }
            this.roomData = docSnap.data();

            // 술래 턴이 증가했을 때 학생 단말기에서도 퀴즈 자동 갱신 트리거
            if (this.roomData.taggerTurnCount && this.roomData.taggerTurnCount > lastKnownTurn) {
                lastKnownTurn = this.roomData.taggerTurnCount;
                if (!this.isHost && typeof window !== 'undefined' && window.onNewTaggerTurn) {
                    window.onNewTaggerTurn(lastKnownTurn);
                }
            }

            this.notifyStateChange(this.roomData);
        }, err => {
            console.warn("[MugunghwaMultiplayer] Snapshot error:", err);
        });
    }

    onStateChange(cb) {
        if (typeof cb === 'function') {
            this.onStateChangeCallbacks.push(cb);
        }
    }

    notifyStateChange(data) {
        this.onStateChangeCallbacks.forEach(cb => {
            try { cb(data); } catch (e) { console.error(e); }
        });
    }

    // 호스트: 게임 참여 여부 실시간 변경 (대기실 토글)
    async setHostParticipation(isParticipating, hostName = '선생님') {
        const flag = Boolean(isParticipating);
        if (this.isMock) {
            if (!this.roomData) return;
            this.roomData.hostParticipating = flag;
            if (!this.roomData.players) this.roomData.players = {};
            if (flag) {
                this.roomData.players[this.myPlayerId] = {
                    id: this.myPlayerId,
                    name: hostName,
                    isHost: true,
                    grade: '선생님',
                    classNum: 0,
                    studentNum: 0,
                    avatarSkin: 'runner_teacher',
                    m: 0,
                    checkpoint: 0,
                    combo: 0,
                    items: [],
                    activeBuffs: { boostShoes: false, shield: false, starDash: false },
                    isFinished: false,
                    finishRank: 0,
                    isOut: false,
                    joinedAt: Date.now()
                };
            } else {
                delete this.roomData.players[this.myPlayerId];
            }
            this.notifyStateChange(this.roomData);
            return;
        }

        if (!this.isHost || !this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);
        const updateObj = {
            hostParticipating: flag
        };
        if (flag) {
            updateObj[`players.${this.myPlayerId}`] = {
                id: this.myPlayerId,
                name: hostName,
                isHost: true,
                grade: '선생님',
                classNum: 0,
                studentNum: 0,
                avatarSkin: 'runner_teacher',
                m: 0,
                checkpoint: 0,
                combo: 0,
                items: [],
                activeBuffs: { boostShoes: false, shield: false, starDash: false },
                isFinished: false,
                finishRank: 0,
                isOut: false,
                joinedAt: Date.now()
            };
        } else {
            updateObj[`players.${this.myPlayerId}`] = window.firebase.firestore.FieldValue.delete();
        }
        await roomRef.update(updateObj);
    }

    // 4. 호스트 전용: 게임 시작
    async startHostGame() {
        if (this.isMock) {
            this.roomData.status = 'playing';
            this.roomData.startedAt = Date.now();
            this.roomData.taggerState = 'CHANTING';
            this.roomData.taggerSpeed = 1.0;
            this.roomData.taggerTurnCount = 1;
            this.notifyStateChange(this.roomData);
            return;
        }
        if (!this.isHost || !this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);
        await roomRef.update({
            status: 'playing',
            startedAt: Date.now(),
            taggerState: 'CHANTING',
            taggerSpeed: 1.0,
            taggerTurnCount: 1
        });
    }

    // 5. 호스트 전용: 술래 상태 브로드캐스트
    async broadcastTaggerState(state, speed = 1.0, turnCount = 0) {
        if (!this.isHost || !this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);
        await roomRef.update({
            taggerState: state, // CHANTING | TURNING | SCANNING
            taggerSpeed: speed,
            taggerTurnCount: turnCount
        });
    }

    // 6. 호스트 전용: 남은 시간 동기화
    async syncRemainingTime(sec) {
        if (!this.isHost || !this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);
        await roomRef.update({
            remainingTime: Math.max(0, sec)
        });
    }

    // 7. 학생: 퀴즈 정답 맞히고 앞으로 전진
    async advancePlayer(metersToAdd, newBuffs = null) {
        if (!this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);

        const curPlayer = this.roomData && this.roomData.players ? this.roomData.players[this.myPlayerId] : null;
        if (!curPlayer || curPlayer.isFinished) return;

        const currentM = curPlayer.m || 0;
        let nextM = Math.min(100, currentM + metersToAdd);

        // 체크포인트 갱신 (25m, 50m, 75m)
        let nextCheckpoint = curPlayer.checkpoint || 0;
        if (nextM >= 75) nextCheckpoint = Math.max(nextCheckpoint, 75);
        else if (nextM >= 50) nextCheckpoint = Math.max(nextCheckpoint, 50);
        else if (nextM >= 25) nextCheckpoint = Math.max(nextCheckpoint, 25);

        const updateObj = {};
        updateObj[`players.${this.myPlayerId}.m`] = nextM;
        updateObj[`players.${this.myPlayerId}.checkpoint`] = nextCheckpoint;
        updateObj[`players.${this.myPlayerId}.combo`] = (curPlayer.combo || 0) + 1;

        if (newBuffs) {
            updateObj[`players.${this.myPlayerId}.activeBuffs`] = newBuffs;
        }

        // 100m 골인 판정!
        if (nextM >= 100 && !curPlayer.isFinished) {
            const currentFinishers = (this.roomData && this.roomData.finishers) ? this.roomData.finishers : [];
            const myRank = currentFinishers.length + 1;
            const finishRecord = {
                id: this.myPlayerId,
                name: curPlayer.name,
                rank: myRank,
                finishTime: Date.now()
            };
            updateObj[`players.${this.myPlayerId}.isFinished`] = true;
            updateObj[`players.${this.myPlayerId}.finishRank`] = myRank;
            updateObj['finishers'] = window.firebase.firestore.FieldValue.arrayUnion(finishRecord);
        }

        await roomRef.update(updateObj);
        return nextM;
    }

    // 8. 학생: 퀴즈 오답 시 직전 체크포인트(또는 출발선 0m 원위치)로 강제 후퇴!
    async resetPlayerToCheckpoint() {
        if (!this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);

        const curPlayer = this.roomData && this.roomData.players ? this.roomData.players[this.myPlayerId] : null;
        if (!curPlayer || curPlayer.isFinished) return;

        // 방어막 쉴드가 있으면 1회 세이프 (후퇴 방어)
        if (curPlayer.activeBuffs && curPlayer.activeBuffs.shield) {
            const updateObj = {};
            updateObj[`players.${this.myPlayerId}.activeBuffs.shield`] = false;
            await roomRef.update(updateObj);
            return { defended: true, m: curPlayer.m };
        }

        const safePos = curPlayer.checkpoint || 0; // 안전 체크포인트 (0, 25, 50, 75m)
        const updateObj = {};
        updateObj[`players.${this.myPlayerId}.m`] = safePos;
        updateObj[`players.${this.myPlayerId}.combo`] = 0;

        await roomRef.update(updateObj);
        return { defended: false, m: safePos };
    }

    // 9. 필드 아이템 획득
    async claimFieldItem(itemId, itemType) {
        if (!this.currentRoomId || !this.db) return;
        const roomRef = this.db.collection('mugunghwa_rooms').doc(this.currentRoomId);
        const curPlayer = this.roomData && this.roomData.players ? this.roomData.players[this.myPlayerId] : null;
        if (!curPlayer) return;

        const updateObj = {};
        if (itemType === 'shoes') {
            updateObj[`players.${this.myPlayerId}.activeBuffs.boostShoes`] = true;
        } else if (itemType === 'shield') {
            updateObj[`players.${this.myPlayerId}.activeBuffs.shield`] = true;
        } else if (itemType === 'star') {
            updateObj[`players.${this.myPlayerId}.activeBuffs.starDash`] = true;
        } else if (itemType === 'time') {
            // 시간 보너스: 호스트/방 전체에 +10초 추가
            const curRemain = this.roomData.remainingTime || 60;
            updateObj['remainingTime'] = curRemain + 10;
        }

        await roomRef.update(updateObj);
    }

    // 10. 방 퇴장
    leaveRoom() {
        if (this.roomUnsubscribe) {
            this.roomUnsubscribe();
            this.roomUnsubscribe = null;
        }
        this.currentRoomId = null;
        this.roomData = null;
        this.isHost = false;
    }
}

window.MugunghwaMultiplayer = new MugunghwaMultiplayer();
