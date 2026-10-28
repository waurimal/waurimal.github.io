/**
 * 와우리말 영어 윷놀이 - 실시간 온라인 멀티플레이 엔진 (Firebase Firestore)
 * 2026 Eui Elementary School - Waurimal Project
 */

import { initializeApp } from "https://www.gstatic.com/firebasejs/10.12.2/firebase-app.js";
import { getAuth, signInAnonymously } from "https://www.gstatic.com/firebasejs/10.12.2/firebase-auth.js";
import { 
    getFirestore, doc, setDoc, getDoc, updateDoc, onSnapshot, deleteDoc 
} from "https://www.gstatic.com/firebasejs/10.12.2/firebase-firestore.js";

// 1. Firebase 통합 설정 (Waurimal 2026 표준)
const firebaseConfig = (window.WaurimalFirebase && window.WaurimalFirebase.getConfig) 
    ? window.WaurimalFirebase.getConfig() 
    : {
        apiKey: "AIzaSyDHCnRrVE2dVuJpf77P-1oftGpczR5WDd4",
        authDomain: "eng-activity.firebaseapp.com",
        projectId: "eng-activity",
        storageBucket: "eng-activity.firebasestorage.app",
        messagingSenderId: "528859753713",
        appId: "1:528859753713:web:a1ed4a054380d181c10d50"
    };

const app = initializeApp(firebaseConfig);
const auth = getAuth(app);
const db = getFirestore(app);

// 익명 로그인 보장
signInAnonymously(auth).catch(err => console.warn("[Multiplayer] Anonymous Auth Warning:", err));

// 2. 세션 식별자 생성 및 영구 유지 (새로고침/재접속 지원)
let mySessionId = '';
try {
    mySessionId = localStorage.getItem('yutnori_session_id') || sessionStorage.getItem('yutnori_session_id');
} catch (e) {}

if (!mySessionId) {
    mySessionId = 'user_' + Math.random().toString(36).substring(2, 9);
}
try {
    localStorage.setItem('yutnori_session_id', mySessionId);
    sessionStorage.setItem('yutnori_session_id', mySessionId);
} catch (e) {}

const ROOMS_COLLECTION = 'yutnori_rooms';

class YutnoriMultiplayerManager {
    constructor() {
        this.currentRoomId = null;
        this.roomData = null;
        this.roomUnsubscribe = null;
        this.mySessionId = mySessionId;
        this.isHost = false;
        this.onStateChangeCallbacks = [];
        this.gameEngine = null;
    }

    saveActiveSession(info) {
        if (!info || !info.roomId) return;
        try {
            const payload = {
                roomId: String(info.roomId).trim(),
                name: (info.name || '').trim(),
                grade: Number(info.grade) || 6,
                classNum: Number(info.classNum) || 1,
                studentNum: Number(info.studentNum) || 1,
                savedAt: Date.now()
            };
            localStorage.setItem('yutnori_active_multi_session', JSON.stringify(payload));
        } catch (e) {
            console.warn("[Multiplayer] saveActiveSession error:", e);
        }
    }

    clearActiveSession() {
        try {
            localStorage.removeItem('yutnori_active_multi_session');
        } catch (e) {}
    }

    getActiveSession() {
        try {
            const raw = localStorage.getItem('yutnori_active_multi_session');
            if (!raw) return null;
            const parsed = JSON.parse(raw);
            if (!parsed || !parsed.roomId) return null;
            // 2시간 이내 세션만 유효
            if (Date.now() - parsed.savedAt > 2 * 60 * 60 * 1000) {
                this.clearActiveSession();
                return null;
            }
            return parsed;
        } catch (e) {
            return null;
        }
    }

    async checkRoomActive(code) {
        const cleanCode = (code || '').trim();
        if (!cleanCode || cleanCode.length !== 4) return null;
        try {
            const roomRef = doc(db, ROOMS_COLLECTION, cleanCode);
            const snap = await getDoc(roomRef);
            if (!snap.exists()) return null;
            return snap.data();
        } catch (e) {
            console.warn("[Multiplayer] checkRoomActive warning:", e);
            return null;
        }
    }

    setGameEngine(engine) {
        this.gameEngine = engine;
    }

    onStateChange(cb) {
        this.onStateChangeCallbacks.push(cb);
    }

    notifyStateChange(data) {
        this.roomData = data;
        this.onStateChangeCallbacks.forEach(cb => {
            try { cb(data); } catch (e) { console.error(e); }
        });
    }

    getMyPlayerInfo() {
        if (!this.roomData || !this.roomData.players) return null;
        return this.roomData.players[this.mySessionId] || null;
    }

    isMyTurn() {
        if (!this.roomData || this.roomData.status !== 'playing') return false;
        return this.roomData.currentTurnId === this.mySessionId;
    }

    generateRoomCode() {
        return Math.floor(1000 + Math.random() * 9000).toString();
    }

    // ==========================================
    // 1. 방 만들기 (Host)
    // ==========================================
    async createRoom({ hostName, grade = 6, classNum = 1, studentNum = 1, mode = '1v1', pieceCount = 4, gradeFilter = 'all' }) {
        const cleanName = (hostName || '').trim();
        if (!cleanName) {
            throw new Error("닉네임 대신 학생 본인의 실명을 입력해주세요.");
        }
        const code = this.generateRoomCode();
        const roomRef = doc(db, ROOMS_COLLECTION, code);

        const initialRoom = {
            roomId: code,
            status: 'lobby', // 'lobby' | 'playing' | 'finished'
            mode: mode, // '1v1' | 'team'
            pieceCount: pieceCount,
            grade: gradeFilter,
            hostId: this.mySessionId,
            players: {
                [this.mySessionId]: {
                    id: this.mySessionId,
                    name: cleanName,
                    grade: Number(grade) || 6,
                    classNum: Number(classNum) || 1,
                    studentNum: Number(studentNum) || 1,
                    team: 'blue', // 청팀 기본 배정
                    isHost: true,
                    joinedAt: Date.now()
                }
            },
            turnOrder: [this.mySessionId],
            currentTurnIndex: 0,
            currentTurnId: this.mySessionId,
            currentTeam: 'blue',
            
            // 인게임 동기화 데이터
            gameState: 'IDLE',
            currentRoll: null,
            sticks: [true, true, true, true],
            activeQuiz: null,
            quizResult: null,
            teamsState: null,
            winner: null,
            lastAction: {
                type: 'CREATE_ROOM',
                actorId: this.mySessionId,
                timestamp: Date.now()
            },
            createdAt: Date.now(),
            updatedAt: Date.now()
        };

        await setDoc(roomRef, initialRoom);
        this.currentRoomId = code;
        this.isHost = true;
        this.listenToRoom(code);
        this.saveActiveSession({
            roomId: code,
            name: cleanName,
            grade: Number(grade) || 6,
            classNum: Number(classNum) || 1,
            studentNum: Number(studentNum) || 1
        });
        return code;
    }

    // ==========================================
    // 2. 방 참여하기 (Join) - 튕김 발생 시 정보 일치 재접속 지원
    // ==========================================
    async joinRoom(code, playerInput) {
        let cleanName = '';
        let sGrade = 6;
        let sClass = 1;
        let sNum = 1;

        if (typeof playerInput === 'string') {
            cleanName = (playerInput || '').trim();
        } else if (playerInput && typeof playerInput === 'object') {
            cleanName = (playerInput.name || '').trim();
            sGrade = Number(playerInput.grade) || 6;
            sClass = Number(playerInput.classNum) || 1;
            sNum = Number(playerInput.studentNum) || 1;
        }

        if (!cleanName) {
            throw new Error("닉네임 대신 학생 본인의 실명을 입력해주세요.");
        }

        const cleanCode = (code || '').trim();
        if (cleanCode.length !== 4) {
            throw new Error("4자리 숫자 방 코드를 정확히 입력해주세요.");
        }

        const roomRef = doc(db, ROOMS_COLLECTION, cleanCode);
        const snap = await getDoc(roomRef);

        if (!snap.exists()) {
            this.clearActiveSession();
            throw new Error("존재하지 않는 방 코드이거나 이미 종료된 방입니다.");
        }

        const data = snap.data();
        const existingPlayers = Object.values(data.players || {});

        // 1. 정보 일치 플레이어 탐색 (세션 ID 또는 실명 + 학적)
        // 1-1) 세션 ID 일치 확인 (새로고침 등 동일 브라우저 탭 세션)
        let matchedPlayer = data.players ? data.players[this.mySessionId] : null;

        // 1-2) 세션 ID가 다르더라도 학생 본인의 정보가 일치하는지 확인 (튕긴 후 브라우저 재시작 등)
        if (!matchedPlayer && cleanName) {
            // (1) 실명 + 학년 + 반 일치 (번호는 둘 다 있을 때만 비교)
            matchedPlayer = existingPlayers.find(p => {
                if (!p || !p.name) return false;
                const isNameMatch = p.name.trim().toLowerCase() === cleanName.toLowerCase();
                if (!isNameMatch) return false;

                const isGradeMatch = (!p.grade || !sGrade || Number(p.grade) === Number(sGrade));
                const isClassMatch = (!p.classNum || !sClass || Number(p.classNum) === Number(sClass));
                const isNumMatch = (!p.studentNum || !sNum || Number(p.studentNum) === Number(sNum));

                return isGradeMatch && isClassMatch && isNumMatch;
            });

            // (2) 학적 정보가 일부 다르더라도, 해당 방에 동일한 실명을 가진 플레이어가 단 1명뿐인 경우 매칭
            if (!matchedPlayer) {
                const sameNameList = existingPlayers.filter(p => p && p.name && p.name.trim().toLowerCase() === cleanName.toLowerCase());
                if (sameNameList.length === 1) {
                    matchedPlayer = sameNameList[0];
                }
            }
        }

        // ==========================================
        // 2-A. 게임이 이미 진행 중(playing)인 경우 - 튕긴 참가자 재입장
        // ==========================================
        if (data.status === 'playing') {
            if (!matchedPlayer) {
                throw new Error("현재 게임이 진행 중인 방입니다. 기존 참가자의 정보(실명 및 학적)와 일치해야 재입장할 수 있습니다.");
            }

            // 기존 튕긴 참가자 재입장(Rejoin) 처리
            const oldPlayerId = matchedPlayer.id;
            const isIdChanged = (oldPlayerId !== this.mySessionId);

            const updatedPlayers = { ...data.players };
            let updatedTurnOrder = Array.isArray(data.turnOrder) ? [...data.turnOrder] : [];
            let updatedHostId = data.hostId;
            let updatedTurnId = data.currentTurnId;

            if (isIdChanged) {
                delete updatedPlayers[oldPlayerId];
                updatedPlayers[this.mySessionId] = {
                    ...matchedPlayer,
                    id: this.mySessionId,
                    name: cleanName || matchedPlayer.name,
                    grade: sGrade || matchedPlayer.grade,
                    classNum: sClass || matchedPlayer.classNum,
                    studentNum: sNum || matchedPlayer.studentNum,
                    lastSeen: Date.now(),
                    reconnectedAt: Date.now()
                };

                updatedTurnOrder = updatedTurnOrder.map(uid => uid === oldPlayerId ? this.mySessionId : uid);

                if (data.hostId === oldPlayerId) {
                    updatedHostId = this.mySessionId;
                }
                if (data.currentTurnId === oldPlayerId) {
                    updatedTurnId = this.mySessionId;
                }
            } else {
                updatedPlayers[this.mySessionId] = {
                    ...matchedPlayer,
                    name: cleanName || matchedPlayer.name,
                    grade: sGrade || matchedPlayer.grade,
                    classNum: sClass || matchedPlayer.classNum,
                    studentNum: sNum || matchedPlayer.studentNum,
                    lastSeen: Date.now(),
                    reconnectedAt: Date.now()
                };
            }

            await updateDoc(roomRef, {
                players: updatedPlayers,
                turnOrder: updatedTurnOrder,
                hostId: updatedHostId,
                currentTurnId: updatedTurnId,
                lastAction: {
                    type: 'PLAYER_REJOIN',
                    actorId: this.mySessionId,
                    playerName: matchedPlayer.name,
                    team: matchedPlayer.team,
                    timestamp: Date.now()
                },
                updatedAt: Date.now()
            });

            this.currentRoomId = cleanCode;
            this.isHost = (updatedHostId === this.mySessionId);
            this.listenToRoom(cleanCode);

            this.saveActiveSession({
                roomId: cleanCode,
                name: cleanName,
                grade: sGrade,
                classNum: sClass,
                studentNum: sNum
            });

            return cleanCode;
        }

        // ==========================================
        // 2-B. 게임이 이미 종료(finished)된 경우
        // ==========================================
        if (data.status === 'finished') {
            if (!matchedPlayer) {
                throw new Error("이미 종료된 방입니다.");
            }
            this.currentRoomId = cleanCode;
            this.isHost = (data.hostId === this.mySessionId);
            this.listenToRoom(cleanCode);
            return cleanCode;
        }

        // ==========================================
        // 2-C. 대기실(lobby) 상태인 경우
        // ==========================================
        const updatedPlayers = { ...data.players };
        let updatedTurnOrder = Array.isArray(data.turnOrder) ? [...data.turnOrder] : [];
        let updatedHostId = data.hostId;

        if (matchedPlayer) {
            // 이미 등록되어 있던 참가자의 세션 재접속 / 정보 갱신
            const oldPlayerId = matchedPlayer.id;
            if (oldPlayerId !== this.mySessionId) {
                delete updatedPlayers[oldPlayerId];
                updatedPlayers[this.mySessionId] = {
                    ...matchedPlayer,
                    id: this.mySessionId,
                    name: cleanName,
                    grade: sGrade,
                    classNum: sClass,
                    studentNum: sNum,
                    lastSeen: Date.now()
                };
                updatedTurnOrder = updatedTurnOrder.map(uid => uid === oldPlayerId ? this.mySessionId : uid);
                if (data.hostId === oldPlayerId) {
                    updatedHostId = this.mySessionId;
                }
            } else {
                updatedPlayers[this.mySessionId] = {
                    ...matchedPlayer,
                    name: cleanName,
                    grade: sGrade,
                    classNum: sClass,
                    studentNum: sNum,
                    lastSeen: Date.now()
                };
            }
        } else {
            // 신규 참가자 입장
            if (data.mode === '1v1' && existingPlayers.length >= 2) {
                throw new Error("1:1 대전 방 정원(2명)이 이미 찼습니다.");
            }
            if (data.mode === 'team' && existingPlayers.length >= 8) {
                throw new Error("팀전 방 정원(최대 8명)이 찼습니다.");
            }

            // 팀 균형 배정: 청팀 수와 백팀 수 비교하여 적은 쪽에 자동 배정
            let blueCount = 0;
            let whiteCount = 0;
            existingPlayers.forEach(p => {
                if (p.team === 'blue') blueCount++;
                else whiteCount++;
            });
            const assignedTeam = (blueCount <= whiteCount) ? 'blue' : 'white';

            updatedPlayers[this.mySessionId] = {
                id: this.mySessionId,
                name: cleanName,
                grade: sGrade,
                classNum: sClass,
                studentNum: sNum,
                team: assignedTeam,
                isHost: false,
                joinedAt: Date.now(),
                lastSeen: Date.now()
            };
            if (!updatedTurnOrder.includes(this.mySessionId)) {
                updatedTurnOrder.push(this.mySessionId);
            }
        }

        await updateDoc(roomRef, {
            players: updatedPlayers,
            turnOrder: updatedTurnOrder,
            hostId: updatedHostId,
            updatedAt: Date.now()
        });

        this.currentRoomId = cleanCode;
        this.isHost = (updatedHostId === this.mySessionId);
        this.listenToRoom(cleanCode);

        this.saveActiveSession({
            roomId: cleanCode,
            name: cleanName,
            grade: sGrade,
            classNum: sClass,
            studentNum: sNum
        });

        return cleanCode;
    }

    // ==========================================
    // 3. 팀 변경 (청팀 <-> 백팀)
    // ==========================================
    async switchTeam() {
        if (!this.currentRoomId || !this.roomData) return;
        const myInfo = this.getMyPlayerInfo();
        if (!myInfo) return;

        const nextTeam = (myInfo.team === 'blue') ? 'white' : 'blue';
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);

        await updateDoc(roomRef, {
            [`players.${this.mySessionId}.team`]: nextTeam,
            updatedAt: Date.now()
        });
    }

    // ==========================================
    // 4. 게임 시작 (방장 전용)
    // ==========================================
    async startOnlineGame() {
        if (!this.currentRoomId || !this.roomData) return;
        if (this.roomData.hostId !== this.mySessionId) {
            throw new Error("방장만 게임을 시작할 수 있습니다.");
        }

        const players = Object.values(this.roomData.players || {});
        const bluePlayers = players.filter(p => p.team === 'blue');
        const whitePlayers = players.filter(p => p.team === 'white');

        if (bluePlayers.length === 0 || whitePlayers.length === 0) {
            throw new Error("청팀과 백팀에 각각 최소 1명 이상 참가해야 시작할 수 있습니다.");
        }

        // 턴 오더 구성: 청팀 1번 -> 백팀 1번 -> 청팀 2번 -> 백팀 2번 교차
        const orderedTurnIds = [];
        const maxLen = Math.max(bluePlayers.length, whitePlayers.length);
        for (let i = 0; i < maxLen; i++) {
            if (bluePlayers[i]) orderedTurnIds.push(bluePlayers[i].id);
            if (whitePlayers[i]) orderedTurnIds.push(whitePlayers[i].id);
        }

        const pieceCount = this.roomData.pieceCount || 4;
        
        // 윷놀이 팀 및 말 상태 초기화
        const initialTeamsState = [
            {
                id: 0,
                key: 'blue',
                name: '청팀',
                pieces: Array.from({ length: pieceCount }, (_, i) => ({
                    id: i,
                    teamId: 0,
                    team: 'blue',
                    pos: -1,
                    route: 'OUTER',
                    isGoal: false,
                    isHome: true,
                    count: 1,
                    stackedPieces: [],
                    history: []
                }))
            },
            {
                id: 1,
                key: 'white',
                name: '백팀',
                pieces: Array.from({ length: pieceCount }, (_, i) => ({
                    id: i,
                    teamId: 1,
                    team: 'white',
                    pos: -1,
                    route: 'OUTER',
                    isGoal: false,
                    isHome: true,
                    count: 1,
                    stackedPieces: [],
                    history: []
                }))
            }
        ];

        const firstPlayerId = orderedTurnIds[0];
        const firstTeam = this.roomData.players[firstPlayerId].team;

        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);
        await updateDoc(roomRef, {
            status: 'playing',
            turnOrder: orderedTurnIds,
            currentTurnIndex: 0,
            currentTurnId: firstPlayerId,
            currentTeam: firstTeam,
            gameState: 'WAIT_ROLL',
            currentRoll: null,
            activeQuiz: null,
            quizResult: null,
            teamsState: initialTeamsState,
            winner: null,
            lastAction: {
                type: 'START_GAME',
                actorId: this.mySessionId,
                timestamp: Date.now()
            },
            updatedAt: Date.now()
        });
    }

    // ==========================================
    // 5. 실시간 리스너 구독
    // ==========================================
    listenToRoom(code) {
        if (this.roomUnsubscribe) {
            this.roomUnsubscribe();
            this.roomUnsubscribe = null;
        }

        const roomRef = doc(db, ROOMS_COLLECTION, code);
        this.roomUnsubscribe = onSnapshot(roomRef, (snap) => {
            if (!snap.exists()) {
                console.warn("[Multiplayer] 방이 종료되었거나 삭제되었습니다.");
                this.leaveRoom(false);
                if (window.showToast) {
                    window.showToast("알림", "방이 종료되었습니다.", "⚠️");
                }
                return;
            }

            const data = snap.data();
            this.isHost = (data.hostId === this.mySessionId);
            this.notifyStateChange(data);
        }, (err) => {
            console.error("[Multiplayer] Snapshot Error:", err);
        });
    }

    // ==========================================
    // 6. 인게임 액션 송신 (내 차례일 때만)
    // ==========================================

    /**
     * 윷 던지기 결과 및 퀴즈 출제 동기화
     */
    async sendRoll(rollResult, sticks, quizData) {
        if (!this.currentRoomId || !this.isMyTurn()) return;
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);

        try {
            await updateDoc(roomRef, {
                gameState: 'WAIT_PIECE_SELECT',
                currentRoll: rollResult,
                sticks: sticks,
                activeQuiz: quizData || null,
                quizResult: null,
                lastAction: {
                    type: 'ROLL_AND_QUIZ',
                    actorId: this.mySessionId,
                    rollResult: rollResult,
                    timestamp: Date.now()
                },
                updatedAt: Date.now()
            });
        } catch (e) {
            console.error("[Multiplayer] sendRoll Error:", e);
        }
    }

    /**
     * 퀴즈 풀이 결과 동기화
     */
    async sendQuizResult(isCorrect, explanation, answer) {
        if (!this.currentRoomId || !this.isMyTurn()) return;
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);

        try {
            if (isCorrect) {
                // 정답: 말 이동 진행 대기
                await updateDoc(roomRef, {
                    gameState: 'WAIT_MOVE',
                    quizResult: {
                        isCorrect: true,
                        explanation: explanation || '',
                        answer: answer || ''
                    },
                    lastAction: {
                        type: 'QUIZ_CORRECT',
                        actorId: this.mySessionId,
                        timestamp: Date.now()
                    },
                    updatedAt: Date.now()
                });
            } else {
                // 오답: 피드백 표시 후 턴 종료 대기 (턴 넘김은 feedback 닫힘 및 endTurn 시 일괄 동기화)
                await updateDoc(roomRef, {
                    gameState: 'WAIT_QUIZ_FEEDBACK',
                    quizResult: {
                        isCorrect: false,
                        explanation: explanation || '',
                        answer: answer || ''
                    },
                    lastAction: {
                        type: 'QUIZ_WRONG',
                        actorId: this.mySessionId,
                        timestamp: Date.now()
                    },
                    updatedAt: Date.now()
                });
            }
        } catch (e) {
            console.error("[Multiplayer] sendQuizResult Error:", e);
        }
    }

    /**
     * 말 이동 및 턴 종료 / 추가 턴 동기화
     */
    async sendPieceMovement({ teamsState, hasExtraTurn, winnerTeam, pieceId, movePath, caughtEnemy, caughtCount }) {
        if (!this.currentRoomId || !this.isMyTurn()) return;
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);
        const currentStats = (this.gameEngine && this.gameEngine.matchStats) ? this.gameEngine.matchStats : null;

        try {
            if (winnerTeam) {
                // 경기 종료
                await updateDoc(roomRef, {
                    status: 'finished',
                    gameState: 'GAME_OVER',
                    teamsState: teamsState,
                    matchStats: currentStats,
                    winner: winnerTeam,
                    lastAction: {
                        type: 'GAME_OVER',
                        actorId: this.mySessionId,
                        winnerTeam: winnerTeam,
                        timestamp: Date.now()
                    },
                    updatedAt: Date.now()
                });
                return;
            }

            if (hasExtraTurn) {
                // 윷, 모 또는 말 잡기로 인한 추가 턴 유지!
                await updateDoc(roomRef, {
                    gameState: 'WAIT_ROLL',
                    teamsState: teamsState,
                    matchStats: currentStats,
                    currentRoll: null,
                    activeQuiz: null,
                    quizResult: null,
                    lastAction: {
                        type: caughtEnemy ? 'CAUGHT_PIECE' : 'EXTRA_TURN',
                        actorId: this.mySessionId,
                        caughtCount: caughtCount || 1,
                        timestamp: Date.now()
                    },
                    updatedAt: Date.now()
                });
            } else {
                // 일반 이동 또는 오답 종료 -> 다음 차례로 턴 넘김
                const nextTurnInfo = this.calculateNextTurn(true);
                await updateDoc(roomRef, {
                    gameState: 'WAIT_ROLL',
                    teamsState: teamsState,
                    matchStats: currentStats,
                    currentRoll: null,
                    activeQuiz: null,
                    quizResult: null,
                    ...nextTurnInfo,
                    lastAction: {
                        type: 'END_TURN',
                        actorId: this.mySessionId,
                        timestamp: Date.now()
                    },
                    updatedAt: Date.now()
                });
            }
        } catch (e) {
            console.error("[Multiplayer] sendPieceMovement Error:", e);
        }
    }

    /**
     * 다음 플레이어로 턴 계산
     */
    calculateNextTurn(isAfterMove = true) {
        if (!this.roomData || !this.roomData.turnOrder || this.roomData.turnOrder.length === 0) {
            return {};
        }

        const turnOrder = this.roomData.turnOrder;
        const nextIdx = (this.roomData.currentTurnIndex + 1) % turnOrder.length;
        const nextUid = turnOrder[nextIdx];
        const nextPlayer = this.roomData.players ? this.roomData.players[nextUid] : null;
        const nextTeam = nextPlayer ? nextPlayer.team : 'blue';

        return {
            currentTurnIndex: nextIdx,
            currentTurnId: nextUid,
            currentTeam: nextTeam
        };
    }

    // ==========================================
    // 7. 방 나가기
    // ==========================================
    async leaveRoom(shouldDeleteIfHost = true) {
        this.clearActiveSession();

        if (this.roomUnsubscribe) {
            this.roomUnsubscribe();
            this.roomUnsubscribe = null;
        }

        if (this.currentRoomId) {
            const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);
            try {
                if (this.isHost && shouldDeleteIfHost) {
                    await deleteDoc(roomRef);
                } else if (this.roomData && this.roomData.players) {
                    const remainingPlayers = { ...this.roomData.players };
                    delete remainingPlayers[this.mySessionId];
                    const remainingTurnOrder = (this.roomData.turnOrder || []).filter(id => id !== this.mySessionId);
                    await updateDoc(roomRef, {
                        players: remainingPlayers,
                        turnOrder: remainingTurnOrder,
                        updatedAt: Date.now()
                    });
                }
            } catch (e) {
                console.warn("[Multiplayer] Error leaving room:", e);
            }
        }

        this.currentRoomId = null;
        this.roomData = null;
        this.isHost = false;
        this.notifyStateChange(null);
    }
}

export const multiplayerManager = new YutnoriMultiplayerManager();
window.YutnoriMultiplayer = multiplayerManager;
