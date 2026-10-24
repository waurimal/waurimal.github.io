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

// 2. 세션 식별자 생성 및 유지 (새로고침 시 재접속 지원)
let mySessionId = sessionStorage.getItem('yutnori_session_id');
if (!mySessionId) {
    mySessionId = 'user_' + Math.random().toString(36).substring(2, 9);
    sessionStorage.setItem('yutnori_session_id', mySessionId);
}

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
        return code;
    }

    // ==========================================
    // 2. 방 참여하기 (Join)
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
            throw new Error("존재하지 않는 방 코드입니다. 코드를 다시 확인해주세요.");
        }

        const data = snap.data();
        if (data.status !== 'lobby') {
            throw new Error("이미 게임이 진행 중이거나 종료된 방입니다.");
        }

        const existingPlayers = Object.values(data.players || {});
        if (data.mode === '1v1' && existingPlayers.length >= 2 && !data.players[this.mySessionId]) {
            throw new Error("1:1 대전 방 정원(2명)이 이미 찼습니다.");
        }
        if (data.mode === 'team' && existingPlayers.length >= 8 && !data.players[this.mySessionId]) {
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

        const updatedPlayers = { ...data.players };
        const updatedTurnOrder = Array.isArray(data.turnOrder) ? [...data.turnOrder] : [];

        if (!updatedPlayers[this.mySessionId]) {
            updatedPlayers[this.mySessionId] = {
                id: this.mySessionId,
                name: cleanName,
                grade: sGrade,
                classNum: sClass,
                studentNum: sNum,
                team: assignedTeam,
                isHost: false,
                joinedAt: Date.now()
            };
            if (!updatedTurnOrder.includes(this.mySessionId)) {
                updatedTurnOrder.push(this.mySessionId);
            }
        } else {
            // 이미 등록되어 있던 유저가 재접속한 경우 정보 갱신
            updatedPlayers[this.mySessionId].name = cleanName;
            updatedPlayers[this.mySessionId].grade = sGrade;
            updatedPlayers[this.mySessionId].classNum = sClass;
            updatedPlayers[this.mySessionId].studentNum = sNum;
        }

        await updateDoc(roomRef, {
            players: updatedPlayers,
            turnOrder: updatedTurnOrder,
            updatedAt: Date.now()
        });

        this.currentRoomId = cleanCode;
        this.isHost = (data.hostId === this.mySessionId);
        this.listenToRoom(cleanCode);
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
                key: 'blue',
                name: '청팀',
                pieces: Array.from({ length: pieceCount }, (_, i) => ({
                    id: i,
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
                key: 'white',
                name: '백팀',
                pieces: Array.from({ length: pieceCount }, (_, i) => ({
                    id: i,
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

        await updateDoc(roomRef, {
            gameState: 'WAIT_QUIZ',
            currentRoll: rollResult,
            sticks: sticks,
            activeQuiz: quizData,
            quizResult: null,
            lastAction: {
                type: 'ROLL_AND_QUIZ',
                actorId: this.mySessionId,
                rollResult: rollResult,
                timestamp: Date.now()
            },
            updatedAt: Date.now()
        });
    }

    /**
     * 퀴즈 풀이 결과 동기화
     */
    async sendQuizResult(isCorrect, explanation, answer) {
        if (!this.currentRoomId || !this.isMyTurn()) return;
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);

        if (isCorrect) {
            // 정답: 말 선택 대기(WAIT_MOVE)로 전환
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
            // 오답: 피드백 표시 후 턴 넘김 준비
            const nextTurnInfo = this.calculateNextTurn(false);
            await updateDoc(roomRef, {
                gameState: 'FEEDBACK',
                quizResult: {
                    isCorrect: false,
                    explanation: explanation || '',
                    answer: answer || ''
                },
                ...nextTurnInfo,
                lastAction: {
                    type: 'QUIZ_WRONG',
                    actorId: this.mySessionId,
                    timestamp: Date.now()
                },
                updatedAt: Date.now()
            });
        }
    }

    /**
     * 말 이동 및 턴 종료 / 추가 턴 동기화
     */
    async sendPieceMovement({ teamsState, hasExtraTurn, winnerTeam, pieceId, movePath }) {
        if (!this.currentRoomId || !this.isMyTurn()) return;
        const roomRef = doc(db, ROOMS_COLLECTION, this.currentRoomId);

        if (winnerTeam) {
            // 경기 종료
            await updateDoc(roomRef, {
                status: 'finished',
                gameState: 'GAME_OVER',
                teamsState: teamsState,
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
                currentRoll: null,
                activeQuiz: null,
                quizResult: null,
                lastAction: {
                    type: 'EXTRA_TURN',
                    actorId: this.mySessionId,
                    timestamp: Date.now()
                },
                updatedAt: Date.now()
            });
        } else {
            // 일반 이동 완료 -> 다음 차례로 턴 넘김
            const nextTurnInfo = this.calculateNextTurn(true);
            await updateDoc(roomRef, {
                gameState: 'WAIT_ROLL',
                teamsState: teamsState,
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
