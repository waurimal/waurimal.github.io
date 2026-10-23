/**
 * 와우리말 영어 윷놀이 (English Yutnori Challenge) - 메인 게임 엔진
 * 2026 Eui Elementary School - Waurimal Project
 */

(function(window) {
    'use strict';

    // ==========================================
    // 1. 윷판 토폴로지 & 좌표 정의 (29개 노드)
    // ==========================================
    // 0: 참먹이(시작점/도착점) ~ 19: 외곽 하변 끝
    // 20, 21: 우상 모서리(5) -> 중앙(22)
    // 22: 중앙 방
    // 23, 24: 중앙(22) -> 좌하 모서리(15)
    // 25, 26: 좌상 모서리(10) -> 중앙(22)
    // 27, 28: 중앙(22) -> 참먹이(0) (최단 지름길 출구)

    const BOARD_NODES = {
        // 외곽 둘레 (우하 0번 -> 반시계 방향)
        0:  { id: 0,  x: 88, y: 88, name: "참먹이(출발/도착)", isCorner: false, isStart: true },
        1:  { id: 1,  x: 88, y: 72, name: "도밭", isCorner: false },
        2:  { id: 2,  x: 88, y: 56, name: "개밭", isCorner: false },
        3:  { id: 3,  x: 88, y: 40, name: "걸밭", isCorner: false },
        4:  { id: 4,  x: 88, y: 24, name: "윷밭", isCorner: false },
        5:  { id: 5,  x: 88, y: 12, name: "앞밭(첫 모서리)", isCorner: true }, // 지름길 1
        6:  { id: 6,  x: 72, y: 12, name: "여섯", isCorner: false },
        7:  { id: 7,  x: 56, y: 12, name: "일곱", isCorner: false },
        8:  { id: 8,  x: 40, y: 12, name: "여덟", isCorner: false },
        9:  { id: 9,  x: 24, y: 12, name: "아홉", isCorner: false },
        10: { id: 10, x: 12, y: 12, name: "북밭(두번째 모서리)", isCorner: true }, // 지름길 2
        11: { id: 11, x: 12, y: 24, name: "열하나", isCorner: false },
        12: { id: 12, x: 12, y: 40, name: "열둘", isCorner: false },
        13: { id: 13, x: 12, y: 56, name: "열셋", isCorner: false },
        14: { id: 14, x: 12, y: 72, name: "열넷", isCorner: false },
        15: { id: 15, x: 12, y: 88, name: "서밭(세번째 모서리)", isCorner: true },
        16: { id: 16, x: 24, y: 88, name: "열여섯", isCorner: false },
        17: { id: 17, x: 40, y: 88, name: "열일곱", isCorner: false },
        18: { id: 18, x: 56, y: 88, name: "열여덟", isCorner: false },
        19: { id: 19, x: 72, y: 88, name: "날밭 앞", isCorner: false },

        // 대각선 1: 우상(5) -> 중앙(22) -> 좌하(15)
        20: { id: 20, x: 76, y: 24, name: "대각선1-1", isCorner: false },
        21: { id: 21, x: 63, y: 37, name: "대각선1-2", isCorner: false },
        22: { id: 22, x: 50, y: 50, name: "방(중앙)", isCenter: true },
        23: { id: 23, x: 37, y: 63, name: "대각선1-3", isCorner: false },
        24: { id: 24, x: 24, y: 76, name: "대각선1-4", isCorner: false },

        // 대각선 2: 좌상(10) -> 중앙(22)
        25: { id: 25, x: 24, y: 24, name: "대각선2-1", isCorner: false },
        26: { id: 26, x: 37, y: 37, name: "대각선2-2", isCorner: false },

        // 대각선 3: 중앙(22) -> 우하(0 참먹이/골인)
        27: { id: 27, x: 63, y: 63, name: "대각선3-1", isCorner: false },
        28: { id: 28, x: 76, y: 76, name: "대각선3-2", isCorner: false }
    };

    // ==========================================
    // 2. 오디오 신디사이저 & TTS 엔진
    // ==========================================
    class SoundEngine {
        constructor() {
            this.ctx = null;
            this.muted = false;
        }

        init() {
            if (!this.ctx) {
                const AudioCtx = window.AudioContext || window.webkitAudioContext;
                if (AudioCtx) this.ctx = new AudioCtx();
            }
            if (this.ctx && this.ctx.state === 'suspended') {
                this.ctx.resume();
            }
        }

        playTone(freq, duration = 0.15, type = 'sine', gainVal = 0.2) {
            if (this.muted) return;
            this.init();
            if (!this.ctx) return;

            try {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                osc.type = type;
                osc.frequency.setValueAtTime(freq, this.ctx.currentTime);

                gain.gain.setValueAtTime(gainVal, this.ctx.currentTime);
                gain.gain.exponentialRampToValueAtTime(0.001, this.ctx.currentTime + duration);

                osc.connect(gain);
                gain.connect(this.ctx.destination);

                osc.start();
                osc.stop(this.ctx.currentTime + duration);
            } catch (e) {
                console.warn("Sound error:", e);
            }
        }

        // 윷 던질 때 나는 경쾌한 소리
        playToss() {
            if (this.muted) return;
            this.init();
            if (!this.ctx) return;
            // 복합 타격음
            for (let i = 0; i < 4; i++) {
                setTimeout(() => {
                    this.playTone(300 + Math.random() * 200, 0.08, 'triangle', 0.25);
                }, i * 60);
            }
        }

        // 윷 바닥에 떨어져 멈출 때
        playLand(result) {
            if (this.muted) return;
            const freqMap = {
                'back_do': 260,
                'do': 330,
                'gae': 392,
                'geol': 440,
                'yut': 523.25,
                'mo': 659.25
            };
            const f = freqMap[result] || 440;
            this.playTone(f, 0.2, 'sine', 0.3);
            if (result === 'yut' || result === 'mo') {
                setTimeout(() => this.playTone(f * 1.25, 0.25, 'triangle', 0.3), 120);
            }
        }

        // 말 한 칸 이동 소리 (따각)
        playStep() {
            this.playTone(520, 0.05, 'square', 0.15);
        }

        // 상대 말 잡기 소리 (쿵! 쾅!)
        playCatch() {
            this.playTone(180, 0.35, 'triangle', 0.4);
            setTimeout(() => this.playTone(120, 0.4, 'sawtooth', 0.3), 80);
        }

        // 정답 소리 (딩동댕 아르페지오)
        playCorrect() {
            const notes = [523.25, 659.25, 783.99, 1046.5]; // C5, E5, G5, C6
            notes.forEach((freq, idx) => {
                setTimeout(() => this.playTone(freq, 0.18, 'sine', 0.25), idx * 80);
            });
        }

        // 오답 소리 (땡/삐익)
        playWrong() {
            this.playTone(311.13, 0.2, 'sawtooth', 0.3); // Eb4
            setTimeout(() => this.playTone(277.18, 0.3, 'sawtooth', 0.3), 150); // Db4
        }

        // 골인 소리
        playGoal() {
            const notes = [440, 554.37, 659.25, 880];
            notes.forEach((freq, idx) => {
                setTimeout(() => this.playTone(freq, 0.2, 'triangle', 0.3), idx * 100);
            });
        }

        // 최종 승리 팡파르
        playVictory() {
            const melody = [
                { f: 523.25, d: 0.15 }, { f: 523.25, d: 0.15 }, { f: 523.25, d: 0.15 },
                { f: 659.25, d: 0.3 }, { f: 783.99, d: 0.3 }, { f: 1046.5, d: 0.6 }
            ];
            let time = 0;
            melody.forEach(m => {
                setTimeout(() => this.playTone(m.f, m.d, 'triangle', 0.35), time * 1000);
                time += m.d;
            });
        }

        // TTS 발음 읽기
        speakText(text) {
            if (!('speechSynthesis' in window) || this.muted) return;
            try {
                window.speechSynthesis.cancel();
                const utter = new SpeechSynthesisUtterance(text);
                utter.lang = 'en-US';
                utter.rate = 0.9;
                window.speechSynthesis.speak(utter);
            } catch (e) {
                console.warn("TTS error:", e);
            }
        }
    }

    const Sound = new SoundEngine();

    // ==========================================
    // 3. 윷놀이 경로 계산 알고리즘
    // ==========================================

    /**
     * 특정 노드에서 한 걸음(step=1) 앞으로 갈 때 다음 노드를 반환
     * @param {number} current - 현재 노드 ID
     * @param {string} route - 말이 진입해 있는 경로 상태 ('OUTER', 'CORNER1', 'CORNER2', 'CENTER_OUT', 'CENTER_CROSS')
     * @param {boolean} hasStarted - 말이 이미 판에 출발한 상태인지 여부
     * @returns {Object} { nextNode, nextRoute, isGoal }
     */
    function stepForward(current, route, hasStarted = true) {
        // 1. 외곽 경로 진행
        if (route === 'OUTER') {
            if (current === 19) {
                // 19 다음은 참먹이(0) 날밭
                return { nextNode: 0, nextRoute: 'OUTER', isGoal: false };
            }
            if (current === 0) {
                if (!hasStarted) {
                    // 이제 막 출발하는 첫 걸음: 0번에서 1번(도밭)으로 진입
                    return { nextNode: 1, nextRoute: 'OUTER', isGoal: false };
                } else {
                    // 한 바퀴 완전히 돌아서 0번에 도달한 후 또 1보 가면 완주 골인!
                    return { nextNode: -1, nextRoute: 'OUTER', isGoal: true };
                }
            }
            // 일반 외곽 (1~18)
            const nextNode = current + 1;
            return { nextNode, nextRoute: 'OUTER', isGoal: false };
        }

        // 2. 코너 1 (우상 모서리 5번에서 중앙으로 진입하는 대각선)
        if (route === 'CORNER1') {
            if (current === 5) return { nextNode: 20, nextRoute: 'CORNER1', isGoal: false };
            if (current === 20) return { nextNode: 21, nextRoute: 'CORNER1', isGoal: false };
            if (current === 21) return { nextNode: 22, nextRoute: 'CORNER1', isGoal: false }; // 방 도달
            // 방을 멈추지 않고 그대로 직진 통과하는 경우: 좌하(15) 방향으로 계속 직진
            if (current === 22) return { nextNode: 23, nextRoute: 'CENTER_CROSS', isGoal: false };
        }

        // 3. 코너 2 (좌상 모서리 10번에서 중앙으로 진입하는 대각선)
        if (route === 'CORNER2') {
            if (current === 10) return { nextNode: 25, nextRoute: 'CORNER2', isGoal: false };
            if (current === 25) return { nextNode: 26, nextRoute: 'CORNER2', isGoal: false };
            if (current === 26) return { nextNode: 22, nextRoute: 'CORNER2', isGoal: false }; // 방 도달
            // 방 통과 시: 우하(참먹이) 방향으로 직진
            if (current === 22) return { nextNode: 27, nextRoute: 'CENTER_OUT', isGoal: false };
        }

        // 4. 중앙 방(22)에서 우하 참먹이 방향(최단 지름길 탈출)으로 진행
        if (route === 'CENTER_OUT') {
            if (current === 22) return { nextNode: 27, nextRoute: 'CENTER_OUT', isGoal: false };
            if (current === 27) return { nextNode: 28, nextRoute: 'CENTER_OUT', isGoal: false };
            if (current === 28) return { nextNode: 0, nextRoute: 'CENTER_OUT', isGoal: false }; // 참먹이 도달
            if (current === 0) return { nextNode: -1, nextRoute: 'CENTER_OUT', isGoal: true }; // 통과 골인!
        }

        // 5. 중앙 방(22)에서 좌하(15) 방향으로 관통하여 진행
        if (route === 'CENTER_CROSS') {
            if (current === 22) return { nextNode: 23, nextRoute: 'CENTER_CROSS', isGoal: false };
            if (current === 23) return { nextNode: 24, nextRoute: 'CENTER_CROSS', isGoal: false };
            if (current === 24) return { nextNode: 15, nextRoute: 'OUTER', isGoal: false }; // 외곽 합류!
        }

        // 기본 안전장치
        return { nextNode: -1, nextRoute: route, isGoal: true };
    }

    /**
     * 윷 결과(도/개/걸/윷/모/빽도)에 따른 최종 이동 경로 목록 및 목적지 노드 계산
     * @param {Object} piece - 말 객체 { id, pos, route, isHome, isGoal, history }
     * @param {string} yutResult - 'back_do', 'do', 'gae', 'geol', 'yut', 'mo'
     * @returns {Object} { path: [nodeIds], finalNode, finalRoute, isGoal, valid }
     */
    function calculateMovePath(piece, yutResult) {
        const stepCounts = {
            'back_do': -1,
            'do': 1,
            'gae': 2,
            'geol': 3,
            'yut': 4,
            'mo': 5
        };

        const steps = stepCounts[yutResult];
        if (steps === undefined) return { valid: false };

        // 1. 빽도(뒷도, -1) 처리
        if (steps === -1) {
            // A. 아직 판에 나오지 않은 대기 중인 말
            if (piece.isHome) {
                // 출발 대기말 빽도: 19번 칸(골인 바로 앞)으로 순간 전진하는 대박 룰!
                return {
                    path: [19],
                    finalNode: 19,
                    finalRoute: 'OUTER',
                    isGoal: false,
                    valid: true,
                    isBackDoTeleport: true
                };
            }

            // B. 판 위에 있는 말: 직전 위치로 1칸 후퇴
            let prevNode = null;
            let prevRoute = piece.route;

            // 히스토리가 있으면 히스토리의 직전 노드 사용
            if (piece.history && piece.history.length > 0) {
                prevNode = piece.history[piece.history.length - 1];
            } else {
                // 기하학적 역추적
                if (piece.pos === 0) prevNode = 19;
                else if (piece.pos === 1) prevNode = 0;
                else if (piece.pos >= 2 && piece.pos <= 19) prevNode = piece.pos - 1;
                else if (piece.pos === 20) { prevNode = 5; prevRoute = 'OUTER'; }
                else if (piece.pos === 21) prevNode = 20;
                else if (piece.pos === 22) {
                    prevNode = (piece.route === 'CORNER2') ? 26 : 21;
                }
                else if (piece.pos === 23) prevNode = 22;
                else if (piece.pos === 24) prevNode = 23;
                else if (piece.pos === 25) { prevNode = 10; prevRoute = 'OUTER'; }
                else if (piece.pos === 26) prevNode = 25;
                else if (piece.pos === 27) prevNode = 22;
                else if (piece.pos === 28) prevNode = 27;
            }

            return {
                path: [prevNode],
                finalNode: prevNode,
                finalRoute: prevRoute,
                isGoal: false,
                valid: true
            };
        }

        // 2. 일반 전진 (도/개/걸/윷/모: 1 ~ 5)
        let currNode = piece.pos;
        let currRoute = piece.route;
        let path = [];
        let isGoal = false;
        let hasStarted = !piece.isHome;

        // 대기 중인 새 말이 첫 출발할 때
        if (piece.isHome) {
            currNode = 0; // 0번 참먹이 기준 출발 준비
            currRoute = 'OUTER';
            hasStarted = false; // 출발 직전 플래그
        } else {
            // 이미 판 위에 있는 말이 분기점(5, 10, 22)에서 새로 턴을 시작할 때 지름길로 분기 결정!
            if (currNode === 5) {
                currRoute = 'CORNER1'; // 우상 모서리에서 중앙으로 꺾음
            } else if (currNode === 10) {
                currRoute = 'CORNER2'; // 좌상 모서리에서 중앙으로 꺾음
            } else if (currNode === 22) {
                currRoute = 'CENTER_OUT'; // 중앙 방에 멈춰있던 말은 무조건 최단 지름길(우하 출구 27, 28)로 꺾음!
            }
        }

        for (let s = 1; s <= steps; s++) {
            const stepResult = stepForward(currNode, currRoute, hasStarted);
            currNode = stepResult.nextNode;
            currRoute = stepResult.nextRoute;
            hasStarted = true; // 1보 딛고 나면 무조건 started 상태가 됨

            if (stepResult.isGoal) {
                isGoal = true;
                path.push(-1); // 완주 골인
                break;
            } else {
                path.push(currNode);
            }
        }

        return {
            path: path,
            finalNode: isGoal ? -1 : currNode,
            finalRoute: currRoute,
            isGoal: isGoal,
            valid: true
        };
    }

    // ==========================================
    // 4. 윷 던지기 확률 및 시뮬레이터
    // ==========================================
    const YUT_PROBABILITY = {
        // 실제 전통 윷가락 물리 확률 (평평한 배가 나올 확률 약 60%)
        // 도(배1등3), 개(배2등2), 걸(배3등1), 윷(배4), 모(등4), 빽도(표시된 배1)
        roll: function() {
            // 4개의 윷가락: index 0번이 빽도 표시가 있는 윷가락
            // 각 윷가락이 '배(flat, 1)'가 될 확률 0.6, '등(round, 0)'이 될 확률 0.4
            const stick0 = Math.random() < 0.6 ? 1 : 0; // 빽도 윷가락
            const stick1 = Math.random() < 0.6 ? 1 : 0;
            const stick2 = Math.random() < 0.6 ? 1 : 0;
            const stick3 = Math.random() < 0.6 ? 1 : 0;

            const flatCount = stick0 + stick1 + stick2 + stick3;
            const sticks = [stick0, stick1, stick2, stick3];

            let result = '';
            let name = '';
            let steps = 0;
            let extraTurn = false;

            if (flatCount === 0) {
                // 등 4개 = 모
                result = 'mo';
                name = '모';
                steps = 5;
                extraTurn = true;
            } else if (flatCount === 4) {
                // 배 4개 = 윷
                result = 'yut';
                name = '윷';
                steps = 4;
                extraTurn = true;
            } else if (flatCount === 3) {
                // 배 3개 = 걸
                result = 'geol';
                name = '걸';
                steps = 3;
            } else if (flatCount === 2) {
                // 배 2개 = 개
                result = 'gae';
                name = '개';
                steps = 2;
            } else if (flatCount === 1) {
                // 배 1개: 빽도 윷가락이 뒤집혔는지 확인
                if (stick0 === 1) {
                    result = 'back_do';
                    name = '빽도 (뒷도)';
                    steps = -1;
                } else {
                    result = 'do';
                    name = '도';
                    steps = 1;
                }
            }

            return {
                result,
                name,
                steps,
                extraTurn,
                sticks // [0/1, 0/1, 0/1, 0/1] (1: 배(앞), 0: 등(뒤))
            };
        }
    };

    // ==========================================
    // 5. 명예의 전당 (Hall of Fame) & 이름 마스킹
    // ==========================================

    // 학생 이름 개인정보 보호 마스킹 함수 (Waurimal 표준)
    function maskStudentName(name) {
        if (!name) return '익명';
        const str = String(name).trim();
        if (str.includes('인공지능') || str.includes('AI') || str.includes('컴퓨터')) {
            return str;
        }
        if (str.length <= 1) return str;
        if (str.length === 2) return str[0] + 'O';
        if (str.length === 3) return str[0] + 'O' + str[2];
        return str[0] + 'OO' + str[str.length - 1];
    }

    class HallOfFameManager {
        constructor() {
            this.storageKey = 'waurimal_yutnori_hof';
            this.defaultRecords = [
                { id: 'hof_1', name: '세종대왕', wins: 12, totalGames: 14, score: 9850, correctCount: 58, goalCount: 32, catchCount: 18, stackCount: 14, yutMoCount: 24, bestScore: 2150, lastPlayed: '2026-10-22' },
                { id: 'hof_2', name: '이순신', wins: 10, totalGames: 12, score: 8400, correctCount: 46, goalCount: 27, catchCount: 22, stackCount: 9, yutMoCount: 18, bestScore: 1980, lastPlayed: '2026-10-21' },
                { id: 'hof_3', name: '신사임당', wins: 8, totalGames: 9, score: 7100, correctCount: 42, goalCount: 21, catchCount: 12, stackCount: 12, yutMoCount: 15, bestScore: 1820, lastPlayed: '2026-10-20' },
                { id: 'hof_4', name: '율곡이이', wins: 7, totalGames: 8, score: 6250, correctCount: 38, goalCount: 19, catchCount: 10, stackCount: 8, yutMoCount: 12, bestScore: 1650, lastPlayed: '2026-10-19' },
                { id: 'hof_5', name: '장영실', wins: 6, totalGames: 7, score: 5600, correctCount: 35, goalCount: 16, catchCount: 9, stackCount: 7, yutMoCount: 10, bestScore: 1540, lastPlayed: '2026-10-18' },
                { id: 'hof_6', name: '홍길동', wins: 5, totalGames: 6, score: 4750, correctCount: 28, goalCount: 14, catchCount: 7, stackCount: 6, yutMoCount: 8, bestScore: 1420, lastPlayed: '2026-10-17' },
                { id: 'hof_7', name: '김유신', wins: 4, totalGames: 5, score: 3900, correctCount: 24, goalCount: 11, catchCount: 6, stackCount: 5, yutMoCount: 6, bestScore: 1310, lastPlayed: '2026-10-16' }
            ];
        }

        getRecords() {
            try {
                const raw = localStorage.getItem(this.storageKey);
                if (raw) {
                    const parsed = JSON.parse(raw);
                    if (Array.isArray(parsed) && parsed.length > 0) {
                        return this.sortRecords(parsed);
                    }
                }
            } catch(e) {
                console.warn("HOF load error:", e);
            }
            return this.sortRecords([...this.defaultRecords]);
        }

        sortRecords(records) {
            // 정렬 우선순위: 1) 승수(Wins) desc, 2) 기여도 총점(Score) desc, 3) 문제 정답수 desc
            return records.sort((a, b) => {
                if ((b.wins || 0) !== (a.wins || 0)) {
                    return (b.wins || 0) - (a.wins || 0);
                }
                if ((b.score || 0) !== (a.score || 0)) {
                    return (b.score || 0) - (a.score || 0);
                }
                return (b.correctCount || 0) - (a.correctCount || 0);
            });
        }

        saveRecords(records) {
            try {
                localStorage.setItem(this.storageKey, JSON.stringify(records));
            } catch(e) {
                console.warn("HOF save error:", e);
            }
            this.renderAllViews();
        }

        resetToDefault() {
            this.saveRecords([...this.defaultRecords]);
        }

        recordMatch(winningTeam, participantsStats) {
            const records = this.getRecords();
            const today = new Date().toISOString().split('T')[0];

            participantsStats.forEach(stat => {
                const pName = stat.name.trim();
                if (!pName || pName.includes('봇') || pName.includes('AI') || pName.includes('인공지능') || pName.includes('컴퓨터')) {
                    // 컴퓨터는 학생 랭킹에 기록 제외
                    return;
                }

                let existing = records.find(r => r.name.trim() === pName);
                if (existing) {
                    existing.totalGames = (existing.totalGames || 1) + 1;
                    if (stat.isWinner) {
                        existing.wins = (existing.wins || 0) + 1;
                    }
                    existing.score = (existing.score || 0) + (stat.matchScore || 0);
                    existing.correctCount = (existing.correctCount || 0) + (stat.correctCount || 0);
                    existing.goalCount = (existing.goalCount || 0) + (stat.goalCount || 0);
                    existing.catchCount = (existing.catchCount || 0) + (stat.catchCount || 0);
                    existing.stackCount = (existing.stackCount || 0) + (stat.stackCount || 0);
                    existing.yutMoCount = (existing.yutMoCount || 0) + (stat.yutMoCount || 0);
                    existing.lastPlayed = today;
                    if (!existing.bestScore || stat.matchScore > existing.bestScore) {
                        existing.bestScore = stat.matchScore;
                    }
                } else {
                    records.push({
                        id: 'hof_' + Date.now() + '_' + Math.random().toString(36).substr(2, 4),
                        name: pName,
                        wins: stat.isWinner ? 1 : 0,
                        totalGames: 1,
                        score: stat.matchScore || 0,
                        correctCount: stat.correctCount || 0,
                        goalCount: stat.goalCount || 0,
                        catchCount: stat.catchCount || 0,
                        stackCount: stat.stackCount || 0,
                        yutMoCount: stat.yutMoCount || 0,
                        bestScore: stat.matchScore || 0,
                        lastPlayed: today
                    });
                }
            });

            this.saveRecords(records);
        }

        renderAllViews() {
            this.renderHomeWidget();
            this.renderModalList();
        }

        renderHomeWidget() {
            if (typeof document === 'undefined' || !document.getElementById) return;
            const container = document.getElementById('homeHofList');
            if (!container) return;

            const records = this.getRecords().slice(0, 5); // TOP 5 노출
            if (records.length === 0) {
                container.innerHTML = '<div class="text-xs text-slate-400 p-3 text-center">등록된 명예의 전당 기록이 없습니다.</div>';
                return;
            }

            container.innerHTML = records.map((r, idx) => {
                const rank = idx + 1;
                let rankBadge = `<span class="w-5 h-5 rounded-full bg-slate-700 text-slate-300 flex items-center justify-center text-[10px] font-black">${rank}</span>`;
                if (rank === 1) rankBadge = `<span class="w-5 h-5 rounded-full bg-amber-400 text-slate-950 flex items-center justify-center text-[10px] font-black shadow shadow-amber-500/50">🥇</span>`;
                else if (rank === 2) rankBadge = `<span class="w-5 h-5 rounded-full bg-slate-300 text-slate-950 flex items-center justify-center text-[10px] font-black shadow">🥈</span>`;
                else if (rank === 3) rankBadge = `<span class="w-5 h-5 rounded-full bg-amber-700 text-white flex items-center justify-center text-[10px] font-black shadow">🥉</span>`;

                const masked = maskStudentName(r.name);
                return `
                    <div class="flex items-center justify-between p-2 rounded-xl bg-slate-900/70 hover:bg-slate-700/60 border border-slate-700/60 transition text-xs">
                        <div class="flex items-center gap-2">
                            ${rankBadge}
                            <span class="font-bold text-slate-200">${masked}</span>
                        </div>
                        <div class="flex items-center gap-2">
                            <span class="px-1.5 py-0.5 rounded-md bg-amber-500/20 text-amber-300 font-black text-[11px] border border-amber-500/30">${r.wins || 0}승</span>
                            <span class="font-bold text-emerald-400 text-[11px]">${(r.score || 0).toLocaleString()}점</span>
                        </div>
                    </div>
                `;
            }).join('');
        }

        renderModalList() {
            if (typeof document === 'undefined' || !document.getElementById) return;
            const container = document.getElementById('modalHofList');
            if (!container) return;

            const records = this.getRecords();
            if (records.length === 0) {
                container.innerHTML = '<tr><td colspan="7" class="text-center py-6 text-slate-400">기록이 없습니다.</td></tr>';
                return;
            }

            container.innerHTML = records.map((r, idx) => {
                const rank = idx + 1;
                let medal = `${rank}위`;
                let rowBg = 'hover:bg-slate-800/60';
                if (rank === 1) { medal = '🥇 1위'; rowBg = 'bg-amber-500/10 hover:bg-amber-500/20'; }
                else if (rank === 2) { medal = '🥈 2위'; rowBg = 'bg-slate-300/10 hover:bg-slate-300/20'; }
                else if (rank === 3) { medal = '🥉 3위'; rowBg = 'bg-amber-700/10 hover:bg-amber-700/20'; }

                const masked = maskStudentName(r.name);
                const winRate = r.totalGames ? Math.round(((r.wins || 0) / r.totalGames) * 100) : 0;

                return `
                    <tr class="border-b border-slate-800 ${rowBg} transition text-center text-xs">
                        <td class="py-3 font-black text-amber-400">${medal}</td>
                        <td class="py-3 font-bold text-white text-left pl-3">${masked}</td>
                        <td class="py-3 font-black text-amber-300">${r.wins || 0}승 <span class="text-[10px] text-slate-400 font-normal">(${winRate}%)</span></td>
                        <td class="py-3 font-black text-emerald-400 text-right pr-4">${(r.score || 0).toLocaleString()}점</td>
                        <td class="py-3 text-slate-300 font-semibold">${r.correctCount || 0}개</td>
                        <td class="py-3 text-slate-300 font-semibold">${r.goalCount || 0}마리</td>
                        <td class="py-3 text-slate-400 text-[11px]">${r.lastPlayed || '-'}</td>
                    </tr>
                `;
            }).join('');
        }
    }

    const HallOfFame = new HallOfFameManager();

    // ==========================================
    // 6. 윷놀이 메인 게임 엔진 클래스
    // ==========================================
    class YutnoriGame {
        constructor() {
            this.mode = 'ai'; // 'ai', '1v1', 'team'
            this.difficulty = 'normal'; // 'easy', 'normal', 'hard'
            this.pieceCount = 4; // 팀당 말의 수 (3 또는 4)
            this.teams = [];
            this.currentTeamIndex = 0;
            this.gameState = 'IDLE'; // 'IDLE', 'WAIT_ROLL', 'WAIT_PIECE_SELECT', 'WAIT_QUIZ', 'MOVING', 'GAME_OVER'

            this.currentRoll = null; // 방금 던진 윷 결과 { result, name, steps, extraTurn, sticks }
            this.pendingExtraTurn = false; // 윷/모 또는 말 잡기로 인한 추가 턴 여부
            this.selectedPiece = null;
            this.currentQuestion = null;
            this.gameLogs = [];
            this.matchStats = {}; // 경기 중 플레이어별 기여도 통계

            // 실시간 온라인 멀티플레이 상태
            this.isOnline = false;
            this.currentRoomData = null;
            this._lastHandledActionTimestamp = 0;

            // DOM 캐시
            this.dom = {};
        }

        initDOM() {
            this.dom = {
                boardWrapper: document.getElementById('boardWrapper'),
                boardSvg: document.getElementById('boardSvg'),
                nodesLayer: document.getElementById('nodesLayer'),
                piecesLayer: document.getElementById('piecesLayer'),
                stick0: document.getElementById('yutStick0'),
                stick1: document.getElementById('yutStick1'),
                stick2: document.getElementById('yutStick2'),
                stick3: document.getElementById('yutStick3'),
                yutLabel0: document.getElementById('yutLabel0'),
                yutLabel1: document.getElementById('yutLabel1'),
                yutLabel2: document.getElementById('yutLabel2'),
                yutLabel3: document.getElementById('yutLabel3'),
                rollBtn: document.getElementById('rollBtn'),
                resultBadge: document.getElementById('resultBadge'),
                resultText: document.getElementById('resultText'),
                resultStepsBadge: document.getElementById('resultStepsBadge'),
                resultDetailText: document.getElementById('resultDetailText'),
                extraTurnBadge: document.getElementById('extraTurnBadge'),
                turnBanner: document.getElementById('turnBanner'),
                turnTeamName: document.getElementById('turnTeamName'),
                turnPlayerName: document.getElementById('turnPlayerName'),
                teamsStatusContainer: document.getElementById('teamsStatusContainer'),
                gameLogContainer: document.getElementById('gameLogContainer'),

                // 온라인 멀티플레이 전용 DOM
                remoteTurnNotice: document.getElementById('remoteTurnNotice'),
                remoteTurnNoticeText: document.getElementById('remoteTurnNoticeText'),
                onlineRoomBanner: document.getElementById('onlineRoomBanner'),
                activeRoomCodeText: document.getElementById('activeRoomCodeText'),
                activeMyRoleText: document.getElementById('activeMyRoleText'),
                leaveOnlineRoomBtn: document.getElementById('leaveOnlineRoomBtn'),

                // 퀴즈 모달
                quizModal: document.getElementById('quizModal'),
                quizCategoryBadge: document.getElementById('quizCategoryBadge'),
                quizPrompt: document.getElementById('quizPrompt'),
                quizQuestionText: document.getElementById('quizQuestionText'),
                quizSubText: document.getElementById('quizSubText'),
                quizTtsBtn: document.getElementById('quizTtsBtn'),
                quizChoicesGrid: document.getElementById('quizChoicesGrid'),
                quizScrambleArea: document.getElementById('quizScrambleArea'),
                quizAnswerTokensContainer: document.getElementById('quizAnswerTokensContainer'),
                quizSourceTokensContainer: document.getElementById('quizSourceTokensContainer'),
                quizScrambleSubmitBtn: document.getElementById('quizScrambleSubmitBtn'),
                quizScrambleResetBtn: document.getElementById('quizScrambleResetBtn'),

                // 피드백 모달
                feedbackModal: document.getElementById('feedbackModal'),
                feedbackTitle: document.getElementById('feedbackTitle'),
                feedbackMessage: document.getElementById('feedbackMessage'),
                feedbackExplanation: document.getElementById('feedbackExplanation'),
                feedbackOkBtn: document.getElementById('feedbackOkBtn'),
                feedbackAutoCloseNotice: document.getElementById('feedbackAutoCloseNotice'),
                feedbackCountdownText: document.getElementById('feedbackCountdownText'),
                feedbackOkBtnText: document.getElementById('feedbackOkBtnText'),

                // 승리 모달
                victoryModal: document.getElementById('victoryModal'),
                victoryTeamName: document.getElementById('victoryTeamName'),
                victoryDetail: document.getElementById('victoryDetail'),
                restartBtn: document.getElementById('restartBtn'),

                // 명예의 전당 UI
                homeHofList: document.getElementById('homeHofList'),
                modalHofList: document.getElementById('modalHofList'),
                hofModal: document.getElementById('hofModal'),
                openHofHeaderBtn: document.getElementById('openHofHeaderBtn'),
                openHofWidgetBtn: document.getElementById('openHofWidgetBtn'),
                closeHofModalBtn: document.getElementById('closeHofModalBtn'),
                resetHofModalBtn: document.getElementById('resetHofModalBtn')
            };

            this.renderBoardNodes();
            this.bindEvents();
            HallOfFame.renderAllViews();
        }

        /**
         * 윷판 SVG 선 및 29개 노드 DOM 렌더링
         */
        renderBoardNodes() {
            if (!this.dom.nodesLayer || !this.dom.boardSvg) return;

            // 1. 노드 렌더링
            this.dom.nodesLayer.innerHTML = '';
            for (let id = 0; id <= 28; id++) {
                const nodeInfo = BOARD_NODES[id];
                if (!nodeInfo) continue;

                const nodeDiv = document.createElement('div');
                nodeDiv.id = `node-${id}`;
                nodeDiv.className = `yut-node ${nodeInfo.isCorner ? 'special-node' : ''} ${nodeInfo.isStart ? 'start-node' : ''} ${nodeInfo.isCenter ? 'center-node special-node' : ''}`;
                nodeDiv.style.left = `${nodeInfo.x}%`;
                nodeDiv.style.top = `${nodeInfo.y}%`;
                nodeDiv.innerHTML = `<span>${nodeInfo.name.substring(0, 4)}</span>`;
                nodeDiv.title = `${nodeInfo.name} (${id}번)`;

                // 클릭 시 말 이동 목적지 선택
                nodeDiv.addEventListener('click', () => this.onNodeClicked(id));
                this.dom.nodesLayer.appendChild(nodeDiv);
            }

            // 2. SVG 연결 선 렌더링
            this.drawBoardLines();
        }

        drawBoardLines() {
            if (!this.dom.boardSvg) return;

            // SVG 내부 크기: 100 x 100
            const lines = [
                // 외곽 사각형
                { x1: 88, y1: 88, x2: 88, y2: 12 }, // 우변
                { x1: 88, y1: 12, x2: 12, y2: 12 }, // 상변
                { x1: 12, y1: 12, x2: 12, y2: 88 }, // 좌변
                { x1: 12, y1: 88, x2: 88, y2: 88 }, // 하변

                // 대각선 X자
                { x1: 88, y1: 12, x2: 12, y2: 88 }, // 우상 -> 좌하
                { x1: 12, y1: 12, x2: 88, y2: 88 }  // 좌상 -> 우하
            ];

            let svgContent = '';
            lines.forEach(l => {
                svgContent += `<line x1="${l.x1}%" y1="${l.y1}%" x2="${l.x2}%" y2="${l.y2}%" stroke="#78350f" stroke-width="4" stroke-dasharray="1 1" opacity="0.65" stroke-linecap="round" />`;
            });
            this.dom.boardSvg.innerHTML = svgContent;
        }

        bindEvents() {
            if (this.dom.rollBtn) {
                this.dom.rollBtn.addEventListener('click', () => this.onRollClicked());
            }

            if (this.dom.quizTtsBtn) {
                this.dom.quizTtsBtn.addEventListener('click', () => {
                    if (this.currentQuestion && this.currentQuestion.ttsText) {
                        Sound.speakText(this.currentQuestion.ttsText);
                    }
                });
            }

            if (this.dom.feedbackOkBtn) {
                this.dom.feedbackOkBtn.addEventListener('click', () => {
                    this.closeFeedbackModal();
                });
            }

            if (this.dom.restartBtn) {
                this.dom.restartBtn.addEventListener('click', () => {
                    this.dom.victoryModal.classList.add('hidden');
                    this.startNewGame();
                });
            }

            // 명예의 전당 모달 열기/닫기/리셋
            if (this.dom.openHofHeaderBtn) {
                this.dom.openHofHeaderBtn.addEventListener('click', () => {
                    HallOfFame.renderAllViews();
                    if (this.dom.hofModal) this.dom.hofModal.classList.remove('hidden');
                });
            }
            if (this.dom.openHofWidgetBtn) {
                this.dom.openHofWidgetBtn.addEventListener('click', () => {
                    HallOfFame.renderAllViews();
                    if (this.dom.hofModal) this.dom.hofModal.classList.remove('hidden');
                });
            }
            if (this.dom.closeHofModalBtn) {
                this.dom.closeHofModalBtn.addEventListener('click', () => {
                    if (this.dom.hofModal) this.dom.hofModal.classList.add('hidden');
                });
            }
            if (this.dom.resetHofModalBtn) {
                this.dom.resetHofModalBtn.addEventListener('click', () => {
                    if (confirm("명예의 전당 기록을 초기화하시겠습니까?")) {
                        HallOfFame.resetToDefault();
                    }
                });
            }

            // 어순 배열 버튼들
            if (this.dom.quizScrambleResetBtn) {
                this.dom.quizScrambleResetBtn.addEventListener('click', () => this.resetScrambleQuiz());
            }
            if (this.dom.quizScrambleSubmitBtn) {
                this.dom.quizScrambleSubmitBtn.addEventListener('click', () => this.submitScrambleQuiz());
            }
        }

        /**
         * 새 게임 시작 및 팀 설정
         */
        startNewGame(config = {}) {
            if (!this.dom || !this.dom.boardWrapper) {
                this.initDOM();
            }

            this.mode = config.mode || this.mode || 'ai';
            this.difficulty = config.difficulty || 'normal';
            this.pieceCount = config.pieceCount || 4;

            // 팀 구성 (컴퓨터 대전 시 학생 실명 반영)
            const realName = config.playerName || '나(플레이어)';
            this.teams = [
                { id: 0, name: `${realName}(청팀)`, isAI: false, colorClass: 'team-0', players: [realName], currentPlayerIdx: 0, pieces: [] },
                { id: 1, name: '컴퓨터(백팀)', isAI: true, colorClass: 'team-1', players: ['인공지능 봇'], currentPlayerIdx: 0, pieces: [] }
            ];

            // 각 팀 말(Piece) 초기화
            this.teams.forEach(team => {
                team.pieces = [];
                for (let i = 0; i < this.pieceCount; i++) {
                    team.pieces.push({
                        id: i,
                        teamId: team.id,
                        pos: -1, // -1: 대기실(START)
                        route: 'OUTER', // 현재 진행 경로
                        isHome: true, // 아직 판에 안 나옴
                        isGoal: false, // 완주 여부
                        stackedPieces: [], // 업힌 말들의 ID 목록
                        history: [] // 이전 위치 기록 (빽도 용)
                    });
                }
            });

            // 플레이어별 기여도 통계 초기화
            this.matchStats = {};
            this.teams.forEach(team => {
                team.players.forEach(pName => {
                    this.matchStats[pName] = {
                        name: pName,
                        teamId: team.id,
                        teamName: team.name,
                        correctCount: 0,
                        wrongCount: 0,
                        goalCount: 0,
                        catchCount: 0,
                        stackCount: 0,
                        yutMoCount: 0,
                        matchScore: 0
                    };
                });
            });

            this.currentTeamIndex = 0;
            this.pendingExtraTurn = false;
            this.currentRoll = null;
            this.selectedPiece = null;
            this.gameLogs = [];

            this.logMessage(`✨ 새 윷놀이 게임이 시작되었습니다! (${this.getModeName()} 모드)`);
            this.updateTurnUI();
            this.renderAllPieces();

            // 윷가락 비주얼 초기화
            const sticks = [this.dom.stick0, this.dom.stick1, this.dom.stick2, this.dom.stick3];
            const labels = [this.dom.yutLabel0, this.dom.yutLabel1, this.dom.yutLabel2, this.dom.yutLabel3];
            sticks.forEach((st, idx) => {
                if (st) {
                    st.classList.remove('tossing', 'is-back');
                    st.classList.add('is-front');
                }
                if (labels[idx]) {
                    labels[idx].textContent = (idx === 0) ? '앞 (빽도)' : '앞 (배)';
                    labels[idx].className = 'yut-stick-label is-front-label text-[11px] px-2 py-0.5 rounded-full border';
                }
            });
            if (this.dom.resultBadge) {
                this.dom.resultBadge.classList.add('hidden');
            }

            this.setGameState('WAIT_ROLL');

            // 첫 턴이 AI면 자동 롤 실행
            this.checkAndTriggerAI();
        }

        // ==========================================
        // 5-B. 실시간 온라인 멀티플레이 게임 시작 & 동기화
        // ==========================================
        startOnlineGame(roomData) {
            this.isOnline = true;
            this.mode = 'online';
            this.difficulty = 'normal';
            this.pieceCount = roomData.pieceCount || 4;
            this.gradeFilter = roomData.grade || 'all';
            this.currentRoomData = roomData;
            this._lastHandledActionTimestamp = 0;

            const players = Object.values(roomData.players || {});
            const bluePlayers = players.filter(p => p.team === 'blue');
            const whitePlayers = players.filter(p => p.team === 'white');

            this.teams = [
                {
                    id: 0,
                    name: '청팀',
                    key: 'blue',
                    isAI: false,
                    colorClass: 'team-0',
                    players: bluePlayers.map(p => p.name || '청팀 플레이어'),
                    playerUids: bluePlayers.map(p => p.id),
                    currentPlayerIdx: 0,
                    pieces: []
                },
                {
                    id: 1,
                    name: '백팀',
                    key: 'white',
                    isAI: false,
                    colorClass: 'team-1',
                    players: whitePlayers.map(p => p.name || '백팀 플레이어'),
                    playerUids: whitePlayers.map(p => p.id),
                    currentPlayerIdx: 0,
                    pieces: []
                }
            ];

            // 말(Pieces) 초기화
            this.teams.forEach(team => {
                team.pieces = [];
                for (let i = 0; i < this.pieceCount; i++) {
                    team.pieces.push({
                        id: i,
                        teamId: team.id,
                        pos: -1,
                        route: 'OUTER',
                        isHome: true,
                        isGoal: false,
                        stackedPieces: [],
                        history: []
                    });
                }
            });

            // 플레이어별 기여도 통계 초기화
            this.matchStats = {};
            this.teams.forEach(team => {
                team.players.forEach(pName => {
                    this.matchStats[pName] = {
                        name: pName,
                        teamId: team.id,
                        teamName: team.name,
                        correctCount: 0,
                        wrongCount: 0,
                        goalCount: 0,
                        catchCount: 0,
                        stackCount: 0,
                        yutMoCount: 0,
                        matchScore: 0
                    };
                });
            });

            this.currentTeamIndex = (roomData.currentTeam === 'white') ? 1 : 0;
            this.pendingExtraTurn = false;
            this.currentRoll = null;
            this.selectedPiece = null;
            this.gameLogs = [];

            this.logMessage(`🌐 실시간 온라인 방 [${roomData.roomId}] 게임이 시작되었습니다!`);

            // 상단 온라인 배너 활성화
            if (this.dom.onlineRoomBanner) {
                this.dom.onlineRoomBanner.classList.remove('hidden');
                if (this.dom.activeRoomCodeText) {
                    this.dom.activeRoomCodeText.textContent = roomData.roomId;
                }
                if (this.dom.activeMyRoleText && window.YutnoriMultiplayer) {
                    const myInfo = window.YutnoriMultiplayer.getMyPlayerInfo();
                    if (myInfo) {
                        const teamName = myInfo.team === 'blue' ? '청팀' : '백팀';
                        this.dom.activeMyRoleText.textContent = `${teamName} (${myInfo.name})`;
                        this.dom.activeMyRoleText.className = `font-black ml-1 ${myInfo.team === 'blue' ? 'text-blue-400' : 'text-rose-400'}`;
                    }
                }
            }

            this.updateTurnUI();
            this.renderAllPieces();

            // 윷가락 비주얼 초기화
            const sticks = [this.dom.stick0, this.dom.stick1, this.dom.stick2, this.dom.stick3];
            const labels = [this.dom.yutLabel0, this.dom.yutLabel1, this.dom.yutLabel2, this.dom.yutLabel3];
            sticks.forEach((st, idx) => {
                if (st) {
                    st.classList.remove('tossing', 'is-back');
                    st.classList.add('is-front');
                }
                if (labels[idx]) {
                    labels[idx].textContent = (idx === 0) ? '앞 (빽도)' : '앞 (배)';
                    labels[idx].className = 'yut-stick-label is-front-label text-[11px] px-2 py-0.5 rounded-full border';
                }
            });
            if (this.dom.resultBadge) {
                this.dom.resultBadge.classList.add('hidden');
            }

            this.setGameState('WAIT_ROLL');
            this.syncWithRoomData(roomData, true);
        }

        syncWithRoomData(roomData, isInitial = false) {
            this.currentRoomData = roomData;
            this.currentTeamIndex = (roomData.currentTeam === 'white') ? 1 : 0;

            const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
            const isMyTurn = (roomData.currentTurnId === mySessionId);

            // 상대방이 진행한 말 위치 상태 동기화 (내가 움직인 턴이 아닐 때)
            if (roomData.teamsState && (!isMyTurn || isInitial)) {
                if (roomData.teamsState[0] && roomData.teamsState[0].pieces) {
                    this.teams[0].pieces = JSON.parse(JSON.stringify(roomData.teamsState[0].pieces));
                }
                if (roomData.teamsState[1] && roomData.teamsState[1].pieces) {
                    this.teams[1].pieces = JSON.parse(JSON.stringify(roomData.teamsState[1].pieces));
                }
                this.renderAllPieces();
            }

            this.updateTurnUI();
            this.updateRollButtonState();

            // 액션 리스너 처리
            if (roomData.lastAction && roomData.lastAction.timestamp > this._lastHandledActionTimestamp) {
                this.handleRemoteAction(roomData.lastAction, roomData);
            }
        }

        handleRemoteAction(action, roomData) {
            this._lastHandledActionTimestamp = action.timestamp;
            const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
            const isMe = (action.actorId === mySessionId);

            if (isMe) return; // 내가 전송한 액션은 이미 로컬에서 재생 완료

            const actorPlayer = roomData.players ? roomData.players[action.actorId] : null;
            const actorName = actorPlayer ? actorPlayer.name : '상대방';

            if (action.type === 'ROLL_AND_QUIZ') {
                if (action.rollResult) {
                    this.playRemoteRollAnimation(action.rollResult);
                }
                if (this.dom.remoteTurnNotice) {
                    this.dom.remoteTurnNotice.classList.remove('hidden');
                    if (this.dom.remoteTurnNoticeText) {
                        this.dom.remoteTurnNoticeText.textContent = `${actorName}님이 영어 퀴즈를 풀고 있습니다... 🤔`;
                    }
                }
            } else if (action.type === 'QUIZ_CORRECT') {
                Sound.playCorrect();
                this.logMessage(`👏 [${actorName}]님이 영어 문제를 맞혔습니다! 말이 전진합니다.`);
                if (this.dom.remoteTurnNoticeText) {
                    this.dom.remoteTurnNoticeText.textContent = `${actorName}님이 정답을 맞혀 이동할 말을 고르고 있습니다! 🎯`;
                }
            } else if (action.type === 'QUIZ_WRONG') {
                Sound.playWrong();
                this.logMessage(`❌ [${actorName}]님이 아쉽게 문제를 틀렸습니다. 전진하지 못합니다.`);
                if (this.dom.remoteTurnNoticeText) {
                    this.dom.remoteTurnNoticeText.textContent = `${actorName}님이 문제를 틀려 턴이 넘어옵니다.`;
                }
            } else if (action.type === 'END_TURN') {
                this.logMessage(`🔄 ${actorName}님의 턴이 끝나고 차례가 넘어왔습니다.`);
                Sound.playStep();
            } else if (action.type === 'EXTRA_TURN') {
                this.logMessage(`🎉 ${actorName}님이 윷/모 또는 말 잡기로 한 번 더 윷을 던집니다!`);
                Sound.playStep();
            } else if (action.type === 'GAME_OVER') {
                const winnerTeamObj = this.teams.find(t => t.name === action.winnerTeam) || this.teams[0];
                this.triggerVictory(winnerTeamObj);
            }
        }

        playRemoteRollAnimation(rollResult) {
            this.currentRoll = rollResult;
            Sound.playToss();

            const sticks = [this.dom.stick0, this.dom.stick1, this.dom.stick2, this.dom.stick3];
            const labels = [this.dom.yutLabel0, this.dom.yutLabel1, this.dom.yutLabel2, this.dom.yutLabel3];

            sticks.forEach((st, idx) => {
                if (st) {
                    st.classList.remove('tossing');
                    void st.offsetWidth;
                    st.classList.add('tossing');
                }
                if (labels[idx]) {
                    labels[idx].textContent = '회전 중...';
                    labels[idx].className = 'yut-stick-label text-[11px] px-2 py-0.5 rounded-full border bg-slate-700 text-slate-300 border-slate-600 animate-pulse';
                }
            });

            setTimeout(() => {
                sticks.forEach((st, idx) => {
                    if (st) {
                        st.classList.remove('tossing', 'is-front', 'is-back');
                        const isFlat = (rollResult.sticks[idx] === 1);
                        st.classList.add(isFlat ? 'is-front' : 'is-back');
                    }
                    if (labels[idx]) {
                        const isFlat = (rollResult.sticks[idx] === 1);
                        labels[idx].textContent = isFlat ? ((idx === 0) ? '앞 (빽도)' : '앞 (배)') : '뒤 (등)';
                        labels[idx].className = `yut-stick-label ${isFlat ? 'is-front-label' : 'is-back-label'} text-[11px] px-2 py-0.5 rounded-full border`;
                    }
                });

                Sound.playLand(rollResult.result);
                this.displayRollResult(rollResult);
            }, 700);
        }

        getModeName() {
            if (this.mode === 'online') return '실시간 온라인 멀티';
            return '컴퓨터 대전';
        }

        getCurrentTeam() {
            return this.teams[this.currentTeamIndex];
        }

        getCurrentPlayerName() {
            const team = this.getCurrentTeam();
            return team.players[team.currentPlayerIdx] || team.name;
        }

        recordStat(playerName, field, amount = 1) {
            if (!this.matchStats || !playerName) return;
            if (!this.matchStats[playerName]) {
                const currentTeam = this.getCurrentTeam();
                this.matchStats[playerName] = {
                    name: playerName,
                    teamId: currentTeam.id,
                    teamName: currentTeam.name,
                    correctCount: 0,
                    wrongCount: 0,
                    goalCount: 0,
                    catchCount: 0,
                    stackCount: 0,
                    yutMoCount: 0,
                    matchScore: 0
                };
            }
            this.matchStats[playerName][field] = (this.matchStats[playerName][field] || 0) + amount;
        }

        setGameState(newState) {
            this.gameState = newState;
            this.updateRollButtonState();
        }

        updateRollButtonState() {
            if (!this.dom.rollBtn) return;

            if (this.isOnline) {
                const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
                const isMyTurn = (this.currentRoomData && this.currentRoomData.currentTurnId === mySessionId);

                if (isMyTurn && this.gameState === 'WAIT_ROLL') {
                    this.dom.rollBtn.disabled = false;
                    this.dom.rollBtn.classList.remove('opacity-50', 'cursor-not-allowed');
                    this.dom.rollBtn.innerHTML = '<i class="fa-solid fa-dice mr-2"></i> 내 차례! 윷 던지기';
                } else {
                    this.dom.rollBtn.disabled = true;
                    this.dom.rollBtn.classList.add('opacity-50', 'cursor-not-allowed');
                    const currentTurnPlayer = (this.currentRoomData && this.currentRoomData.players) 
                        ? this.currentRoomData.players[this.currentRoomData.currentTurnId] 
                        : null;
                    const name = currentTurnPlayer ? currentTurnPlayer.name : '상대방';
                    this.dom.rollBtn.innerHTML = `<i class="fa-solid fa-hourglass-half mr-2"></i> ${name}님의 차례...`;
                }
                return;
            }

            const currentTeam = this.getCurrentTeam();
            if (currentTeam.isAI || this.gameState !== 'WAIT_ROLL') {
                this.dom.rollBtn.disabled = true;
                this.dom.rollBtn.classList.add('opacity-50', 'cursor-not-allowed');
                this.dom.rollBtn.innerHTML = '<i class="fa-solid fa-dice mr-2"></i> 윷 던지기';
            } else {
                this.dom.rollBtn.disabled = false;
                this.dom.rollBtn.classList.remove('opacity-50', 'cursor-not-allowed');
                this.dom.rollBtn.innerHTML = '<i class="fa-solid fa-dice mr-2"></i> 윷 던지기';
            }
        }

        /**
         * 턴 배너 및 팀 현황 UI 갱신
         */
        updateTurnUI() {
            const currentTeam = this.getCurrentTeam();
            const playerName = this.getCurrentPlayerName();

            if (this.dom.turnTeamName) {
                this.dom.turnTeamName.textContent = currentTeam.name;
                this.dom.turnTeamName.className = `font-black text-lg ${currentTeam.id === 0 ? 'text-blue-500' : 'text-rose-500'}`;
            }

            if (this.dom.turnPlayerName) {
                if (this.isOnline) {
                    const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
                    const isMyTurn = (this.currentRoomData && this.currentRoomData.currentTurnId === mySessionId);

                    if (isMyTurn) {
                        this.dom.turnPlayerName.textContent = `🎯 내 차례입니다 (${playerName})`;
                        if (this.dom.remoteTurnNotice) this.dom.remoteTurnNotice.classList.add('hidden');
                    } else {
                        this.dom.turnPlayerName.textContent = `⏳ ${playerName}님의 차례`;
                        if (this.dom.remoteTurnNotice) {
                            this.dom.remoteTurnNotice.classList.remove('hidden');
                            if (this.dom.remoteTurnNoticeText) {
                                this.dom.remoteTurnNoticeText.textContent = `${playerName}님이 윷을 던지고 있습니다...`;
                            }
                        }
                    }
                } else {
                    this.dom.turnPlayerName.textContent = currentTeam.isAI ? '🤖 인공지능이 윷을 던집니다...' : `🎯 차례: ${playerName}`;
                }
            }

            // 팀별 상태 (골인한 말 개수 / 남은 말) 렌더링
            if (this.dom.teamsStatusContainer) {
                this.dom.teamsStatusContainer.innerHTML = '';
                this.teams.forEach(team => {
                    const goalCount = team.pieces.filter(p => p.isGoal).length;
                    const isCurrent = (team.id === currentTeam.id);

                    const teamCard = document.createElement('div');
                    teamCard.className = `p-3 rounded-2xl border-2 transition-all ${isCurrent ? 'bg-amber-50 border-amber-400 shadow-md scale-102' : 'bg-slate-800/80 border-slate-700 text-slate-200'}`;

                    teamCard.innerHTML = `
                        <div class="flex items-center justify-between mb-1">
                            <span class="font-bold text-sm ${isCurrent ? 'text-amber-900' : 'text-white'}">${team.name}</span>
                            <span class="text-xs px-2 py-0.5 rounded-full font-bold ${isCurrent ? 'bg-amber-200 text-amber-900' : 'bg-slate-700 text-slate-300'}">완주 ${goalCount}/${this.pieceCount}</span>
                        </div>
                        <div class="flex gap-1.5 items-center">
                            ${team.pieces.map((p, idx) => {
                                let statusBg = 'bg-slate-300 border-slate-400 text-slate-600';
                                let statusText = (idx + 1);
                                if (p.isGoal) {
                                    statusBg = 'bg-emerald-500 border-emerald-600 text-white font-black';
                                    statusText = '★';
                                } else if (!p.isHome) {
                                    statusBg = (team.id === 0) ? 'bg-blue-500 text-white font-bold' : 'bg-rose-500 text-white font-bold';
                                }
                                return `<span class="w-6 h-6 rounded-full flex items-center justify-center text-xs border ${statusBg}" title="말 #${idx+1}">${statusText}</span>`;
                            }).join('')}
                        </div>
                    `;
                    this.dom.teamsStatusContainer.appendChild(teamCard);
                });
            }
        }

        logMessage(msg) {
            this.gameLogs.unshift(msg);
            if (this.gameLogs.length > 20) this.gameLogs.pop();

            if (this.dom.gameLogContainer) {
                this.dom.gameLogContainer.innerHTML = this.gameLogs.map((log, idx) => {
                    return `<div class="text-xs py-1 border-b border-slate-700/50 ${idx === 0 ? 'text-amber-300 font-semibold' : 'text-slate-300'}">${log}</div>`;
                }).join('');
            }
        }

        // ==========================================
        // 6. 윷 던지기 실행
        // ==========================================
        onRollClicked() {
            if (this.gameState !== 'WAIT_ROLL') return;

            if (this.isOnline) {
                const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
                if (!this.currentRoomData || this.currentRoomData.currentTurnId !== mySessionId) {
                    return; // 내 차례가 아닐 때는 클릭 차단
                }
            } else {
                const currentTeam = this.getCurrentTeam();
                if (currentTeam.isAI) return;
            }

            this.executeRoll();
        }

        executeRoll() {
            this.setGameState('ROLLING');
            Sound.playToss();

            // 윷가락 애니메이션 트리거
            const sticks = [this.dom.stick0, this.dom.stick1, this.dom.stick2, this.dom.stick3];
            const labels = [this.dom.yutLabel0, this.dom.yutLabel1, this.dom.yutLabel2, this.dom.yutLabel3];

            sticks.forEach((st, idx) => {
                if (st) {
                    st.classList.remove('tossing');
                    void st.offsetWidth; // 리플로우 강제
                    st.classList.add('tossing');
                }
                if (labels[idx]) {
                    labels[idx].textContent = '회전 중...';
                    labels[idx].className = 'yut-stick-label text-[11px] px-2 py-0.5 rounded-full border bg-slate-700 text-slate-300 border-slate-600 animate-pulse';
                }
            });

            // 윷 확률 계산
            const rollResult = YUT_PROBABILITY.roll();
            this.currentRoll = rollResult;

            // 실시간 온라인 모드일 때 윷 결과를 Firestore로 즉시 전송
            if (this.isOnline && window.YutnoriMultiplayer) {
                window.YutnoriMultiplayer.sendRoll(rollResult, rollResult.sticks);
            }

            setTimeout(() => {
                // 1. tossing 애니메이션 클래스 제거 (forwards 스타일 고정 버그 원천 차단)
                sticks.forEach((st, idx) => {
                    if (st) {
                        st.classList.remove('tossing');
                        const isFlat = (rollResult.sticks[idx] === 1); // 1: 배(앞), 0: 등(뒤)

                        st.classList.remove('is-front', 'is-back');
                        if (isFlat) {
                            st.classList.add('is-front');
                        } else {
                            st.classList.add('is-back');
                        }
                    }

                    // 2. 윷가락 하단 레이블 상태 갱신
                    if (labels[idx]) {
                        const isFlat = (rollResult.sticks[idx] === 1);
                        if (isFlat) {
                            labels[idx].textContent = (idx === 0) ? '앞 (빽도)' : '앞 (배)';
                            labels[idx].className = 'yut-stick-label is-front-label text-[11px] px-2 py-0.5 rounded-full border';
                        } else {
                            labels[idx].textContent = '뒤 (등)';
                            labels[idx].className = 'yut-stick-label is-back-label text-[11px] px-2 py-0.5 rounded-full border';
                        }
                    }
                });

                // 3. 결과 효과음 및 결과 뱃지 표시
                Sound.playLand(rollResult.result);
                this.displayRollResult(rollResult);

                const currentTeam = this.getCurrentTeam();
                const playerName = this.getCurrentPlayerName();
                this.logMessage(`🎲 [${currentTeam.name}] ${playerName}님이 윷을 던져 <strong>'${rollResult.name}'</strong>이(가) 나왔습니다!`);

                // 윷이나 모가 나온 경우 추가 턴 예약 및 기여도 기록
                if (rollResult.extraTurn) {
                    this.pendingExtraTurn = true;
                    this.recordStat(playerName, 'yutMoCount', 1);
                    this.logMessage(`🎉 '${rollResult.name}'(으)로 윷을 한 번 더 던질 기회를 얻었습니다!`);
                }

                // 이동 가능한 말이 있는지 확인
                this.preparePieceSelection();
            }, 700);
        }

        displayRollResult(rollResult) {
            if (!this.dom.resultBadge || !this.dom.resultText) return;

            // 1. 결과 이름 및 스텝 수 뱃지
            this.dom.resultText.textContent = rollResult.name;
            if (this.dom.resultStepsBadge) {
                const stepText = (rollResult.steps > 0) ? `+${rollResult.steps}칸` : `${rollResult.steps}칸`;
                this.dom.resultStepsBadge.textContent = stepText;
            }

            // 2. 상세 설명 문구 (앞/뒤 개수 및 전진 칸수)
            if (this.dom.resultDetailText) {
                const detailMap = {
                    'back_do': '[ 빽도 윷 1개만 앞(배) = 뒤로 1칸 후퇴! ]',
                    'do': '[ 앞(배) 1개, 뒤(등) 3개 = 1칸 전진 ]',
                    'gae': '[ 앞(배) 2개, 뒤(등) 2개 = 2칸 전진 ]',
                    'geol': '[ 앞(배) 3개, 뒤(등) 1개 = 3칸 전진 ]',
                    'yut': '[ 앞(배) 4개 모두 뒤집힘 = 4칸 전진! ]',
                    'mo': '[ 뒤(등) 4개 모두 뒤집힘 = 5칸 전진! ]'
                };
                this.dom.resultDetailText.textContent = detailMap[rollResult.result] || `[ ${rollResult.steps}칸 이동 ]`;
            }

            // 3. 보너스 턴 뱃지
            if (this.dom.extraTurnBadge) {
                if (rollResult.extraTurn) {
                    this.dom.extraTurnBadge.classList.remove('hidden');
                } else {
                    this.dom.extraTurnBadge.classList.add('hidden');
                }
            }

            this.dom.resultBadge.classList.remove('hidden');
            this.dom.resultBadge.classList.remove('yut-result-badge');
            void this.dom.resultBadge.offsetWidth;
            this.dom.resultBadge.classList.add('yut-result-badge');
        }

        // ==========================================
        // 7. 말 선택 및 하이라이트
        // ==========================================
        preparePieceSelection() {
            const currentTeam = this.getCurrentTeam();
            // 완주하지 않은 모든 말 중 이동 가능한 말 탐색
            const movablePieces = currentTeam.pieces.filter(p => {
                if (p.isGoal) return false;
                const pathInfo = calculateMovePath(p, this.currentRoll.result);
                return pathInfo.valid;
            });

            if (movablePieces.length === 0) {
                this.logMessage(`⚠️ 이동할 수 있는 말이 없습니다. 턴이 넘어갑니다.`);
                setTimeout(() => this.endTurn(), 1200);
                return;
            }

            this.setGameState('WAIT_PIECE_SELECT');
            this.renderAllPieces(movablePieces);

            // 컴퓨터 AI인 경우 스스로 말 선택
            if (currentTeam.isAI) {
                setTimeout(() => this.executeAIChoice(movablePieces), 1000);
            } else {
                this.logMessage(`👉 이동할 말을 클릭하세요! (말을 선택하면 영어 문제가 출제됩니다)`);
            }
        }

        /**
         * 말 렌더링 및 하이라이트
         */
        renderAllPieces(selectablePieces = []) {
            if (!this.dom.piecesLayer) return;
            this.dom.piecesLayer.innerHTML = '';

            const selectableSet = new Set((selectablePieces || []).map(p => `${p.teamId}_${p.id}`));

            // 노드별로 말들을 그룹핑 (말 업기 시 겹쳐서 표시)
            const nodeMap = new Map();

            this.teams.forEach(team => {
                team.pieces.forEach(piece => {
                    if (piece.isGoal) return; // 완주한 말은 판에 표시하지 않음
                    const key = piece.isHome ? `home_${team.id}` : `node_${piece.pos}`;
                    if (!nodeMap.has(key)) nodeMap.set(key, []);
                    nodeMap.get(key).push(piece);
                });
            });

            // 1. 판 위의 말들 렌더링
            nodeMap.forEach((piecesAtLocation, locKey) => {
                if (locKey.startsWith('home_')) {
                    // 대기실의 말들은 대기실 영역에 배치 (우하단 대기존)
                    return;
                }

                const nodeId = parseInt(locKey.replace('node_', ''), 10);
                const nodeInfo = BOARD_NODES[nodeId];
                if (!nodeInfo) return;

                // 대표 말 (업힌 말의 경우 가장 상위 말)
                const topPiece = piecesAtLocation[0];
                const isSelectable = selectableSet.has(`${topPiece.teamId}_${topPiece.id}`);
                const isSelected = (this.selectedPiece && this.selectedPiece.teamId === topPiece.teamId && this.selectedPiece.id === topPiece.id);

                const pieceEl = document.createElement('div');
                pieceEl.className = `yut-piece team-${topPiece.teamId} ${isSelectable ? 'selectable' : ''} ${isSelected ? 'selected' : ''}`;
                pieceEl.style.left = `${nodeInfo.x}%`;
                pieceEl.style.top = `${nodeInfo.y}%`;
                pieceEl.style.transform = 'translate(-50%, -50%)';
                pieceEl.innerHTML = `<span>${topPiece.id + 1}</span>`;

                // 말 업기 배지 (2개 이상 겹쳤을 때)
                const totalStacked = piecesAtLocation.length;
                if (totalStacked > 1) {
                    const badge = document.createElement('span');
                    badge.className = 'stacked-badge';
                    badge.textContent = `x${totalStacked}`;
                    pieceEl.appendChild(badge);
                }

                if (isSelectable) {
                    pieceEl.addEventListener('click', (e) => {
                        e.stopPropagation();
                        this.onPieceSelected(topPiece);
                    });
                }

                this.dom.piecesLayer.appendChild(pieceEl);
            });

            // 2. 대기실(START)에 있는 말들 렌더링
            this.renderWaitingRoomPieces(selectableSet);
        }

        renderWaitingRoomPieces(selectableSet) {
            const waitingArea = document.getElementById('waitingPiecesArea');
            if (!waitingArea) return;

            waitingArea.innerHTML = '';
            this.teams.forEach(team => {
                const homePieces = team.pieces.filter(p => p.isHome && !p.isGoal);
                if (homePieces.length === 0) return;

                const teamBox = document.createElement('div');
                teamBox.className = 'flex items-center gap-2 mb-2 p-2 rounded-xl bg-slate-800/60 border border-slate-700';

                const title = document.createElement('span');
                title.className = 'text-xs font-bold text-slate-300 w-16 truncate';
                title.textContent = team.name;
                teamBox.appendChild(title);

                const piecesWrapper = document.createElement('div');
                piecesWrapper.className = 'flex gap-2';

                homePieces.forEach(p => {
                    const isSelectable = selectableSet.has(`${p.teamId}_${p.id}`);
                    const isSelected = (this.selectedPiece && this.selectedPiece.teamId === p.teamId && this.selectedPiece.id === p.id);

                    const pBtn = document.createElement('button');
                    pBtn.className = `w-8 h-8 rounded-full border-2 border-white flex items-center justify-center font-black text-xs text-white shadow-md transition-all team-${p.teamId} ${isSelectable ? 'selectable hover:scale-110' : 'opacity-60 cursor-not-allowed'} ${isSelected ? 'ring-4 ring-yellow-400 scale-110' : ''}`;
                    pBtn.textContent = p.id + 1;
                    pBtn.title = `${team.name} 대기 말 #${p.id + 1} (출발 대기)`;

                    if (isSelectable) {
                        pBtn.addEventListener('click', () => this.onPieceSelected(p));
                    }
                    piecesWrapper.appendChild(pBtn);
                });

                teamBox.appendChild(piecesWrapper);
                waitingArea.appendChild(teamBox);
            });
        }

        onPieceSelected(piece) {
            if (this.gameState !== 'WAIT_PIECE_SELECT') return;

            if (this.isOnline) {
                const mySessionId = (window.YutnoriMultiplayer && window.YutnoriMultiplayer.mySessionId);
                if (!this.currentRoomData || this.currentRoomData.currentTurnId !== mySessionId) return;
            }

            const currentTeam = this.getCurrentTeam();
            if (piece.teamId !== currentTeam.id) return;

            this.selectedPiece = piece;
            Sound.playStep();

            // 이동 경로 및 목적지 노드 계산
            const movePath = calculateMovePath(piece, this.currentRoll.result);
            this.highlightTargetNode(movePath);

            // 영어 퀴즈 팝업 출제!
            setTimeout(() => {
                this.presentQuiz(piece, movePath);
            }, 300);
        }

        highlightTargetNode(movePath) {
            // 기존 하이라이트 제거
            document.querySelectorAll('.yut-node.highlight-target').forEach(el => {
                el.classList.remove('highlight-target');
            });

            if (movePath.isGoal) {
                // 골인은 참먹이(0)를 반짝이게 표시
                const node0 = document.getElementById('node-0');
                if (node0) node0.classList.add('highlight-target');
            } else if (movePath.finalNode >= 0) {
                const targetEl = document.getElementById(`node-${movePath.finalNode}`);
                if (targetEl) targetEl.classList.add('highlight-target');
            }
        }

        // ==========================================
        // 8. 영어 퀴즈 출제 & 풀이 시스템
        // ==========================================
        presentQuiz(piece, movePath) {
            this.setGameState('WAIT_QUIZ');
            const q = window.YutnoriQuestions.generateQuestion();
            this.currentQuestion = q;

            if (!this.dom.quizModal) return;

            // 컴퓨터 AI인 경우 시뮬레이션으로 처리
            const currentTeam = this.getCurrentTeam();
            if (currentTeam.isAI) {
                this.simulateAIQuiz(piece, movePath, q);
                return;
            }

            // 모달 UI 세팅
            this.dom.quizCategoryBadge.textContent = q.category;
            this.dom.quizPrompt.textContent = q.prompt;
            this.dom.quizQuestionText.textContent = q.questionText;
            this.dom.quizSubText.textContent = q.subText || '';

            // TTS 자동 재생 (원어민 발음)
            if (q.ttsText) {
                setTimeout(() => Sound.speakText(q.ttsText), 200);
            }

            if (q.type === 'sent_scramble') {
                // 어순 배열 퀴즈 UI
                this.dom.quizChoicesGrid.classList.add('hidden');
                this.dom.quizScrambleArea.classList.remove('hidden');
                this.setupScrambleQuizUI(q, piece, movePath);
            } else {
                // 객관식 4지선다 퀴즈 UI
                this.dom.quizScrambleArea.classList.add('hidden');
                this.dom.quizChoicesGrid.classList.remove('hidden');
                this.setupMultipleChoiceUI(q, piece, movePath);
            }

            this.dom.quizModal.classList.remove('hidden');
        }

        setupMultipleChoiceUI(q, piece, movePath) {
            this.dom.quizChoicesGrid.innerHTML = '';
            q.choices.forEach((choiceText, idx) => {
                const btn = document.createElement('button');
                btn.className = 'w-full py-3.5 px-4 rounded-xl border-2 border-slate-200 bg-white text-slate-800 font-bold text-base hover:bg-amber-50 hover:border-amber-400 active:scale-98 transition-all flex items-center justify-between shadow-sm';
                btn.innerHTML = `
                    <div class="flex items-center gap-3">
                        <span class="w-7 h-7 rounded-full bg-slate-100 text-slate-600 flex items-center justify-center text-sm font-black border border-slate-300">${idx + 1}</span>
                        <span>${choiceText}</span>
                    </div>
                    <i class="fa-solid fa-chevron-right text-slate-400 text-xs"></i>
                `;

                btn.addEventListener('click', () => {
                    this.onChoiceSelected(idx, choiceText, q, piece, movePath);
                });
                this.dom.quizChoicesGrid.appendChild(btn);
            });
        }

        setupScrambleQuizUI(q, piece, movePath) {
            this.scrambleState = {
                tokens: [...q.tokens],
                selected: [],
                target: q,
                piece: piece,
                movePath: movePath
            };
            this.renderScrambleTokens();
        }

        renderScrambleTokens() {
            if (!this.dom.quizAnswerTokensContainer || !this.dom.quizSourceTokensContainer) return;

            // 1. 조립된 단어들 (답안 영역)
            this.dom.quizAnswerTokensContainer.innerHTML = '';
            if (this.scrambleState.selected.length === 0) {
                this.dom.quizAnswerTokensContainer.innerHTML = '<span class="text-slate-400 text-sm">아래 단어들을 눌러 문장을 만드세요.</span>';
            } else {
                this.scrambleState.selected.forEach((word, idx) => {
                    const chip = document.createElement('span');
                    chip.className = 'scramble-chip px-3 py-1.5 rounded-lg bg-amber-500 text-white font-bold text-sm shadow hover:bg-amber-600 flex items-center gap-1.5';
                    chip.innerHTML = `${word} <i class="fa-solid fa-xmark text-xs opacity-75"></i>`;
                    chip.addEventListener('click', () => {
                        // 취소하고 다시 보기로 복원
                        this.scrambleState.selected.splice(idx, 1);
                        this.scrambleState.tokens.push(word);
                        this.renderScrambleTokens();
                    });
                    this.dom.quizAnswerTokensContainer.appendChild(chip);
                });
            }

            // 2. 선택 가능한 단어들 (보기 영역)
            this.dom.quizSourceTokensContainer.innerHTML = '';
            this.scrambleState.tokens.forEach((word, idx) => {
                const chip = document.createElement('button');
                chip.className = 'scramble-chip px-3.5 py-2 rounded-xl bg-slate-100 hover:bg-slate-200 border border-slate-300 text-slate-800 font-bold text-sm shadow-sm';
                chip.textContent = word;
                chip.addEventListener('click', () => {
                    this.scrambleState.selected.push(word);
                    this.scrambleState.tokens.splice(idx, 1);
                    this.renderScrambleTokens();
                });
                this.dom.quizSourceTokensContainer.appendChild(chip);
            });
        }

        resetScrambleQuiz() {
            if (!this.scrambleState) return;
            this.scrambleState.tokens = [...this.scrambleState.target.tokens];
            this.scrambleState.selected = [];
            this.renderScrambleTokens();
        }

        submitScrambleQuiz() {
            if (!this.scrambleState) return;
            const userSentence = this.scrambleState.selected.join(' ').trim();
            const correctSentence = this.scrambleState.target.answerSentence.trim();
            const isCorrect = (userSentence === correctSentence);

            this.dom.quizModal.classList.add('hidden');
            this.handleQuizResult(isCorrect, this.scrambleState.target, this.scrambleState.piece, this.scrambleState.movePath);
        }

        onChoiceSelected(choiceIdx, choiceText, q, piece, movePath) {
            this.dom.quizModal.classList.add('hidden');
            const isCorrect = (choiceText === q.answer);
            this.handleQuizResult(isCorrect, q, piece, movePath);
        }

        /**
         * 퀴즈 정답/오답 결과 처리 및 피드백 팝업
         */
        handleQuizResult(isCorrect, q, piece, movePath) {
            const currentTeam = this.getCurrentTeam();
            const playerName = this.getCurrentPlayerName();
            const isAI = currentTeam && currentTeam.isAI;

            // 실시간 온라인 모드일 때 결과 송신
            if (this.isOnline && window.YutnoriMultiplayer) {
                window.YutnoriMultiplayer.sendQuizResult(isCorrect, q.explanation, q.answer || q.answerSentence);
            }

            if (isCorrect) {
                Sound.playCorrect();
                this.recordStat(playerName, 'correctCount', 1);
                this.logMessage(`🎉 [${currentTeam.name}] ${playerName}님이 문제를 맞혔습니다! 말이 ${this.currentRoll.name}만큼 전진합니다.`);

                this.showFeedbackModal({
                    title: isAI ? '컴퓨터 정답! 🤖' : '정답입니다! 👏',
                    message: isAI
                        ? `컴퓨터가 정답을 맞혀 말이 ${this.currentRoll.name}만큼 전진합니다!`
                        : `훌륭합니다! 말이 ${this.currentRoll.name}만큼 윷판을 전진합니다!`,
                    explanation: q.explanation,
                    isSuccess: true,
                    onClose: () => {
                        this.executePieceMovement(piece, movePath);
                    }
                });
            } else {
                Sound.playWrong();
                this.recordStat(playerName, 'wrongCount', 1);
                this.logMessage(`❌ [${currentTeam.name}] ${playerName}님이 아쉽게 문제를 틀렸습니다. (정답: ${q.answer || q.answerSentence}) 전진하지 못합니다.`);

                this.showFeedbackModal({
                    title: isAI ? '컴퓨터 오답! 🤖' : '아쉽습니다! 💡',
                    message: isAI
                        ? '컴퓨터가 문제를 맞히지 못하여 전진하지 못하고 턴이 종료됩니다.'
                        : '문제를 맞히지 못하여 전진하지 못하고 턴이 종료됩니다.',
                    explanation: q.explanation,
                    isSuccess: false,
                    onClose: () => {
                        this.clearHighlights();
                        this.endTurn();
                    }
                });
            }
        }

        closeFeedbackModal() {
            if (this._feedbackAutoCloseTimer) {
                clearTimeout(this._feedbackAutoCloseTimer);
                this._feedbackAutoCloseTimer = null;
            }
            if (this.dom.feedbackModal) {
                this.dom.feedbackModal.classList.add('hidden');
            }
            if (this._onFeedbackClosed) {
                const cb = this._onFeedbackClosed;
                this._onFeedbackClosed = null;
                cb();
            }
        }

        showFeedbackModal({ title, message, explanation, isSuccess, onClose }) {
            if (this._feedbackAutoCloseTimer) {
                clearTimeout(this._feedbackAutoCloseTimer);
                this._feedbackAutoCloseTimer = null;
            }

            this.dom.feedbackTitle.textContent = title;
            this.dom.feedbackTitle.className = `font-black text-2xl ${isSuccess ? 'text-emerald-600' : 'text-rose-600'}`;
            this.dom.feedbackMessage.textContent = message;
            this.dom.feedbackExplanation.textContent = explanation || '';

            this._onFeedbackClosed = onClose;
            this.dom.feedbackModal.classList.remove('hidden');

            const currentTeam = this.getCurrentTeam();
            const isAI = currentTeam && currentTeam.isAI;

            if (isAI) {
                // 컴퓨터 턴인 경우: 자동 진행 안내 및 1.5초 후 자동 닫힘
                if (this.dom.feedbackAutoCloseNotice) {
                    this.dom.feedbackAutoCloseNotice.classList.remove('hidden');
                }
                if (this.dom.feedbackCountdownText) {
                    this.dom.feedbackCountdownText.textContent = '1.5초 후 자동 진행...';
                }
                if (this.dom.feedbackOkBtnText) {
                    this.dom.feedbackOkBtnText.textContent = '확인 (자동 진행 중...)';
                }

                this._feedbackAutoCloseTimer = setTimeout(() => {
                    this.closeFeedbackModal();
                }, 1500);
            } else {
                // 사람 플레이어 차례: 수동 확인 대기
                if (this.dom.feedbackAutoCloseNotice) {
                    this.dom.feedbackAutoCloseNotice.classList.add('hidden');
                }
                if (this.dom.feedbackOkBtnText) {
                    this.dom.feedbackOkBtnText.textContent = '확인';
                }
            }
        }

        // ==========================================
        // 9. 말 이동 애니메이션 및 말 잡기/업기
        // ==========================================
        executePieceMovement(piece, movePath) {
            this.setGameState('MOVING');
            this.clearHighlights();

            const currentTeam = this.getCurrentTeam();

            // 이전 위치를 히스토리에 기록 (빽도 용)
            if (!piece.isHome && piece.pos >= 0) {
                piece.history.push(piece.pos);
            }

            piece.isHome = false;

            // 한 칸씩 따각따각 전진 연출
            let stepIndex = 0;
            const animateStep = () => {
                if (stepIndex < movePath.path.length) {
                    const nextNodeId = movePath.path[stepIndex];
                    Sound.playStep();

                    if (nextNodeId === -1) {
                        // 완주
                        piece.isGoal = true;
                    } else {
                        piece.pos = nextNodeId;
                    }

                    // 업혀있는 말들도 함께 위치 동기화
                    if (piece.stackedPieces && piece.stackedPieces.length > 0) {
                        piece.stackedPieces.forEach(subId => {
                            const subP = currentTeam.pieces.find(p => p.id === subId);
                            if (subP) {
                                subP.isHome = false;
                                subP.pos = piece.pos;
                                subP.isGoal = piece.isGoal;
                            }
                        });
                    }

                    this.renderAllPieces();
                    stepIndex++;
                    setTimeout(animateStep, 220);
                } else {
                    // 최종 착지 처리
                    this.onPieceLanded(piece, movePath);
                }
            };

            animateStep();
        }

        onPieceLanded(piece, movePath) {
            const currentTeam = this.getCurrentTeam();
            piece.route = movePath.finalRoute;

            // 업혀있는 말들의 경로도 동기화
            if (piece.stackedPieces && piece.stackedPieces.length > 0) {
                piece.stackedPieces.forEach(subId => {
                    const subP = currentTeam.pieces.find(p => p.id === subId);
                    if (subP) {
                        subP.route = piece.route;
                        subP.pos = piece.pos;
                    }
                });
            }

            // 1. 골인(완주) 확인
            if (movePath.isGoal) {
                piece.isGoal = true;
                Sound.playGoal();
                const currentPlayerName = this.getCurrentPlayerName();
                this.recordStat(currentPlayerName, 'goalCount', 1);
                this.logMessage(`🚩 [${currentTeam.name}] 말이 완주(골인)했습니다!`);

                // 함께 업혀있던 말들도 모두 완주!
                if (piece.stackedPieces && piece.stackedPieces.length > 0) {
                    this.recordStat(currentPlayerName, 'goalCount', piece.stackedPieces.length);
                    piece.stackedPieces.forEach(pId => {
                        const stPiece = currentTeam.pieces.find(p => p.id === pId);
                        if (stPiece) stPiece.isGoal = true;
                    });
                    this.logMessage(`🎉 업혀있던 말들도 함께 완주하여 총 ${piece.stackedPieces.length + 1}마리가 골인했습니다!`);
                }

                // 승리 조건 체크 (팀의 모든 말이 골인했는지)
                const remaining = currentTeam.pieces.filter(p => !p.isGoal);
                if (remaining.length === 0) {
                    this.triggerVictory(currentTeam);
                    return;
                }

                this.renderAllPieces();
                setTimeout(() => this.endTurn(), 800);
                return;
            }

            // 2. 다른 말들과의 상호작용 (업기 or 잡기)
            const targetNode = movePath.finalNode;

            // 판 위의 다른 말들 탐색
            let caughtEnemy = false;
            let stackedAlly = false;

            this.teams.forEach(team => {
                if (team.id === currentTeam.id) {
                    // 아군 말: 업기 (포개기)
                    team.pieces.forEach(otherPiece => {
                        if (otherPiece.id !== piece.id && !otherPiece.isGoal && !otherPiece.isHome && otherPiece.pos === targetNode) {
                            stackedAlly = true;
                            // piece에 otherPiece를 업음
                            if (!piece.stackedPieces.includes(otherPiece.id)) {
                                piece.stackedPieces.push(otherPiece.id);
                            }
                            if (otherPiece.stackedPieces) {
                                otherPiece.stackedPieces.forEach(subId => {
                                    if (!piece.stackedPieces.includes(subId)) piece.stackedPieces.push(subId);
                                });
                            }
                            // otherPiece는 piece의 서브가 됨
                            otherPiece.pos = targetNode;
                        }
                    });
                } else {
                    // 적군 말: 잡기!
                    team.pieces.forEach(enemyPiece => {
                        if (!enemyPiece.isGoal && !enemyPiece.isHome && enemyPiece.pos === targetNode) {
                            caughtEnemy = true;
                            // 상대 말 대기실로 퇴출!
                            enemyPiece.isHome = true;
                            enemyPiece.pos = -1;
                            enemyPiece.route = 'OUTER';
                            enemyPiece.history = [];

                            // 상대 말에 업혀있던 말들도 모두 퇴출!
                            if (enemyPiece.stackedPieces) {
                                enemyPiece.stackedPieces.forEach(subId => {
                                    const subP = team.pieces.find(p => p.id === subId);
                                    if (subP) {
                                        subP.isHome = true;
                                        subP.pos = -1;
                                        subP.route = 'OUTER';
                                        subP.history = [];
                                    }
                                });
                                enemyPiece.stackedPieces = [];
                            }

                            Sound.playCatch();
                            this.logMessage(`💥 [${currentTeam.name}]이(가) [${team.name}]의 말을 잡았습니다! (윷을 한 번 더 던집니다!)`);
                            this.pendingExtraTurn = true; // 말 잡으면 한 번 더 던지기!
                        }
                    });
                }
            });

            if (caughtEnemy) {
                const currentPlayerName = this.getCurrentPlayerName();
                this.recordStat(currentPlayerName, 'catchCount', 1);
            }

            if (stackedAlly) {
                const currentPlayerName = this.getCurrentPlayerName();
                this.recordStat(currentPlayerName, 'stackCount', 1);
                this.logMessage(`🤝 아군 말을 업었습니다! (함께 전진합니다: x${piece.stackedPieces.length + 1})`);
            }

            this.renderAllPieces();
            setTimeout(() => this.endTurn(), 800);
        }

        clearHighlights() {
            document.querySelectorAll('.yut-node.highlight-target').forEach(el => {
                el.classList.remove('highlight-target');
            });
        }

        // ==========================================
        // 10. 턴 종료 & 다음 턴 관리
        // ==========================================
        endTurn() {
            this.selectedPiece = null;
            this.clearHighlights();

            // 실시간 온라인 모드일 때 Firestore 동기화
            if (this.isOnline && window.YutnoriMultiplayer) {
                const mySessionId = window.YutnoriMultiplayer.mySessionId;
                if (this.currentRoomData && this.currentRoomData.currentTurnId === mySessionId) {
                    const teamsState = this.teams.map(t => ({
                        key: t.key || (t.id === 0 ? 'blue' : 'white'),
                        name: t.name,
                        pieces: t.pieces
                    }));
                    window.YutnoriMultiplayer.sendPieceMovement({
                        teamsState,
                        hasExtraTurn: this.pendingExtraTurn,
                        winnerTeam: null
                    });
                }
            }

            // 추가 턴(윷, 모, 또는 말 잡기)이 남아있는 경우
            if (this.pendingExtraTurn) {
                this.pendingExtraTurn = false;
                this.logMessage(`🔄 추가 기회! [${this.getCurrentTeam().name}] 윷을 한 번 더 던지세요!`);
                this.setGameState('WAIT_ROLL');
                this.updateTurnUI();
                this.checkAndTriggerAI();
                return;
            }

            // 다음 팀(또는 다음 플레이어)으로 턴 넘기기
            const currentTeam = this.getCurrentTeam();
            // 팀 내 다음 선수로 로테이션
            currentTeam.currentPlayerIdx = (currentTeam.currentPlayerIdx + 1) % currentTeam.players.length;

            // 다음 팀으로 교체
            this.currentTeamIndex = (this.currentTeamIndex + 1) % this.teams.length;
            this.setGameState('WAIT_ROLL');
            this.updateTurnUI();

            this.checkAndTriggerAI();
        }

        // ==========================================
        // 11. 인공지능 (AI) 의사결정 알고리즘
        // ==========================================
        checkAndTriggerAI() {
            const currentTeam = this.getCurrentTeam();
            if (currentTeam.isAI && this.gameState === 'WAIT_ROLL') {
                setTimeout(() => {
                    this.executeRoll();
                }, 1000);
            }
        }

        executeAIChoice(movablePieces) {
            const currentTeam = this.getCurrentTeam();
            const roll = this.currentRoll;

            // AI 최적의 말 선택 평가 함수
            // 1순위: 상대 말을 잡을 수 있는 말 (+100점)
            // 2순위: 골인할 수 있는 말 (+80점)
            // 3순위: 아군 말을 업을 수 있는 말 (+50점)
            // 4순위: 대각선 지름길로 진입할 수 있는 말 (+35점)
            // 5순위: 판에 이미 나와 있는 말 전진 (+10점)
            // 6순위: 새 말 출발 (+5점)

            let bestScore = -9999;
            let bestPiece = movablePieces[0];

            movablePieces.forEach(p => {
                let score = 0;
                const pathInfo = calculateMovePath(p, roll.result);
                if (!pathInfo.valid) return;

                if (pathInfo.isGoal) {
                    score += 80 + (p.stackedPieces ? p.stackedPieces.length * 40 : 0);
                } else {
                    const targetNode = pathInfo.finalNode;

                    // 상대 말 잡기 가능 여부
                    const enemies = this.teams.filter(t => t.id !== currentTeam.id);
                    let canCatch = false;
                    enemies.forEach(et => {
                        et.pieces.forEach(ep => {
                            if (!ep.isGoal && !ep.isHome && ep.pos === targetNode) canCatch = true;
                        });
                    });
                    if (canCatch) score += 100;

                    // 아군 말 업기 가능 여부
                    let canStack = false;
                    currentTeam.pieces.forEach(ap => {
                        if (ap.id !== p.id && !ap.isGoal && !ap.isHome && ap.pos === targetNode) canStack = true;
                    });
                    if (canStack) score += 50;

                    // 지름길(5번 모서리, 10번 모서리, 22번 방) 안착
                    if (targetNode === 5 || targetNode === 10 || targetNode === 22) {
                        score += 35;
                    }

                    // 전진 보너스
                    if (!p.isHome) score += 15;
                }

                // 약간의 랜덤성 가미
                score += Math.random() * 5;

                if (score > bestScore) {
                    bestScore = score;
                    bestPiece = p;
                }
            });

            this.onPieceSelected(bestPiece);
        }

        simulateAIQuiz(piece, movePath, q) {
            // AI 난이도별 정답 확률
            let correctProb = 0.85; // normal
            if (this.difficulty === 'easy') correctProb = 0.70;
            if (this.difficulty === 'hard') correctProb = 0.96;

            const isCorrect = Math.random() < correctProb;

            // AI 생각하는 시간 연출
            this.logMessage(`🤖 컴퓨터가 영어 문제를 풀고 있습니다...`);
            setTimeout(() => {
                this.handleQuizResult(isCorrect, q, piece, movePath);
            }, 1200);
        }

        // ==========================================
        // 12. 게임 승리 & 축하 팡파르 & 기여도 HOF 저장
        // ==========================================
        triggerVictory(winningTeam) {
            this.setGameState('GAME_OVER');
            Sound.playVictory();

            // 실시간 온라인 모드일 때 승리 상태 Firestore 동기화
            if (this.isOnline && window.YutnoriMultiplayer) {
                const mySessionId = window.YutnoriMultiplayer.mySessionId;
                if (this.currentRoomData && this.currentRoomData.currentTurnId === mySessionId) {
                    const teamsState = this.teams.map(t => ({
                        key: t.key || (t.id === 0 ? 'blue' : 'white'),
                        name: t.name,
                        pieces: t.pieces
                    }));
                    window.YutnoriMultiplayer.sendPieceMovement({
                        teamsState,
                        hasExtraTurn: false,
                        winnerTeam: winningTeam.name
                    });
                }
            }

            // 폭죽 효과 (canvas-confetti)
            if (typeof confetti === 'function') {
                confetti({
                    particleCount: 120,
                    spread: 80,
                    origin: { y: 0.6 }
                });
                setTimeout(() => {
                    confetti({ particleCount: 80, spread: 100, origin: { x: 0.2, y: 0.5 } });
                    confetti({ particleCount: 80, spread: 100, origin: { x: 0.8, y: 0.5 } });
                }, 400);
            }

            this.logMessage(`🏆🎉 [${winningTeam.name}]이(가) 모든 말을 완주시켜 최종 승리했습니다!`);

            // 1. 경기 참가자별 기여도 점수 산출
            // 공식: (승리팀 ? +500 : 0) + (정답수 * 50) + (완주수 * 150) + (잡은말 * 100) + (업은말 * 40) + (윷/모 * 30)
            const participants = [];
            Object.values(this.matchStats).forEach(stat => {
                const isWinner = (stat.teamId === winningTeam.id);
                const winBonus = isWinner ? 500 : 0;
                const matchScore = winBonus
                    + ((stat.correctCount || 0) * 50)
                    + ((stat.goalCount || 0) * 150)
                    + ((stat.catchCount || 0) * 100)
                    + ((stat.stackCount || 0) * 40)
                    + ((stat.yutMoCount || 0) * 30);

                stat.isWinner = isWinner;
                stat.matchScore = matchScore;
                participants.push(stat);
            });

            // 기여도 점수 높은 순 정렬
            participants.sort((a, b) => b.matchScore - a.matchScore);
            const mvp = participants[0];

            // 2. 명예의 전당 저장 및 뷰 갱신
            HallOfFame.recordMatch(winningTeam, participants);

            // 3. 승리 모달 UI 렌더링
            if (this.dom.victoryModal) {
                this.dom.victoryTeamName.textContent = winningTeam.name;
                this.dom.victoryDetail.textContent = `축하합니다! 영어 퀴즈를 맞히며 지혜롭게 말을 완주하여 ${winningTeam.name}이(가) 승리를 거머쥐었습니다!`;

                // 이번 경기 MVP 카드
                const mvpContainer = document.getElementById('victoryMvpCard');
                if (mvpContainer && mvp) {
                    mvpContainer.innerHTML = `
                        <div class="flex items-center justify-between p-3.5 rounded-2xl bg-gradient-to-r from-amber-500/20 via-amber-400/25 to-amber-500/20 border-2 border-amber-400 text-left">
                            <div class="flex items-center gap-3">
                                <div class="w-11 h-11 rounded-2xl bg-gradient-to-tr from-amber-400 to-amber-300 text-slate-950 flex items-center justify-center font-black text-xl shadow-md">
                                    👑
                                </div>
                                <div>
                                    <div class="text-[10px] font-black text-amber-400 uppercase tracking-widest">이번 경기 MVP</div>
                                    <div class="font-black text-base text-white">${maskStudentName(mvp.name)} <span class="text-xs text-emerald-400 font-bold ml-1">+${mvp.matchScore.toLocaleString()}점</span></div>
                                </div>
                            </div>
                            <span class="text-xs font-black px-2.5 py-1 rounded-full bg-amber-400 text-slate-950 shadow">
                                최고 기여도
                            </span>
                        </div>
                    `;
                }

                // 경기 기여도 순위표
                const statsTable = document.getElementById('victoryStatsTable');
                if (statsTable) {
                    statsTable.innerHTML = participants.map((p, idx) => {
                        const isMvp = (idx === 0);
                        const rowBg = isMvp ? 'bg-amber-500/15 font-black text-amber-300' : 'hover:bg-slate-800/60 text-slate-300';
                        const badge = isMvp ? '🥇 MVP' : `${idx + 1}위`;

                        return `
                            <tr class="border-b border-slate-800 text-center text-xs ${rowBg}">
                                <td class="py-2.5 font-bold">${badge}</td>
                                <td class="py-2.5 text-left pl-3 text-white font-semibold">${maskStudentName(p.name)}</td>
                                <td class="py-2.5 text-emerald-400 font-black">${p.matchScore.toLocaleString()}점</td>
                                <td class="py-2.5">${p.correctCount || 0}문제</td>
                                <td class="py-2.5">${p.goalCount || 0}마리</td>
                                <td class="py-2.5">${p.catchCount || 0}회</td>
                            </tr>
                        `;
                    }).join('');
                }

                this.dom.victoryModal.classList.remove('hidden');
            }
        }
    }

    // 전역 싱글턴 등록 및 유틸리티 노출
    window.YutnoriGameInstance = new YutnoriGame();
    window.YutnoriHOF = HallOfFame;
    window.YutnoriEngine = {
        BOARD_NODES,
        calculateMovePath,
        stepForward,
        YUT_PROBABILITY,
        Sound,
        HallOfFame,
        maskStudentName
    };

    // DOM 로드 완료 시 초기화
    if (typeof document !== 'undefined' && document.addEventListener) {
        document.addEventListener('DOMContentLoaded', () => {
            window.YutnoriGameInstance.initDOM();
        });
    }

})(typeof window !== 'undefined' ? window : this);
