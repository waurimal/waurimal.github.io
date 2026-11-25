/**
 * 무궁화 꽃이 피었습니다: 영어 런 - 캔버스 물리 트랙 & 게임 루프 엔진
 * 2026 Eui Elementary School - Waurimal Project
 */

class MugunghwaGame {
    constructor() {
        this.canvas = document.getElementById('gameCanvas');
        this.ctx = this.canvas ? this.canvas.getContext('2d') : null;

        // 가상 캔버스 규격 (1200 x 600)
        this.WIDTH = 1200;
        this.HEIGHT = 600;
        this.TRACK_LEFT = 100; // 0m 출발선 X
        this.TRACK_RIGHT = 1050; // 100m 결승선 X
        this.TRACK_WIDTH = this.TRACK_RIGHT - this.TRACK_LEFT;
        this.TRACK_Y_MIN = 170;
        this.TRACK_Y_MAX = 530;

        // 5개 트랙 레인 Y 좌표 (균등 배치: 210, 275, 340, 405, 470)
        this.LANES = [210, 275, 340, 405, 470];

        // 게임 상태
        this.isRunning = false;
        this.isPaused = false;
        this.playMode = 'solo'; // 'solo' (AI 봇 반 친구들과 함께) | 'multi' (온라인 실시간)
        this.timeLimit = 90; // 설정된 제한 시간(초)
        this.remainingTime = 90;
        this.gameScore = 0;

        // 술래 (Tagger) 상태 머신
        // states: 'CHANTING' (초록불: 뒤돌아 구호 외침) | 'TURNING' (주의) | 'SCANNING' (빨간불: 뒤돌아 정면 주시 & 레이저 스캔)
        this.tagger = {
            state: 'CHANTING',
            x: 1110,
            y: 330,
            headAngle: Math.PI, // 0 = 왼쪽(플레이어 주시), Math.PI = 오른쪽(나무 주시)
            chantTimer: 0,
            chantDuration: 210, // 약 3.5초
            scanTimer: 0,
            scanDuration: 130, // 약 2.1초 (정면 응시)
            speedMultiplier: 1.0,
            turnCount: 0,
            laserAlpha: 0
        };

        // 로컬 플레이어 정보
        this.player = {
            id: 'local_player',
            name: '나 (도전자)',
            grade: 6,
            classNum: 1,
            studentNum: 1,
            avatarSkin: 'runner_1',
            m: 0, // 0 ~ 100m
            checkpoint: 0, // 0, 25, 50, 75m
            laneIndex: 2, // 1~5번 레인 중 중앙(3번 레인, index 2)
            hasMovedThisTurn: false, // 한 텀에 1회 이동 제한 플래그
            pendingMove: null, // 화살표 키로 선택한 이동 대기 { direction, dirLabel, targetM, targetLane, targetX, targetY }
            displayX: this.TRACK_LEFT,
            displayY: 340,
            targetX: this.TRACK_LEFT,
            targetY: 340,
            isMoving: false,
            runFrame: 0,
            isFinished: false,
            finishRank: 0,
            isFallen: false, // 오답 후퇴 연출
            fallProgress: 0,
            buffs: {
                shoes: false, // 부스터 신발 (2배 전진)
                shield: false, // 오답 1회 방어 세이프
                star: false // 무적 대시
            }
        };

        // 반 친구들 (AI 봇 또는 멀티플레이어 참여자들)
        this.peers = [];
        this.botNames = [
            '김민준', '이서연', '박도윤', '최서윤', '정하준',
            '윤지아', '강은우', '조하은', '임시우', '한서아',
            '송예준', '신아린', '유민재', '배지우', '오시윤'
        ];

        // 100m 트랙 상의 아이템 박스들 [?]
        this.fieldItems = [];
        this.initFieldItems();

        // 파티클 & 플로팅 텍스트
        this.particles = [];
        this.floatingTexts = [];

        // 애니메이션 루프
        this.lastTime = 0;
        this.animFrameId = null;

        this.initBots();
    }

    initFieldItems() {
        this.fieldItems = [
            { id: 'f_1', m: 18, laneIndex: 1, type: 'shoes', name: '부스터 신발', icon: '⚡', collected: false },
            { id: 'f_2', m: 38, laneIndex: 3, type: 'shield', name: '방어막 쉴드', icon: '🛡️', collected: false },
            { id: 'f_3', m: 58, laneIndex: 0, type: 'time', name: '타임 캡슐 (+10초)', icon: '⏱️', collected: false },
            { id: 'f_4', m: 76, laneIndex: 4, type: 'hint', name: '50:50 힌트 패스', icon: '🎯', collected: false },
            { id: 'f_5', m: 90, laneIndex: 2, type: 'star', name: '황금 스타 대시', icon: '⭐', collected: false }
        ];
    }

    initBots() {
        this.peers = [];
        const botCount = 14;
        for (let i = 0; i < botCount; i++) {
            const laneY = this.TRACK_Y_MIN + ((this.TRACK_Y_MAX - this.TRACK_Y_MIN) / (botCount + 1)) * (i + 1);
            this.peers.push({
                id: 'bot_' + i,
                name: this.botNames[i] || `학생 ${i + 1}`,
                grade: 6,
                classNum: 1,
                studentNum: i + 2,
                avatarSkin: `runner_${(i % 5) + 1}`,
                m: 0,
                checkpoint: 0,
                displayX: this.TRACK_LEFT - (i % 3) * 15,
                displayY: laneY,
                targetX: this.TRACK_LEFT,
                targetY: laneY,
                runFrame: Math.random() * 10,
                isFinished: false,
                finishRank: 0,
                isFallen: false,
                botAnswerTimer: Math.floor(Math.random() * 80) + 40,
                buffs: { shoes: false, shield: false }
            });
        }
    }

    // 멀티플레이어 참여 학생 목록 동기화
    syncMultiplayerPlayers(playersObj) {
        if (!playersObj) return;
        const myId = (window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.myPlayerId) ? window.MugunghwaMultiplayer.myPlayerId : null;
        const playerIds = Object.keys(playersObj).filter(id => id !== myId);

        const newPeers = [];
        playerIds.forEach((id, idx) => {
            const p = playersObj[id];
            let existing = this.peers.find(peer => peer.id === id);
            const laneY = this.TRACK_Y_MIN + ((this.TRACK_Y_MAX - this.TRACK_Y_MIN) / Math.max(2, playerIds.length + 1)) * (idx + 1);
            if (!existing) {
                existing = {
                    id: id,
                    name: p.name || (p.isHost ? '선생님' : '학생'),
                    isHost: Boolean(p.isHost),
                    m: p.m || 0,
                    checkpoint: p.checkpoint || 0,
                    displayX: this.mToX(p.m || 0),
                    displayY: laneY,
                    targetX: this.mToX(p.m || 0),
                    targetY: laneY,
                    runFrame: 0,
                    isFinished: p.isFinished || false,
                    finishRank: p.finishRank || 0,
                    isFallen: p.isFallen || false,
                    buffs: p.activeBuffs || { shoes: false, shield: false }
                };
            } else {
                existing.name = p.name || existing.name;
                existing.isHost = Boolean(p.isHost);
                existing.m = p.m || 0;
                existing.checkpoint = p.checkpoint || 0;
                existing.targetX = this.mToX(p.m || 0);
                existing.isFinished = p.isFinished || false;
                existing.finishRank = p.finishRank || existing.finishRank;
                existing.buffs = p.activeBuffs || existing.buffs;
            }
            newPeers.push(existing);
        });
        this.peers = newPeers;
    }

    setTimeLimit(sec) {
        this.timeLimit = Number(sec) || 90;
        this.remainingTime = this.timeLimit;
    }

    // 미터(0~100m)를 캔버스 X 좌표로 변환
    mToX(m) {
        const clamped = Math.max(0, Math.min(100, m));
        return this.TRACK_LEFT + (clamped / 100) * this.TRACK_WIDTH;
    }

    // 경기 지속 가능 여부 판별 (관전 모드 및 호스트 참여 상태 완벽 지원)
    canContinueGame() {
        if (!this.isRunning || this.isPaused) return false;
        if (this.timeLimit > 0 && this.remainingTime <= 0) return false;
        // 1. 관전 모드: 참가 학생들이 모두 완주하면 종료
        if (this.isSpectator) {
            if (this.peers.length > 0 && this.peers.every(p => p.isFinished)) {
                return false;
            }
            return true;
        }
        // 2. 호스트가 직접 참여한 경우: 호스트 완주 후에도 남은 학생이 달리고 있으면 술래 구호 지속
        if (this.isHostPlayer) {
            if (this.player.isFinished) {
                if (this.peers.length > 0 && this.peers.every(p => p.isFinished)) {
                    return false;
                }
                return true;
            }
            return true;
        }
        // 3. 일반 솔로/학생 플레이어 모드
        return !this.player.isFinished;
    }

    startNewGame(settings = {}) {
        this.playMode = settings.mode || 'solo';
        this.timeLimit = settings.timeLimit !== undefined ? Number(settings.timeLimit) : 90;
        this.remainingTime = this.timeLimit;
        this.isRunning = true;
        this.isPaused = false;
        this.gameScore = 0;

        // 관리자 참여 여부 상태 설정
        this.isSpectator = (this.playMode === 'multi' && window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.isHost && !settings.hostParticipating);
        this.isHostPlayer = (this.playMode === 'multi' && window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.isHost && Boolean(settings.hostParticipating));

        // 플레이어 초기화
        this.player.name = settings.playerName || (this.isHostPlayer ? '선생님' : '나');
        this.player.isHost = Boolean(this.isHostPlayer);
        this.player.m = 0;
        this.player.checkpoint = 0;
        this.player.laneIndex = 2; // 중앙(3번) 레인에서 시작
        this.player.hasMovedThisTurn = false;
        this.player.pendingMove = null;
        this.player.displayX = this.TRACK_LEFT;
        this.player.targetX = this.TRACK_LEFT;
        this.player.displayY = this.LANES[2];
        this.player.targetY = this.LANES[2];
        this.player.isFinished = false;
        this.player.finishRank = 0;
        this.player.isFallen = false;
        this.player.buffs = { shoes: false, shield: false, star: false };

        this.initFieldItems();
        if (this.playMode === 'solo') {
            this.initBots();
        } else {
            this.peers = [];
            if (settings.players) {
                this.syncMultiplayerPlayers(settings.players);
            }
        }

        // 술래 초기화
        this.tagger.state = 'CHANTING';
        this.tagger.chantTimer = 0;
        this.tagger.scanTimer = 0;
        this.tagger.turnCount = 0;
        this.tagger.speedMultiplier = 1.0;
        this.tagger.laserAlpha = 0;

        this.particles = [];
        this.floatingTexts = [];

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.init();
            this.speakTaggerChant();
        }

        this.lastTime = performance.now();
        if (this.animFrameId) cancelAnimationFrame(this.animFrameId);
        this.loop = this.loop.bind(this);
        this.animFrameId = requestAnimationFrame(this.loop);
    }

    speakTaggerChant() {
        if (!this.canContinueGame()) return;

        this.tagger.turnCount++;

        // 관전 모드가 아니고 본인이 아직 완주 전일 때만 이동/문제 턴 리셋
        if (!this.isSpectator && !this.player.isFinished) {
            if (!this.player.pendingMove) {
                this.player.hasMovedThisTurn = false;
                if (typeof window !== 'undefined' && window.onNewTaggerTurn) {
                    window.onNewTaggerTurn(this.tagger.turnCount);
                }
            }
        }

        // 멀티플레이어 호스트인 경우 상태 브로드캐스트
        if (this.playMode === 'multi' && window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.isHost) {
            window.MugunghwaMultiplayer.broadcastTaggerState('CHANTING', this.tagger.speedMultiplier, this.tagger.turnCount);
        }

        // 구호 속도: 턴이 지날수록 약간씩 긴장감 넘치게 변동 (0.9 ~ 1.45배속)
        const speed = 0.95 + (Math.random() * 0.45);
        this.tagger.speedMultiplier = speed;
        this.tagger.chantDuration = Math.floor(190 / speed);

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.speakChant('무궁화 꽃이 피었습니다!', speed, () => {
                // 게임이 정상 진행 중일 때만 돌아보기
                if (this.canContinueGame()) {
                    if (this.tagger.state === 'CHANTING') {
                        this.triggerTaggerTurn();
                    }
                }
            });
        }
    }

    triggerTaggerTurn() {
        if (!this.canContinueGame()) return;

        this.tagger.state = 'SCANNING';
        this.tagger.scanTimer = 0;
        this.tagger.scanDuration = Math.floor(100 + Math.random() * 60); // 1.6 ~ 2.6초 동안 전방 레이저 스캔

        // 술래가 빨간 눈(SCANNING)으로 돌아봐도, 이미 이동을 선택한 플레이어의 pendingMove는 유지하여 문제 풀기 허용!
        if (!this.isSpectator && typeof window !== 'undefined' && window.onTaggerScanStart) {
            window.onTaggerScanStart();
        }

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.playTagScan();
        }

        // 멀티플레이어 호스트인 경우 상태 브로드캐스트
        if (this.playMode === 'multi' && window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.isHost) {
            window.MugunghwaMultiplayer.broadcastTaggerState('SCANNING', this.tagger.speedMultiplier, this.tagger.turnCount);
        }
    }

    // 플레이어가 화살표 키(←↑↓→ / WASD) 또는 온스크린 버튼으로 이동 방향을 선택
    requestMove(direction) {
        if (!this.isRunning || this.isPaused || this.player.isFinished) {
            return { success: false, reason: 'not_running' };
        }

        // 1. 술래가 빨간불 주시 중(SCANNING)일 때 움직이면 즉시 적발!
        if (this.tagger.state === 'SCANNING') {
            this.player.pendingMove = null;
            this.handleTaggerCaught();
            return { success: false, reason: 'caught_by_tagger' };
        }

        // 2. 한 텀에 1회 이동 제한: 이미 이번 턴에 이동 완료했거나 소비한 경우
        if (this.player.hasMovedThisTurn) {
            this.addFloatingText('⚠️ 이번 텀에는 이미 이동했습니다! 다음 구호를 기다리세요.', this.player.displayX, this.player.displayY - 30, '#FBBF24');
            return { success: false, reason: 'already_moved' };
        }

        const curM = this.player.m;
        const curLane = this.player.laneIndex !== undefined ? this.player.laneIndex : 2;
        let targetM = curM;
        let targetLane = curLane;
        let dirLabel = '';

        if (direction === 'up') {
            if (curLane <= 0) {
                this.addFloatingText('⚠️ 이미 최상단 1번 레인입니다!', this.player.displayX, this.player.displayY - 25, '#F87171');
                return { success: false, reason: 'boundary_top' };
            }
            targetLane = curLane - 1;
            dirLabel = `위쪽 ${targetLane + 1}번 레인으로 이동`;
        } else if (direction === 'down') {
            if (curLane >= this.LANES.length - 1) {
                this.addFloatingText('⚠️ 이미 최하단 5번 레인입니다!', this.player.displayX, this.player.displayY - 25, '#F87171');
                return { success: false, reason: 'boundary_bottom' };
            }
            targetLane = curLane + 1;
            dirLabel = `아래쪽 ${targetLane + 1}번 레인으로 이동`;
        } else if (direction === 'right') {
            if (curM >= 100) {
                return { success: false, reason: 'already_goal' };
            }
            const stepM = this.player.buffs.shoes ? 20 : 10;
            targetM = Math.min(100, curM + stepM);
            dirLabel = `앞으로 +${stepM}m 전진`;
        } else if (direction === 'left') {
            if (curM <= 0) {
                this.addFloatingText('⚠️ 출발선(0m) 이전으로는 갈 수 없습니다!', this.player.displayX, this.player.displayY - 25, '#F87171');
                return { success: false, reason: 'boundary_start' };
            }
            targetM = Math.max(0, curM - 10);
            dirLabel = `뒤로 -10m 후퇴`;
        } else {
            return { success: false, reason: 'unknown_direction' };
        }

        const targetX = this.mToX(targetM);
        const targetY = this.LANES[targetLane];

        this.player.pendingMove = {
            direction,
            dirLabel,
            targetM,
            targetLane,
            targetX,
            targetY,
            prevM: curM,
            prevLane: curLane
        };

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.playStep();
        }

        this.addFloatingText(`🎯 ${dirLabel} 선택! 문제를 맞히세요!`, this.player.displayX, this.player.displayY - 25, '#FBBF24');

        return {
            success: true,
            pendingMove: this.player.pendingMove
        };
    }

    // 퀴즈 답안 선택 시 이동 확정 또는 오답 후퇴 처리 (이동한 상태에서는 빨간 눈일 때도 정답 입력 허용)
    confirmPendingMove(isCorrect) {
        if (!this.isRunning || this.player.isFinished) return { success: false };

        const move = this.player.pendingMove;
        if (!move) {
            // 이동 방향 선택 없이(pendingMove 없음) 답변한 경우: 술래가 빨간 눈(SCANNING)이면 적발
            if (this.tagger.state === 'SCANNING') {
                this.player.hasMovedThisTurn = true;
                this.handleTaggerCaught();
                return { success: false, reason: 'caught_by_tagger' };
            }
            if (isCorrect) {
                this.handleCorrectAnswer();
            } else {
                this.handleWrongAnswer();
            }
            this.player.hasMovedThisTurn = true;
            return { success: true, moved: isCorrect };
        }

        // 이동한 상태(pendingMove 존재)에서는 술래가 빨간 눈(SCANNING) 상태여도 문제 맞히기 허용!
        // 이번 텀 이동 1회 소비 완료
        this.player.hasMovedThisTurn = true;
        this.player.pendingMove = null;

        if (isCorrect) {
            // 정답 시: 빨간 눈 상태여도 선택한 레인 및 거리로 정상 이동 완료!
            if (move.direction === 'right') {
                if (this.player.buffs.shoes) {
                    this.player.buffs.shoes = false;
                    if (window.MugunghwaAudio) window.MugunghwaAudio.playBoost();
                    this.addFloatingText('⚡ 부스터 2배 질주! +20m 전진 완료', move.targetX, move.targetY - 25, '#F59E0B');
                } else {
                    if (window.MugunghwaAudio) window.MugunghwaAudio.playStep();
                    this.addFloatingText('+10m 전진 완료!', move.targetX, move.targetY - 20, '#10B981');
                }
            } else if (move.direction === 'left') {
                if (window.MugunghwaAudio) window.MugunghwaAudio.playStep();
                this.addFloatingText('-10m 뒤로 이동 완료!', move.targetX, move.targetY - 20, '#60A5FA');
            } else if (move.direction === 'up') {
                if (window.MugunghwaAudio) window.MugunghwaAudio.playStep();
                this.addFloatingText(`⬆️ 위쪽 ${move.targetLane + 1}번 레인 이동 완료!`, move.targetX, move.targetY - 20, '#34D399');
            } else if (move.direction === 'down') {
                if (window.MugunghwaAudio) window.MugunghwaAudio.playStep();
                this.addFloatingText(`⬇️ 아래쪽 ${move.targetLane + 1}번 레인 이동 완료!`, move.targetX, move.targetY - 20, '#34D399');
            }

            this.player.m = move.targetM;
            this.player.laneIndex = move.targetLane;
            this.player.targetX = move.targetX;
            this.player.targetY = move.targetY;
            this.player.isMoving = true;
            this.gameScore += 150;

            // 체크포인트 도달 확인 (25m, 50m, 75m)
            if (this.player.m >= 75 && this.player.checkpoint < 75) {
                this.player.checkpoint = 75;
                this.addFloatingText('🚩 75m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
            } else if (this.player.m >= 50 && this.player.checkpoint < 50) {
                this.player.checkpoint = 50;
                this.addFloatingText('🚩 50m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
            } else if (this.player.m >= 25 && this.player.checkpoint < 25) {
                this.player.checkpoint = 25;
                this.addFloatingText('🚩 25m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
            }

            // 트랙 상 아이템 획득 검사
            this.checkFieldItemPickup(this.player);

            // 100m 결승선 골인 판정!
            if (this.player.m >= 100 && !this.player.isFinished) {
                this.triggerPlayerGoal();
            }

            // 멀티플레이어 위치 동기화
            if (this.playMode === 'multi' && window.MugunghwaMultiplayer) {
                window.MugunghwaMultiplayer.advancePlayer(move.targetM - move.prevM, null, move.targetLane);
            }

            return { success: true, moved: true, move };
        } else {
            // 오답 시: 이동 취소 및 직전 체크포인트로 강제 후퇴!
            this.handleWrongAnswer();
            return { success: false, moved: false, move };
        }
    }

    // 퀴즈 정답 시: 앞으로 전진!
    handleCorrectAnswer() {
        if (!this.isRunning || this.player.isFinished) return;

        // 부스터 신발 착용 시 2배 전진 (+20m), 기본 +10m 전진
        let stepM = 10;
        if (this.player.buffs.shoes) {
            stepM = 20;
            this.player.buffs.shoes = false;
            if (window.MugunghwaAudio) window.MugunghwaAudio.playBoost();
            this.addFloatingText('⚡ 부스터 2배 질주! +20m', this.player.displayX, this.player.displayY - 25, '#F59E0B');
        } else {
            if (window.MugunghwaAudio) window.MugunghwaAudio.playStep();
            this.addFloatingText('+10m 전진!', this.player.displayX, this.player.displayY - 20, '#10B981');
        }

        this.player.m = Math.min(100, this.player.m + stepM);
        this.player.targetX = this.mToX(this.player.m);
        this.player.isMoving = true;
        this.gameScore += 150;

        // 체크포인트 도달 확인 (25m, 50m, 75m)
        if (this.player.m >= 75 && this.player.checkpoint < 75) {
            this.player.checkpoint = 75;
            this.addFloatingText('🚩 75m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
        } else if (this.player.m >= 50 && this.player.checkpoint < 50) {
            this.player.checkpoint = 50;
            this.addFloatingText('🚩 50m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
        } else if (this.player.m >= 25 && this.player.checkpoint < 25) {
            this.player.checkpoint = 25;
            this.addFloatingText('🚩 25m 안전 체크포인트 확보!', this.player.displayX, this.player.displayY - 35, '#38BDF8');
        }

        // 트랙 상 아이템 획득 검사
        this.checkFieldItemPickup(this.player);

        // 100m 결승선 골인 판정!
        if (this.player.m >= 100 && !this.player.isFinished) {
            this.triggerPlayerGoal();
        }

        // 멀티플레이어 위치 동기화
        if (this.playMode === 'multi' && window.MugunghwaMultiplayer) {
            window.MugunghwaMultiplayer.advancePlayer(stepM);
        }
    }

    // 퀴즈 오답 시: 직전 체크포인트(원위치)로 강제 후퇴!
    handleWrongAnswer() {
        if (!this.isRunning || this.player.isFinished) return;

        // 쉴드 방어막이 있으면 1회 세이프!
        if (this.player.buffs.shield) {
            this.player.buffs.shield = false;
            if (window.MugunghwaAudio) window.MugunghwaAudio.playShieldDefend();
            this.addFloatingText('🛡️ 쉴드 방어 세이프! 후퇴 무효화', this.player.displayX, this.player.displayY - 25, '#38BDF8');
            return;
        }

        const safeM = this.player.checkpoint;
        const prevM = this.player.m;
        this.player.m = safeM;
        this.player.targetX = this.mToX(safeM);
        this.player.isFallen = true;
        this.player.fallProgress = 0;

        if (window.MugunghwaAudio) window.MugunghwaAudio.playWrongFall();

        const msg = safeM === 0 ? '💥 오답! 출발선 원위치로 강제 후퇴!' : `💥 오답! ${safeM}m 체크포인트로 후퇴!`;
        this.addFloatingText(msg, this.player.displayX, this.player.displayY - 30, '#EF4444');

        if (this.playMode === 'multi' && window.MugunghwaMultiplayer) {
            window.MugunghwaMultiplayer.resetPlayerToCheckpoint();
        }
    }

    // 술래가 빨간불로 주시 중(SCANNING)일 때 오답이거나 함부로 움직였을 때 적발
    handleTaggerCaught() {
        if (!this.isRunning || this.player.isFinished) return;

        if (this.player.buffs.shield) {
            this.player.buffs.shield = false;
            if (window.MugunghwaAudio) window.MugunghwaAudio.playShieldDefend();
            this.addFloatingText('🛡️ 술래 감시 쉴드 방어 세이프!', this.player.displayX, this.player.displayY - 25, '#38BDF8');
            return;
        }

        const safeM = this.player.checkpoint;
        this.player.m = safeM;
        this.player.targetX = this.mToX(safeM);
        this.player.isFallen = true;
        this.player.fallProgress = 0;

        if (window.MugunghwaAudio) window.MugunghwaAudio.playWrongFall();
        this.addFloatingText('🚨 술래에게 포착! 원위치로 후퇴!', this.player.displayX, this.player.displayY - 30, '#DC2626');
    }

    checkFieldItemPickup(p) {
        this.fieldItems.forEach(item => {
            if (!item.collected) {
                const laneMatch = (item.laneIndex === undefined || item.laneIndex === p.laneIndex);
                if (laneMatch && p.m >= item.m - 4 && p.m <= item.m + 8) {
                    item.collected = true;
                    if (window.MugunghwaAudio) window.MugunghwaAudio.playItemGet();

                    if (item.type === 'shoes') {
                        p.buffs.shoes = true;
                        this.addFloatingText('⚡ 부스터 신발 획득! (다음 정답 시 2배 전진)', p.displayX, p.displayY - 30, '#F59E0B');
                    } else if (item.type === 'shield') {
                        p.buffs.shield = true;
                        this.addFloatingText('🛡️ 방어막 쉴드 획득! (오답/감시 1회 방어)', p.displayX, p.displayY - 30, '#38BDF8');
                    } else if (item.type === 'time') {
                        this.remainingTime += 10;
                        this.addFloatingText('⏱️ 시간 보너스! +10초 추가', p.displayX, p.displayY - 30, '#10B981');
                    } else if (item.type === 'hint') {
                        if (window.triggerHintPass) window.triggerHintPass();
                        this.addFloatingText('🎯 50:50 힌트 패스 획득!', p.displayX, p.displayY - 30, '#A855F7');
                    } else if (item.type === 'star') {
                        p.buffs.star = true;
                        this.addFloatingText('⭐ 무적 스타 대시 획득!', p.displayX, p.displayY - 30, '#FBBF24');
                    }
                }
            }
        });
    }

    triggerPlayerGoal() {
        this.player.isFinished = true;

        const finishers = this.peers.filter(bot => bot.isFinished).length;
        this.player.finishRank = finishers + 1;

        // 스피드런 보너스 점수 산정 (빨리 들어올수록 대폭 높은 점수 획득!)
        // 1. 순위 보너스: 1위 2,500점, 2위 1,800점, 3위 1,200점, 기타 800점
        const rankBonus = this.player.finishRank === 1 ? 2500 : (this.player.finishRank === 2 ? 1800 : (this.player.finishRank === 3 ? 1200 : 800));

        // 2. 시간 보너스: 남은 시간 1초당 60점 (빠를수록 대량 득점)
        const safeRemaining = Math.max(0, this.remainingTime);
        const timeBonus = Math.floor(safeRemaining * 60);

        // 3. 완주 소요 시간 (초)
        const elapsedSec = (Math.max(0.1, this.timeLimit - safeRemaining)).toFixed(1);

        const totalScore = this.gameScore + rankBonus + timeBonus;

        this.addFloatingText(`🏆 ${this.player.finishRank}등 골인! (+${rankBonus}점)`, this.player.displayX, this.player.displayY - 35, '#FBBF24');
        this.addFloatingText(`⚡ 스피드런 시간 보너스 +${timeBonus}점!`, this.player.displayX, this.player.displayY - 55, '#F59E0B');

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.playGoal();
        }

        // 호스트 플레이어인데 아직 완주하지 못한 학생이 있는 경우:
        // 게임 루프를 즉시 멈추지 않고, 학생들이 달릴 수 있도록 지속
        if (this.isHostPlayer && this.peers.length > 0 && !this.peers.every(p => p.isFinished)) {
            this.addFloatingText('🎉 선생님 골인 완료! 학생들의 완주를 중계 중입니다.', this.player.displayX, this.player.displayY - 75, '#38BDF8');
            return;
        }

        this.isRunning = false;
        this.isPaused = true;
        this.tagger.state = 'GOAL_IN';

        if (this.animFrameId) {
            cancelAnimationFrame(this.animFrameId);
            this.animFrameId = null;
        }

        if (window.speechSynthesis) {
            window.speechSynthesis.cancel();
        }
        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.stopAll();
            window.MugunghwaAudio.playGoal();
        }

        // 게임 승리 모달 호출
        if (window.showVictoryModal) {
            window.showVictoryModal({
                rank: this.player.finishRank,
                score: totalScore,
                baseScore: this.gameScore,
                rankBonus: rankBonus,
                timeBonus: timeBonus,
                elapsedSec: elapsedSec,
                remainingTime: Math.floor(safeRemaining),
                playerName: this.player.name
            });
        }
    }

    // 관전 모드: 참가 학생 전원 완주 시 경기 종료
    triggerAllPeersGoal() {
        this.isRunning = false;
        this.isPaused = true;
        this.tagger.state = 'GOAL_IN';

        if (this.animFrameId) {
            cancelAnimationFrame(this.animFrameId);
            this.animFrameId = null;
        }

        if (window.speechSynthesis) {
            window.speechSynthesis.cancel();
        }
        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.stopAll();
            window.MugunghwaAudio.playGoal();
        }

        this.addFloatingText('🎉 참가 학생 전원 100m 완주 성공!', this.WIDTH / 2, this.HEIGHT / 2, '#FBBF24');

        if (window.showVictoryModal) {
            const safeRemaining = Math.max(0, this.remainingTime);
            window.showVictoryModal({
                rank: 1,
                score: 10000,
                baseScore: 5000,
                rankBonus: 2500,
                timeBonus: 2500,
                elapsedSec: (Math.max(0.1, this.timeLimit - safeRemaining)).toFixed(1),
                remainingTime: Math.floor(safeRemaining),
                playerName: '참가 학생 전원 완주!'
            });
        }
    }

    addFloatingText(text, x, y, color = '#FFFFFF') {
        this.floatingTexts.push({
            text: text,
            x: x,
            y: y,
            vy: -1.2,
            alpha: 1.0,
            color: color,
            life: 65
        });
    }

    update(dt) {
        if (!this.isRunning || this.isPaused) return;

        // 1. 제한 시간 카운트다운
        if (this.timeLimit > 0) {
            this.remainingTime -= dt;
            if (this.remainingTime <= 0) {
                this.remainingTime = 0;
                this.updateHUD();
                this.triggerTimeOver();
                return;
            }
        }

        // 2. 술래 상태 머신 업데이트
        if (this.tagger.state === 'CHANTING') {
            this.tagger.chantTimer++;
            // 머리를 나무 쪽으로 돌려놓음
            this.tagger.headAngle += (Math.PI - this.tagger.headAngle) * 0.15;
            this.tagger.laserAlpha = 0;
        } else if (this.tagger.state === 'SCANNING') {
            this.tagger.scanTimer++;
            // 머리를 정면(플레이어 쪽 0도)으로 휙 돌림!
            this.tagger.headAngle += (0 - this.tagger.headAngle) * 0.35;
            this.tagger.laserAlpha = Math.min(0.65, this.tagger.laserAlpha + 0.08);

            // 주시 시간이 끝나면 다시 CHANTING으로 복귀 (시간이 남아있고 게임 진행 중일 때만)
            if (this.tagger.scanTimer >= this.tagger.scanDuration) {
                if (this.canContinueGame()) {
                    this.tagger.state = 'CHANTING';
                    this.tagger.chantTimer = 0;
                    this.speakTaggerChant();
                }
            }
        }

        // 관전 모드: 참가 학생 전원이 완주했는지 검사
        if (this.isSpectator && this.peers.length > 0 && this.peers.every(p => p.isFinished)) {
            if (!this.allPeersFinished) {
                this.allPeersFinished = true;
                this.triggerAllPeersGoal();
                return;
            }
        }

        // 3. 로컬 플레이어 위치 보간 (부드러운 전진, 상하 레인 이동 및 후퇴 애니메이션)
        this.player.displayX += (this.player.targetX - this.player.displayX) * 0.15;
        this.player.displayY += (this.player.targetY - this.player.displayY) * 0.15;
        if (Math.abs(this.player.targetX - this.player.displayX) > 2 || Math.abs(this.player.targetY - this.player.displayY) > 2) {
            this.player.runFrame += 0.35;
            this.player.isMoving = true;
        } else {
            this.player.isMoving = false;
        }

        if (this.player.isFallen) {
            this.player.fallProgress += 0.08;
            if (this.player.fallProgress >= 1.0) {
                this.player.isFallen = false;
            }
        }

        // 4. AI 봇 친구들 업데이트 (솔로 모드)
        if (this.playMode === 'solo') {
            this.peers.forEach(bot => {
                if (bot.isFinished) return;

                // 술래가 CHANTING 중일 때 확률적으로 전진
                if (this.tagger.state === 'CHANTING') {
                    bot.botAnswerTimer--;
                    if (bot.botAnswerTimer <= 0) {
                        // 85% 확률 정답, 15% 오답 후퇴
                        if (Math.random() < 0.85) {
                            bot.m = Math.min(100, bot.m + (Math.random() < 0.2 ? 15 : 10));
                            bot.targetX = this.mToX(bot.m);
                            // 체크포인트 갱신
                            if (bot.m >= 75) bot.checkpoint = Math.max(bot.checkpoint, 75);
                            else if (bot.m >= 50) bot.checkpoint = Math.max(bot.checkpoint, 50);
                            else if (bot.m >= 25) bot.checkpoint = Math.max(bot.checkpoint, 25);

                            if (bot.m >= 100 && !bot.isFinished) {
                                bot.isFinished = true;
                                const finishedCount = this.peers.filter(p => p.isFinished).length + (this.player.isFinished ? 1 : 0);
                                bot.finishRank = finishedCount;
                            }
                        } else {
                            // 오답 시 체크포인트로 후퇴
                            bot.m = bot.checkpoint;
                            bot.targetX = this.mToX(bot.checkpoint);
                        }
                        bot.botAnswerTimer = Math.floor(Math.random() * 90) + 60;
                    }
                } else if (this.tagger.state === 'SCANNING') {
                    // 술래가 볼 때 가끔 걸리는 봇 연출
                    if (Math.random() < 0.003 && bot.m > bot.checkpoint) {
                        bot.m = bot.checkpoint;
                        bot.targetX = this.mToX(bot.checkpoint);
                    }
                }

                bot.displayX += (bot.targetX - bot.displayX) * 0.12;
                if (Math.abs(bot.targetX - bot.displayX) > 2) {
                    bot.runFrame += 0.3;
                }
            });
        } else if (this.playMode === 'multi') {
            this.peers.forEach(peer => {
                peer.displayX += (peer.targetX - peer.displayX) * 0.14;
                if (Math.abs(peer.targetX - peer.displayX) > 2) {
                    peer.runFrame += 0.35;
                }
            });
        }

        // 5. 플로팅 텍스트 업데이트
        for (let i = this.floatingTexts.length - 1; i >= 0; i--) {
            const ft = this.floatingTexts[i];
            ft.y += ft.vy;
            ft.life--;
            ft.alpha = Math.max(0, ft.life / 65);
            if (ft.life <= 0) {
                this.floatingTexts.splice(i, 1);
            }
        }

        this.updateHUD();
    }

    triggerTimeOver() {
        this.isRunning = false;
        this.isPaused = true;
        this.tagger.state = 'TIME_OVER';

        if (this.animFrameId) {
            cancelAnimationFrame(this.animFrameId);
            this.animFrameId = null;
        }

        // 음성 및 구호 즉시 정지
        if (typeof window !== 'undefined' && window.speechSynthesis) {
            window.speechSynthesis.cancel();
        }
        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.stopAll();
        }

        if (window.showTimeOverModal) {
            window.showTimeOverModal({
                myMeters: Math.floor(this.player.m),
                finishers: this.peers.filter(p => p.isFinished)
            });
        }
    }

    updateHUD() {
        // 남은 시간
        const timeEl = document.getElementById('hudRemainingTime');
        if (timeEl) {
            const totalSec = Math.max(0, Math.ceil(this.remainingTime));
            const m = Math.floor(totalSec / 60);
            const s = totalSec % 60;
            timeEl.textContent = `${m}:${s < 10 ? '0' : ''}${s}`;
            if (totalSec <= 15) {
                timeEl.className = 'font-black text-rose-500 animate-pulse text-lg';
            } else {
                timeEl.className = 'font-black text-amber-300 text-lg';
            }
        }

        // 플레이어 거리 (관전 모드 vs 플레이어 모드)
        const distEl = document.getElementById('hudPlayerDistance');
        if (distEl) {
            if (this.isSpectator) {
                distEl.innerHTML = '<span class="text-amber-300 font-black">👀 대형 TV 관전 중계</span>';
            } else {
                distEl.textContent = `${Math.floor(this.player.m)}m / 100m`;
            }
        }

        // 체크포인트 (관전 모드 vs 플레이어 모드)
        const cpEl = document.getElementById('hudCheckpoint');
        if (cpEl) {
            if (this.isSpectator) {
                const finishedCount = this.peers.filter(p => p.isFinished).length;
                cpEl.textContent = `완주 ${finishedCount}/${this.peers.length}명`;
            } else {
                cpEl.textContent = `${this.player.checkpoint}m`;
            }
        }

        // 술래 상태
        const taggerBadge = document.getElementById('hudTaggerState');
        if (taggerBadge) {
            if (this.tagger.state === 'CHANTING') {
                taggerBadge.className = 'px-3 py-1 rounded-full text-xs font-black bg-emerald-500/20 text-emerald-300 border border-emerald-500/40 flex items-center gap-1.5';
                taggerBadge.innerHTML = '<span class="w-2 h-2 rounded-full bg-emerald-400 animate-ping"></span> 🟢 전진 찬스 (말하는 중!)';
            } else {
                taggerBadge.className = 'px-3 py-1 rounded-full text-xs font-black bg-rose-500/20 text-rose-300 border border-rose-500/40 flex items-center gap-1.5 animate-pulse';
                taggerBadge.innerHTML = '<span class="w-2 h-2 rounded-full bg-rose-500"></span> 🔴 술래 주시 중! (얼음!)';
            }
        }
    }

    render() {
        if (!this.ctx) return;
        const ctx = this.ctx;

        ctx.save();
        ctx.clearRect(0, 0, this.WIDTH, this.HEIGHT);

        // 1. 경기장 잔디 & 트랙 배경
        this.renderStadium(ctx);

        // 2. 100m 트랙 라인 및 체크포인트 (0m, 25m, 50m, 75m, 100m)
        this.renderTrackMarkers(ctx);

        // 3. 트랙 위 필드 아이템 큐브 [?]
        this.renderFieldItems(ctx);

        // 4. 술래 (느티나무 & 영희 로봇 인형)
        this.renderTagger(ctx);

        // 5. 달리는 학생들 (반 친구들 & 내 아바타)
        this.renderRunners(ctx);

        // 6. 술래 레이저 스캔 효과 (SCANNING 상태일 때)
        if (this.tagger.state === 'SCANNING' && this.tagger.laserAlpha > 0) {
            this.renderLaserScan(ctx);
        }

        // 7. 플로팅 알림 텍스트
        this.floatingTexts.forEach(ft => {
            ctx.save();
            ctx.globalAlpha = Math.max(0, ft.alpha);
            ctx.fillStyle = ft.color;
            ctx.strokeStyle = 'rgba(15, 23, 42, 0.9)';
            ctx.lineWidth = 3;
            ctx.font = 'black 14px "Pretendard Variable", Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.strokeText(ft.text, ft.x, ft.y);
            ctx.fillText(ft.text, ft.x, ft.y);
            ctx.restore();
        });

        ctx.restore();
    }

    renderStadium(ctx) {
        // 잔디 그라데이션
        const fieldGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
        fieldGrad.addColorStop(0, '#064E3B');
        fieldGrad.addColorStop(0.3, '#047857');
        fieldGrad.addColorStop(1, '#065F46');
        ctx.fillStyle = fieldGrad;
        ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

        // 경기장 달리기 트랙 (흙빛 주황 웜톤)
        const trackGrad = ctx.createLinearGradient(0, this.TRACK_Y_MIN, 0, this.TRACK_Y_MAX);
        trackGrad.addColorStop(0, '#C2410C');
        trackGrad.addColorStop(0.5, '#EA580C');
        trackGrad.addColorStop(1, '#9A3412');
        ctx.fillStyle = trackGrad;
        ctx.fillRect(this.TRACK_LEFT - 30, this.TRACK_Y_MIN, this.TRACK_WIDTH + 60, this.TRACK_Y_MAX - this.TRACK_Y_MIN);

        // 트랙 테두리 흰색 라인
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.9)';
        ctx.lineWidth = 3;
        ctx.strokeRect(this.TRACK_LEFT - 30, this.TRACK_Y_MIN, this.TRACK_WIDTH + 60, this.TRACK_Y_MAX - this.TRACK_Y_MIN);

        // 5개 레인 구분 점선 및 레인 번호 표시
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.22)';
        ctx.lineWidth = 1.5;
        ctx.setLineDash([10, 8]);
        const laneDividers = [242, 307, 372, 437];
        laneDividers.forEach(ly => {
            ctx.beginPath();
            ctx.moveTo(this.TRACK_LEFT - 25, ly);
            ctx.lineTo(this.TRACK_RIGHT + 25, ly);
            ctx.stroke();
        });
        ctx.setLineDash([]);

        // 출발선 좌측 레인 번호 뱃지 (1~5번)
        for (let i = 0; i < this.LANES.length; i++) {
            const ly = this.LANES[i];
            ctx.fillStyle = (this.player.laneIndex === i) ? '#FBBF24' : 'rgba(255, 255, 255, 0.45)';
            ctx.font = 'bold 11px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText(`${i + 1}레인`, this.TRACK_LEFT - 18, ly + 4);
        }
    }

    renderTrackMarkers(ctx) {
        // 0m 출발선 (START LINE)
        ctx.fillStyle = '#FFFFFF';
        ctx.fillRect(this.TRACK_LEFT - 6, this.TRACK_Y_MIN, 12, this.TRACK_Y_MAX - this.TRACK_Y_MIN);
        ctx.fillStyle = '#FBBF24';
        ctx.font = 'black 14px Pretendard, sans-serif';
        ctx.fillText('출발선 (START 0m)', this.TRACK_LEFT - 55, this.TRACK_Y_MIN - 12);

        // 25m, 50m, 75m 체크포인트 라인
        [25, 50, 75].forEach(m => {
            const x = this.mToX(m);
            ctx.strokeStyle = 'rgba(56, 189, 248, 0.8)';
            ctx.lineWidth = 2.5;
            ctx.setLineDash([6, 4]);
            ctx.beginPath();
            ctx.moveTo(x, this.TRACK_Y_MIN);
            ctx.lineTo(x, this.TRACK_Y_MAX);
            ctx.stroke();
            ctx.setLineDash([]);

            // 깃발 마커
            ctx.fillStyle = '#38BDF8';
            ctx.font = 'bold 11px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText(`🚩 ${m}m`, x, this.TRACK_Y_MIN - 8);
        });

        // 100m 결승선 (FINISH / GOAL LINE)
        const gx = this.TRACK_RIGHT;
        // 체크무늬 레이싱 피니시 라인
        const boxSize = 12;
        for (let y = this.TRACK_Y_MIN; y < this.TRACK_Y_MAX; y += boxSize) {
            const isWhite = Math.floor(y / boxSize) % 2 === 0;
            ctx.fillStyle = isWhite ? '#FFFFFF' : '#0F172A';
            ctx.fillRect(gx - boxSize, y, boxSize, boxSize);
            ctx.fillStyle = isWhite ? '#0F172A' : '#FFFFFF';
            ctx.fillRect(gx, y, boxSize, boxSize);
        }

        ctx.fillStyle = '#F59E0B';
        ctx.font = 'black 16px Pretendard, sans-serif';
        ctx.textAlign = 'center';
        ctx.fillText('🏁 결승선 (GOAL 100m)', gx, this.TRACK_Y_MIN - 12);
    }

    renderFieldItems(ctx) {
        this.fieldItems.forEach(it => {
            if (it.collected) return;
            const x = this.mToX(it.m);
            const y = (it.laneIndex !== undefined && this.LANES[it.laneIndex] !== undefined)
                ? this.LANES[it.laneIndex]
                : (this.TRACK_Y_MIN + 40 + ((it.m * 17) % (this.TRACK_Y_MAX - this.TRACK_Y_MIN - 80)));

            ctx.save();
            ctx.translate(x, y);

            // 반짝이는 황금 큐브
            ctx.shadowColor = '#F59E0B';
            ctx.shadowBlur = 12;
            ctx.fillStyle = '#FBBF24';
            ctx.fillRect(-12, -12, 24, 24);
            ctx.strokeStyle = '#FFFFFF';
            ctx.lineWidth = 2;
            ctx.strokeRect(-12, -12, 24, 24);

            // 아이템 아이콘
            ctx.shadowBlur = 0;
            ctx.font = '14px sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(it.icon, 0, 1);
            ctx.restore();
        });
    }

    renderTagger(ctx) {
        const tx = this.tagger.x;
        const ty = this.tagger.y;

        // 거대한 느티나무 (술래가 기대어 선 나무)
        ctx.fillStyle = '#78350F';
        ctx.fillRect(tx + 25, ty - 120, 35, 240); // 줄기
        ctx.fillStyle = '#15803D';
        ctx.beginPath();
        ctx.arc(tx + 42, ty - 140, 75, 0, Math.PI * 2); // 풍성한 잎
        ctx.fill();

        ctx.save();
        ctx.translate(tx, ty);

        // 술래 인형 (영희 캐릭터)
        // 치마/몸통 (주황 원피스 & 노란 티셔츠)
        ctx.fillStyle = '#EA580C';
        ctx.beginPath();
        ctx.moveTo(-16, 10);
        ctx.lineTo(-24, 55);
        ctx.lineTo(24, 55);
        ctx.lineTo(16, 10);
        ctx.closePath();
        ctx.fill();

        ctx.fillStyle = '#FBBF24';
        ctx.fillRect(-15, -12, 30, 24);

        // 다리 & 양말
        ctx.fillStyle = '#FFFFFF';
        ctx.fillRect(-12, 55, 9, 22);
        ctx.fillRect(3, 55, 9, 22);
        ctx.fillStyle = '#DC2626';
        ctx.fillRect(-14, 73, 13, 6);
        ctx.fillRect(1, 73, 13, 6);

        // 머리 & 회전 각도
        ctx.save();
        ctx.translate(0, -32);
        const lookDir = Math.cos(this.tagger.headAngle); // 1 = 플레이어 주시(정면), -1 = 나무(뒤통수)

        if (lookDir <= 0.15) {
            // ==========================================
            // [술래 뒤통수 렌더링 (나무를 바라보고 있을 때)]
            // ==========================================
            // 1. 머리 전체 (칠흑 같은 검은 머리카락)
            ctx.fillStyle = '#111827';
            ctx.beginPath();
            ctx.arc(0, 0, 22, 0, Math.PI * 2);
            ctx.fill();

            // 2. 뒤통수 가르마 (5:5 정수리~목덜미 수직 분할선)
            ctx.strokeStyle = '#030712';
            ctx.lineWidth = 2.2;
            ctx.beginPath();
            ctx.moveTo(0, -21);
            ctx.lineTo(0, 18);
            ctx.stroke();

            // 3. 머릿결 텍스처 (양쪽 땋은 머리로 모이는 흐름선)
            ctx.strokeStyle = 'rgba(75, 85, 99, 0.45)';
            ctx.lineWidth = 1.3;
            ctx.beginPath();
            ctx.moveTo(0, -10);
            ctx.quadraticCurveTo(-10, -8, -20, 2);
            ctx.moveTo(0, -10);
            ctx.quadraticCurveTo(10, -8, 20, 2);
            ctx.moveTo(0, 4);
            ctx.quadraticCurveTo(-10, 6, -20, 12);
            ctx.moveTo(0, 4);
            ctx.quadraticCurveTo(10, 6, 20, 12);
            ctx.stroke();

            // 4. 뒤통수 상단 은은한 윤기 하이라이트
            ctx.strokeStyle = 'rgba(255, 255, 255, 0.18)';
            ctx.lineWidth = 2.0;
            ctx.beginPath();
            ctx.arc(0, -3, 17, Math.PI * 1.15, Math.PI * 1.85);
            ctx.stroke();

            // 5. 양쪽 땋은 머리 (Pigtails)
            ctx.fillStyle = '#111827';
            // 왼쪽 땋은 머리
            ctx.beginPath();
            if (ctx.roundRect) ctx.roundRect(-27, 4, 11, 26, 4);
            else ctx.fillRect(-27, 4, 11, 26);
            ctx.fill();
            // 오른쪽 땋은 머리
            ctx.beginPath();
            if (ctx.roundRect) ctx.roundRect(16, 4, 11, 26, 4);
            else ctx.fillRect(16, 4, 11, 26);
            ctx.fill();

            // 땋은 머리 마디 선
            ctx.strokeStyle = '#374151';
            ctx.lineWidth = 1.5;
            ctx.beginPath();
            ctx.moveTo(-26, 12); ctx.lineTo(-17, 14);
            ctx.moveTo(-26, 19); ctx.lineTo(-17, 21);
            ctx.moveTo(-26, 26); ctx.lineTo(-17, 28);
            ctx.moveTo(17, 14); ctx.lineTo(26, 12);
            ctx.moveTo(17, 21); ctx.lineTo(26, 19);
            ctx.moveTo(17, 28); ctx.lineTo(26, 26);
            ctx.stroke();

            // 6. 빨간색 머리끈 / 리본 방울
            ctx.fillStyle = '#DC2626';
            ctx.beginPath();
            ctx.arc(-21, 3, 5.5, 0, Math.PI * 2);
            ctx.arc(21, 3, 5.5, 0, Math.PI * 2);
            ctx.fill();

            ctx.fillStyle = '#EF4444';
            ctx.beginPath();
            ctx.arc(-22, 2, 2.5, 0, Math.PI * 2);
            ctx.arc(20, 2, 2.5, 0, Math.PI * 2);
            ctx.fill();
        } else {
            // ==========================================
            // [술래 정면 렌더링 (플레이어들을 노려볼 때)]
            // ==========================================
            // 1. 뒤 배경 머리카락 베이스
            ctx.fillStyle = '#111827';
            ctx.beginPath();
            ctx.arc(0, 0, 22, 0, Math.PI * 2);
            ctx.fill();

            // 2. 양쪽 땋은 머리
            ctx.fillRect(-27, 4, 11, 26);
            ctx.fillRect(16, 4, 11, 26);

            // 3. 빨간 머리끈 방울
            ctx.fillStyle = '#DC2626';
            ctx.beginPath();
            ctx.arc(-21, 3, 5.5, 0, Math.PI * 2);
            ctx.arc(21, 3, 5.5, 0, Math.PI * 2);
            ctx.fill();

            // 4. 살구색 얼굴
            ctx.fillStyle = '#FED7AA';
            ctx.beginPath();
            ctx.arc(lookDir * 4, 2, 17, 0, Math.PI * 2);
            ctx.fill();

            // 5. 이마 일자 앞머리 (뱅 헤어)
            ctx.fillStyle = '#111827';
            ctx.beginPath();
            ctx.arc(lookDir * 4, -5, 17.5, Math.PI * 0.95, Math.PI * 2.05);
            ctx.lineTo(lookDir * 4 + 17, -1);
            ctx.lineTo(lookDir * 4 - 17, -1);
            ctx.closePath();
            ctx.fill();

            // 6. 눈 (적색 센서 렌즈 & 동공 하이라이트)
            ctx.fillStyle = '#EF4444';
            ctx.beginPath();
            ctx.arc(-5 + lookDir * 4, 2, 4, 0, Math.PI * 2);
            ctx.arc(6 + lookDir * 4, 2, 4, 0, Math.PI * 2);
            ctx.fill();

            ctx.fillStyle = '#FFFFFF';
            ctx.beginPath();
            ctx.arc(-6 + lookDir * 4, 1, 1.3, 0, Math.PI * 2);
            ctx.arc(5 + lookDir * 4, 1, 1.3, 0, Math.PI * 2);
            ctx.fill();

            // 7. 분홍빛 볼터치
            ctx.fillStyle = 'rgba(244, 63, 94, 0.65)';
            ctx.beginPath();
            ctx.arc(-7 + lookDir * 4, 9, 3.5, 0, Math.PI * 2);
            ctx.arc(8 + lookDir * 4, 9, 3.5, 0, Math.PI * 2);
            ctx.fill();

            // 8. 미소 짓는 입
            ctx.strokeStyle = '#B91C1C';
            ctx.lineWidth = 1.5;
            ctx.beginPath();
            ctx.arc(lookDir * 4, 11, 3.5, 0.2, Math.PI - 0.2);
            ctx.stroke();
        }

        ctx.restore();
        ctx.restore();
    }

    renderRunners(ctx) {
        // 목표 이동 위치 타겟 가이드 렌더링 (관전 모드가 아닐 때, 방향 선택 후 정답 대기 상태)
        if (!this.isSpectator && this.player.pendingMove) {
            const pm = this.player.pendingMove;
            ctx.save();

            // 점선 궤적 라인
            ctx.strokeStyle = 'rgba(250, 204, 21, 0.85)';
            ctx.lineWidth = 2.5;
            ctx.setLineDash([6, 6]);
            ctx.beginPath();
            ctx.moveTo(this.player.displayX, this.player.displayY);
            ctx.lineTo(pm.targetX, pm.targetY);
            ctx.stroke();
            ctx.setLineDash([]);

            // 펄스 애니메이션 목표 마커 링
            const pulse = Math.sin(Date.now() / 150) * 4;
            ctx.strokeStyle = '#FACC15';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.arc(pm.targetX, pm.targetY, 20 + pulse, 0, Math.PI * 2);
            ctx.stroke();

            ctx.fillStyle = 'rgba(250, 204, 21, 0.25)';
            ctx.beginPath();
            ctx.arc(pm.targetX, pm.targetY, 20 + pulse, 0, Math.PI * 2);
            ctx.fill();

            // 타겟 텍스트
            ctx.fillStyle = '#FEF08A';
            ctx.font = 'bold 11px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText('🎯 ' + (pm.dirLabel || '이동 목표'), pm.targetX, pm.targetY - 28);

            ctx.restore();
        }

        // 모든 주자 정렬 (관전 모드일 때는 player 제외)
        const allRunners = (this.isSpectator ? [...this.peers] : [...this.peers, this.player]).sort((a, b) => a.displayY - b.displayY);

        allRunners.forEach(r => {
            const isMe = (!this.isSpectator && r.id === this.player.id);
            const isTeacher = Boolean(r.isHost || (isMe && this.isHostPlayer));
            ctx.save();
            ctx.translate(r.displayX, r.displayY);

            // 오답 후퇴 시 찌그러지거나 넘어지는 연출
            if (r.isFallen) {
                ctx.rotate(0.35);
                ctx.scale(1.2, 0.7);
            }

            // 쉴드 버프 아우라
            if (r.buffs && r.buffs.shield) {
                ctx.strokeStyle = 'rgba(56, 189, 248, 0.85)';
                ctx.lineWidth = 3;
                ctx.beginPath();
                ctx.arc(0, -18, 28, 0, Math.PI * 2);
                ctx.stroke();
            }

            // 신발 부스터 불꽃
            if (r.buffs && r.buffs.shoes) {
                ctx.fillStyle = '#F59E0B';
                ctx.fillRect(-18, 0, 8, 4);
            }

            // 다리 (달리기 모션)
            const legPhase = Math.sin(r.runFrame || 0) * 8;
            ctx.fillStyle = isTeacher ? '#78350F' : (isMe ? '#1E3A8A' : '#334155');
            ctx.fillRect(-6, -4, 4, 10 + legPhase * 0.5);
            ctx.fillRect(2, -4, 4, 10 - legPhase * 0.5);

            // 몸통 (진행자/선생님은 황금빛 주황 톤)
            ctx.fillStyle = isTeacher ? '#D97706' : (isMe ? '#2563EB' : '#475569');
            ctx.fillRect(-8, -20, 16, 16);

            // 머리 & 얼굴
            ctx.fillStyle = '#FED7AA';
            ctx.beginPath();
            ctx.arc(0, -28, 9, 0, Math.PI * 2);
            ctx.fill();

            // 눈
            ctx.fillStyle = '#0F172A';
            ctx.fillRect(2, -30, 2, 3);

            // 머리 위 강조 화살표 & 뱃지
            if (isTeacher) {
                ctx.fillStyle = '#FBBF24';
                ctx.font = 'bold 11px Pretendard, sans-serif';
                ctx.textAlign = 'center';
                const laneText = (r.laneIndex !== undefined) ? ` (${r.laneIndex + 1}레인)` : '';
                ctx.fillText(`👑 진행자${laneText}`, 0, -42);
            } else if (isMe) {
                ctx.fillStyle = '#FBBF24';
                ctx.font = 'bold 11px Pretendard, sans-serif';
                ctx.textAlign = 'center';
                const laneText = (this.player.laneIndex !== undefined) ? `${this.player.laneIndex + 1}레인` : '';
                ctx.fillText(`▼ 나 (${laneText})`, 0, -42);
            }

            // 이름표 및 등번호
            ctx.fillStyle = isTeacher ? '#FDE047' : (isMe ? '#FDE047' : '#E2E8F0');
            ctx.font = isTeacher ? 'black 11px Pretendard, sans-serif' : 'bold 10px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText(r.name || (isTeacher ? '선생님' : '학생'), 0, 16);

            // 골인한 경우 메달 표시
            if (r.isFinished) {
                ctx.font = '14px sans-serif';
                ctx.fillText('🥇', 0, -46);
            }

            ctx.restore();
        });
    }

    renderLaserScan(ctx) {
        ctx.save();
        const tx = this.tagger.x - 15;
        const ty = this.tagger.y - 32;

        // 술래 눈에서 방사형으로 뿜어지는 붉은 스캔 레이저 빔
        const grad = ctx.createRadialGradient(tx, ty, 10, tx - 400, ty, 950);
        grad.addColorStop(0, `rgba(239, 68, 68, ${this.tagger.laserAlpha * 0.9})`);
        grad.addColorStop(0.5, `rgba(220, 38, 38, ${this.tagger.laserAlpha * 0.4})`);
        grad.addColorStop(1, 'rgba(220, 38, 38, 0)');

        ctx.fillStyle = grad;
        ctx.beginPath();
        ctx.moveTo(tx, ty);
        ctx.lineTo(0, 0);
        ctx.lineTo(0, this.HEIGHT);
        ctx.closePath();
        ctx.fill();

        // 상단 경고 바
        ctx.fillStyle = 'rgba(239, 68, 68, 0.85)';
        ctx.fillRect(0, 0, this.WIDTH, 36);
        ctx.fillStyle = '#FFFFFF';
        ctx.font = 'black 16px Pretendard, sans-serif';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('🚨 RED LIGHT! 술래가 주시 중입니다! 움직이지 마세요! 🚨', this.WIDTH / 2, 18);

        ctx.restore();
    }

    loop(timestamp) {
        const dt = (timestamp - this.lastTime) / 1000;
        this.lastTime = timestamp;

        this.update(Math.min(dt, 0.1));
        this.render();

        if (this.isRunning) {
            this.animFrameId = requestAnimationFrame(this.loop);
        }
    }
}

window.MugunghwaGame = MugunghwaGame;
