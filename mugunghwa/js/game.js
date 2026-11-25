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
            { id: 'f_1', m: 18, type: 'shoes', name: '부스터 신발', icon: '⚡', collected: false },
            { id: 'f_2', m: 38, type: 'shield', name: '방어막 쉴드', icon: '🛡️', collected: false },
            { id: 'f_3', m: 58, type: 'time', name: '타임 캡슐 (+10초)', icon: '⏱️', collected: false },
            { id: 'f_4', m: 76, type: 'hint', name: '50:50 힌트 패스', icon: '🎯', collected: false },
            { id: 'f_5', m: 90, type: 'star', name: '황금 스타 대시', icon: '⭐', collected: false }
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

    setTimeLimit(sec) {
        this.timeLimit = Number(sec) || 90;
        this.remainingTime = this.timeLimit;
    }

    // 미터(0~100m)를 캔버스 X 좌표로 변환
    mToX(m) {
        const clamped = Math.max(0, Math.min(100, m));
        return this.TRACK_LEFT + (clamped / 100) * this.TRACK_WIDTH;
    }

    startNewGame(settings = {}) {
        this.playMode = settings.mode || 'solo';
        this.timeLimit = settings.timeLimit !== undefined ? Number(settings.timeLimit) : 90;
        this.remainingTime = this.timeLimit;
        this.isRunning = true;
        this.isPaused = false;
        this.gameScore = 0;

        // 플레이어 초기화
        this.player.m = 0;
        this.player.checkpoint = 0;
        this.player.displayX = this.TRACK_LEFT;
        this.player.targetX = this.TRACK_LEFT;
        this.player.displayY = 350;
        this.player.targetY = 350;
        this.player.isFinished = false;
        this.player.finishRank = 0;
        this.player.isFallen = false;
        this.player.buffs = { shoes: false, shield: false, star: false };

        this.initFieldItems();
        this.initBots();

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
        if (!this.isRunning || this.isPaused) return;

        // 구호 속도: 턴이 지날수록 약간씩 긴장감 넘치게 변동 (0.9 ~ 1.45배속)
        const speed = 0.95 + (Math.random() * 0.45);
        this.tagger.speedMultiplier = speed;
        this.tagger.chantDuration = Math.floor(190 / speed);

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.speakChant('무궁화 꽃이 피었습니다!', speed, () => {
                // 구호가 끝나면 술래가 휙 돌아봄!
                if (this.tagger.state === 'CHANTING') {
                    this.triggerTaggerTurn();
                }
            });
        }
    }

    triggerTaggerTurn() {
        this.tagger.state = 'SCANNING';
        this.tagger.scanTimer = 0;
        this.tagger.turnCount++;
        this.tagger.scanDuration = Math.floor(100 + Math.random() * 60); // 1.6 ~ 2.6초 동안 전방 레이저 스캔

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.playTagScan();
        }

        // 멀티플레이어 호스트인 경우 상태 브로드캐스트
        if (this.playMode === 'multi' && window.MugunghwaMultiplayer && window.MugunghwaMultiplayer.isHost) {
            window.MugunghwaMultiplayer.broadcastTaggerState('SCANNING', this.tagger.speedMultiplier, this.tagger.turnCount);
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
            if (!item.collected && p.m >= item.m - 2 && p.m <= item.m + 8) {
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
        });
    }

    triggerPlayerGoal() {
        this.player.isFinished = true;
        const finishers = this.peers.filter(bot => bot.isFinished).length;
        this.player.finishRank = finishers + 1;

        if (window.MugunghwaAudio) {
            window.MugunghwaAudio.playGoal();
        }

        this.addFloatingText(`🏆 ${this.player.finishRank}등으로 골인 성공!`, this.player.displayX, this.player.displayY - 35, '#FBBF24');

        // 게임 승리 모달 호출
        if (window.showVictoryModal) {
            window.showVictoryModal({
                rank: this.player.finishRank,
                score: this.gameScore,
                remainingTime: Math.floor(this.remainingTime)
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

            // 주시 시간이 끝나면 다시 CHANTING으로 복귀
            if (this.tagger.scanTimer >= this.tagger.scanDuration) {
                this.tagger.state = 'CHANTING';
                this.tagger.chantTimer = 0;
                this.speakTaggerChant();
            }
        }

        // 3. 로컬 플레이어 위치 보간 (부드러운 전진 및 후퇴 애니메이션)
        this.player.displayX += (this.player.targetX - this.player.displayX) * 0.15;
        if (Math.abs(this.player.targetX - this.player.displayX) > 2) {
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

        // 플레이어 거리
        const distEl = document.getElementById('hudPlayerDistance');
        if (distEl) distEl.textContent = `${Math.floor(this.player.m)}m / 100m`;

        // 체크포인트
        const cpEl = document.getElementById('hudCheckpoint');
        if (cpEl) cpEl.textContent = `${this.player.checkpoint}m`;

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

        // 레인 구분 점선
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.25)';
        ctx.lineWidth = 1.5;
        ctx.setLineDash([12, 10]);
        for (let y = this.TRACK_Y_MIN + 60; y < this.TRACK_Y_MAX; y += 60) {
            ctx.beginPath();
            ctx.moveTo(this.TRACK_LEFT - 25, y);
            ctx.lineTo(this.TRACK_RIGHT + 25, y);
            ctx.stroke();
        }
        ctx.setLineDash([]);
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
            const y = this.TRACK_Y_MIN + 40 + ((it.m * 17) % (this.TRACK_Y_MAX - this.TRACK_Y_MIN - 80));

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
        const lookDir = Math.cos(this.tagger.headAngle); // 1 = 플레이어 주시(정면), -1 = 나무(뒤)

        // 머리카락 (양갈래 헤어)
        ctx.fillStyle = '#1E1B4B';
        ctx.beginPath();
        ctx.arc(0, 0, 22, 0, Math.PI * 2);
        ctx.fill();
        // 양갈래 땋은 머리
        ctx.fillRect(-28, -2, 10, 26);
        ctx.fillRect(18, -2, 10, 26);

        // 얼굴
        ctx.fillStyle = '#FED7AA';
        ctx.beginPath();
        ctx.arc(lookDir * 5, 2, 17, 0, Math.PI * 2);
        ctx.fill();

        // 정면 주시 중일 때만 눈과 볼터치 그림
        if (lookDir > 0) {
            // 눈 (적색 센서 렌즈)
            ctx.fillStyle = '#EF4444';
            ctx.beginPath();
            ctx.arc(-2 + lookDir * 4, 0, 4, 0, Math.PI * 2);
            ctx.arc(8 + lookDir * 4, 0, 4, 0, Math.PI * 2);
            ctx.fill();

            // 볼터치
            ctx.fillStyle = 'rgba(244, 63, 94, 0.6)';
            ctx.beginPath();
            ctx.arc(-4 + lookDir * 4, 7, 3, 0, Math.PI * 2);
            ctx.arc(10 + lookDir * 4, 7, 3, 0, Math.PI * 2);
            ctx.fill();
        }

        ctx.restore();
        ctx.restore();
    }

    renderRunners(ctx) {
        // 모든 주자 정렬 (Y축 깊이 기준)
        const allRunners = [...this.peers, this.player].sort((a, b) => a.displayY - b.displayY);

        allRunners.forEach(r => {
            const isMe = (r.id === this.player.id);
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
            ctx.fillStyle = isMe ? '#1E3A8A' : '#334155';
            ctx.fillRect(-6, -4, 4, 10 + legPhase * 0.5);
            ctx.fillRect(2, -4, 4, 10 - legPhase * 0.5);

            // 몸통
            ctx.fillStyle = isMe ? '#2563EB' : '#475569';
            ctx.fillRect(-8, -20, 16, 16);

            // 머리 & 얼굴
            ctx.fillStyle = '#FED7AA';
            ctx.beginPath();
            ctx.arc(0, -28, 9, 0, Math.PI * 2);
            ctx.fill();

            // 눈
            ctx.fillStyle = '#0F172A';
            ctx.fillRect(2, -30, 2, 3);

            // 나인 경우 머리 위 강조 화살표 & 뱃지
            if (isMe) {
                ctx.fillStyle = '#FBBF24';
                ctx.font = 'bold 11px Pretendard, sans-serif';
                ctx.textAlign = 'center';
                ctx.fillText('▼ 나', 0, -42);
            }

            // 이름표 및 등번호
            ctx.fillStyle = isMe ? '#FDE047' : '#E2E8F0';
            ctx.font = 'bold 10px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText(r.name, 0, 16);

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
