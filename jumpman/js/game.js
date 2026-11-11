/**
 * 점프맨 게임 (Jumpman Game) - 캔버스 물리 엔진 및 게임 루프
 * 날아오는 공, 가시, 몬스터 회피 및 3회 부활 찬스 지원
 */

class JumpmanGame {
    constructor() {
        this.canvas = document.getElementById('gameCanvas');
        this.ctx = this.canvas ? this.canvas.getContext('2d') : null;

        // 가상 캔버스 해상도 (16:9)
        this.WIDTH = 960;
        this.HEIGHT = 540;
        this.FLOOR_Y = 440;

        // 게임 상태
        this.isRunning = false;
        this.isPaused = false;
        this.gameMode = 'endless'; // 'endless' | 'stage'
        this.currentStage = 1;
        this.stageGoalDistance = 600;

        // 플레이어 상태
        this.player = {
            x: 160,
            y: this.FLOOR_Y - 48,
            width: 36,
            height: 48,
            vx: 0,
            vy: 0,
            speed: 5.5,
            jumpPower: -12.8,
            doubleJumpPower: -11.5,
            gravity: 0.58,
            onGround: true,
            jumpCount: 0,
            invulnerableTimer: 0,
            facingRight: true,
            runFrame: 0,
            scaleY: 1,
            scaleX: 1
        };

        // 점수 및 기록
        this.score = 0;
        this.distance = 0;
        this.coins = 0;
        this.quizzesSolved = 0;
        this.survivalTime = 0; // 초
        this.chances = 3; // 기본 찬스 3회 (❤️ ❤️ ❤️)

        // 장애물 및 아이템 목록
        this.obstacles = [];
        this.collectibles = [];
        this.particles = [];
        this.backgroundStars = [];

        // 카메라 및 속도
        this.gameSpeed = 5.0;
        this.baseSpeed = 5.0;
        this.spawnTimer = 0;
        this.coinSpawnTimer = 0;
        this.screenShake = 0;

        // 키 입력 상태
        this.keys = {
            left: false,
            right: false,
            jump: false
        };

        // 타임스탬프
        this.lastTime = 0;
        this.animFrameId = null;

        this.initBackground();
        this.bindEvents();
    }

    initBackground() {
        this.backgroundStars = [];
        for (let i = 0; i < 40; i++) {
            this.backgroundStars.push({
                x: Math.random() * this.WIDTH,
                y: Math.random() * (this.FLOOR_Y - 80),
                size: Math.random() * 2 + 1,
                alpha: Math.random() * 0.7 + 0.3,
                speed: Math.random() * 0.4 + 0.1
            });
        }
    }

    bindEvents() {
        // 키보드 조작: A/D 또는 ←/→ (이동), Space 또는 마우스 좌클릭 (점프)
        window.addEventListener('keydown', (e) => {
            if (this.isPaused || !this.isRunning) return;

            if (e.code === 'KeyA' || e.code === 'ArrowLeft') {
                this.keys.left = true;
            } else if (e.code === 'KeyD' || e.code === 'ArrowRight') {
                this.keys.right = true;
            } else if (e.code === 'Space' || e.code === 'ArrowUp' || e.code === 'KeyW') {
                e.preventDefault();
                this.jump();
            }
        });

        window.addEventListener('keyup', (e) => {
            if (e.code === 'KeyA' || e.code === 'ArrowLeft') {
                this.keys.left = false;
            } else if (e.code === 'KeyD' || e.code === 'ArrowRight') {
                this.keys.right = false;
            }
        });

        // 캔버스 마우스 클릭 점프
        if (this.canvas) {
            this.canvas.addEventListener('mousedown', (e) => {
                if (e.button === 0 && this.isRunning && !this.isPaused) {
                    this.jump();
                }
            });

            // 터치 디바이스 탭 점프
            this.canvas.addEventListener('touchstart', (e) => {
                if (this.isRunning && !this.isPaused) {
                    this.jump();
                }
            }, { passive: true });
        }

        // 모바일 온스크린 가상 조작 버튼
        const btnLeft = document.getElementById('touchBtnLeft');
        const btnRight = document.getElementById('touchBtnRight');
        const btnJump = document.getElementById('touchBtnJump');

        if (btnLeft) {
            const pressL = (e) => { e.preventDefault(); this.keys.left = true; };
            const releaseL = (e) => { e.preventDefault(); this.keys.left = false; };
            btnLeft.addEventListener('mousedown', pressL);
            btnLeft.addEventListener('mouseup', releaseL);
            btnLeft.addEventListener('touchstart', pressL, { passive: false });
            btnLeft.addEventListener('touchend', releaseL, { passive: false });
        }

        if (btnRight) {
            const pressR = (e) => { e.preventDefault(); this.keys.right = true; };
            const releaseR = (e) => { e.preventDefault(); this.keys.right = false; };
            btnRight.addEventListener('mousedown', pressR);
            btnRight.addEventListener('mouseup', releaseR);
            btnRight.addEventListener('touchstart', pressR, { passive: false });
            btnRight.addEventListener('touchend', releaseR, { passive: false });
        }

        if (btnJump) {
            const pressJ = (e) => { e.preventDefault(); this.jump(); };
            btnJump.addEventListener('mousedown', pressJ);
            btnJump.addEventListener('touchstart', pressJ, { passive: false });
        }
    }

    startNewGame(mode = 'endless', stage = 1) {
        this.gameMode = mode;
        this.currentStage = stage;
        this.stageGoalDistance = 400 + stage * 200;

        this.isRunning = true;
        this.isPaused = false;
        this.score = 0;
        this.distance = 0;
        this.coins = 0;
        this.quizzesSolved = 0;
        this.survivalTime = 0;
        this.chances = 3; // 찬스 3회 완충!
        this.gameSpeed = 5.0 + (mode === 'stage' ? (stage - 1) * 0.4 : 0);

        this.player.x = 160;
        this.player.y = this.FLOOR_Y - this.player.height;
        this.player.vx = 0;
        this.player.vy = 0;
        this.player.onGround = true;
        this.player.jumpCount = 0;
        this.player.invulnerableTimer = 60; // 게임 시작 직후 1초간 안전 보호

        this.obstacles = [];
        this.collectibles = [];
        this.particles = [];
        this.spawnTimer = 40;
        this.coinSpawnTimer = 30;
        this.screenShake = 0;

        this.updateHUD();
        if (window.JumpmanAudio) {
            window.JumpmanAudio.init();
            window.JumpmanAudio.startBgm();
        }

        this.lastTime = performance.now();
        if (this.animFrameId) cancelAnimationFrame(this.animFrameId);
        this.loop = this.loop.bind(this);
        this.animFrameId = requestAnimationFrame(this.loop);
    }

    jump() {
        if (this.player.onGround) {
            // 1단 점프
            this.player.vy = this.player.jumpPower;
            this.player.onGround = false;
            this.player.jumpCount = 1;
            this.player.scaleY = 1.35;
            this.player.scaleX = 0.8;
            this.createDustParticles(this.player.x + this.player.width / 2, this.FLOOR_Y, 8);
            if (window.JumpmanAudio) window.JumpmanAudio.playJump();
        } else if (this.player.jumpCount === 1) {
            // 2단 점프 (더블 점프!)
            this.player.vy = this.player.doubleJumpPower;
            this.player.jumpCount = 2;
            this.player.scaleY = 1.25;
            this.player.scaleX = 0.85;
            this.createStarPuff(this.player.x + this.player.width / 2, this.player.y + this.player.height);
            if (window.JumpmanAudio) window.JumpmanAudio.playDoubleJump();
        }
    }

    createDustParticles(x, y, count = 6) {
        for (let i = 0; i < count; i++) {
            this.particles.push({
                x: x,
                y: y,
                vx: (Math.random() - 0.5) * 4 - this.gameSpeed * 0.3,
                vy: -Math.random() * 2.5 - 0.5,
                radius: Math.random() * 3 + 2,
                color: '#CBD5E1',
                alpha: 0.8,
                decay: 0.04
            });
        }
    }

    createStarPuff(x, y) {
        for (let i = 0; i < 8; i++) {
            const angle = (Math.PI * 2 * i) / 8;
            this.particles.push({
                x: x,
                y: y,
                vx: Math.cos(angle) * 3,
                vy: Math.sin(angle) * 3,
                radius: Math.random() * 3 + 2,
                color: '#FDE047',
                alpha: 1,
                decay: 0.05
            });
        }
    }

    createBlastEffect(x, y) {
        for (let i = 0; i < 25; i++) {
            const angle = Math.random() * Math.PI * 2;
            const spd = Math.random() * 8 + 3;
            this.particles.push({
                x: x,
                y: y,
                vx: Math.cos(angle) * spd,
                vy: Math.sin(angle) * spd,
                radius: Math.random() * 5 + 3,
                color: Math.random() < 0.5 ? '#60A5FA' : '#38BDF8',
                alpha: 1,
                decay: 0.03
            });
        }
    }

    spawnObstacle() {
        const types = ['rolling_ball', 'spikes', 'flying_bat', 'bouncing_ball', 'falling_rock'];
        // 난이도에 따른 타입 선택
        let availableTypes = ['rolling_ball', 'spikes'];
        if (this.distance > 80 || this.currentStage >= 2) availableTypes.push('bouncing_ball');
        if (this.distance > 160 || this.currentStage >= 2) availableTypes.push('flying_bat');
        if (this.distance > 280 || this.currentStage >= 3) availableTypes.push('falling_rock');

        const chosenType = availableTypes[Math.floor(Math.random() * availableTypes.length)];

        if (chosenType === 'rolling_ball') {
            const radius = Math.floor(Math.random() * 8) + 18; // 18 ~ 26px
            this.obstacles.push({
                type: 'rolling_ball',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - radius,
                radius: radius,
                width: radius * 2,
                height: radius * 2,
                rotation: 0,
                speedMultiplier: 1.05 + Math.random() * 0.25,
                color: '#EF4444'
            });
        } else if (chosenType === 'bouncing_ball') {
            const radius = 20;
            this.obstacles.push({
                type: 'bouncing_ball',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - radius,
                radius: radius,
                width: radius * 2,
                height: radius * 2,
                rotation: 0,
                bounceVy: -7.5,
                speedMultiplier: 1.0,
                color: '#F97316'
            });
        } else if (chosenType === 'spikes') {
            const count = Math.random() < 0.6 ? 2 : 3;
            const w = count * 20;
            const h = 26;
            this.obstacles.push({
                type: 'spikes',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - h,
                width: w,
                height: h,
                count: count,
                speedMultiplier: 1.0,
                color: '#DC2626'
            });
        } else if (chosenType === 'flying_bat') {
            const yPositions = [this.FLOOR_Y - 60, this.FLOOR_Y - 95, this.FLOOR_Y - 130];
            const chosenY = yPositions[Math.floor(Math.random() * yPositions.length)];
            this.obstacles.push({
                type: 'flying_bat',
                x: this.WIDTH + 30,
                y: chosenY,
                baseY: chosenY,
                width: 38,
                height: 26,
                wingPhase: 0,
                speedMultiplier: 1.25,
                color: '#8B5CF6'
            });
        } else if (chosenType === 'falling_rock') {
            this.obstacles.push({
                type: 'falling_rock',
                x: this.WIDTH + 20,
                y: -40,
                width: 32,
                height: 32,
                vy: 0,
                isFalling: false,
                triggerDistance: 320,
                speedMultiplier: 1.0,
                color: '#78716C'
            });
        }
    }

    spawnCoin() {
        const patterns = ['single', 'arc', 'line'];
        const pattern = patterns[Math.floor(Math.random() * patterns.length)];

        if (pattern === 'single') {
            const y = this.FLOOR_Y - 50 - Math.random() * 80;
            this.collectibles.push({ x: this.WIDTH + 30, y: y, size: 14, collected: false });
        } else if (pattern === 'arc') {
            const count = 4;
            for (let i = 0; i < count; i++) {
                const angle = (Math.PI / (count - 1)) * i;
                const arcY = (this.FLOOR_Y - 60) - Math.sin(angle) * 75;
                this.collectibles.push({
                    x: this.WIDTH + 30 + i * 36,
                    y: arcY,
                    size: 14,
                    collected: false
                });
            }
        } else if (pattern === 'line') {
            const count = 3;
            const y = this.FLOOR_Y - 45;
            for (let i = 0; i < count; i++) {
                this.collectibles.push({
                    x: this.WIDTH + 30 + i * 35,
                    y: y,
                    size: 14,
                    collected: false
                });
            }
        }
    }

    update(dt) {
        if (!this.isRunning || this.isPaused) return;

        // 생존 시간 및 거리 누적
        this.survivalTime += dt;
        this.distance += Math.round(this.gameSpeed * 0.15);
        this.score += Math.round(this.gameSpeed * 0.4);

        // 점진적 속도 증가 (무한 서바이벌 모드)
        if (this.gameMode === 'endless') {
            this.gameSpeed = this.baseSpeed + Math.min(6.0, this.survivalTime * 0.035);
        }

        // 스테이지 모드 목적지 도달 검사
        if (this.gameMode === 'stage' && this.distance >= this.stageGoalDistance) {
            this.triggerStageClear();
            return;
        }

        // 플레이어 좌우 이동
        if (this.keys.left) {
            this.player.vx = -this.player.speed;
            this.player.facingRight = false;
        } else if (this.keys.right) {
            this.player.vx = this.player.speed;
            this.player.facingRight = true;
        } else {
            this.player.vx *= 0.75;
        }

        this.player.x += this.player.vx;
        // 화면 좌우 경계 유지
        if (this.player.x < 40) this.player.x = 40;
        if (this.player.x > this.WIDTH - 120) this.player.x = this.WIDTH - 120;

        // 플레이어 중력 및 수직 점프 적용
        this.player.vy += this.player.gravity;
        this.player.y += this.player.vy;

        // 착지 확인
        if (this.player.y >= this.FLOOR_Y - this.player.height) {
            if (!this.player.onGround && this.player.vy > 2) {
                // 착지 찌그러짐 연출
                this.player.scaleY = 0.8;
                this.player.scaleX = 1.25;
                this.createDustParticles(this.player.x + this.player.width / 2, this.FLOOR_Y, 5);
            }
            this.player.y = this.FLOOR_Y - this.player.height;
            this.player.vy = 0;
            this.player.onGround = true;
            this.player.jumpCount = 0;
        }

        // 스케일 회복 (통통 튀는 만화적 느낌)
        this.player.scaleX += (1 - this.player.scaleX) * 0.2;
        this.player.scaleY += (1 - this.player.scaleY) * 0.2;

        // 달리기 프레임 업데이트
        if (this.player.onGround) {
            this.player.runFrame += this.gameSpeed * 0.08;
        }

        // 무적 타이머 감소
        if (this.player.invulnerableTimer > 0) {
            this.player.invulnerableTimer--;
        }

        // 장애물 생성 타이머
        this.spawnTimer--;
        if (this.spawnTimer <= 0) {
            this.spawnObstacle();
            // 다음 생성 주기 (난이도에 따라 점차 빨라짐)
            const minGap = Math.max(45, 95 - Math.floor(this.gameSpeed * 4.5));
            const maxGap = Math.max(75, 140 - Math.floor(this.gameSpeed * 5));
            this.spawnTimer = Math.floor(Math.random() * (maxGap - minGap)) + minGap;
        }

        // 코인 생성 타이머
        this.coinSpawnTimer--;
        if (this.coinSpawnTimer <= 0) {
            this.spawnCoin();
            this.coinSpawnTimer = Math.floor(Math.random() * 90) + 70;
        }

        // 장애물 업데이트 및 충돌 검사
        for (let i = this.obstacles.length - 1; i >= 0; i--) {
            const obs = this.obstacles[i];
            obs.x -= this.gameSpeed * (obs.speedMultiplier || 1.0);

            // 타입별 특수 움직임
            if (obs.type === 'rolling_ball') {
                obs.rotation += (this.gameSpeed * 0.05);
            } else if (obs.type === 'bouncing_ball') {
                obs.rotation += (this.gameSpeed * 0.04);
                obs.y += obs.bounceVy;
                obs.bounceVy += 0.35; // 바운스 중력
                if (obs.y >= this.FLOOR_Y - obs.radius) {
                    obs.y = this.FLOOR_Y - obs.radius;
                    obs.bounceVy = -7.2; // 튕기기
                }
            } else if (obs.type === 'flying_bat') {
                obs.wingPhase += 0.25;
                obs.y = obs.baseY + Math.sin(obs.wingPhase) * 16;
            } else if (obs.type === 'falling_rock') {
                if (!obs.isFalling && obs.x - this.player.x < obs.triggerDistance) {
                    obs.isFalling = true;
                }
                if (obs.isFalling) {
                    obs.vy += 0.6;
                    obs.y += obs.vy;
                    if (obs.y >= this.FLOOR_Y - obs.height) {
                        obs.y = this.FLOOR_Y - obs.height;
                        obs.vy = 0;
                    }
                }
            }

            // 충돌 검사
            if (this.checkCollision(this.player, obs)) {
                if (this.player.invulnerableTimer > 0) {
                    // 무적 상태일 때 충돌한 장애물 파괴
                    this.createBlastEffect(obs.x + obs.width / 2, obs.y + obs.height / 2);
                    this.obstacles.splice(i, 1);
                    this.score += 50;
                    continue;
                } else {
                    // 충돌 발생! 찬스 검사 및 부활 퀴즈 트리거
                    this.handlePlayerHit(obs);
                    return;
                }
            }

            // 화면 밖 제거
            if (obs.x + (obs.width || 50) < -50) {
                this.obstacles.splice(i, 1);
            }
        }

        // 코인 업데이트 및 수집 검사
        for (let i = this.collectibles.length - 1; i >= 0; i--) {
            const c = this.collectibles[i];
            c.x -= this.gameSpeed;

            // 자석 효과 (플레이어와 가까워지면 당겨짐)
            const dx = (this.player.x + this.player.width / 2) - c.x;
            const dy = (this.player.y + this.player.height / 2) - c.y;
            const dist = Math.hypot(dx, dy);

            if (dist < 42 && !c.collected) {
                c.collected = true;
                this.coins++;
                this.score += 100;
                this.createStarPuff(c.x, c.y);
                if (window.JumpmanAudio) window.JumpmanAudio.playCoin();
                this.collectibles.splice(i, 1);
                continue;
            }

            if (c.x < -30) {
                this.collectibles.splice(i, 1);
            }
        }

        // 파티클 업데이트
        for (let i = this.particles.length - 1; i >= 0; i--) {
            const p = this.particles[i];
            p.x += p.vx;
            p.y += p.vy;
            p.alpha -= p.decay;
            if (p.alpha <= 0) {
                this.particles.splice(i, 1);
            }
        }

        // 배경 별 이동
        this.backgroundStars.forEach(s => {
            s.x -= s.speed * (this.gameSpeed * 0.2);
            if (s.x < 0) s.x = this.WIDTH;
        });

        // 화면 흔들림 감쇠
        if (this.screenShake > 0) {
            this.screenShake *= 0.88;
            if (this.screenShake < 0.3) this.screenShake = 0;
        }

        this.updateHUD();
    }

    checkCollision(player, obs) {
        // 부드러운 플레이를 위해 히트박스 3px 살짝 여유 부여
        const pLeft = player.x + 4;
        const pRight = player.x + player.width - 4;
        const pTop = player.y + 4;
        const pBottom = player.y + player.height - 2;

        let oLeft = obs.x + 3;
        let oRight = obs.x + obs.width - 3;
        let oTop = obs.y + 3;
        let oBottom = obs.y + obs.height - 3;

        if (obs.type === 'rolling_ball' || obs.type === 'bouncing_ball') {
            const cx = obs.x;
            const cy = obs.y;
            const r = obs.radius * 0.88;
            oLeft = cx - r;
            oRight = cx + r;
            oTop = cy - r;
            oBottom = cy + r;
        }

        return (pLeft < oRight && pRight > oLeft && pTop < oBottom && pBottom > oTop);
    }

    handlePlayerHit(obstacle) {
        this.isPaused = true;
        this.screenShake = 18;

        if (window.JumpmanAudio) {
            window.JumpmanAudio.playCrash();
        }

        // 찬스가 남아있는 경우: 영어 퀴즈 찬스 모달 팝업!
        if (this.chances > 0) {
            if (window.openReviveModal) {
                window.openReviveModal(this.chances, (success) => {
                    if (success) {
                        // 퀴즈 정답 -> 부활 성공!
                        this.chances--;
                        this.quizzesSolved++;
                        this.score += 200; // 부활 퀴즈 정답 보너스 점수

                        if (window.JumpmanAudio) {
                            window.JumpmanAudio.playCorrect();
                            window.JumpmanAudio.playReviveShield();
                        }

                        // 플레이어 주변 장애물 폭발 제거
                        this.createBlastEffect(this.player.x + this.player.width / 2, this.player.y + this.player.height / 2);
                        this.obstacles = this.obstacles.filter(o => Math.abs(o.x - this.player.x) > 300);

                        // 2.8초간 무적 보호막 부여 (약 170프레임)
                        this.player.invulnerableTimer = 170;
                        this.isPaused = false;
                        this.updateHUD();
                    } else {
                        // 퀴즈 오답 -> 즉시 게임오버
                        if (window.JumpmanAudio) window.JumpmanAudio.playWrong();
                        this.triggerGameOver('부활 퀴즈 오답');
                    }
                });
            } else {
                this.triggerGameOver('부활 모달 연결 실패');
            }
        } else {
            // 찬스 3회 모두 소진 -> 게임 오버
            this.triggerGameOver('찬스 소진');
        }
    }

    triggerGameOver(reason = '') {
        this.isRunning = false;
        this.isPaused = true;
        if (window.JumpmanAudio) {
            window.JumpmanAudio.stopBgm();
            window.JumpmanAudio.playGameOver();
        }

        if (window.showGameOverModal) {
            window.showGameOverModal({
                score: this.score,
                distance: this.distance,
                survivalTime: Math.floor(this.survivalTime),
                coins: this.coins,
                quizzesSolved: this.quizzesSolved,
                gameMode: this.gameMode,
                stage: this.currentStage,
                reason: reason
            });
        }
    }

    triggerStageClear() {
        this.isRunning = false;
        this.isPaused = true;
        if (window.JumpmanAudio) {
            window.JumpmanAudio.stopBgm();
            window.JumpmanAudio.playStageClear();
        }

        if (window.showStageClearModal) {
            window.showStageClearModal({
                stage: this.currentStage,
                score: this.score,
                coins: this.coins,
                quizzesSolved: this.quizzesSolved,
                survivalTime: Math.floor(this.survivalTime),
                distance: this.distance
            });
        }
    }

    updateHUD() {
        // 찬스 하트 렌더링
        const hudChances = document.getElementById('hudChances');
        if (hudChances) {
            let hearts = '';
            for (let i = 0; i < 3; i++) {
                hearts += (i < this.chances) ? '❤️ ' : '🖤 ';
            }
            hudChances.innerHTML = hearts.trim();
        }

        const hudScore = document.getElementById('hudScore');
        if (hudScore) hudScore.textContent = this.score.toLocaleString();

        const hudDistance = document.getElementById('hudDistance');
        if (hudDistance) {
            if (this.gameMode === 'stage') {
                hudDistance.textContent = `${this.distance}m / ${this.stageGoalDistance}m`;
            } else {
                hudDistance.textContent = `${this.distance}m`;
            }
        }

        const hudTime = document.getElementById('hudTime');
        if (hudTime) {
            const sec = Math.floor(this.survivalTime);
            const m = Math.floor(sec / 60);
            const s = sec % 60;
            hudTime.textContent = `${m}:${s < 10 ? '0' : ''}${s}`;
        }

        const hudCoins = document.getElementById('hudCoins');
        if (hudCoins) hudCoins.textContent = this.coins;

        const hudStageBadge = document.getElementById('hudStageBadge');
        if (hudStageBadge) {
            if (this.gameMode === 'stage') {
                hudStageBadge.textContent = `STAGE ${this.currentStage}`;
                hudStageBadge.className = 'px-2.5 py-1 rounded-full text-xs font-black bg-amber-500/20 text-amber-300 border border-amber-500/40';
            } else {
                hudStageBadge.textContent = '무한 서바이벌';
                hudStageBadge.className = 'px-2.5 py-1 rounded-full text-xs font-black bg-rose-500/20 text-rose-300 border border-rose-500/40';
            }
        }
    }

    render() {
        if (!this.ctx) return;
        const ctx = this.ctx;

        ctx.save();
        // 화면 흔들림 효과
        if (this.screenShake > 0) {
            const shakeX = (Math.random() - 0.5) * this.screenShake;
            const shakeY = (Math.random() - 0.5) * this.screenShake;
            ctx.translate(shakeX, shakeY);
        }

        // 1. 배경 그라데이션 (네온 레트로 스카이)
        const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
        skyGrad.addColorStop(0, '#0F172A'); // 딥 나이트 블루
        skyGrad.addColorStop(0.65, '#1E1B4B'); // 인디고 퍼플
        skyGrad.addColorStop(1, '#311042'); // 노을빛 퍼플
        ctx.fillStyle = skyGrad;
        ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

        // 2. 배경 별
        this.backgroundStars.forEach(s => {
            ctx.fillStyle = `rgba(255, 255, 255, ${s.alpha})`;
            ctx.beginPath();
            ctx.arc(s.x, s.y, s.size, 0, Math.PI * 2);
            ctx.fill();
        });

        // 3. 원경 산/도시 실루엣
        ctx.fillStyle = 'rgba(15, 23, 42, 0.6)';
        ctx.beginPath();
        ctx.moveTo(0, this.FLOOR_Y);
        for (let x = 0; x <= this.WIDTH; x += 60) {
            const h = Math.sin((x + this.distance * 0.2) * 0.015) * 50 + 60;
            ctx.lineTo(x, this.FLOOR_Y - h);
        }
        ctx.lineTo(this.WIDTH, this.FLOOR_Y);
        ctx.closePath();
        ctx.fill();

        // 4. 바닥 (지면 및 체크무늬 패턴)
        const floorGrad = ctx.createLinearGradient(0, this.FLOOR_Y, 0, this.HEIGHT);
        floorGrad.addColorStop(0, '#10B981'); // 잔디 라인
        floorGrad.addColorStop(0.08, '#047857');
        floorGrad.addColorStop(1, '#064E3B');
        ctx.fillStyle = floorGrad;
        ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

        // 바닥 격자 애니메이션 라인
        ctx.strokeStyle = 'rgba(52, 211, 153, 0.35)';
        ctx.lineWidth = 2;
        const gridOffset = (this.distance * 2) % 40;
        for (let x = -gridOffset; x <= this.WIDTH; x += 40) {
            ctx.beginPath();
            ctx.moveTo(x, this.FLOOR_Y);
            ctx.lineTo(x, this.HEIGHT);
            ctx.stroke();
        }

        // 지면 탑 네온 라인
        ctx.strokeStyle = '#34D399';
        ctx.lineWidth = 3;
        ctx.beginPath();
        ctx.moveTo(0, this.FLOOR_Y);
        ctx.lineTo(this.WIDTH, this.FLOOR_Y);
        ctx.stroke();

        // 5. 수집 아이템 (코인 🪙)
        this.collectibles.forEach(c => {
            ctx.save();
            ctx.translate(c.x, c.y);
            // 회전/반짝임
            ctx.fillStyle = '#FBBF24';
            ctx.beginPath();
            ctx.arc(0, 0, c.size, 0, Math.PI * 2);
            ctx.fill();

            // 내부 테두리 & 달러/별 마크
            ctx.strokeStyle = '#D97706';
            ctx.lineWidth = 2;
            ctx.stroke();

            ctx.fillStyle = '#FEF3C7';
            ctx.font = 'bold 11px Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('★', 0, 1);
            ctx.restore();
        });

        // 6. 장애물 렌더링
        this.obstacles.forEach(obs => {
            ctx.save();
            if (obs.type === 'rolling_ball' || obs.type === 'bouncing_ball') {
                ctx.translate(obs.x, obs.y);
                ctx.rotate(obs.rotation);

                // 구체 본체
                const ballGrad = ctx.createRadialGradient(-obs.radius * 0.3, -obs.radius * 0.3, 2, 0, 0, obs.radius);
                if (obs.type === 'rolling_ball') {
                    ballGrad.addColorStop(0, '#F87171');
                    ballGrad.addColorStop(1, '#DC2626');
                } else {
                    ballGrad.addColorStop(0, '#FDBA74');
                    ballGrad.addColorStop(1, '#EA580C');
                }
                ctx.fillStyle = ballGrad;
                ctx.beginPath();
                ctx.arc(0, 0, obs.radius, 0, Math.PI * 2);
                ctx.fill();

                // 볼 패턴 (스파이크 밴드 / 회전선)
                ctx.strokeStyle = 'rgba(255, 255, 255, 0.7)';
                ctx.lineWidth = 2.5;
                ctx.beginPath();
                ctx.moveTo(-obs.radius, 0);
                ctx.lineTo(obs.radius, 0);
                ctx.moveTo(0, -obs.radius);
                ctx.lineTo(0, obs.radius);
                ctx.stroke();
            } else if (obs.type === 'spikes') {
                ctx.fillStyle = '#EF4444';
                const count = obs.count || 2;
                const spikeWidth = obs.width / count;
                for (let k = 0; k < count; k++) {
                    const sx = obs.x + k * spikeWidth;
                    ctx.beginPath();
                    ctx.moveTo(sx, obs.y + obs.height);
                    ctx.lineTo(sx + spikeWidth / 2, obs.y);
                    ctx.lineTo(sx + spikeWidth, obs.y + obs.height);
                    ctx.closePath();
                    ctx.fill();
                    ctx.strokeStyle = '#991B1B';
                    ctx.lineWidth = 1.5;
                    ctx.stroke();
                }
            } else if (obs.type === 'flying_bat') {
                ctx.translate(obs.x, obs.y);
                // 날개 펄럭임
                const wingY = Math.sin(obs.wingPhase * 2) * 8;
                ctx.fillStyle = '#7C3AED';

                // 왼쪽 날개
                ctx.beginPath();
                ctx.moveTo(-16, wingY);
                ctx.lineTo(-4, 0);
                ctx.lineTo(-12, 10);
                ctx.closePath();
                ctx.fill();

                // 오른쪽 날개
                ctx.beginPath();
                ctx.moveTo(16, wingY);
                ctx.lineTo(4, 0);
                ctx.lineTo(12, 10);
                ctx.closePath();
                ctx.fill();

                // 박쥐 몸통 & 눈
                ctx.fillStyle = '#5B21B6';
                ctx.beginPath();
                ctx.arc(0, 0, 9, 0, Math.PI * 2);
                ctx.fill();

                // 빨간 눈
                ctx.fillStyle = '#EF4444';
                ctx.fillRect(-4, -2, 2, 2);
                ctx.fillRect(2, -2, 2, 2);
            } else if (obs.type === 'falling_rock') {
                ctx.fillStyle = '#78716C';
                ctx.fillRect(obs.x, obs.y, obs.width, obs.height);
                ctx.strokeStyle = '#44403C';
                ctx.lineWidth = 2;
                ctx.strokeRect(obs.x, obs.y, obs.width, obs.height);
                // 낙하 경고 라인
                if (!obs.isFalling) {
                    ctx.fillStyle = 'rgba(239, 68, 68, 0.7)';
                    ctx.font = 'bold 14px sans-serif';
                    ctx.textAlign = 'center';
                    ctx.fillText('▼', obs.x + obs.width / 2, obs.y + obs.height + 15);
                }
            }
            ctx.restore();
        });

        // 7. 파티클
        this.particles.forEach(p => {
            ctx.save();
            ctx.globalAlpha = Math.max(0, p.alpha);
            ctx.fillStyle = p.color;
            ctx.beginPath();
            ctx.arc(p.x, p.y, p.radius, 0, Math.PI * 2);
            ctx.fill();
            ctx.restore();
        });

        // 8. 플레이어 (Jumpman 캐릭터 렌더링)
        this.renderPlayer(ctx);

        // 9. 목적지 깃발 (스테이지 모드일 때)
        if (this.gameMode === 'stage') {
            const remainDist = this.stageGoalDistance - this.distance;
            if (remainDist < 120) {
                const flagX = this.WIDTH - remainDist * 3;
                if (flagX > 0 && flagX < this.WIDTH + 50) {
                    this.renderGoalFlag(ctx, flagX, this.FLOOR_Y);
                }
            }
        }

        ctx.restore();
    }

    renderPlayer(ctx) {
        const p = this.player;

        // 무적 상태일 때 깜빡임 연출
        if (p.invulnerableTimer > 0 && Math.floor(p.invulnerableTimer / 4) % 2 === 0) {
            ctx.globalAlpha = 0.45;
        }

        ctx.save();
        ctx.translate(p.x + p.width / 2, p.y + p.height);
        ctx.scale(p.scaleX * (p.facingRight ? 1 : -1), p.scaleY);

        // 무적 보호막 아우라 링
        if (p.invulnerableTimer > 0) {
            ctx.strokeStyle = 'rgba(56, 189, 248, 0.85)';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.arc(0, -p.height / 2, p.height * 0.75, 0, Math.PI * 2);
            ctx.stroke();

            ctx.fillStyle = 'rgba(56, 189, 248, 0.15)';
            ctx.fill();
        }

        // Jumpman 캐릭터 본체 (복고풍 플랫포머 히어로)
        // A. 다리 (달리기 애니메이션)
        ctx.fillStyle = '#1E3A8A'; // 진청색 바지
        const legPhase = p.onGround ? Math.sin(p.runFrame) * 10 : -4;
        // 뒷다리
        ctx.fillRect(-10, -12, 7, 12 + legPhase * 0.5);
        // 앞다리
        ctx.fillRect(3, -12, 7, 12 - legPhase * 0.5);

        // 신발 (스니커즈)
        ctx.fillStyle = '#EF4444';
        ctx.fillRect(-12, -2 + legPhase * 0.5, 9, 4);
        ctx.fillRect(3, -2 - legPhase * 0.5, 9, 4);

        // B. 몸통 (레드 오버올 & 셔츠)
        ctx.fillStyle = '#DC2626'; // 레드 셔츠
        ctx.fillRect(-11, -32, 22, 20);

        // 오버올 멜빵
        ctx.fillStyle = '#2563EB';
        ctx.fillRect(-8, -26, 4, 14);
        ctx.fillRect(4, -26, 4, 14);
        // 노란 단추
        ctx.fillStyle = '#FBBF24';
        ctx.fillRect(-7, -24, 2, 2);
        ctx.fillRect(5, -24, 2, 2);

        // C. 머리 및 모자
        // 얼굴 살색
        ctx.fillStyle = '#FED7AA';
        ctx.fillRect(-9, -46, 18, 14);

        // 눈
        ctx.fillStyle = '#0F172A';
        ctx.fillRect(2, -42, 3, 4);

        // 코
        ctx.fillStyle = '#FDBA74';
        ctx.fillRect(5, -39, 4, 4);

        // 빨간 모자 (Jumpman Cap)
        ctx.fillStyle = '#DC2626';
        ctx.fillRect(-11, -48, 22, 6);
        ctx.fillRect(1, -44, 12, 4); // 챙

        ctx.restore();
        ctx.globalAlpha = 1.0;
    }

    renderGoalFlag(ctx, x, y) {
        ctx.save();
        ctx.translate(x, y);

        // 깃대
        ctx.fillStyle = '#E2E8F0';
        ctx.fillRect(-2, -90, 4, 90);

        // 깃발 (체크무늬 레이싱 골 플래그)
        ctx.fillStyle = '#F59E0B';
        ctx.beginPath();
        ctx.moveTo(2, -88);
        ctx.lineTo(36, -72);
        ctx.lineTo(2, -56);
        ctx.closePath();
        ctx.fill();

        ctx.fillStyle = '#FFFFFF';
        ctx.font = 'black 12px Pretendard, sans-serif';
        ctx.fillText('GOAL', 6, -68);

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

window.JumpmanGame = JumpmanGame;
