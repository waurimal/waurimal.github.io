/**
 * 스펠링 갤러그 (Spelling Galaga) - 게임 엔진 및 메인 루프
 * 갤러그 편대 비행, 듀얼 파이터 구출, 트랙터 빔, 파워업 아이템, 5 목숨 시스템, 초등 맞춤 3단계 난이도
 */

class SpellingGalagaGame {
  constructor(canvasId) {
    this.canvas = document.getElementById(canvasId);
    this.ctx = this.canvas.getContext('2d');

    // 가상 해상도 (800x600 4:3 갤러그 아케이드 비율)
    this.virtualWidth = 800;
    this.virtualHeight = 600;
    this.scale = 1;

    // 모듈 초기화
    this.wordManager = new WordManager();
    this.sounds = new SoundEffects();
    this.sprites = new SpriteRenderer();

    // 게임 상태: 'TITLE' | 'PLAYING' | 'WORD_CLEAR' | 'GAME_OVER' | 'VICTORY'
    this.state = 'TITLE';

    // 기본 난이도 모드: 'easy' (초등 배려: 오답 시 목숨 보호 & 조준 가이드) | 'normal' | 'hard'
    this.difficulty = 'easy';
    this.wrongShotCount = 0; // 중급 모드용 연속 오답 카운터

    // 기본 게임 규칙 & 상태 변수
    this.maxLives = 5;
    this.lives = 5;
    this.score = 0;
    this.highScore = parseInt(localStorage.getItem('spelling_galaga_highscore') || '0', 10);
    this.combo = 0;
    this.stage = 1;
    this.currentCategory = 'grade4';

    // 단어 진행 관리
    this.wordList = [];
    this.wordIndex = 0;
    this.currentWordObj = null;
    this.currentWord = "";
    this.targetLetterIndex = 0;
    this.spellingSlots = [];

    // 학습 리포트용 기록
    this.learnedWords = [];
    this.missedWords = [];
    this.totalShots = 0;
    this.hitShots = 0;

    // 플레이어 객체 (스피드 8.2로 민첩한 회피 지원)
    this.player = {
      x: this.virtualWidth / 2,
      y: this.virtualHeight - 50,
      width: 46,
      height: 42,
      speed: 8.2,
      tilt: 0,
      isInvincible: false,
      invincibleTimer: 0,
      isDual: false,
      hasShield: false,
      captured: false
    };

    this.capturedFighterBoss = null;

    // 엔티티 컬렉션
    this.missiles = [];
    this.enemyBullets = [];
    this.enemies = [];
    this.powerups = [];
    this.particles = [];
    this.floatingTexts = [];
    this.stars = [];

    // 입력 상태
    this.keys = {};
    this.lastFireTime = 0;
    this.fireCooldown = 150; // ms
    this.touchControl = {
      active: false,
      targetX: this.virtualWidth / 2,
      isFiring: false
    };

    // 편대 비행 및 효과 제어
    this.formationOffset = 0;
    this.formationDirection = 1;
    this.diveTimer = 0;
    this.activeTractorBeam = null;
    this.shakeTimer = 0;
    this.warpTimer = 0;
    this.slowMotionTimer = 0;

    // 초기화
    this.initStars();
    this.bindEvents();
    this.resizeCanvas();
    window.addEventListener('resize', () => this.resizeCanvas());

    this.lastFrameTime = performance.now();
    requestAnimationFrame((t) => this.loop(t));
  }

  // 별무리 (Starfield Parallax)
  initStars() {
    this.stars = [];
    for (let i = 0; i < 95; i++) {
      this.stars.push({
        x: Math.random() * this.virtualWidth,
        y: Math.random() * this.virtualHeight,
        size: Math.random() < 0.2 ? 2.5 : (Math.random() < 0.5 ? 1.5 : 1),
        speed: 0.4 + Math.random() * 1.8,
        color: ['#ffffff', '#7dd3fc', '#fef08a', '#f43f5e'][Math.floor(Math.random() * 4)]
      });
    }
  }

  // 캔버스 반응형 리사이징
  resizeCanvas() {
    const container = this.canvas.parentElement;
    const cw = container.clientWidth;
    const ch = container.clientHeight;

    const aspect = this.virtualWidth / this.virtualHeight;
    let w = cw;
    let h = cw / aspect;

    if (h > ch) {
      h = ch;
      w = ch * aspect;
    }

    this.canvas.style.width = `${Math.floor(w)}px`;
    this.canvas.style.height = `${Math.floor(h)}px`;

    this.canvas.width = this.virtualWidth;
    this.canvas.height = this.virtualHeight;
  }

  // 이벤트 바인딩
  bindEvents() {
    window.addEventListener('keydown', (e) => {
      this.keys[e.code] = true;

      if (['Space', 'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].includes(e.code)) {
        e.preventDefault();
      }

      this.sounds.init();

      if (e.code === 'Space' || e.code === 'Enter') {
        if (this.state === 'TITLE' || this.state === 'GAME_OVER' || this.state === 'VICTORY') {
          this.startGame();
        } else if (this.state === 'PLAYING') {
          this.shootMissile();
        }
      }
    });

    window.addEventListener('keyup', (e) => {
      this.keys[e.code] = false;
    });

    // 마우스 및 터치스크린 부드러운 드래그
    const handlePointerMove = (e) => {
      const rect = this.canvas.getBoundingClientRect();
      const clientX = e.touches ? e.touches[0].clientX : e.clientX;
      const x = ((clientX - rect.left) / rect.width) * this.virtualWidth;
      this.touchControl.targetX = Math.max(25, Math.min(this.virtualWidth - 25, x));
      this.touchControl.active = true;
    };

    const handlePointerDown = (e) => {
      this.sounds.init();
      handlePointerMove(e);
      if (this.state === 'TITLE' || this.state === 'GAME_OVER' || this.state === 'VICTORY') {
        this.startGame();
      } else if (this.state === 'PLAYING') {
        this.shootMissile();
      }
    };

    this.canvas.addEventListener('mousedown', handlePointerDown);
    this.canvas.addEventListener('mousemove', (e) => {
      if (e.buttons > 0) handlePointerMove(e);
    });

    this.canvas.addEventListener('touchstart', (e) => {
      handlePointerDown(e);
      e.preventDefault();
    }, { passive: false });

    this.canvas.addEventListener('touchmove', (e) => {
      handlePointerMove(e);
      e.preventDefault();
    }, { passive: false });
  }

  // 난이도 설정 변경
  setDifficulty(diff) {
    if (['easy', 'normal', 'hard'].includes(diff)) {
      this.difficulty = diff;
      this.wrongShotCount = 0;
      this.updateHUD();
    }
  }

  // 새 게임 시작
  startGame(categoryKey = null) {
    if (categoryKey) {
      this.currentCategory = categoryKey;
    }
    this.wordList = this.wordManager.getWordsForCategory(this.currentCategory);
    if (!this.wordList || this.wordList.length === 0) {
      alert("선택한 카테고리에 단어가 없습니다. 다른 단어장을 선택해주세요.");
      return;
    }

    this.wordIndex = 0;
    this.score = 0;
    this.stage = 1;
    this.lives = this.maxLives;
    this.combo = 0;
    this.wrongShotCount = 0;
    this.learnedWords = [];
    this.missedWords = [];
    this.totalShots = 0;
    this.hitShots = 0;

    this.player.x = this.virtualWidth / 2;
    this.player.isInvincible = false;
    this.player.isDual = false;
    this.player.hasShield = false;
    this.player.captured = false;
    this.capturedFighterBoss = null;

    this.missiles = [];
    this.enemyBullets = [];
    this.powerups = [];
    this.particles = [];
    this.floatingTexts = [];
    this.activeTractorBeam = null;

    this.state = 'PLAYING';
    this.sounds.startBgm();
    this.sounds.playStageStart();
    this.loadWord(this.wordList[this.wordIndex]);
  }

  // 단어 로드 & 편대 적 배치
  loadWord(wordObj) {
    this.currentWordObj = wordObj;
    this.currentWord = wordObj.word.toUpperCase();
    this.targetLetterIndex = 0;
    this.wrongShotCount = 0;
    this.spellingSlots = new Array(this.currentWord.length).fill(null);

    this.updateHUD();
    this.spawnEnemiesForWord();
  }

  // 갤러그 편대 생성
  spawnEnemiesForWord() {
    this.enemies = [];
    const neededLetters = this.currentWord.split('');

    const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    const distractorPool = alphabet.split('').filter(ch => !neededLetters.includes(ch));

    const rows = 4;
    const cols = 8;
    const startX = 140;
    const startY = 110;
    const colSpacing = 74;
    const rowSpacing = 48;

    let assignedLetters = [];

    // 목표 단어의 모든 글자를 최소 1~2개씩 반드시 배치
    neededLetters.forEach(ch => assignedLetters.push(ch));
    if (assignedLetters.length < 8) {
      neededLetters.forEach(ch => assignedLetters.push(ch));
    }

    const totalEnemies = 20;
    while (assignedLetters.length < totalEnemies) {
      const randomDistractor = distractorPool[Math.floor(Math.random() * distractorPool.length)];
      assignedLetters.push(randomDistractor);
    }

    // 셔플
    for (let i = assignedLetters.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [assignedLetters[i], assignedLetters[j]] = [assignedLetters[j], assignedLetters[i]];
    }

    let enemyIdx = 0;
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        if (enemyIdx >= totalEnemies) break;

        let type = 'zako';
        if (r === 0) type = 'boss';
        else if (r === 1) type = 'goei';

        const targetGridX = startX + c * colSpacing;
        const targetGridY = startY + r * rowSpacing;
        const letter = assignedLetters[enemyIdx++];

        const entrySide = (c % 2 === 0) ? -1 : 1;
        const startXPos = entrySide === -1 ? -40 - c * 20 : this.virtualWidth + 40 + c * 20;

        // 난이도별 다이빙 속도 및 총알 쿨다운 차등화
        let baseDiveSpeed = (this.difficulty === 'easy') ? 1.5 : (this.difficulty === 'normal' ? 2.2 : 2.9);
        let baseShootCooldown = (this.difficulty === 'easy') ? 600 : (this.difficulty === 'normal' ? 300 : 150);

        this.enemies.push({
          type,
          letter,
          formationX: targetGridX,
          formationY: targetGridY,
          x: startXPos,
          y: -40 - r * 30,
          width: type === 'boss' ? 44 : 36,
          height: type === 'boss' ? 40 : 32,
          state: 'ENTERING',
          diveSpeed: baseDiveSpeed + Math.random() * 0.6,
          diveCurve: (Math.random() - 0.5) * 3,
          shootCooldown: Math.floor(baseShootCooldown + Math.random() * 300),
          hasCapturedFighter: false
        });
      }
    }
  }

  // 플레이어 미사일 발사 (더 빠르고 시원한 명중)
  shootMissile() {
    if (this.player.captured) return;
    const now = performance.now();
    if (now - this.lastFireTime < this.fireCooldown) return;
    this.lastFireTime = now;
    this.totalShots++;

    if (this.player.isDual) {
      this.missiles.push({
        x: this.player.x - 14,
        y: this.player.y - 15,
        width: 6,
        height: 16,
        speed: 15
      });
      this.missiles.push({
        x: this.player.x + 14,
        y: this.player.y - 15,
        width: 6,
        height: 16,
        speed: 15
      });
      this.sounds.playDualLaser();
    } else {
      this.missiles.push({
        x: this.player.x,
        y: this.player.y - 15,
        width: 6,
        height: 16,
        speed: 15
      });
      this.sounds.playLaser();
    }
  }

  // 메인 루프
  loop(currentTime) {
    const dt = Math.min((currentTime - this.lastFrameTime) / 1000, 0.1);
    this.lastFrameTime = currentTime;

    this.update(dt);
    this.render();

    requestAnimationFrame((t) => this.loop(t));
  }

  // 논리 업데이트
  update(dt) {
    this.sprites.update();

    const speedMult = this.slowMotionTimer > 0 ? 0.5 : 1.0;
    if (this.slowMotionTimer > 0) {
      this.slowMotionTimer -= dt;
    }

    const starSpeedMult = this.warpTimer > 0 ? 8.0 : 1.0;
    this.stars.forEach(s => {
      s.y += s.speed * starSpeedMult;
      if (s.y > this.virtualHeight) {
        s.y = 0;
        s.x = Math.random() * this.virtualWidth;
      }
    });

    if (this.shakeTimer > 0) this.shakeTimer--;
    if (this.warpTimer > 0) this.warpTimer -= dt;

    this.particles = this.particles.filter(p => {
      p.x += p.vx;
      p.y += p.vy;
      p.vy += p.gravity || 0;
      p.life -= dt;
      return p.life > 0;
    });

    this.floatingTexts = this.floatingTexts.filter(ft => {
      ft.y += ft.vy;
      ft.life -= dt;
      return ft.life > 0;
    });

    if (this.state !== 'PLAYING') return;

    this.updatePlayer(dt);
    this.updateMissiles(dt);
    this.updatePowerups(dt);
    this.updateEnemyBullets(dt * speedMult);
    this.updateEnemies(dt * speedMult);

    if (this.keys['Space'] || this.keys['Enter']) {
      this.shootMissile();
    }
  }

  // 플레이어 이동 & 롤링 뱅크
  updatePlayer(dt) {
    if (this.player.captured) return;

    let targetTilt = 0;

    if (this.keys['ArrowLeft'] || this.keys['KeyA']) {
      this.player.x -= this.player.speed;
      this.touchControl.active = false;
      targetTilt = -0.22;
    }
    if (this.keys['ArrowRight'] || this.keys['KeyD']) {
      this.player.x += this.player.speed;
      this.touchControl.active = false;
      targetTilt = 0.22;
    }

    if (this.touchControl.active) {
      const dx = this.touchControl.targetX - this.player.x;
      this.player.x += dx * 0.32;
      targetTilt = Math.max(-0.25, Math.min(0.25, dx * 0.02));
    }

    this.player.tilt += (targetTilt - this.player.tilt) * 0.25;

    const halfW = (this.player.isDual ? this.player.width * 1.2 : this.player.width) / 2;
    this.player.x = Math.max(halfW + 10, Math.min(this.virtualWidth - halfW - 10, this.player.x));

    if (this.player.isInvincible) {
      this.player.invincibleTimer -= dt;
      if (this.player.invincibleTimer <= 0) {
        this.player.isInvincible = false;
      }
    }
  }

  // 파워업 아이템 이동 및 획득
  updatePowerups(dt) {
    for (let i = this.powerups.length - 1; i >= 0; i--) {
      const p = this.powerups[i];
      p.y += p.speed;

      if (p.y > this.virtualHeight + 20) {
        this.powerups.splice(i, 1);
        continue;
      }

      const dist = Math.hypot(p.x - this.player.x, p.y - this.player.y);
      if (dist < (this.player.width / 2 + 20) && !this.player.captured) {
        this.handlePowerupPickup(p.type, p.x, p.y);
        this.powerups.splice(i, 1);
      }
    }
  }

  // 아이템 획득 처리
  handlePowerupPickup(type, x, y) {
    this.sounds.playItemPickup();

    if (type === 'heart') {
      if (this.lives < this.maxLives) {
        this.lives++;
        this.addFloatingText('❤️ LIFE +1', x, y - 20, '#ff3366', 20);
      } else {
        this.score += 300;
        this.addFloatingText('+300 BONUS', x, y - 20, '#ffd700', 18);
      }
      this.updateHUD();
    } else if (type === 'dual') {
      this.player.isDual = true;
      this.addFloatingText('⚡ DUAL FIGHTER!', x, y - 25, '#00e5ff', 22);
    } else if (type === 'shield') {
      this.player.hasShield = true;
      this.addFloatingText('🛡️ SHIELD ACTIVE!', x, y - 25, '#38bdf8', 22);
    } else if (type === 'bomb') {
      this.sounds.playBombExplosion();
      this.shakeTimer = 20;
      this.addFloatingText('💣 SMART EMP BOMB!', this.virtualWidth / 2, this.virtualHeight / 2, '#f97316', 26);

      const expectedLetter = this.currentWord[this.targetLetterIndex];
      for (let i = this.enemies.length - 1; i >= 0; i--) {
        const e = this.enemies[i];
        if (e.letter !== expectedLetter) {
          this.createExplosion(e.x, e.y, '#f97316');
          this.enemies.splice(i, 1);
        }
      }
      this.ensureTargetLetterAvailable();
    } else if (type === 'slow') {
      this.slowMotionTimer = 8.0;
      this.addFloatingText('⏱️ TIME SLOW WARP!', x, y - 25, '#c084fc', 20);
    }
  }

  // 미사일 업데이트 및 적 피격 판정
  updateMissiles(dt) {
    for (let i = this.missiles.length - 1; i >= 0; i--) {
      const m = this.missiles[i];
      m.y -= m.speed;

      if (m.y < 30) {
        this.missiles.splice(i, 1);
        continue;
      }

      let hit = false;
      for (let j = this.enemies.length - 1; j >= 0; j--) {
        const enemy = this.enemies[j];
        const dist = Math.hypot(m.x - enemy.x, m.y - enemy.y);
        const hitRadius = (enemy.width + m.width) / 2;

        if (dist < hitRadius) {
          hit = true;
          this.hitShots++;
          this.missiles.splice(i, 1);
          this.handleEnemyHit(enemy, j);
          break;
        }
      }
    }
  }

  // 외계인 피격 처리 (난이도별 오답 패널티 완화)
  handleEnemyHit(enemy, index) {
    const expectedLetter = this.currentWord[this.targetLetterIndex];
    const isCorrect = (enemy.letter === expectedLetter);

    this.createExplosion(enemy.x, enemy.y, isCorrect ? '#ffd700' : '#ff1744');

    if (enemy.hasCapturedFighter) {
      this.sounds.playDualRescue();
      this.player.isDual = true;
      this.addFloatingText('RESCUED! DUAL FIGHTER!', enemy.x, enemy.y - 30, '#00e5ff', 24);
      this.capturedFighterBoss = null;
    }

    // 아이템 드롭 확률 대폭 상향 (45% 확률로 쏟아짐)
    if (Math.random() < 0.45 || this.combo >= 3) {
      this.spawnRandomPowerup(enemy.x, enemy.y);
    }

    this.enemies.splice(index, 1);

    if (isCorrect) {
      // 🎯 정답 글자 맞힘!
      this.sounds.playCorrect();
      this.sounds.playExplosion();
      this.wrongShotCount = 0; // 오답 누적 초기화

      this.spellingSlots[this.targetLetterIndex] = expectedLetter;
      this.targetLetterIndex++;

      this.combo++;
      const gainedScore = Math.round(100 * (1 + (this.combo - 1) * 0.5));
      this.score += gainedScore;
      if (this.score > this.highScore) {
        this.highScore = this.score;
        localStorage.setItem('spelling_galaga_highscore', this.highScore.toString());
      }

      this.addFloatingText(`+${gainedScore}`, enemy.x, enemy.y - 15, '#4ade80');
      if (this.combo >= 2) {
        this.addFloatingText(`COMBO x${this.combo}!`, enemy.x, enemy.y - 35, '#facc15');
      }

      this.updateHUD();

      if (this.targetLetterIndex >= this.currentWord.length) {
        this.handleWordCleared();
      } else {
        this.ensureTargetLetterAvailable();
      }
    } else {
      // ❌ 오답 글자 맞힘 (난이도별 친화적 규칙 적용)
      this.sounds.playWrong();
      this.combo = 0;

      if (this.difficulty === 'easy') {
        // 🟢 초급 모드: 목숨 절대 안 깎임! (학생들의 좌절 방지)
        this.shakeTimer = 6;
        this.addFloatingText(`MISS! '${enemy.letter}' (초급: 목숨 보호)`, enemy.x, enemy.y - 15, '#fbbf24', 16);
      } else if (this.difficulty === 'normal') {
        // 🟡 중급 모드: 3번 누적 실수할 때만 목숨 1개 차감
        this.wrongShotCount++;
        if (this.wrongShotCount >= 3) {
          this.wrongShotCount = 0;
          this.lives--;
          this.shakeTimer = 14;
          this.addFloatingText(`3연속 오답! -1 LIFE`, this.virtualWidth / 2, this.virtualHeight / 2, '#f87171', 20);
        } else {
          this.shakeTimer = 6;
          this.addFloatingText(`주의! (${this.wrongShotCount}/3 경고)`, enemy.x, enemy.y - 15, '#fbbf24', 16);
        }
      } else {
        // 🔴 고급 모드: 즉시 목숨 1개 차감
        this.lives--;
        this.shakeTimer = 16;
        this.addFloatingText(`MISS! -1 LIFE`, enemy.x, enemy.y - 15, '#ef4444', 18);
      }

      this.updateHUD();
      this.ensureTargetLetterAvailable();

      if (this.lives <= 0) {
        this.handleGameOver();
      }
    }
  }

  // 아이템 생성 (하트 & 쉴드 가중치 상향)
  spawnRandomPowerup(x, y) {
    const types = ['heart', 'dual', 'shield', 'bomb', 'slow'];
    const weights = [0.35, 0.25, 0.20, 0.12, 0.08];
    let rand = Math.random();
    let selected = types[0];

    for (let i = 0; i < types.length; i++) {
      if (rand < weights[i]) {
        selected = types[i];
        break;
      }
      rand -= weights[i];
    }

    this.powerups.push({
      x,
      y,
      type: selected,
      speed: 1.6
    });
  }

  // 편대 내 타깃 알파벳 보장
  ensureTargetLetterAvailable() {
    if (this.targetLetterIndex >= this.currentWord.length) return;
    const nextChar = this.currentWord[this.targetLetterIndex];
    const exists = this.enemies.some(e => e.letter === nextChar);

    if (!exists) {
      if (this.enemies.length > 0) {
        const candidate = this.enemies[Math.floor(Math.random() * this.enemies.length)];
        candidate.letter = nextChar;
      } else {
        this.spawnEnemiesForWord();
      }
    }
  }

  // 단어 완성
  handleWordCleared() {
    this.state = 'WORD_CLEAR';
    this.warpTimer = 2.0;
    this.learnedWords.push(this.currentWordObj);

    this.sounds.playWordComplete();
    this.sounds.speakWord(this.currentWord);

    const wordBonus = 500;
    this.score += wordBonus;
    this.addFloatingText(`WORD CLEAR! +${wordBonus}`, this.virtualWidth / 2, this.virtualHeight / 2 - 20, '#38bdf8', 26);

    for (let i = 0; i < 70; i++) {
      this.createExplosion(
        150 + Math.random() * 500,
        100 + Math.random() * 250,
        ['#ffd700', '#38bdf8', '#ec4899', '#4ade80', '#ffffff'][Math.floor(Math.random() * 5)]
      );
    }

    setTimeout(() => {
      this.wordIndex++;
      this.stage++;
      if (this.wordIndex < this.wordList.length) {
        this.state = 'PLAYING';
        this.loadWord(this.wordList[this.wordIndex]);
      } else {
        this.handleVictory();
      }
    }, 2000);
  }

  // 게임 오버
  handleGameOver() {
    this.state = 'GAME_OVER';
    if (this.currentWordObj) {
      this.missedWords.push(this.currentWordObj);
    }
    this.sounds.stopBgm();
    this.sounds.playGameOver();

    this.saveScoreToCloud();

    setTimeout(() => {
      this.showResultModal(false);
    }, 1300);
  }

  // 전체 승리
  handleVictory() {
    this.state = 'VICTORY';
    this.sounds.stopBgm();
    this.sounds.playWordComplete();

    this.saveScoreToCloud();

    setTimeout(() => {
      this.showResultModal(true);
    }, 1500);
  }

  // 클라우드 점수 저장
  async saveScoreToCloud() {
    try {
      const studentInfo = (typeof window !== 'undefined' && window.WaurimalNav && window.WaurimalNav.getStudentInfo)
        ? window.WaurimalNav.getStudentInfo()
        : null;

      const record = {
        name: studentInfo ? studentInfo.name : (localStorage.getItem('waurimal_student_name') || '도전자'),
        grade: studentInfo ? studentInfo.grade : (localStorage.getItem('waurimal_student_grade') || 4),
        classNum: studentInfo ? studentInfo.classNum : (localStorage.getItem('waurimal_student_class') || 1),
        score: this.score,
        accuracy: this.totalShots > 0 ? Math.round((this.hitShots / this.totalShots) * 100) : 0,
        category: this.currentCategory,
        clearedCount: this.learnedWords.length,
        timestamp: Date.now()
      };

      let localScores = JSON.parse(localStorage.getItem('spelling_galaga_scores_history') || '[]');
      localScores.push(record);
      localScores.sort((a, b) => b.score - a.score);
      localStorage.setItem('spelling_galaga_scores_history', JSON.stringify(localScores.slice(0, 50)));

      if (typeof window !== 'undefined' && window.WaurimalFirebase && window.WaurimalFirebase.getDb) {
        const db = window.WaurimalFirebase.getDb();
        if (db) {
          await db.collection('games').doc('spelling_galaga').collection('scores').add({
            ...record,
            createdAt: firebase.firestore.FieldValue.serverTimestamp()
          });
        }
      }
    } catch (e) {
      console.warn("Save score to cloud failed:", e);
    }
  }

  // 결과 모달
  showResultModal(isWin) {
    const dialog = document.getElementById('resultModal');
    if (!dialog) return;

    const titleEl = document.getElementById('resultTitle');
    const scoreEl = document.getElementById('resultScore');
    const accuracyEl = document.getElementById('resultAccuracy');
    const wordsListEl = document.getElementById('resultWordsList');

    titleEl.textContent = isWin ? '🏆 MISSION COMPLETE! 은하계 수호 성공!' : '💀 MISSION FAILED (게임 오버)';
    titleEl.className = isWin ? 'result-title win' : 'result-title lose';
    scoreEl.textContent = this.score.toLocaleString();

    const accuracy = this.totalShots > 0 ? Math.round((this.hitShots / this.totalShots) * 100) : 0;
    accuracyEl.textContent = `${accuracy}% (${this.hitShots}/${this.totalShots})`;

    wordsListEl.innerHTML = '';
    const allWords = [...this.learnedWords, ...(this.state === 'GAME_OVER' ? this.missedWords : [])];

    allWords.forEach(w => {
      const isCleared = this.learnedWords.includes(w);
      const li = document.createElement('li');
      li.className = isCleared ? 'cleared' : 'missed';
      li.innerHTML = `
        <span class="word-badge">${isCleared ? '✓ 완벽 완성' : '✗ 미완성'}</span>
        <strong class="word-text">${w.word}</strong>
        <span class="word-mean">${w.meaning}</span>
        <button type="button" class="btn-tts" onclick="window.gameInstance.sounds.speakWord('${w.word}')">🔊 듣기</button>
      `;
      wordsListEl.appendChild(li);
    });

    dialog.showModal();
  }

  // 외계인 편대 & 급강하 AI 루프 (난이도별 다이빙 빈도 완화)
  updateEnemies(dt) {
    this.formationOffset += 0.8 * this.formationDirection;
    if (Math.abs(this.formationOffset) > 25) {
      this.formationDirection *= -1;
    }

    // 급강하 쿨다운 (초급은 5.2초로 매우 넉넉함)
    const diveInterval = (this.difficulty === 'easy') ? 5.2 : (this.difficulty === 'normal' ? 3.5 : 2.0);

    this.diveTimer += dt;
    if (this.diveTimer > diveInterval && this.enemies.length > 0) {
      this.diveTimer = 0;
      const formationEnemies = this.enemies.filter(e => e.state === 'FORMATION');
      if (formationEnemies.length > 0) {
        const diver = formationEnemies[Math.floor(Math.random() * formationEnemies.length)];
        diver.state = 'DIVING';
        diver.diveTargetX = this.player.x;

        // 초급에서는 트랙터 빔 비활성화, 중/고급에서만 발동
        if (this.difficulty !== 'easy' && diver.type === 'boss' && !this.player.isDual && !this.capturedFighterBoss && Math.random() < 0.3) {
          diver.isTractorBoss = true;
        }
      }
    }

    for (let i = this.enemies.length - 1; i >= 0; i--) {
      const e = this.enemies[i];

      if (e.state === 'ENTERING') {
        e.y += 3.8;
        const dx = e.formationX - e.x;
        e.x += dx * 0.08;

        if (e.y >= e.formationY) {
          e.y = e.formationY;
          e.x = e.formationX;
          e.state = 'FORMATION';
        }
      } else if (e.state === 'FORMATION') {
        e.x = e.formationX + this.formationOffset;
        e.y = e.formationY;

        e.shootCooldown--;
        if (e.shootCooldown <= 0) {
          const cooldownBase = (this.difficulty === 'easy') ? 700 : (this.difficulty === 'normal' ? 350 : 200);
          e.shootCooldown = cooldownBase + Math.random() * 300;
          this.shootEnemyBullet(e.x, e.y);
        }
      } else if (e.state === 'DIVING') {
        e.y += e.diveSpeed;
        e.x += Math.sin(e.y * 0.03) * 2.5 + (e.diveCurve || 0);

        if (e.isTractorBoss && e.y > 180 && e.y < 350) {
          this.activeTractorBeam = {
            bossX: e.x,
            bossY: e.y + e.height / 2,
            height: 240
          };
          this.sounds.playTractorBeam();

          const beamTopW = 20;
          const beamBottomW = 90;
          const dy = this.player.y - this.activeTractorBeam.bossY;
          if (dy > 0 && dy < 240) {
            const currentBeamW = beamTopW + (beamBottomW - beamTopW) * (dy / 240);
            if (Math.abs(this.player.x - e.x) < currentBeamW / 2) {
              if (this.player.hasShield) {
                this.player.hasShield = false;
                this.sounds.playShieldDeflect();
                this.addFloatingText('SHIELD BLOCKED BEAM!', this.player.x, this.player.y - 25, '#38bdf8');
              } else if (!this.player.isInvincible && !this.player.captured) {
                e.hasCapturedFighter = true;
                this.capturedFighterBoss = e;
                this.handleFighterCaptured();
              }
            }
          }
        } else {
          this.activeTractorBeam = null;
        }

        if (Math.abs(e.y - (this.virtualHeight * 0.5)) < 4 && this.difficulty !== 'easy') {
          this.shootEnemyBullet(e.x, e.y);
        }

        if (e.y > this.virtualHeight + 40) {
          e.y = -40;
          e.x = e.formationX;
          e.state = 'ENTERING';
          e.isTractorBoss = false;
        }
      }

      // 플레이어와 적 본체 충돌
      if (!this.player.isInvincible && !this.player.captured) {
        const pDist = Math.hypot(e.x - this.player.x, e.y - this.player.y);
        if (pDist < (e.width / 2 + this.player.width / 2)) {
          this.handlePlayerDamage();
          this.createExplosion(e.x, e.y, '#ff1744');
          this.enemies.splice(i, 1);
          this.ensureTargetLetterAvailable();
          continue;
        }
      }
    }
  }

  // 기체 납치
  handleFighterCaptured() {
    this.player.captured = true;
    this.addFloatingText('CAPTURED BY BOSS!', this.player.x, this.player.y - 30, '#ef4444', 22);

    setTimeout(() => {
      this.player.captured = false;
      this.player.x = this.virtualWidth / 2;
      this.player.isInvincible = true;
      this.player.invincibleTimer = 3.0;
      this.lives--;
      this.updateHUD();
      if (this.lives <= 0) {
        this.handleGameOver();
      }
    }, 1200);
  }

  // 적 탄환 발사 (초급은 최대 2발로 아주 안전)
  shootEnemyBullet(x, y) {
    const maxBullets = (this.difficulty === 'easy') ? 2 : (this.difficulty === 'normal' ? 4 : 7);
    if (this.enemyBullets.length >= maxBullets) return;

    const bulletSpeed = (this.difficulty === 'easy') ? 1.8 : (this.difficulty === 'normal' ? 2.5 : 3.2);

    this.enemyBullets.push({
      x,
      y: y + 10,
      size: 5,
      speed: bulletSpeed
    });
  }

  // 적 탄환 이동 및 피격
  updateEnemyBullets(dt) {
    for (let i = this.enemyBullets.length - 1; i >= 0; i--) {
      const b = this.enemyBullets[i];
      b.y += b.speed;

      if (b.y > this.virtualHeight + 10) {
        this.enemyBullets.splice(i, 1);
        continue;
      }

      if (!this.player.isInvincible && !this.player.captured) {
        const dist = Math.hypot(b.x - this.player.x, b.y - this.player.y);
        if (dist < (this.player.width / 2 + b.size)) {
          this.enemyBullets.splice(i, 1);
          this.handlePlayerDamage();
        }
      }
    }
  }

  // 플레이어 피격 처리 (3초간 넉넉한 무적 시간 부여)
  handlePlayerDamage() {
    if (this.player.hasShield) {
      this.player.hasShield = false;
      this.sounds.playShieldDeflect();
      this.player.isInvincible = true;
      this.player.invincibleTimer = 2.0;
      this.addFloatingText('SHIELD DEFLECTED!', this.player.x, this.player.y - 25, '#38bdf8', 18);
      return;
    }

    if (this.player.isDual) {
      this.player.isDual = false;
      this.sounds.playExplosion();
      this.createExplosion(this.player.x + 14, this.player.y, '#ffd700');
      this.player.isInvincible = true;
      this.player.invincibleTimer = 3.0;
      this.shakeTimer = 10;
      this.addFloatingText('DUAL LOST! (1 FIGHTER REMAIN)', this.player.x, this.player.y - 25, '#f59e0b', 18);
      return;
    }

    this.lives--;
    this.sounds.playLifeLost();
    this.sounds.playExplosion();
    this.shakeTimer = 14;
    this.player.isInvincible = true;
    this.player.invincibleTimer = 3.0; // 3초간 무적으로 피격 후 안전 회피 지원

    this.addFloatingText('충돌 피격! -1 LIFE', this.player.x, this.player.y - 30, '#ef4444', 18);
    this.updateHUD();

    if (this.lives <= 0) {
      this.handleGameOver();
    }
  }

  // 폭발 파티클
  createExplosion(x, y, color = '#ffd700') {
    for (let i = 0; i < 18; i++) {
      const angle = (Math.PI * 2 * i) / 18 + (Math.random() - 0.5);
      const spd = 1.6 + Math.random() * 4.5;
      this.particles.push({
        x,
        y,
        vx: Math.cos(angle) * spd,
        vy: Math.sin(angle) * spd,
        color,
        size: 2 + Math.random() * 3,
        life: 0.45 + Math.random() * 0.35
      });
    }
  }

  // 플로팅 텍스트
  addFloatingText(text, x, y, color = '#ffffff', size = 14) {
    this.floatingTexts.push({
      text,
      x,
      y,
      vy: -1.2,
      color,
      size,
      life: 1.25
    });
  }

  // 상단 HUD 업데이트 (지금 쏠 글자 또렷하게 노출)
  updateHUD() {
    const meaningEl = document.getElementById('hudMeaning');
    if (meaningEl && this.currentWordObj) {
      meaningEl.textContent = this.currentWordObj.meaning;
    }

    // 🎯 지금 쏴야 하는 글자 뱃지 업데이트
    const nextLetterEl = document.getElementById('hudNextLetter');
    if (nextLetterEl) {
      const nextChar = (this.currentWord && this.targetLetterIndex < this.currentWord.length)
        ? this.currentWord[this.targetLetterIndex]
        : '✓';
      nextLetterEl.textContent = nextChar;
    }

    // 스펠링 슬롯들
    const slotsContainer = document.getElementById('spellingSlots');
    if (slotsContainer && this.currentWord) {
      slotsContainer.innerHTML = '';
      for (let i = 0; i < this.currentWord.length; i++) {
        const slot = document.createElement('div');
        slot.className = 'letter-slot';

        if (i < this.targetLetterIndex) {
          slot.classList.add('filled');
          slot.textContent = this.currentWord[i];
        } else if (i === this.targetLetterIndex) {
          slot.classList.add('target');
          if (this.difficulty === 'easy') {
            slot.textContent = this.currentWord[i]; // 초급: 타깃 철자 힌트 직접 표시
          } else if (this.difficulty === 'normal') {
            slot.textContent = '?'; // 중급: 물음표 타깃
          } else {
            slot.textContent = '_'; // 고급: 빈칸
          }
        } else {
          slot.textContent = '_';
        }
        slotsContainer.appendChild(slot);
      }
    }

    const livesEl = document.getElementById('hudLives');
    if (livesEl) {
      let hearts = '';
      for (let i = 0; i < this.maxLives; i++) {
        hearts += (i < this.lives) ? '❤️ ' : '🖤 ';
      }
      livesEl.textContent = hearts.trim();
    }

    const scoreEl = document.getElementById('hudScore');
    if (scoreEl) {
      scoreEl.textContent = this.score.toLocaleString();
    }
    const highScoreEl = document.getElementById('hudHighScore');
    if (highScoreEl) {
      highScoreEl.textContent = this.highScore.toLocaleString();
    }
    const stageEl = document.getElementById('hudStage');
    if (stageEl) {
      stageEl.textContent = `STAGE ${this.stage}`;
    }
  }

  // 캔버스 렌더링
  render() {
    this.ctx.save();

    if (this.shakeTimer > 0) {
      const shakeMag = (this.shakeTimer / 14) * 6;
      const ox = (Math.random() - 0.5) * shakeMag;
      const oy = (Math.random() - 0.5) * shakeMag;
      this.ctx.translate(ox, oy);
    }

    this.ctx.fillStyle = '#050814';
    this.ctx.fillRect(0, 0, this.virtualWidth, this.virtualHeight);

    const isWarp = this.warpTimer > 0;
    this.stars.forEach(s => {
      this.ctx.fillStyle = s.color;
      if (isWarp) {
        this.ctx.fillRect(s.x, s.y, s.size, s.size * 12);
      } else {
        this.ctx.fillRect(s.x, s.y, s.size, s.size);
      }
    });

    if (this.activeTractorBeam) {
      this.sprites.drawTractorBeam(
        this.ctx,
        this.activeTractorBeam.bossX,
        this.activeTractorBeam.bossY,
        this.activeTractorBeam.height
      );
    }

    this.powerups.forEach(p => {
      this.sprites.drawPowerup(this.ctx, p.x, p.y, p.type);
    });

    this.enemyBullets.forEach(b => {
      this.sprites.drawEnemyBullet(this.ctx, b.x, b.y, b.size);
    });

    // 외계인 편대 렌더링
    const expectedLetter = this.currentWord ? this.currentWord[this.targetLetterIndex] : null;
    this.enemies.forEach(e => {
      const isTarget = (e.letter === expectedLetter);
      this.sprites.drawEnemy(
        this.ctx,
        e.x,
        e.y,
        e.width,
        e.height,
        e.type,
        e.letter,
        isTarget,
        e.state === 'DIVING' ? Math.sin(e.y * 0.05) * 0.3 : 0,
        this.difficulty,
        e.hasCapturedFighter
      );
    });

    this.missiles.forEach(m => {
      this.sprites.drawMissile(this.ctx, m.x, m.y, m.width, m.height);
    });

    if ((this.state === 'PLAYING' || this.state === 'WORD_CLEAR') && !this.player.captured) {
      this.sprites.drawPlayer(
        this.ctx,
        this.player.x,
        this.player.y,
        this.player.width,
        this.player.height,
        this.player.isDual,
        this.player.isInvincible,
        this.player.tilt,
        this.player.hasShield
      );
    }

    this.particles.forEach(p => {
      this.ctx.fillStyle = p.color;
      this.ctx.fillRect(p.x, p.y, p.size, p.size);
    });

    this.floatingTexts.forEach(ft => {
      this.ctx.save();
      this.ctx.fillStyle = ft.color;
      this.ctx.shadowColor = ft.color;
      this.ctx.shadowBlur = 6;
      this.ctx.font = `bold ${ft.size}px 'Press Start 2P', monospace, sans-serif`;
      this.ctx.textAlign = 'center';
      this.ctx.fillText(ft.text, ft.x, ft.y);
      this.ctx.restore();
    });

    if (this.state === 'TITLE') {
      this.renderTitleScreen();
    } else if (this.state === 'WORD_CLEAR') {
      this.renderWordClearScreen();
    }

    this.ctx.restore();
  }

  // 타이틀 화면
  renderTitleScreen() {
    this.ctx.save();
    this.ctx.fillStyle = 'rgba(5, 8, 20, 0.78)';
    this.ctx.fillRect(0, 0, this.virtualWidth, this.virtualHeight);

    this.ctx.textAlign = 'center';
    this.ctx.fillStyle = '#ffd700';
    this.ctx.shadowColor = '#ff6b00';
    this.ctx.shadowBlur = 20;
    this.ctx.font = "bold 34px 'Press Start 2P', monospace, sans-serif";
    this.ctx.fillText("SPELLING GALAGA", this.virtualWidth / 2, 160);

    this.ctx.fillStyle = '#38bdf8';
    this.ctx.shadowBlur = 10;
    this.ctx.font = "bold 18px 'DungGeunMo', sans-serif";
    this.ctx.fillText("우주 단어 수호대 - 스펠링 갤러그", this.virtualWidth / 2, 205);

    this.ctx.fillStyle = '#ffffff';
    this.ctx.shadowBlur = 0;
    this.ctx.font = "14px 'DungGeunMo', sans-serif";
    this.ctx.fillText("1. 상단 전광판의 [단어 뜻]과 [지금 쏠 글자]를 확인하세요.", this.virtualWidth / 2, 270);
    this.ctx.fillText("2. 🎯 머리 위에 노란 화살표가 있는 외계인을 조준 사격하세요!", this.virtualWidth / 2, 300);
    this.ctx.fillText("3. 🟢 초급 모드에서는 오답을 쏴도 목숨이 깎이지 않습니다 (안심 사격)!", this.virtualWidth / 2, 330);
    this.ctx.fillText("4. ⚡ 듀얼 파이터, 🛡️ 쉴드, 💣 폭탄, ❤️ 하트 아이템을 획득하세요!", this.virtualWidth / 2, 360);
    this.ctx.fillText("5. 크롬북: 방향키 이동 & 스페이스바 발사 (또는 화면 드래그 & 터치)!", this.virtualWidth / 2, 390);

    const blink = Math.floor(performance.now() / 450) % 2 === 0;
    if (blink) {
      this.ctx.fillStyle = '#4ade80';
      this.ctx.shadowColor = '#4ade80';
      this.ctx.shadowBlur = 12;
      this.ctx.font = "bold 16px 'Press Start 2P', monospace, sans-serif";
      this.ctx.fillText("PRESS SPACE OR TOUCH TO START!", this.virtualWidth / 2, 465);
    }

    this.ctx.restore();
  }

  // 단어 클리어 화면
  renderWordClearScreen() {
    this.ctx.save();
    this.ctx.textAlign = 'center';
    this.ctx.fillStyle = '#38bdf8';
    this.ctx.shadowColor = '#38bdf8';
    this.ctx.shadowBlur = 15;
    this.ctx.font = "bold 26px 'Press Start 2P', monospace, sans-serif";
    this.ctx.fillText("EXCELLENT!", this.virtualWidth / 2, 250);

    this.ctx.fillStyle = '#ffd700';
    this.ctx.shadowColor = '#ffd700';
    this.ctx.font = "bold 34px 'Press Start 2P', monospace, sans-serif";
    this.ctx.fillText(this.currentWord, this.virtualWidth / 2, 305);

    this.ctx.fillStyle = '#ffffff';
    this.ctx.shadowBlur = 0;
    this.ctx.font = "20px 'DungGeunMo', sans-serif";
    this.ctx.fillText(this.currentWordObj ? this.currentWordObj.meaning : '', this.virtualWidth / 2, 350);

    this.ctx.restore();
  }
}

window.SpellingGalagaGame = SpellingGalagaGame;
