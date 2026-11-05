/**
 * 스펠링 갤러그 (Spelling Galaga) - 게임 엔진 및 메인 루프
 * 갤러그 곡선 편대 비행, 듀얼 파이터 구출, 트랙터 빔, 파워업 아이템, 5 목숨 시스템, 크롬북 터치/키보드 완벽 대응
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

    // 게임 상태: 'TITLE' | 'ENTERING' | 'PLAYING' | 'WORD_CLEAR' | 'CHALLENGE' | 'GAME_OVER' | 'VICTORY'
    this.state = 'TITLE';

    // 난이도 모드: 'easy' (힌트 글자+황금광채) | 'normal' (?표시+은은광채) | 'hard' (스펠링 암기 챌린지)
    this.difficulty = 'normal';

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

    // 플레이어 객체
    this.player = {
      x: this.virtualWidth / 2,
      y: this.virtualHeight - 50,
      width: 44,
      height: 40,
      speed: 6.8,
      tilt: 0,
      isInvincible: false,
      invincibleTimer: 0,
      isDual: false,
      hasShield: false,
      captured: false // 트랙터 빔에 납치된 상태
    };

    // 납치된 아군 기체 추적 (트랙터 빔으로 납치되어 보스에 붙어있는 경우)
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
    this.fireCooldown = 160; // ms
    this.touchControl = {
      active: false,
      targetX: this.virtualWidth / 2,
      isFiring: false
    };

    // 편대 비행 및 효과 제어
    this.formationOffset = 0;
    this.formationDirection = 1;
    this.diveTimer = 0;
    this.tractorBeamTimer = 0;
    this.activeTractorBeam = null;
    this.shakeTimer = 0;
    this.warpTimer = 0;
    this.slowMotionTimer = 0;

    // 초기화
    this.initStars();
    this.bindEvents();
    this.resizeCanvas();
    window.addEventListener('resize', () => this.resizeCanvas());

    // 메인 루프 가동
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
    // 키보드 조작
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
    this.spellingSlots = new Array(this.currentWord.length).fill(null);

    this.updateHUD();
    this.spawnEnemiesForWord();
  }

  // 갤러그 편대 곡선 진입 생성
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

        // 좌/우 곡선 진입 진로 생성
        const entrySide = (c % 2 === 0) ? -1 : 1;
        const startXPos = entrySide === -1 ? -40 - c * 20 : this.virtualWidth + 40 + c * 20;

        this.enemies.push({
          type,
          letter,
          formationX: targetGridX,
          formationY: targetGridY,
          x: startXPos,
          y: -40 - r * 30,
          width: type === 'boss' ? 44 : 36,
          height: type === 'boss' ? 40 : 32,
          state: 'ENTERING', // 'ENTERING' | 'FORMATION' | 'DIVING' | 'TRACTOR_BEAM'
          diveSpeed: 2.8 + Math.random() * 1.2,
          diveCurve: (Math.random() - 0.5) * 4,
          shootCooldown: Math.floor(80 + Math.random() * 260),
          entryProgress: 0,
          entryStartX: startXPos,
          entryStartY: -40 - r * 30,
          hasCapturedFighter: false
        });
      }
    }
  }

  // 플레이어 미사일 발사
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
        width: 4,
        height: 14,
        speed: 13
      });
      this.missiles.push({
        x: this.player.x + 14,
        y: this.player.y - 15,
        width: 4,
        height: 14,
        speed: 13
      });
      this.sounds.playDualLaser();
    } else {
      this.missiles.push({
        x: this.player.x,
        y: this.player.y - 15,
        width: 5,
        height: 15,
        speed: 13
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

    // 슬로우 모션 타이머 차감
    const speedMult = this.slowMotionTimer > 0 ? 0.5 : 1.0;
    if (this.slowMotionTimer > 0) {
      this.slowMotionTimer -= dt;
    }

    // 스타필드 스크롤 (워프 시 초고속)
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

    // 파티클 & 텍스트 업데이트
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

    if (this.state !== 'PLAYING' && this.state !== 'CHALLENGE') return;

    // 플레이어 업데이트
    this.updatePlayer(dt);

    // 미사일 업데이트 & 적 피격
    this.updateMissiles(dt);

    // 드롭 파워업 아이템 업데이트
    this.updatePowerups(dt);

    // 적 탄환 업데이트
    this.updateEnemyBullets(dt * speedMult);

    // 외계인 편대 & 급강하 AI
    this.updateEnemies(dt * speedMult);

    // 연속 발사 지원 (스페이스바 유지 시)
    if (this.keys['Space'] || this.keys['Enter']) {
      this.shootMissile();
    }
  }

  // 플레이어 이동 & 롤링 뱅크
  updatePlayer(dt) {
    if (this.player.captured) return;

    let targetTilt = 0;

    // 키보드 조작
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

    // 마우스/터치스크린 추종
    if (this.touchControl.active) {
      const dx = this.touchControl.targetX - this.player.x;
      this.player.x += dx * 0.28;
      targetTilt = Math.max(-0.25, Math.min(0.25, dx * 0.02));
    }

    // 롤링 뱅크 부드러운 보간
    this.player.tilt += (targetTilt - this.player.tilt) * 0.2;

    // 경계 제한
    const halfW = (this.player.isDual ? this.player.width * 1.2 : this.player.width) / 2;
    this.player.x = Math.max(halfW + 10, Math.min(this.virtualWidth - halfW - 10, this.player.x));

    // 무적 시간 차감
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

      // 플레이어 획득 검사
      const dist = Math.hypot(p.x - this.player.x, p.y - this.player.y);
      if (dist < (this.player.width / 2 + 18) && !this.player.captured) {
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
      this.shakeTimer = 22;
      this.addFloatingText('💣 SMART EMP BOMB!', this.virtualWidth / 2, this.virtualHeight / 2, '#f97316', 26);

      // 화면 내 모든 오답 외계인 제거
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
      this.slowMotionTimer = 8.0; // 8초간 50% 감속
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

      // 적과의 충돌 판정
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

  // 외계인 피격 처리 (스펠링 규칙의 핵심 & 버그 원천 차단)
  handleEnemyHit(enemy, index) {
    const expectedLetter = this.currentWord[this.targetLetterIndex];
    const isCorrect = (enemy.letter === expectedLetter);

    this.createExplosion(enemy.x, enemy.y, isCorrect ? '#ffd700' : '#ff1744');

    // 보스에 납치된 아군 기체가 있는 경우 구출!
    if (enemy.hasCapturedFighter) {
      this.sounds.playDualRescue();
      this.player.isDual = true;
      this.addFloatingText('RESCUED! DUAL FIGHTER!', enemy.x, enemy.y - 30, '#00e5ff', 24);
      this.capturedFighterBoss = null;
    }

    // 아이템 드롭 확률 (다이빙 중 처치하거나 콤보 달성 시)
    if (Math.random() < 0.22 || this.combo >= 4) {
      this.spawnRandomPowerup(enemy.x, enemy.y);
    }

    this.enemies.splice(index, 1);

    if (isCorrect) {
      // 🎯 정답 글자 맞힘!
      this.sounds.playCorrect();
      this.sounds.playExplosion();

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

      // 단어 완성 여부 확인
      if (this.targetLetterIndex >= this.currentWord.length) {
        this.handleWordCleared();
      } else {
        // 다음 글자가 편대에 없으면 보장
        this.ensureTargetLetterAvailable();
      }
    } else {
      // ❌ 오답 글자 맞힘! (사용자 요구사항: 잘못 맞히면 5개 목숨 중 1개 차감, 5번 실패 시 종료)
      this.sounds.playWrong();
      this.shakeTimer = 16;
      this.combo = 0;
      this.lives--;

      this.addFloatingText(`MISS! '${enemy.letter}'`, enemy.x, enemy.y - 15, '#ef4444');
      this.addFloatingText(`목숨 -1 (남은 목숨: ${this.lives})`, this.virtualWidth / 2, this.virtualHeight / 2, '#f87171', 20);

      this.updateHUD();

      // 오답 격추로 인해 정답 글자 외계인이 사라졌을 수 있으므로 즉시 보장!
      this.ensureTargetLetterAvailable();

      if (this.lives <= 0) {
        this.handleGameOver();
      }
    }
  }

  // 아이템 생성
  spawnRandomPowerup(x, y) {
    const types = ['heart', 'dual', 'shield', 'bomb', 'slow'];
    const weights = [0.3, 0.25, 0.2, 0.15, 0.1]; // 확률 배정
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
      speed: 1.8
    });
  }

  // 편대 내에 현재 타깃 알파벳이 반드시 1개 이상 존재하도록 보장 (게임 프리징 원천 방어!)
  ensureTargetLetterAvailable() {
    if (this.targetLetterIndex >= this.currentWord.length) return;
    const nextChar = this.currentWord[this.targetLetterIndex];
    const exists = this.enemies.some(e => e.letter === nextChar);

    if (!exists) {
      if (this.enemies.length > 0) {
        const candidate = this.enemies[Math.floor(Math.random() * this.enemies.length)];
        candidate.letter = nextChar;
      } else {
        // 편대가 완전히 전멸했을 경우 새 편대 긴급 재보충
        this.spawnEnemiesForWord();
      }
    }
  }

  // 단어 완성 성공!
  handleWordCleared() {
    this.state = 'WORD_CLEAR';
    this.warpTimer = 2.0; // 하이퍼스페이스 워프 연출
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

  // 5회 실패로 게임 오버
  handleGameOver() {
    this.state = 'GAME_OVER';
    if (this.currentWordObj) {
      this.missedWords.push(this.currentWordObj);
    }
    this.sounds.stopBgm();
    this.sounds.playGameOver();

    // 명예의 전당 저장 시도
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

  // 명예의 전당 (Firebase Firestore) 클라우드 기록 저장
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

      // 1. 로컬 스토리지 랭킹 백업
      let localScores = JSON.parse(localStorage.getItem('spelling_galaga_scores_history') || '[]');
      localScores.push(record);
      localScores.sort((a, b) => b.score - a.score);
      localStorage.setItem('spelling_galaga_scores_history', JSON.stringify(localScores.slice(0, 50)));

      // 2. Firebase Firestore 저장
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

  // 결과 리포트 모달 표시
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

  // 외계인 편대 및 급강하/트랙터 빔 AI 루프
  updateEnemies(dt) {
    this.formationOffset += 0.8 * this.formationDirection;
    if (Math.abs(this.formationOffset) > 25) {
      this.formationDirection *= -1;
    }

    // 급강하 트리거
    this.diveTimer += dt;
    if (this.diveTimer > 1.8 && this.enemies.length > 0) {
      this.diveTimer = 0;
      const formationEnemies = this.enemies.filter(e => e.state === 'FORMATION');
      if (formationEnemies.length > 0) {
        const diver = formationEnemies[Math.floor(Math.random() * formationEnemies.length)];
        diver.state = 'DIVING';
        diver.diveTargetX = this.player.x;

        // 보스 외계인 급강하 시 일정 확률로 트랙터 빔 발사 (아군 미납치 상태일 때)
        if (diver.type === 'boss' && !this.player.isDual && !this.capturedFighterBoss && Math.random() < 0.4) {
          diver.isTractorBoss = true;
        }
      }
    }

    for (let i = this.enemies.length - 1; i >= 0; i--) {
      const e = this.enemies[i];

      if (e.state === 'ENTERING') {
        // 부드러운 곡선 진입
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
          e.shootCooldown = 220 + Math.random() * 280;
          this.shootEnemyBullet(e.x, e.y);
        }
      } else if (e.state === 'DIVING') {
        e.y += e.diveSpeed;
        e.x += Math.sin(e.y * 0.03) * 3 + (e.diveCurve || 0);

        // 보스 트랙터 빔 발동 검사
        if (e.isTractorBoss && e.y > 180 && e.y < 350) {
          this.activeTractorBeam = {
            bossX: e.x,
            bossY: e.y + e.height / 2,
            height: 240
          };
          this.sounds.playTractorBeam();

          // 플레이어가 트랙터 빔 원뿔 영역에 있는지 검사
          const beamTopW = 20;
          const beamBottomW = 90;
          const dy = this.player.y - this.activeTractorBeam.bossY;
          if (dy > 0 && dy < 240) {
            const currentBeamW = beamTopW + (beamBottomW - beamTopW) * (dy / 240);
            if (Math.abs(this.player.x - e.x) < currentBeamW / 2) {
              if (this.player.hasShield) {
                // 쉴드가 있으면 트랙터 빔 튕겨냄
                this.player.hasShield = false;
                this.sounds.playShieldDeflect();
                this.addFloatingText('SHIELD BLOCKED BEAM!', this.player.x, this.player.y - 25, '#38bdf8');
              } else if (!this.player.isInvincible && !this.player.captured) {
                // 빔에 납치됨!
                e.hasCapturedFighter = true;
                this.capturedFighterBoss = e;
                this.handleFighterCaptured();
              }
            }
          }
        } else {
          this.activeTractorBeam = null;
        }

        // 플레이어 방향으로 탄환 1발
        if (Math.abs(e.y - (this.virtualHeight * 0.5)) < 4) {
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

  // 기체 납치 처리
  handleFighterCaptured() {
    this.player.captured = true;
    this.addFloatingText('CAPTURED BY BOSS!', this.player.x, this.player.y - 30, '#ef4444', 22);

    // 1초 후 새 기체 출격
    setTimeout(() => {
      this.player.captured = false;
      this.player.x = this.virtualWidth / 2;
      this.player.isInvincible = true;
      this.player.invincibleTimer = 2.0;
      this.lives--;
      this.updateHUD();
      if (this.lives <= 0) {
        this.handleGameOver();
      }
    }, 1200);
  }

  // 적 탄환 발사
  shootEnemyBullet(x, y) {
    if (this.enemyBullets.length > 7) return;
    this.enemyBullets.push({
      x,
      y: y + 10,
      size: 6,
      speed: 3.2
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

  // 플레이어 피격 처리 (쉴드 -> 듀얼 파이터 -> 일반 목숨 차감)
  handlePlayerDamage() {
    if (this.player.hasShield) {
      this.player.hasShield = false;
      this.sounds.playShieldDeflect();
      this.player.isInvincible = true;
      this.player.invincibleTimer = 1.0;
      this.addFloatingText('SHIELD DEFLECTED!', this.player.x, this.player.y - 25, '#38bdf8', 18);
      return;
    }

    if (this.player.isDual) {
      this.player.isDual = false;
      this.sounds.playExplosion();
      this.createExplosion(this.player.x + 14, this.player.y, '#ffd700');
      this.player.isInvincible = true;
      this.player.invincibleTimer = 2.0;
      this.shakeTimer = 12;
      this.addFloatingText('DUAL LOST! (1 FIGHTER REMAIN)', this.player.x, this.player.y - 25, '#f59e0b', 18);
      return;
    }

    this.lives--;
    this.sounds.playLifeLost();
    this.sounds.playExplosion();
    this.shakeTimer = 18;
    this.player.isInvincible = true;
    this.player.invincibleTimer = 2.0;

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

  // 상단 웹 DOM HUD 업데이트 (난이도 모드별 힌트 분기)
  updateHUD() {
    // 1. 단어 뜻
    const meaningEl = document.getElementById('hudMeaning');
    if (meaningEl && this.currentWordObj) {
      meaningEl.textContent = this.currentWordObj.meaning;
    }

    // 2. 스펠링 슬롯들
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

    // 3. 목숨(하트 5개) 표시
    const livesEl = document.getElementById('hudLives');
    if (livesEl) {
      let hearts = '';
      for (let i = 0; i < this.maxLives; i++) {
        hearts += (i < this.lives) ? '❤️ ' : '🖤 ';
      }
      livesEl.textContent = hearts.trim();
    }

    // 4. 점수 & 최고 점수 & 스테이지
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
      const shakeMag = (this.shakeTimer / 16) * 8;
      const ox = (Math.random() - 0.5) * shakeMag;
      const oy = (Math.random() - 0.5) * shakeMag;
      this.ctx.translate(ox, oy);
    }

    // 깊은 우주 배경
    this.ctx.fillStyle = '#050814';
    this.ctx.fillRect(0, 0, this.virtualWidth, this.virtualHeight);

    // 스타필드 (워프 시 속도선 스트레치)
    const isWarp = this.warpTimer > 0;
    this.stars.forEach(s => {
      this.ctx.fillStyle = s.color;
      if (isWarp) {
        this.ctx.fillRect(s.x, s.y, s.size, s.size * 12);
      } else {
        this.ctx.fillRect(s.x, s.y, s.size, s.size);
      }
    });

    // 트랙터 빔 렌더링
    if (this.activeTractorBeam) {
      this.sprites.drawTractorBeam(
        this.ctx,
        this.activeTractorBeam.bossX,
        this.activeTractorBeam.bossY,
        this.activeTractorBeam.height
      );
    }

    // 파워업 아이템 렌더링
    this.powerups.forEach(p => {
      this.sprites.drawPowerup(this.ctx, p.x, p.y, p.type);
    });

    // 적 탄환
    this.enemyBullets.forEach(b => {
      this.sprites.drawEnemyBullet(this.ctx, b.x, b.y, b.size);
    });

    // 외계인 편대
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

    // 플레이어 미사일
    this.missiles.forEach(m => {
      this.sprites.drawMissile(this.ctx, m.x, m.y, m.width, m.height);
    });

    // 플레이어 전투기
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

    // 폭발 파티클
    this.particles.forEach(p => {
      this.ctx.fillStyle = p.color;
      this.ctx.fillRect(p.x, p.y, p.size, p.size);
    });

    // 플로팅 텍스트
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

    // 오버레이 화면들
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

    // 가이드 텍스트
    this.ctx.fillStyle = '#ffffff';
    this.ctx.shadowBlur = 0;
    this.ctx.font = "14px 'DungGeunMo', sans-serif";
    this.ctx.fillText("1. 화면 상단의 [단어 뜻]을 보고 올바른 스펠링을 확인하세요.", this.virtualWidth / 2, 270);
    this.ctx.fillText("2. 적 외계인이 품은 알파벳을 철자 순서대로 정확하게 격추하세요!", this.virtualWidth / 2, 300);
    this.ctx.fillText("3. 엉뚱한 글자를 쏘면 목숨이 깎입니다 (총 5개 목숨 시스템).", this.virtualWidth / 2, 330);
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
