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
            tripleJumpPower: -11.0, // 3단 점프 (트리플 점프!)
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
        this.totalDistance = 0;
        this.coins = 0;
        this.quizzesSolved = 0;
        this.wordsCollected = 0; // 수집한 영어 단어 버블 수
        this.itemsAcquired = 0; // 획득한 아이템 수
        this.survivalTime = 0; // 초
        this.chances = 3; // 기본 찬스 3회 (❤️ ❤️ ❤️)

        // 코인 상점 연동 (장착 스킨, 배경 테마, 시간대 및 특수 기믹)
        this.equippedSkin = 'mario';
        this.equippedBg = 'cycle';
        this.timeOfDay = 0.0; // 0.0 (낮) ~ 0.35 (노을) ~ 0.7 (밤) ~ 0.95 (새벽)
        this.fireballs = []; // 파이어 마리오 투사체
        this.fireballTimer = 0;
        this.bulletWarnings = []; // 킬러 대포알 경고선
        this.clouds = [
            { x: 80, y: 50, speed: 0.35, size: 45 },
            { x: 320, y: 75, speed: 0.22, size: 60 },
            { x: 620, y: 40, speed: 0.32, size: 42 },
            { x: 860, y: 65, speed: 0.25, size: 52 }
        ];

        // 활성 아이템 상태 버프 (에너지 쉴드, 코인 자석, 스타 대시)
        this.activeItems = {
            shield: false,
            magnetTimer: 0,
            starDashTimer: 0
        };

        // 장애물 및 아이템 목록
        this.obstacles = [];
        this.collectibles = [];
        this.itemBoxes = []; // 럭키 아이템 상자 [?]
        this.wordOrbs = []; // 공중에 떠다니는 영어 단어 버블
        this.floatingTexts = []; // 플로팅 점수 및 알림 텍스트
        this.particles = [];
        this.backgroundStars = [];

        this.itemBoxSpawnTimer = 200; // 약 3.5초 후 첫 스폰
        this.wordOrbSpawnTimer = 110; // 약 2초 후 첫 스폰

        // 배경 패럴랙스 교육용 영어 단어 & 문장 플로팅 시스템
        this.backgroundTexts = [];
        this.bgLanes = [65, 125, 185, 245, 305]; // 5개 수직 레인 (겹침 방지)
        this.bgTextSpawnTimer = 30;
        this.selectedGrade = 'all';

        // 큐레이션된 초등 3~6학년 핵심 영어 단어 및 실생활 대화 문장 데이터셋
        this.curatedBgItems = [
            // [3학년] 핵심 단어 & 문장
            { en: 'jump', kr: '뛰다, 점프하다', type: 'word', grade: 3 },
            { en: 'run', kr: '달리다', type: 'word', grade: 3 },
            { en: 'walk', kr: '걷다', type: 'word', grade: 3 },
            { en: 'pencil', kr: '연필', type: 'word', grade: 3 },
            { en: 'eraser', kr: '지우개', type: 'word', grade: 3 },
            { en: 'book', kr: '책', type: 'word', grade: 3 },
            { en: 'apple', kr: '사과', type: 'word', grade: 3 },
            { en: 'happy', kr: '행복한', type: 'word', grade: 3 },
            { en: 'friend', kr: '친구', type: 'word', grade: 3 },
            { en: 'star', kr: '별', type: 'word', grade: 3 },
            { en: 'heart', kr: '마음, 하트', type: 'word', grade: 3 },
            { en: 'bird', kr: '새', type: 'word', grade: 3 },
            { en: 'Do you have a pencil?', kr: '연필 있니?', type: 'sentence', grade: 3 },
            { en: 'Here you are.', kr: '여기 있어.', type: 'sentence', grade: 3 },
            { en: 'I like apples.', kr: '나는 사과를 좋아해.', type: 'sentence', grade: 3 },
            { en: 'How are you today?', kr: '오늘 기분 어때?', type: 'sentence', grade: 3 },
            { en: 'Nice to meet you!', kr: '만나서 반가워!', type: 'sentence', grade: 3 },
            { en: 'Open your book.', kr: '책을 펴세요.', type: 'sentence', grade: 3 },
            { en: 'Look at the star!', kr: '저 별을 봐!', type: 'sentence', grade: 3 },
            { en: 'Thank you very much.', kr: '정말 고마워.', type: 'sentence', grade: 3 },

            // [4학년] 핵심 단어 & 문장
            { en: 'school', kr: '학교', type: 'word', grade: 4 },
            { en: 'clock', kr: '시계', type: 'word', grade: 4 },
            { en: 'music', kr: '음악', type: 'word', grade: 4 },
            { en: 'summer', kr: '여름', type: 'word', grade: 4 },
            { en: 'winter', kr: '겨울', type: 'word', grade: 4 },
            { en: 'breakfast', kr: '아침식사', type: 'word', grade: 4 },
            { en: 'lunch', kr: '점심식사', type: 'word', grade: 4 },
            { en: 'clean', kr: '깨끗한, 청소하다', type: 'word', grade: 4 },
            { en: 'weather', kr: '날씨', type: 'word', grade: 4 },
            { en: 'fast', kr: '빠른, 빨리', type: 'word', grade: 4 },
            { en: 'high', kr: '높은, 높이', type: 'word', grade: 4 },
            { en: 'energy', kr: '에너지, 힘', type: 'word', grade: 4 },
            { en: 'What time is it?', kr: '지금 몇 시니?', type: 'sentence', grade: 4 },
            { en: 'It is time for lunch.', kr: '점심 먹을 시간이야.', type: 'sentence', grade: 4 },
            { en: 'How is the weather?', kr: '날씨가 어때?', type: 'sentence', grade: 4 },
            { en: 'It is sunny today.', kr: '오늘은 맑아.', type: 'sentence', grade: 4 },
            { en: 'Can you swim fast?', kr: '너 빨리 수영할 수 있니?', type: 'sentence', grade: 4 },
            { en: 'Let us play together!', kr: '우리 함께 놀자!', type: 'sentence', grade: 4 },
            { en: 'I can jump high.', kr: '난 높이 점프할 수 있어.', type: 'sentence', grade: 4 },
            { en: 'What are you doing?', kr: '너 지금 뭐 하고 있니?', type: 'sentence', grade: 4 },

            // [5학년] 핵심 단어 & 문장
            { en: 'library', kr: '도서관', type: 'word', grade: 5 },
            { en: 'hospital', kr: '병원', type: 'word', grade: 5 },
            { en: 'museum', kr: '박물관', type: 'word', grade: 5 },
            { en: 'science', kr: '과학', type: 'word', grade: 5 },
            { en: 'subject', kr: '과목', type: 'word', grade: 5 },
            { en: 'favorite', kr: '가장 좋아하는', type: 'word', grade: 5 },
            { en: 'yesterday', kr: '어제', type: 'word', grade: 5 },
            { en: 'tomorrow', kr: '내일', type: 'word', grade: 5 },
            { en: 'shield', kr: '방패, 보호막', type: 'word', grade: 5 },
            { en: 'danger', kr: '위험', type: 'word', grade: 5 },
            { en: 'escape', kr: '탈출하다', type: 'word', grade: 5 },
            { en: 'speed', kr: '속도', type: 'word', grade: 5 },
            { en: 'Where is the library?', kr: '도서관이 어디에 있니?', type: 'sentence', grade: 5 },
            { en: 'Go straight and turn right.', kr: '직진해서 오른쪽으로 돌아.', type: 'sentence', grade: 5 },
            { en: 'What is your favorite subject?', kr: '가장 좋아하는 과목이 뭐니?', type: 'sentence', grade: 5 },
            { en: 'My favorite subject is English.', kr: '내가 제일 좋아하는 과목은 영어야.', type: 'sentence', grade: 5 },
            { en: 'What did you do yesterday?', kr: '어제 뭐 했니?', type: 'sentence', grade: 5 },
            { en: 'I played soccer with friends.', kr: '친구들과 축구를 했어.', type: 'sentence', grade: 5 },
            { en: 'May I help you?', kr: '제가 도와드릴까요?', type: 'sentence', grade: 5 },
            { en: 'You did a great job!', kr: '정말 잘했어!', type: 'sentence', grade: 5 },

            // [6학년] 핵심 단어 & 문장
            { en: 'future', kr: '미래', type: 'word', grade: 6 },
            { en: 'adventure', kr: '모험', type: 'word', grade: 6 },
            { en: 'discover', kr: '발견하다', type: 'word', grade: 6 },
            { en: 'invent', kr: '발명하다', type: 'word', grade: 6 },
            { en: 'protect', kr: '보호하다', type: 'word', grade: 6 },
            { en: 'survive', kr: '살아남다', type: 'word', grade: 6 },
            { en: 'environment', kr: '환경', type: 'word', grade: 6 },
            { en: 'champion', kr: '챔피언, 우승자', type: 'word', grade: 6 },
            { en: 'challenge', kr: '도전', type: 'word', grade: 6 },
            { en: 'obstacle', kr: '장애물', type: 'word', grade: 6 },
            { en: 'fantastic', kr: '환상적인', type: 'word', grade: 6 },
            { en: 'wonderful', kr: '멋진, 훌륭한', type: 'word', grade: 6 },
            { en: 'What will you do this weekend?', kr: '이번 주말에 뭐 할 예정이니?', type: 'sentence', grade: 6 },
            { en: 'I will visit the science museum.', kr: '과학 박물관에 방문할 거야.', type: 'sentence', grade: 6 },
            { en: 'We should protect our Earth.', kr: '우리는 지구를 보호해야 해.', type: 'sentence', grade: 6 },
            { en: 'Never give up on your dreams!', kr: '너의 꿈을 절대 포기하지 마!', type: 'sentence', grade: 6 },
            { en: 'You can do anything!', kr: '너는 무엇이든 해낼 수 있어!', type: 'sentence', grade: 6 },
            { en: 'Practice makes perfect.', kr: '연습이 완벽을 만든다.', type: 'sentence', grade: 6 },
            { en: 'Keep running forward!', kr: '계속 앞으로 힘차게 달려가자!', type: 'sentence', grade: 6 },
            { en: 'Have a wonderful day!', kr: '행복하고 멋진 하루 보내!', type: 'sentence', grade: 6 }
        ];

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
        this.render(); // 초기 화면 캔버스 렌더링
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
        this.initBackgroundTexts();
    }

    // 배경 영어 단어 & 문장 초기 배치
    initBackgroundTexts() {
        this.backgroundTexts = [];
        this.bgTextSpawnTimer = 40;

        // 게임 시작 직후에도 화면 전체에 5개의 단어/문장이 떠 있도록 균일하게 배치
        const initialXPositions = [120, 310, 500, 690, 880];
        initialXPositions.forEach(x => {
            this.spawnBackgroundText(x);
        });
    }

    setGrade(grade) {
        this.selectedGrade = grade;
    }

    // 학년 조건 및 공통 교육과정 모듈(WaurimalCurriculum) 연동 단어/문장 풀 수합
    getBackgroundItemPool() {
        let pool = [];
        const gradeFilter = this.selectedGrade;

        // 1. WaurimalCurriculum 공통 모듈에서 동적 데이터 수합
        if (typeof window !== 'undefined' && window.WaurimalCurriculum) {
            try {
                if (!gradeFilter || gradeFilter === 'all') {
                    const cWords = window.WaurimalCurriculum.getAllWords() || [];
                    cWords.forEach(w => {
                        if (w && w.en) pool.push({ en: w.en, kr: w.kr || '', type: 'word', grade: w.grade || 3 });
                    });
                    const cSentences = window.WaurimalCurriculum.getAllSentences() || [];
                    cSentences.forEach(s => {
                        if (s && s.en) pool.push({ en: s.en, kr: s.kr || '', type: 'sentence', grade: s.grade || 3 });
                    });
                } else {
                    const targetG = Number(gradeFilter);
                    const lessons = window.WaurimalCurriculum.getLessonsByGrade(targetG) || [];
                    lessons.forEach(l => {
                        if (l && Array.isArray(l.words)) {
                            l.words.forEach(w => {
                                if (w && w.en) pool.push({ en: w.en, kr: w.kr || '', type: 'word', grade: l.grade });
                            });
                        }
                        if (l && Array.isArray(l.sentences)) {
                            l.sentences.forEach(s => {
                                const en = typeof s === 'string' ? s : (s.en || '');
                                const kr = typeof s === 'string' ? '' : (s.kr || '');
                                if (en) pool.push({ en, kr, type: 'sentence', grade: l.grade });
                            });
                        }
                    });
                }
            } catch (e) {
                console.warn('Curriculum pool error:', e);
            }
        }

        // 2. 큐레이션된 기본 데이터셋과 병합
        const curated = (gradeFilter && gradeFilter !== 'all')
            ? this.curatedBgItems.filter(item => Number(item.grade) === Number(gradeFilter))
            : this.curatedBgItems;

        const combined = pool.concat(curated);
        return combined.length > 0 ? combined : this.curatedBgItems;
    }

    // 배경 영어 텍스트 신규 생성 (지정 X 또는 우측 화면 밖 스폰)
    spawnBackgroundText(xPos = null) {
        const pool = this.getBackgroundItemPool();
        if (!pool || pool.length === 0) return;

        const spawnX = (xPos !== null) ? xPos : (this.WIDTH + 50);

        // 겹침 방지: 해당 레인에 spawnX와 230px 이내로 근접한 기존 텍스트가 없는 레인만 필터링
        const availableLanes = this.bgLanes.filter(laneY => {
            return !this.backgroundTexts.some(item => {
                return item.baseY === laneY && Math.abs(item.x - spawnX) < 230;
            });
        });

        if (availableLanes.length === 0) return; // 모든 레인이 차 있으면 대기

        const laneY = availableLanes[Math.floor(Math.random() * availableLanes.length)];
        const item = pool[Math.floor(Math.random() * pool.length)];

        // 선명하고 아름다운 6종 네온/파스텔 컬러 테마
        const colorThemes = [
            { enColor: '#38BDF8', borderColor: 'rgba(56, 189, 248, 0.45)', glowColor: 'rgba(56, 189, 248, 0.35)' }, // Sky Cyan
            { enColor: '#34D399', borderColor: 'rgba(52, 211, 153, 0.45)', glowColor: 'rgba(52, 211, 153, 0.35)' }, // Emerald Mint
            { enColor: '#FBBF24', borderColor: 'rgba(251, 191, 36, 0.45)', glowColor: 'rgba(251, 191, 36, 0.35)' }, // Warm Amber
            { enColor: '#F472B6', borderColor: 'rgba(244, 114, 182, 0.45)', glowColor: 'rgba(244, 114, 182, 0.35)' }, // Rose Pink
            { enColor: '#C084FC', borderColor: 'rgba(192, 132, 252, 0.45)', glowColor: 'rgba(192, 132, 252, 0.35)' }, // Lavender Violet
            { enColor: '#60A5FA', borderColor: 'rgba(96, 165, 250, 0.45)', glowColor: 'rgba(96, 165, 250, 0.35)' }   // Cool Blue
        ];
        const theme = colorThemes[Math.floor(Math.random() * colorThemes.length)];

        // 패럴랙스 계수 (지면 달리기 속도보다 은은하게 흘러가는 원경 효과: 0.30 ~ 0.50)
        const parallaxFactor = 0.30 + Math.random() * 0.20;

        this.backgroundTexts.push({
            en: item.en,
            kr: item.kr || '',
            type: item.type || (item.en.includes(' ') && item.en.length > 14 ? 'sentence' : 'word'),
            grade: item.grade || 3,
            x: spawnX,
            baseY: laneY,
            parallaxFactor: parallaxFactor,
            floatPhase: Math.random() * Math.PI * 2,
            floatSpeed: 0.03 + Math.random() * 0.02,
            floatAmp: 3.5 + Math.random() * 2.5,
            colorTheme: theme
        });
    }

    // 배경 영어 텍스트 위치 갱신 및 주기적 스폰
    updateBackgroundTexts(dt) {
        // 스폰 타이머
        this.bgTextSpawnTimer--;
        if (this.bgTextSpawnTimer <= 0) {
            if (this.backgroundTexts.length < 8) {
                this.spawnBackgroundText();
            }
            this.bgTextSpawnTimer = Math.floor(Math.random() * 50) + 60; // 약 1.0 ~ 1.8초 주기
        }

        // 텍스트 이동 및 상하 부유 애니메이션
        for (let i = this.backgroundTexts.length - 1; i >= 0; i--) {
            const item = this.backgroundTexts[i];

            // 게임 진행 중일 때는 게임 속도와 연동된 패럴랙스 이동, 대기 중일 때는 부드러운 0.6px 유영
            const speed = (this.isRunning && !this.isPaused)
                ? (this.gameSpeed * item.parallaxFactor)
                : 0.6;

            item.x -= speed;
            item.floatPhase += item.floatSpeed;

            // 화면 왼쪽 밖으로 충분히 벗어나면 제거
            if (item.x < -500) {
                this.backgroundTexts.splice(i, 1);
            }
        }
    }

    // 배경 영어 단어 & 문장 패럴랙스 렌더링
    renderBackgroundTexts(ctx) {
        if (!this.backgroundTexts || this.backgroundTexts.length === 0) return;

        ctx.save();

        for (let i = 0; i < this.backgroundTexts.length; i++) {
            const item = this.backgroundTexts[i];

            // 화면 좌우 가장자리 페이드 인/아웃 알파 처리
            let edgeAlpha = 1.0;
            if (item.x > this.WIDTH - 100) {
                edgeAlpha = Math.max(0, (this.WIDTH - item.x) / 100 + 1);
            } else if (item.x < 100) {
                edgeAlpha = Math.max(0, (item.x + 300) / 400);
            }
            edgeAlpha = Math.min(1.0, Math.max(0, edgeAlpha));
            if (edgeAlpha <= 0.02) continue;

            // 사인파를 이용한 부드러운 둥실둥실 부유 좌표
            const y = item.baseY + Math.sin(item.floatPhase) * item.floatAmp;

            const isSentence = item.type === 'sentence';
            const enFont = isSentence
                ? 'bold 15px "Pretendard Variable", Pretendard, sans-serif'
                : 'bold 17px "Pretendard Variable", Pretendard, sans-serif';
            const krFont = '12px "Pretendard Variable", Pretendard, sans-serif';

            ctx.font = enFont;
            const enWidth = ctx.measureText(item.en).width;

            ctx.font = krFont;
            const krWidth = item.kr ? ctx.measureText(item.kr).width : 0;

            const spacing = item.kr ? 10 : 0;
            const totalContentWidth = enWidth + spacing + krWidth;
            const paddingX = 14;
            const boxHeight = isSentence ? 32 : 34;
            const boxWidth = totalContentWidth + paddingX * 2;
            const boxX = item.x;
            const boxY = y - boxHeight / 2;

            // 1. 반투명 글래스모피즘 캡슐(Pill) 배경 박스
            ctx.globalAlpha = 0.82 * edgeAlpha;
            ctx.fillStyle = 'rgba(15, 23, 42, 0.72)';
            ctx.strokeStyle = item.colorTheme.borderColor;
            ctx.lineWidth = 1.2;

            this.drawRoundedRect(ctx, boxX, boxY, boxWidth, boxHeight, boxHeight / 2);
            ctx.fill();
            ctx.stroke();

            // 2. 영어 단어/문장 텍스트 (네온 컬러 + 은은한 글로우)
            ctx.font = enFont;
            ctx.textAlign = 'left';
            ctx.textBaseline = 'middle';
            ctx.fillStyle = item.colorTheme.enColor;
            ctx.shadowColor = item.colorTheme.glowColor;
            ctx.shadowBlur = 6;
            ctx.fillText(item.en, boxX + paddingX, y);

            // 3. 한글 뜻 (부드러운 화이트/슬레이트)
            if (item.kr) {
                ctx.shadowBlur = 0;
                ctx.font = krFont;
                ctx.fillStyle = 'rgba(226, 232, 240, 0.85)';
                ctx.fillText(item.kr, boxX + paddingX + enWidth + spacing, y);
            }
        }

        ctx.restore();
    }

    // 둥근 사각형 캔버스 그리기 헬퍼 함수
    drawRoundedRect(ctx, x, y, width, height, radius) {
        if (typeof ctx.roundRect === 'function') {
            ctx.beginPath();
            ctx.roundRect(x, y, width, height, radius);
            return;
        }
        ctx.beginPath();
        ctx.moveTo(x + radius, y);
        ctx.lineTo(x + width - radius, y);
        ctx.arcTo(x + width, y, x + width, y + height, radius);
        ctx.lineTo(x + width, y + height - radius);
        ctx.arcTo(x + width, y + height, x, y + height, radius);
        ctx.lineTo(x + radius, y + height);
        ctx.arcTo(x, y + height, x, y, radius);
        ctx.lineTo(x, y + radius);
        ctx.arcTo(x, y, x + width, y, radius);
        ctx.closePath();
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
        this.totalDistance = 0;
        this.coins = 0;
        this.quizzesSolved = 0;
        this.wordsCollected = 0;
        this.itemsAcquired = 0;
        this.survivalTime = 0;
        this.chances = 3;
        this.gameSpeed = 5.0 + (mode === 'stage' ? (stage - 1) * 0.4 : 0);

        // 상점 데이터 및 시작 부스트 장착 적용
        let extraChances = 0;
        if (typeof window !== 'undefined' && window.JumpmanShop) {
            this.equippedSkin = window.JumpmanShop.getEquippedSkin() || 'mario';
            this.equippedBg = window.JumpmanShop.getEquippedBg() || 'cycle';
            const boosts = window.JumpmanShop.consumeActiveBoosts();
            this.activeItems = {
                shield: !!boosts.shield,
                magnetTimer: boosts.magnetSeconds ? boosts.magnetSeconds * 60 : 0,
                starDashTimer: boosts.starSeconds ? boosts.starSeconds * 60 : 0
            };
            if (boosts.extraChances) {
                extraChances = boosts.extraChances;
            }
        } else {
            this.equippedSkin = 'mario';
            this.equippedBg = 'cycle';
            this.activeItems = {
                shield: false,
                magnetTimer: 0,
                starDashTimer: 0
            };
        }
        this.chances = 3 + extraChances;

        // 장착된 캐릭터에 따른 고유 능력치 적용
        if (this.equippedSkin === 'luigi') {
            this.player.speed = 5.5;
            this.player.jumpPower = -13.7; // 하이 점프
            this.player.gravity = 0.54; // 체공시간 증가
        } else if (this.equippedSkin === 'wario') {
            this.player.speed = 5.4;
            this.player.jumpPower = -12.5;
            this.player.gravity = 0.60;
        } else if (this.equippedSkin === 'toad') {
            this.player.speed = 6.3; // 쾌속 질주
            this.player.jumpPower = -12.7;
            this.player.gravity = 0.58;
        } else if (this.equippedSkin === 'peach') {
            this.player.speed = 5.5;
            this.player.jumpPower = -12.8;
            this.player.gravity = 0.46; // 우아한 활공
        } else if (this.equippedSkin === 'fire_mario') {
            this.player.speed = 5.6;
            this.player.jumpPower = -12.8;
            this.player.gravity = 0.58;
        } else {
            this.player.speed = 5.5;
            this.player.jumpPower = -12.8;
            this.player.gravity = 0.58;
        }

        this.fireballs = [];
        this.fireballTimer = 180;
        this.bulletWarnings = [];
        this.timeOfDay = 0.0;

        this.player.x = 160;
        this.player.y = this.FLOOR_Y - this.player.height;
        this.player.vx = 0;
        this.player.vy = 0;
        this.player.onGround = true;
        this.player.jumpCount = 0;
        this.player.invulnerableTimer = 60; // 게임 시작 직후 1초간 안전 보호

        this.obstacles = [];
        this.collectibles = [];
        this.itemBoxes = [];
        this.wordOrbs = [];
        this.floatingTexts = [];
        this.particles = [];
        this.spawnTimer = 40;
        this.coinSpawnTimer = 30;
        this.itemBoxSpawnTimer = 220;
        this.wordOrbSpawnTimer = 110;
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

    // 다음 스테이지로 진입 (점수, 코인, 총 거리, 퀴즈 풀이 등 누적 합산 유지)
    startNextStage() {
        this.currentStage++;
        this.stageGoalDistance = 400 + this.currentStage * 200;
        this.distance = 0; // 해당 스테이지 내 진행 거리 초기화
        this.gameSpeed = 5.0 + (this.currentStage - 1) * 0.4;
        this.chances = 3; // 다음 스테이지를 위해 찬스 3회 충전!

        this.activeItems = {
            shield: false,
            magnetTimer: 0,
            starDashTimer: 0
        };

        this.isRunning = true;
        this.isPaused = false;

        // 점수(this.score), 총 누적거리(this.totalDistance), 코인(this.coins), 퀴즈풀이(this.quizzesSolved), 생존시간(this.survivalTime), 단어수집(this.wordsCollected)은 누적 유지!

        this.player.x = 160;
        this.player.y = this.FLOOR_Y - this.player.height;
        this.player.vx = 0;
        this.player.vy = 0;
        this.player.onGround = true;
        this.player.jumpCount = 0;
        this.player.invulnerableTimer = 75; // 스테이지 진입 직후 1.25초간 안전 보호막

        this.obstacles = [];
        this.collectibles = [];
        this.itemBoxes = [];
        this.wordOrbs = [];
        this.floatingTexts = [];
        this.particles = [];
        this.spawnTimer = 40;
        this.coinSpawnTimer = 30;
        this.itemBoxSpawnTimer = 200;
        this.wordOrbSpawnTimer = 110;
        this.screenShake = 0;

        this.updateHUD();
        if (window.JumpmanAudio) {
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
        } else if (this.player.jumpCount === 2) {
            // 3단 점프 (트리플 점프! 화려한 파티클 & 버스트)
            this.player.vy = this.player.tripleJumpPower || -11.0;
            this.player.jumpCount = 3;
            this.player.scaleY = 1.45;
            this.player.scaleX = 0.75;
            this.createTripleJumpBurst(this.player.x + this.player.width / 2, this.player.y + this.player.height);
            this.addFloatingText('3단 점프! ✨', this.player.x + this.player.width / 2, this.player.y - 12, '#38BDF8');
            if (window.JumpmanAudio && window.JumpmanAudio.playTripleJump) {
                window.JumpmanAudio.playTripleJump();
            }
        }
    }

    createTripleJumpBurst(x, y) {
        const colors = ['#38BDF8', '#818CF8', '#C084FC', '#F472B6', '#FBBF24'];
        for (let i = 0; i < 14; i++) {
            const angle = (Math.PI * 2 * i) / 14 + (Math.random() - 0.5) * 0.4;
            const spd = Math.random() * 3.5 + 2.0;
            this.particles.push({
                x: x,
                y: y,
                vx: Math.cos(angle) * spd,
                vy: Math.sin(angle) * spd + 1,
                radius: Math.random() * 3 + 2,
                color: colors[i % colors.length],
                alpha: 1,
                decay: 0.045
            });
        }
    }

    addFloatingText(text, x, y, color = '#FBBF24') {
        this.floatingTexts.push({
            text: text,
            x: x,
            y: y,
            color: color,
            alpha: 1.0,
            vy: -1.2,
            life: 60
        });
    }

    spawnWordOrb() {
        const pool = this.getBackgroundItemPool().filter(i => i.type === 'word');
        const item = pool.length > 0 ? pool[Math.floor(Math.random() * pool.length)] : { en: 'JUMP', kr: '점프하다' };
        const y = this.FLOOR_Y - 55 - Math.random() * 90;
        this.wordOrbs.push({
            en: item.en,
            kr: item.kr || '',
            x: this.WIDTH + 40,
            y: y,
            baseY: y,
            width: 75,
            height: 28,
            collected: false,
            floatPhase: Math.random() * Math.PI * 2
        });
    }

    spawnItemBox() {
        const y = this.FLOOR_Y - 60 - Math.random() * 80;
        this.itemBoxes.push({
            x: this.WIDTH + 35,
            y: y,
            baseY: y,
            size: 28,
            floatPhase: Math.random() * Math.PI * 2,
            collected: false
        });
    }

    handleItemQuizResult(res) {
        this.isPaused = false;
        if (!res || !res.success) return;

        this.itemsAcquired++;
        this.quizzesSolved++;

        const wonItem = res.item;
        if (wonItem === 'shield') {
            this.activeItems.shield = true;
            this.addFloatingText('🛡️ 에너지 쉴드 장착!', this.player.x + 20, this.player.y - 15, '#38BDF8');
        } else if (wonItem === 'magnet') {
            this.activeItems.magnetTimer = 12 * 60; // 12초
            this.addFloatingText('🧲 코인 자석 발동! (12s)', this.player.x + 20, this.player.y - 15, '#F59E0B');
        } else if (wonItem === 'starDash') {
            this.activeItems.starDashTimer = 6 * 60; // 6초
            this.addFloatingText('🚀 스타 대시! 무적 돌진 (6s)', this.player.x + 20, this.player.y - 15, '#F43F5E');
        } else if (wonItem === 'heart') {
            this.chances = Math.min(3, this.chances + 1);
            this.addFloatingText('💖 부활 찬스 +1 회복!', this.player.x + 20, this.player.y - 15, '#EC4899');
        } else if (wonItem === 'gold') {
            this.score += 500;
            this.coins += 10;
            this.addFloatingText('🪙 황금 보따리! +500점 +10코인', this.player.x + 20, this.player.y - 15, '#FBBF24');
        }

        this.updateHUD();
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
        const types = [
            'rolling_ball', 'spikes', 'goomba', 'bouncing_ball',
            'koopa', 'flying_bat', 'piranha_plant', 'falling_rock',
            'fire_bar', 'boo', 'bullet_bill'
        ];
        // 난이도 및 진행 거리에 따른 타입 선택
        let availableTypes = ['rolling_ball', 'spikes', 'goomba'];
        if (this.distance > 60 || this.currentStage >= 2) availableTypes.push('bouncing_ball', 'koopa');
        if (this.distance > 130 || this.currentStage >= 2) availableTypes.push('flying_bat', 'piranha_plant');
        if (this.distance > 210 || this.currentStage >= 3) availableTypes.push('falling_rock', 'fire_bar');
        if (this.distance > 300 || this.currentStage >= 3) availableTypes.push('boo', 'bullet_bill');

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
        } else if (chosenType === 'goomba') {
            // 🍄 굼바 (버섯 괴물: 위에서 밟기(Stomp) 가능)
            this.obstacles.push({
                type: 'goomba',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - 34,
                width: 34,
                height: 34,
                walkPhase: 0,
                squished: false,
                squishTimer: 0,
                speedMultiplier: 0.95
            });
        } else if (chosenType === 'koopa') {
            // 🐢 엉금엉금 (고속 회전 등껍질)
            this.obstacles.push({
                type: 'koopa',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - 26,
                width: 32,
                height: 26,
                rotation: 0,
                speedMultiplier: 1.35
            });
        } else if (chosenType === 'piranha_plant') {
            // 🌿 뻐끔플라워 & 토관
            this.obstacles.push({
                type: 'piranha_plant',
                x: this.WIDTH + 30,
                y: this.FLOOR_Y - 58,
                width: 44,
                height: 58,
                risePhase: 0,
                speedMultiplier: 1.0
            });
        } else if (chosenType === 'fire_bar') {
            // 🔥 회전 파이어바
            this.obstacles.push({
                type: 'fire_bar',
                x: this.WIDTH + 40,
                y: this.FLOOR_Y - 45,
                width: 36,
                height: 36,
                centerX: this.WIDTH + 58,
                centerY: this.FLOOR_Y - 27,
                angle: 0,
                barLength: 52,
                speedMultiplier: 1.0
            });
        } else if (chosenType === 'boo') {
            // 👻 부끄부끄 유령
            const yPositions = [this.FLOOR_Y - 70, this.FLOOR_Y - 110];
            const chosenY = yPositions[Math.floor(Math.random() * yPositions.length)];
            this.obstacles.push({
                type: 'boo',
                x: this.WIDTH + 30,
                y: chosenY,
                baseY: chosenY,
                width: 36,
                height: 34,
                wavePhase: 0,
                speedMultiplier: 1.15
            });
        } else if (chosenType === 'bullet_bill') {
            // 🚀 킬러 대포알 경고 생성
            const warningY = Math.max(120, Math.min(this.FLOOR_Y - 35, this.player.y + 6));
            this.bulletWarnings.push({
                x: this.WIDTH - 60,
                y: warningY,
                life: 38
            });
            if (window.JumpmanAudio && window.JumpmanAudio.playWarning) {
                window.JumpmanAudio.playWarning();
            }
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

        // 생존 시간 및 거리 누적 (스테이지 및 전체 누적 합산)
        this.survivalTime += dt;
        const distStep = Math.round(this.gameSpeed * 0.15);
        this.distance += distStep;
        this.totalDistance = (this.totalDistance || 0) + distStep;
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

        // 활성 아이템 타이머 감소
        if (this.activeItems.magnetTimer > 0) this.activeItems.magnetTimer--;
        if (this.activeItems.starDashTimer > 0) {
            this.activeItems.starDashTimer--;
            if (Math.random() < 0.35) {
                this.particles.push({
                    x: this.player.x + Math.random() * this.player.width,
                    y: this.player.y + Math.random() * this.player.height,
                    vx: -this.gameSpeed * 0.8 - Math.random() * 2,
                    vy: (Math.random() - 0.5) * 2,
                    radius: Math.random() * 4 + 2,
                    color: Math.random() < 0.5 ? '#FBBF24' : '#F43F5E',
                    alpha: 1,
                    decay: 0.05
                });
            }
        }

        // 단어 버블 생성 타이머
        this.wordOrbSpawnTimer--;
        if (this.wordOrbSpawnTimer <= 0) {
            if (this.wordOrbs.length < 3) {
                this.spawnWordOrb();
            }
            this.wordOrbSpawnTimer = Math.floor(Math.random() * 100) + 120; // 2 ~ 3.5초 주기
        }

        // 아이템 퀴즈 상자 생성 타이머
        this.itemBoxSpawnTimer--;
        if (this.itemBoxSpawnTimer <= 0) {
            if (this.itemBoxes.length < 2) {
                this.spawnItemBox();
            }
            this.itemBoxSpawnTimer = Math.floor(Math.random() * 150) + 240; // 4 ~ 6.5초 주기
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

        // 영어 단어 버블 업데이트 및 수집 검사
        for (let i = this.wordOrbs.length - 1; i >= 0; i--) {
            const orb = this.wordOrbs[i];
            orb.x -= this.gameSpeed * 0.95;
            orb.floatPhase += 0.05;
            orb.y = orb.baseY + Math.sin(orb.floatPhase) * 6;

            // 자석 효과 (자석 활성화 시 플레이어 방향으로 당겨짐)
            if (this.activeItems.magnetTimer > 0) {
                const dx = (this.player.x + this.player.width / 2) - orb.x;
                const dy = (this.player.y + this.player.height / 2) - orb.y;
                const dist = Math.hypot(dx, dy);
                if (dist < 260) {
                    orb.x += (dx / dist) * 7.5;
                    orb.y += (dy / dist) * 7.5;
                }
            }

            // 플레이어와 충돌 (수집) 검사
            const pBox = {
                x: this.player.x,
                y: this.player.y,
                width: this.player.width,
                height: this.player.height
            };
            const oBox = {
                x: orb.x - orb.width / 2,
                y: orb.y - orb.height / 2,
                width: orb.width,
                height: orb.height
            };

            if (pBox.x < oBox.x + oBox.width &&
                pBox.x + pBox.width > oBox.x &&
                pBox.y < oBox.y + oBox.height &&
                pBox.y + pBox.height > oBox.y &&
                !orb.collected) {
                orb.collected = true;
                this.wordsCollected++;
                this.score += 150;
                this.createBlastEffect(orb.x, orb.y);
                const popText = `+150 ${orb.en.toUpperCase()}${orb.kr ? ' (' + orb.kr + ')' : ''}`;
                this.addFloatingText(popText, orb.x, orb.y - 12, '#4ADE80');

                // 발음 TTS 자동 재생 & 수집 음향
                if (window.JumpmanAudio) {
                    window.JumpmanAudio.playWordCollect();
                    window.JumpmanAudio.speak(orb.en);
                }

                this.wordOrbs.splice(i, 1);
                continue;
            }

            if (orb.x < -100) {
                this.wordOrbs.splice(i, 1);
            }
        }

        // 럭키 아이템 상자 업데이트 및 충돌 검사
        for (let i = this.itemBoxes.length - 1; i >= 0; i--) {
            const box = this.itemBoxes[i];
            box.x -= this.gameSpeed;
            box.floatPhase += 0.06;
            box.y = box.baseY + Math.sin(box.floatPhase) * 7;

            // 플레이어 충돌 검사
            const pBox = {
                x: this.player.x,
                y: this.player.y,
                width: this.player.width,
                height: this.player.height
            };
            const bBox = {
                x: box.x - box.size / 2,
                y: box.y - box.size / 2,
                width: box.size,
                height: box.size
            };

            if (pBox.x < bBox.x + bBox.width &&
                pBox.x + pBox.width > bBox.x &&
                pBox.y < bBox.y + bBox.height &&
                pBox.y + pBox.height > bBox.y &&
                !box.collected) {
                box.collected = true;
                this.createBlastEffect(box.x, box.y);
                this.itemBoxes.splice(i, 1);

                if (window.JumpmanAudio) {
                    window.JumpmanAudio.playItemBoxTouch();
                }

                // 게임 루프 일시 정지 후 퀴즈 모달 오픈
                this.isPaused = true;
                if (window.openItemQuizModal) {
                    window.openItemQuizModal((res) => {
                        this.handleItemQuizResult(res);
                    });
                } else {
                    this.isPaused = false;
                }
                return;
            }

            if (box.x < -60) {
                this.itemBoxes.splice(i, 1);
            }
        }

        // 시간 흐름 업데이트 (거리 및 생존 시간에 따른 실시간 낮/노을/밤/새벽 순환)
        this.timeOfDay = ((this.distance * 0.0005 + this.survivalTime * 0.01) % 1.0);
        this.clouds.forEach(c => {
            c.x -= c.speed * (this.gameSpeed * 0.25);
            if (c.x < -100) c.x = this.WIDTH + 50;
        });

        // 킬러(Bullet Bill) 경고선 업데이트 & 탄환 발사
        for (let w = this.bulletWarnings.length - 1; w >= 0; w--) {
            const warn = this.bulletWarnings[w];
            warn.life--;
            if (warn.life <= 0) {
                this.obstacles.push({
                    type: 'bullet_bill',
                    x: this.WIDTH + 30,
                    y: warn.y,
                    width: 52,
                    height: 32,
                    speedMultiplier: 2.3
                });
                this.bulletWarnings.splice(w, 1);
            }
        }

        // 파이어 마리오의 파이어볼 발사 & 업데이트
        if (this.equippedSkin === 'fire_mario') {
            this.fireballTimer--;
            if (this.fireballTimer <= 0) {
                this.fireballs.push({
                    x: this.player.x + this.player.width + 10,
                    y: this.player.y + this.player.height / 2,
                    vx: this.gameSpeed + 7.5,
                    vy: 2.2,
                    radius: 8,
                    rotation: 0
                });
                this.fireballTimer = 330; // 5.5초마다 전방 발사
                if (window.JumpmanAudio && window.JumpmanAudio.playFireball) {
                    window.JumpmanAudio.playFireball();
                }
            }
        }

        for (let fbIdx = this.fireballs.length - 1; fbIdx >= 0; fbIdx--) {
            const fb = this.fireballs[fbIdx];
            fb.x += fb.vx;
            fb.y += fb.vy;
            fb.rotation += 0.35;
            if (fb.y >= this.FLOOR_Y - fb.radius) {
                fb.y = this.FLOOR_Y - fb.radius;
                fb.vy = -3.5; // 통통 튀는 화염구
            }
            let fbHit = false;
            for (let oIdx = this.obstacles.length - 1; oIdx >= 0; oIdx--) {
                const obs = this.obstacles[oIdx];
                if (obs.squished) continue;
                const ow = obs.width || 30;
                const oh = obs.height || 30;
                const dist = Math.hypot(fb.x - (obs.x + ow / 2), fb.y - (obs.y + oh / 2));
                if (dist < fb.radius + ow / 2) {
                    this.createBlastEffect(obs.x + ow / 2, obs.y + oh / 2);
                    this.obstacles.splice(oIdx, 1);
                    this.score += 120;
                    this.addFloatingText('화염 격파! +120', fb.x, fb.y - 12, '#F97316');
                    fbHit = true;
                    break;
                }
            }
            if (fbHit || fb.x > this.WIDTH + 50) {
                this.fireballs.splice(fbIdx, 1);
            }
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
            } else if (obs.type === 'goomba') {
                obs.walkPhase += 0.15;
                if (obs.squished) {
                    obs.squishTimer--;
                    if (obs.squishTimer <= 0) {
                        this.obstacles.splice(i, 1);
                        continue;
                    }
                }
            } else if (obs.type === 'koopa') {
                obs.rotation += 0.35;
            } else if (obs.type === 'piranha_plant') {
                obs.risePhase += 0.045;
                obs.plantHeadY = (this.FLOOR_Y - 26) - Math.abs(Math.sin(obs.risePhase)) * 32;
            } else if (obs.type === 'fire_bar') {
                obs.angle += 0.045;
                obs.centerX = obs.x + obs.width / 2;
                obs.centerY = this.FLOOR_Y - 25;
            } else if (obs.type === 'boo') {
                obs.wavePhase += 0.05;
                obs.y = obs.baseY + Math.sin(obs.wavePhase) * 26;
            }

            // 🍄 굼바 위에서 밟기(Stomp) 판정
            if (obs.type === 'goomba' && !obs.squished) {
                const pFeet = this.player.y + this.player.height;
                const gTop = obs.y + 14;
                if (this.player.vy > 0 && pFeet >= obs.y && pFeet <= gTop &&
                    this.player.x + this.player.width > obs.x + 4 && this.player.x < obs.x + obs.width - 4) {
                    obs.squished = true;
                    obs.squishTimer = 18;
                    this.player.vy = -10.5; // 스프링 반동 점프!
                    this.score += 100;
                    this.addFloatingText('STOMP! +100', obs.x, obs.y - 15, '#FBBF24');
                    if (window.JumpmanAudio && window.JumpmanAudio.playStomp) {
                        window.JumpmanAudio.playStomp();
                    }
                    continue;
                }
            }

            // 충돌 검사
            if (this.checkCollision(this.player, obs)) {
                if (this.activeItems.starDashTimer > 0) {
                    // 스타 대시 돌진 중: 장애물 완파 & 보너스 점수!
                    this.createBlastEffect(obs.x + obs.width / 2, obs.y + obs.height / 2);
                    this.obstacles.splice(i, 1);
                    this.score += 100;
                    this.addFloatingText('대시 격파! +100', obs.x, obs.y, '#FBBF24');
                    if (window.JumpmanAudio && window.JumpmanAudio.playItemGet) {
                        window.JumpmanAudio.playItemGet();
                    }
                    continue;
                } else if (this.player.invulnerableTimer > 0) {
                    // 무적 상태일 때 충돌한 장애물 파괴
                    this.createBlastEffect(obs.x + obs.width / 2, obs.y + obs.height / 2);
                    this.obstacles.splice(i, 1);
                    this.score += 50;
                    continue;
                } else if (this.activeItems.shield) {
                    // 에너지 쉴드가 충돌 피해를 1회 막아주고 소멸!
                    this.activeItems.shield = false;
                    this.player.invulnerableTimer = 60; // 1초간 보호막
                    this.createBlastEffect(obs.x + obs.width / 2, obs.y + obs.height / 2);
                    this.obstacles.splice(i, 1);
                    this.addFloatingText('쉴드 방어! (세이프)', this.player.x, this.player.y - 15, '#38BDF8');
                    if (window.JumpmanAudio && window.JumpmanAudio.playShieldBreak) {
                        window.JumpmanAudio.playShieldBreak();
                    }
                    this.updateHUD();
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
        const warioMagnetDist = (this.equippedSkin === 'wario' ? 85 : 42);
        const magnetDist = (this.activeItems.magnetTimer > 0 ? 280 : warioMagnetDist);

        for (let i = this.collectibles.length - 1; i >= 0; i--) {
            const c = this.collectibles[i];
            c.x -= this.gameSpeed;

            const dx = (this.player.x + this.player.width / 2) - c.x;
            const dy = (this.player.y + this.player.height / 2) - c.y;
            const dist = Math.hypot(dx, dy);

            if (dist < magnetDist) {
                c.x += (dx / dist) * 8.5;
                c.y += (dy / dist) * 8.5;
            }

            if (dist < 42 && !c.collected) {
                c.collected = true;
                this.coins++;
                this.score += 100;
                this.createStarPuff(c.x, c.y);
                if (window.JumpmanAudio) window.JumpmanAudio.playCoin();
                // 영구 코인 상점에 코인 실시간 누적!
                if (typeof window !== 'undefined' && window.JumpmanShop) {
                    window.JumpmanShop.addCoins(1);
                }
                this.collectibles.splice(i, 1);
                continue;
            }

            if (c.x < -30) {
                this.collectibles.splice(i, 1);
            }
        }

        // 플로팅 텍스트 업데이트
        for (let i = this.floatingTexts.length - 1; i >= 0; i--) {
            const ft = this.floatingTexts[i];
            ft.y += ft.vy;
            ft.life--;
            ft.alpha = Math.max(0, ft.life / 60);
            if (ft.life <= 0) {
                this.floatingTexts.splice(i, 1);
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

        // 배경 패럴랙스 영어 단어 & 문장 업데이트
        this.updateBackgroundTexts(dt);

        // 화면 흔들림 감쇠
        if (this.screenShake > 0) {
            this.screenShake *= 0.88;
            if (this.screenShake < 0.3) this.screenShake = 0;
        }

        this.updateHUD();
    }

    checkCollision(player, obs) {
        if (obs.squished) return false;

        // 부드러운 플레이를 위해 히트박스 4px 살짝 여유 부여
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
        } else if (obs.type === 'piranha_plant') {
            // 토관 본체 + 뻐끔 머리 충돌 검사
            const pipeTop = this.FLOOR_Y - 32;
            const isPipeHit = (pLeft < oRight && pRight > oLeft && pBottom > pipeTop);
            const headTop = obs.plantHeadY || obs.y;
            const isHeadHit = (pLeft < obs.x + 38 && pRight > obs.x + 6 && pBottom > headTop && pTop < headTop + 28);
            return isPipeHit || isHeadHit;
        } else if (obs.type === 'fire_bar') {
            // 중심 블록 또는 회전하는 파이어볼 3개와의 충돌 검사
            if (pLeft < oRight && pRight > oLeft && pTop < oBottom && pBottom > oTop) return true;
            for (let k = 1; k <= 3; k++) {
                const dist = (obs.barLength / 3) * k;
                const fx = (obs.centerX || (obs.x + 18)) + Math.cos(obs.angle) * dist;
                const fy = (obs.centerY || (this.FLOOR_Y - 25)) + Math.sin(obs.angle) * dist;
                const fr = 7;
                if (pLeft < fx + fr && pRight > fx - fr && pTop < fy + fr && pBottom > fy - fr) {
                    return true;
                }
            }
            return false;
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
                distance: this.totalDistance || this.distance,
                survivalTime: Math.floor(this.survivalTime),
                coins: this.coins,
                quizzesSolved: this.quizzesSolved,
                wordsCollected: this.wordsCollected || 0,
                itemsAcquired: this.itemsAcquired || 0,
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

        // 스테이지 클리어 보너스 점수 즉시 합산
        const clearBonus = 500 * this.currentStage;
        this.score += clearBonus;

        if (window.showStageClearModal) {
            window.showStageClearModal({
                stage: this.currentStage,
                score: this.score,
                clearBonus: clearBonus,
                coins: this.coins,
                quizzesSolved: this.quizzesSolved,
                wordsCollected: this.wordsCollected || 0,
                itemsAcquired: this.itemsAcquired || 0,
                survivalTime: Math.floor(this.survivalTime),
                distance: this.totalDistance || this.distance
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

        const hudWords = document.getElementById('hudWords');
        if (hudWords) {
            hudWords.textContent = `${this.wordsCollected}개 / 퀴즈 ${this.quizzesSolved}회`;
        }

        const hudActiveItemBadge = document.getElementById('hudActiveItemBadge');
        if (hudActiveItemBadge) {
            if (this.activeItems.starDashTimer > 0) {
                const sec = Math.ceil(this.activeItems.starDashTimer / 60);
                hudActiveItemBadge.textContent = `🚀 스타 대시 (${sec}s)`;
                hudActiveItemBadge.className = 'px-2 py-0.5 rounded-full text-[11px] font-black bg-rose-500/25 text-rose-300 border border-rose-500/50 animate-pulse';
            } else if (this.activeItems.shield) {
                hudActiveItemBadge.textContent = '🛡️ 에너지 쉴드 ON';
                hudActiveItemBadge.className = 'px-2 py-0.5 rounded-full text-[11px] font-black bg-cyan-500/25 text-cyan-300 border border-cyan-500/50';
            } else if (this.activeItems.magnetTimer > 0) {
                const sec = Math.ceil(this.activeItems.magnetTimer / 60);
                hudActiveItemBadge.textContent = `🧲 코인 자석 (${sec}s)`;
                hudActiveItemBadge.className = 'px-2 py-0.5 rounded-full text-[11px] font-black bg-amber-500/25 text-amber-300 border border-amber-500/50 animate-pulse';
            } else {
                hudActiveItemBadge.textContent = '일반 상태';
                hudActiveItemBadge.className = 'px-2 py-0.5 rounded-full text-[11px] font-bold bg-slate-800 text-slate-400 border border-slate-700';
            }
        }

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

        // 1. 배경 렌더링 (다이내믹 낮밤 순환 & 테마 팩)
        this.renderBackground(ctx);

        // 2. 배경 패럴랙스 교육용 영어 단어 & 문장 플로팅
        this.renderBackgroundTexts(ctx);

        // 3. 지면 및 바닥 렌더링
        this.renderFloor(ctx);

        // 4. 수집 아이템 (코인 🪙)
        this.collectibles.forEach(c => {
            ctx.save();
            ctx.translate(c.x, c.y);
            ctx.fillStyle = '#FBBF24';
            ctx.beginPath();
            ctx.arc(0, 0, c.size, 0, Math.PI * 2);
            ctx.fill();

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

        // 4-B. 공중 영어 단어 버블 (Word Orbs)
        this.wordOrbs.forEach(orb => {
            ctx.save();
            ctx.translate(orb.x, orb.y);
            ctx.fillStyle = 'rgba(15, 23, 42, 0.88)';
            ctx.strokeStyle = '#4ADE80';
            ctx.lineWidth = 2;
            ctx.shadowColor = '#4ADE80';
            ctx.shadowBlur = 8;

            const w = orb.width;
            const h = orb.height;
            this.drawRoundedRect(ctx, -w / 2, -h / 2, w, h, h / 2);
            ctx.fill();
            ctx.stroke();

            ctx.shadowBlur = 0;
            ctx.fillStyle = '#86EFAC';
            ctx.font = 'bold 13px "Pretendard Variable", Pretendard, sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(orb.en, 0, 0);

            ctx.font = '10px sans-serif';
            ctx.fillText('🔤', -w / 2 + 10, -h / 2 + 3);
            ctx.restore();
        });

        // 4-C. 럭키 아이템 상자 (Item Boxes)
        this.itemBoxes.forEach(box => {
            ctx.save();
            ctx.translate(box.x, box.y);
            const s = box.size;
            ctx.shadowColor = '#F59E0B';
            ctx.shadowBlur = 12;

            const boxGrad = ctx.createLinearGradient(-s / 2, -s / 2, s / 2, s / 2);
            boxGrad.addColorStop(0, '#FDE047');
            boxGrad.addColorStop(0.5, '#F59E0B');
            boxGrad.addColorStop(1, '#B45309');
            ctx.fillStyle = boxGrad;
            ctx.strokeStyle = '#FEF08A';
            ctx.lineWidth = 2;

            this.drawRoundedRect(ctx, -s / 2, -s / 2, s, s, 6);
            ctx.fill();
            ctx.stroke();

            ctx.shadowBlur = 0;
            ctx.fillStyle = '#FFFFFF';
            ctx.font = 'bold 16px sans-serif';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('?', 0, 1);
            ctx.restore();
        });

        // 5. 킬러(Bullet Bill) 경고선 렌더링
        this.bulletWarnings.forEach(warn => {
            ctx.save();
            const flash = Math.floor(warn.life / 6) % 2 === 0;
            if (flash) {
                ctx.fillStyle = 'rgba(239, 68, 68, 0.2)';
                ctx.fillRect(0, warn.y - 14, this.WIDTH, 28);
                ctx.strokeStyle = '#EF4444';
                ctx.lineWidth = 2;
                ctx.setLineDash([8, 6]);
                ctx.beginPath();
                ctx.moveTo(0, warn.y);
                ctx.lineTo(this.WIDTH, warn.y);
                ctx.stroke();

                ctx.fillStyle = '#EF4444';
                ctx.font = 'bold 12px "Pretendard Variable", Pretendard, sans-serif';
                ctx.textAlign = 'right';
                ctx.textBaseline = 'middle';
                ctx.fillText('⚠️ DANGER! 🚀', this.WIDTH - 20, warn.y);
            }
            ctx.restore();
        });

        // 6. 파이어 마리오의 파이어볼 투사체 렌더링
        this.fireballs.forEach(fb => {
            ctx.save();
            ctx.translate(fb.x, fb.y);
            ctx.rotate(fb.rotation);

            ctx.shadowColor = '#EA580C';
            ctx.shadowBlur = 10;
            const grad = ctx.createRadialGradient(0, 0, 2, 0, 0, fb.radius);
            grad.addColorStop(0, '#FEF08A');
            grad.addColorStop(0.5, '#F97316');
            grad.addColorStop(1, '#DC2626');
            ctx.fillStyle = grad;
            ctx.beginPath();
            ctx.arc(0, 0, fb.radius, 0, Math.PI * 2);
            ctx.fill();

            // 불꽃 꼬리 파티클
            ctx.fillStyle = '#FDE047';
            ctx.fillRect(-fb.radius * 0.8, -2, fb.radius * 0.6, 4);
            ctx.restore();
        });

        // 7. 장애물 및 몬스터 렌더링 (11종)
        this.obstacles.forEach(obs => {
            ctx.save();
            if (obs.type === 'rolling_ball' || obs.type === 'bouncing_ball') {
                ctx.translate(obs.x, obs.y);
                ctx.rotate(obs.rotation);
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
                const wingY = Math.sin(obs.wingPhase * 2) * 8;
                ctx.fillStyle = '#7C3AED';

                ctx.beginPath();
                ctx.moveTo(-16, wingY);
                ctx.lineTo(-4, 0);
                ctx.lineTo(-12, 10);
                ctx.closePath();
                ctx.fill();

                ctx.beginPath();
                ctx.moveTo(16, wingY);
                ctx.lineTo(4, 0);
                ctx.lineTo(12, 10);
                ctx.closePath();
                ctx.fill();

                ctx.fillStyle = '#5B21B6';
                ctx.beginPath();
                ctx.arc(0, 0, 9, 0, Math.PI * 2);
                ctx.fill();

                ctx.fillStyle = '#EF4444';
                ctx.fillRect(-4, -2, 2, 2);
                ctx.fillRect(2, -2, 2, 2);
            } else if (obs.type === 'falling_rock') {
                ctx.fillStyle = '#78716C';
                ctx.fillRect(obs.x, obs.y, obs.width, obs.height);
                ctx.strokeStyle = '#44403C';
                ctx.lineWidth = 2;
                ctx.strokeRect(obs.x, obs.y, obs.width, obs.height);
                if (!obs.isFalling) {
                    ctx.fillStyle = 'rgba(239, 68, 68, 0.7)';
                    ctx.font = 'bold 14px sans-serif';
                    ctx.textAlign = 'center';
                    ctx.fillText('▼', obs.x + obs.width / 2, obs.y + obs.height + 15);
                }
            } else if (obs.type === 'goomba') {
                // 🍄 굼바 (버섯 괴물)
                ctx.translate(obs.x + obs.width / 2, obs.y + obs.height);
                if (obs.squished) {
                    ctx.scale(1.4, 0.35); // 밟혀서 납작해진 모습!
                }

                // 발 (보행 애니메이션)
                const footOffset = Math.sin(obs.walkPhase || 0) * 4;
                ctx.fillStyle = '#1C1917';
                ctx.fillRect(-14 + footOffset, -6, 10, 6);
                ctx.fillRect(4 - footOffset, -6, 10, 6);

                // 몸통
                ctx.fillStyle = '#FED7AA';
                ctx.fillRect(-8, -16, 16, 11);

                // 버섯 갓
                ctx.fillStyle = '#92400E';
                ctx.beginPath();
                ctx.moveTo(-16, -14);
                ctx.bezierCurveTo(-18, -32, 18, -32, 16, -14);
                ctx.closePath();
                ctx.fill();

                // 눈썹 & 눈
                ctx.fillStyle = '#0F172A';
                ctx.fillRect(-10, -22, 6, 2);
                ctx.fillRect(4, -22, 6, 2);

                ctx.fillStyle = '#FFFFFF';
                ctx.fillRect(-9, -20, 5, 5);
                ctx.fillRect(4, -20, 5, 5);
                ctx.fillStyle = '#0F172A';
                ctx.fillRect(-7, -19, 3, 4);
                ctx.fillRect(4, -19, 3, 4);

                // 덧니
                ctx.fillStyle = '#FFFFFF';
                ctx.fillRect(-7, -12, 3, 3);
                ctx.fillRect(4, -12, 3, 3);
            } else if (obs.type === 'koopa') {
                // 🐢 엉금엉금 (회전 등껍질)
                ctx.translate(obs.x + obs.width / 2, obs.y + obs.height / 2);
                ctx.rotate(obs.rotation || 0);

                // 등껍질 본체
                ctx.fillStyle = '#16A34A';
                ctx.beginPath();
                ctx.arc(0, 0, obs.height / 2, 0, Math.PI * 2);
                ctx.fill();

                // 하얀 림 테두리
                ctx.strokeStyle = '#FFFFFF';
                ctx.lineWidth = 3;
                ctx.stroke();

                // 육각형 등 문양 패턴
                ctx.strokeStyle = '#15803D';
                ctx.lineWidth = 2;
                ctx.strokeRect(-6, -6, 12, 12);
            } else if (obs.type === 'piranha_plant') {
                // 🌿 뻐끔플라워 & 초록 토관 파이프
                const pipeTop = this.FLOOR_Y - 32;

                // 뻐끔플라워 머리
                const headY = obs.plantHeadY || (this.FLOOR_Y - 55);
                ctx.fillStyle = '#DC2626';
                ctx.beginPath();
                ctx.arc(obs.x + obs.width / 2, headY, 14, 0, Math.PI * 2);
                ctx.fill();

                // 입술 & 이빨
                ctx.fillStyle = '#FFFFFF';
                ctx.beginPath();
                ctx.arc(obs.x + obs.width / 2 - 4, headY - 4, 3, 0, Math.PI * 2);
                ctx.arc(obs.x + obs.width / 2 + 5, headY + 3, 3, 0, Math.PI * 2);
                ctx.fill();

                // 입
                ctx.fillStyle = '#15803D';
                ctx.fillRect(obs.x + obs.width / 2 - 3, headY + 12, 6, Math.max(0, pipeTop - headY));

                // 토관 림 (입구)
                ctx.fillStyle = '#16A34A';
                ctx.fillRect(obs.x - 3, pipeTop, obs.width + 6, 10);
                ctx.strokeStyle = '#14532D';
                ctx.lineWidth = 2;
                ctx.strokeRect(obs.x - 3, pipeTop, obs.width + 6, 10);

                // 토관 본체
                ctx.fillStyle = '#15803D';
                ctx.fillRect(obs.x, pipeTop + 10, obs.width, 22);
                ctx.strokeRect(obs.x, pipeTop + 10, obs.width, 22);
            } else if (obs.type === 'bullet_bill') {
                // 🚀 킬러 대포알
                ctx.translate(obs.x, obs.y);
                // 추진 연기
                ctx.fillStyle = 'rgba(255, 255, 255, 0.7)';
                ctx.beginPath();
                ctx.arc(obs.width + 8, obs.height / 2, 8, 0, Math.PI * 2);
                ctx.arc(obs.width + 18, obs.height / 2 - 2, 5, 0, Math.PI * 2);
                ctx.fill();

                // 탄두 본체 (검은 총알)
                ctx.fillStyle = '#18181B';
                ctx.beginPath();
                ctx.moveTo(14, 0);
                ctx.lineTo(obs.width, 0);
                ctx.lineTo(obs.width, obs.height);
                ctx.lineTo(14, obs.height);
                ctx.quadraticCurveTo(-4, obs.height / 2, 14, 0);
                ctx.closePath();
                ctx.fill();

                // 하얀 눈 & 눈동자
                ctx.fillStyle = '#FFFFFF';
                ctx.beginPath();
                ctx.arc(16, obs.height / 2 - 3, 6, 0, Math.PI * 2);
                ctx.fill();
                ctx.fillStyle = '#000000';
                ctx.beginPath();
                ctx.arc(13, obs.height / 2 - 3, 3, 0, Math.PI * 2);
                ctx.fill();

                // 앙증맞은 주먹 손
                ctx.fillStyle = '#FFFFFF';
                ctx.beginPath();
                ctx.arc(28, obs.height / 2 + 7, 5, 0, Math.PI * 2);
                ctx.fill();
            } else if (obs.type === 'boo') {
                // 👻 부끄부끄 유령
                ctx.translate(obs.x + obs.width / 2, obs.y + obs.height / 2);
                ctx.fillStyle = '#FFFFFF';
                ctx.shadowColor = '#FFFFFF';
                ctx.shadowBlur = 8;
                ctx.beginPath();
                ctx.arc(0, 0, 16, 0, Math.PI * 2);
                ctx.fill();
                ctx.shadowBlur = 0;

                // 꼬리
                ctx.beginPath();
                ctx.moveTo(12, -4);
                ctx.lineTo(24, 0);
                ctx.lineTo(12, 6);
                ctx.closePath();
                ctx.fill();

                // 눈 & 눈썹
                ctx.fillStyle = '#000000';
                ctx.fillRect(-10, -5, 4, 6);
                ctx.fillRect(-2, -5, 4, 6);

                // 혀 & 입
                ctx.fillStyle = '#DC2626';
                ctx.beginPath();
                ctx.arc(-6, 6, 5, 0, Math.PI);
                ctx.fill();
            } else if (obs.type === 'fire_bar') {
                // 🔥 회전 파이어바
                const cx = obs.centerX || (obs.x + 18);
                const cy = obs.centerY || (this.FLOOR_Y - 25);

                // 중심 블록
                ctx.fillStyle = '#78350F';
                ctx.fillRect(cx - 9, cy - 9, 18, 18);
                ctx.strokeStyle = '#FBBF24';
                ctx.lineWidth = 1.5;
                ctx.strokeRect(cx - 9, cy - 9, 18, 18);

                // 3개 파이어볼 렌더링
                for (let k = 1; k <= 3; k++) {
                    const dist = (obs.barLength / 3) * k;
                    const fx = cx + Math.cos(obs.angle) * dist;
                    const fy = cy + Math.sin(obs.angle) * dist;
                    const fr = 7;

                    ctx.shadowColor = '#F97316';
                    ctx.shadowBlur = 8;
                    const fGrad = ctx.createRadialGradient(fx, fy, 1, fx, fy, fr);
                    fGrad.addColorStop(0, '#FEF08A');
                    fGrad.addColorStop(0.6, '#F97316');
                    fGrad.addColorStop(1, '#DC2626');
                    ctx.fillStyle = fGrad;
                    ctx.beginPath();
                    ctx.arc(fx, fy, fr, 0, Math.PI * 2);
                    ctx.fill();
                }
                ctx.shadowBlur = 0;
            }
            ctx.restore();
        });

        // 8. 파티클
        this.particles.forEach(p => {
            ctx.save();
            ctx.globalAlpha = Math.max(0, p.alpha);
            ctx.fillStyle = p.color;
            ctx.beginPath();
            ctx.arc(p.x, p.y, p.radius, 0, Math.PI * 2);
            ctx.fill();
            ctx.restore();
        });

        // 9. 플레이어 렌더링 (장착된 스킨 캐릭터!)
        this.renderPlayer(ctx);

        // 10. 플로팅 알림 텍스트
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

        // 11. 목적지 깃발 (스테이지 모드일 때)
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

    renderBackground(ctx) {
        const bg = this.equippedBg || 'cycle';

        if (bg === 'desert') {
            // 🏜️ 사막 테마
            const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
            skyGrad.addColorStop(0, '#B45309');
            skyGrad.addColorStop(0.5, '#F59E0B');
            skyGrad.addColorStop(1, '#FEF3C7');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 뜨거운 태양
            ctx.fillStyle = 'rgba(254, 240, 138, 0.9)';
            ctx.beginPath();
            ctx.arc(this.WIDTH * 0.78, 110, 48, 0, Math.PI * 2);
            ctx.fill();

            // 고대 피라미드 실루엣
            ctx.fillStyle = 'rgba(180, 83, 9, 0.45)';
            ctx.beginPath();
            ctx.moveTo(120, this.FLOOR_Y);
            ctx.lineTo(260, this.FLOOR_Y - 140);
            ctx.lineTo(400, this.FLOOR_Y);
            ctx.closePath();
            ctx.fill();

            ctx.beginPath();
            ctx.moveTo(480, this.FLOOR_Y);
            ctx.lineTo(600, this.FLOOR_Y - 110);
            ctx.lineTo(720, this.FLOOR_Y);
            ctx.closePath();
            ctx.fill();

            // 모래언덕 곡선
            ctx.fillStyle = 'rgba(217, 119, 6, 0.6)';
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            for (let x = 0; x <= this.WIDTH; x += 50) {
                const h = Math.sin((x + this.distance * 0.3) * 0.01) * 35 + 40;
                ctx.lineTo(x, this.FLOOR_Y - h);
            }
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.closePath();
            ctx.fill();
            return;
        }

        if (bg === 'cyber') {
            // 🌃 사이버 네온 시티
            const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
            skyGrad.addColorStop(0, '#0F172A');
            skyGrad.addColorStop(0.6, '#4C1D95');
            skyGrad.addColorStop(1, '#831843');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 네온 마천루 빌딩 실루엣 & 창문 불빛
            const offset = (this.distance * 0.2) % 180;
            ctx.fillStyle = 'rgba(15, 23, 42, 0.85)';
            for (let x = -offset; x <= this.WIDTH + 100; x += 90) {
                const bH = 120 + ((x * 13) % 90);
                ctx.fillRect(x, this.FLOOR_Y - bH, 70, bH);

                // 창문 불빛
                ctx.fillStyle = ((x / 90) % 2 === 0) ? 'rgba(56, 189, 248, 0.6)' : 'rgba(244, 114, 182, 0.6)';
                for (let wx = x + 8; wx < x + 62; wx += 14) {
                    for (let wy = this.FLOOR_Y - bH + 12; wy < this.FLOOR_Y - 10; wy += 22) {
                        ctx.fillRect(wx, wy, 8, 10);
                    }
                }
                ctx.fillStyle = 'rgba(15, 23, 42, 0.85)';
            }
            return;
        }

        if (bg === 'lava') {
            // 🌋 쿠파 성 & 용암
            const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
            skyGrad.addColorStop(0, '#18181B');
            skyGrad.addColorStop(0.65, '#7F1D1D');
            skyGrad.addColorStop(1, '#450A0A');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 뾰족한 가시 성채 실루엣
            ctx.fillStyle = 'rgba(24, 24, 27, 0.75)';
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            for (let x = 0; x <= this.WIDTH; x += 80) {
                const h = 70 + (Math.sin(x * 0.03) * 40);
                ctx.lineTo(x + 20, this.FLOOR_Y - h - 35);
                ctx.lineTo(x + 40, this.FLOOR_Y - h);
            }
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.closePath();
            ctx.fill();
            return;
        }

        if (bg === 'galaxy') {
            // 🌌 코스믹 갤럭시
            const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);
            skyGrad.addColorStop(0, '#030712');
            skyGrad.addColorStop(0.5, '#312E81');
            skyGrad.addColorStop(1, '#581C87');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 토성 고리 행성
            ctx.save();
            ctx.translate(this.WIDTH * 0.82, 95);
            ctx.fillStyle = '#F59E0B';
            ctx.beginPath();
            ctx.arc(0, 0, 36, 0, Math.PI * 2);
            ctx.fill();

            // 행성 고리
            ctx.strokeStyle = 'rgba(253, 230, 138, 0.85)';
            ctx.lineWidth = 6;
            ctx.beginPath();
            ctx.ellipse(0, 0, 65, 14, -Math.PI / 8, 0, Math.PI * 2);
            ctx.stroke();
            ctx.restore();

            // 별무리
            this.backgroundStars.forEach(s => {
                ctx.fillStyle = `rgba(255, 255, 255, ${s.alpha})`;
                ctx.beginPath();
                ctx.arc(s.x, s.y, s.size * 1.2, 0, Math.PI * 2);
                ctx.fill();
            });
            return;
        }

        // 🌓 기본: 실시간 낮·노을·밤 순환 (Day & Night Cycle)
        const t = this.timeOfDay || 0.0;
        const skyGrad = ctx.createLinearGradient(0, 0, 0, this.HEIGHT);

        if (t < 0.32) {
            // [낮 ☀️] 화창한 푸른 하늘
            skyGrad.addColorStop(0, '#38BDF8');
            skyGrad.addColorStop(0.65, '#60A5FA');
            skyGrad.addColorStop(1, '#BAE6FD');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 밝은 태양 & 햇살
            ctx.fillStyle = 'rgba(253, 224, 71, 0.95)';
            ctx.shadowColor = '#FBBF24';
            ctx.shadowBlur = 24;
            ctx.beginPath();
            ctx.arc(this.WIDTH * 0.82, 90, 44, 0, Math.PI * 2);
            ctx.fill();
            ctx.shadowBlur = 0;

            // 뭉게구름
            ctx.fillStyle = 'rgba(255, 255, 255, 0.85)';
            this.clouds.forEach(c => {
                ctx.beginPath();
                ctx.arc(c.x, c.y, c.size * 0.5, 0, Math.PI * 2);
                ctx.arc(c.x + c.size * 0.35, c.y - c.size * 0.15, c.size * 0.45, 0, Math.PI * 2);
                ctx.arc(c.x + c.size * 0.7, c.y, c.size * 0.4, 0, Math.PI * 2);
                ctx.fill();
            });
        } else if (t < 0.52) {
            // [노을 🌅] 주황빛에서 자주빛으로 물드는 석양
            skyGrad.addColorStop(0, '#C2410C');
            skyGrad.addColorStop(0.55, '#D946EF');
            skyGrad.addColorStop(1, '#311042');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 거대한 붉은 석양
            ctx.fillStyle = 'rgba(251, 146, 60, 0.9)';
            ctx.shadowColor = '#EA580C';
            ctx.shadowBlur = 30;
            ctx.beginPath();
            ctx.arc(this.WIDTH * 0.75, 135, 56, 0, Math.PI * 2);
            ctx.fill();
            ctx.shadowBlur = 0;
        } else if (t < 0.86) {
            // [밤 🌙] 딥 나이트 블루 & 반짝이는 별무리
            skyGrad.addColorStop(0, '#0F172A');
            skyGrad.addColorStop(0.65, '#1E1B4B');
            skyGrad.addColorStop(1, '#311042');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 밤하늘 별
            this.backgroundStars.forEach(s => {
                ctx.fillStyle = `rgba(255, 255, 255, ${s.alpha})`;
                ctx.beginPath();
                ctx.arc(s.x, s.y, s.size, 0, Math.PI * 2);
                ctx.fill();
            });

            // 빛나는 초승달
            ctx.save();
            ctx.translate(this.WIDTH * 0.8, 85);
            ctx.fillStyle = '#FEF08A';
            ctx.shadowColor = '#FDE047';
            ctx.shadowBlur = 18;
            ctx.beginPath();
            ctx.arc(0, 0, 32, 0, Math.PI * 2);
            ctx.fill();
            ctx.shadowBlur = 0;
            // 달 그림자 마스킹
            ctx.fillStyle = '#0F172A';
            ctx.beginPath();
            ctx.arc(12, -8, 28, 0, Math.PI * 2);
            ctx.fill();
            ctx.restore();
        } else {
            // [새벽/여명 🌄] 어스름한 새벽하늘
            skyGrad.addColorStop(0, '#1E293B');
            skyGrad.addColorStop(0.6, '#4338CA');
            skyGrad.addColorStop(1, '#F472B6');
            ctx.fillStyle = skyGrad;
            ctx.fillRect(0, 0, this.WIDTH, this.HEIGHT);

            // 별이 서서히 옅어짐
            this.backgroundStars.forEach(s => {
                ctx.fillStyle = `rgba(255, 255, 255, ${s.alpha * 0.4})`;
                ctx.beginPath();
                ctx.arc(s.x, s.y, s.size, 0, Math.PI * 2);
                ctx.fill();
            });
        }

        // 원경 산 실루엣
        ctx.fillStyle = t < 0.32 ? 'rgba(30, 58, 138, 0.35)' : 'rgba(15, 23, 42, 0.65)';
        ctx.beginPath();
        ctx.moveTo(0, this.FLOOR_Y);
        for (let x = 0; x <= this.WIDTH; x += 60) {
            const h = Math.sin((x + this.distance * 0.2) * 0.015) * 50 + 60;
            ctx.lineTo(x, this.FLOOR_Y - h);
        }
        ctx.lineTo(this.WIDTH, this.FLOOR_Y);
        ctx.closePath();
        ctx.fill();
    }

    renderFloor(ctx) {
        const bg = this.equippedBg || 'cycle';

        if (bg === 'desert') {
            // 사막 모래 바닥
            const fGrad = ctx.createLinearGradient(0, this.FLOOR_Y, 0, this.HEIGHT);
            fGrad.addColorStop(0, '#F59E0B');
            fGrad.addColorStop(0.12, '#D97706');
            fGrad.addColorStop(1, '#92400E');
            ctx.fillStyle = fGrad;
            ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

            ctx.strokeStyle = '#FDE68A';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.stroke();
            return;
        }

        if (bg === 'cyber') {
            // 사이버 그리드 바닥
            ctx.fillStyle = '#0F172A';
            ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

            ctx.strokeStyle = 'rgba(236, 72, 153, 0.4)';
            ctx.lineWidth = 2;
            const gridOffset = (this.distance * 2) % 40;
            for (let x = -gridOffset; x <= this.WIDTH; x += 40) {
                ctx.beginPath();
                ctx.moveTo(x, this.FLOOR_Y);
                ctx.lineTo(x, this.HEIGHT);
                ctx.stroke();
            }

            ctx.strokeStyle = '#06B6D4';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.stroke();
            return;
        }

        if (bg === 'lava') {
            // 용암 바닥
            const fGrad = ctx.createLinearGradient(0, this.FLOOR_Y, 0, this.HEIGHT);
            fGrad.addColorStop(0, '#DC2626');
            fGrad.addColorStop(0.3, '#EA580C');
            fGrad.addColorStop(1, '#7F1D1D');
            ctx.fillStyle = fGrad;
            ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

            ctx.strokeStyle = '#FEF08A';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.stroke();
            return;
        }

        if (bg === 'galaxy') {
            // 우주 플랫폼 바닥
            const fGrad = ctx.createLinearGradient(0, this.FLOOR_Y, 0, this.HEIGHT);
            fGrad.addColorStop(0, '#4338CA');
            fGrad.addColorStop(0.15, '#1E1B4B');
            fGrad.addColorStop(1, '#0F172A');
            ctx.fillStyle = fGrad;
            ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

            ctx.strokeStyle = '#818CF8';
            ctx.lineWidth = 3;
            ctx.beginPath();
            ctx.moveTo(0, this.FLOOR_Y);
            ctx.lineTo(this.WIDTH, this.FLOOR_Y);
            ctx.stroke();
            return;
        }

        // 기본 에메랄드 잔디 바닥
        const floorGrad = ctx.createLinearGradient(0, this.FLOOR_Y, 0, this.HEIGHT);
        floorGrad.addColorStop(0, '#10B981');
        floorGrad.addColorStop(0.08, '#047857');
        floorGrad.addColorStop(1, '#064E3B');
        ctx.fillStyle = floorGrad;
        ctx.fillRect(0, this.FLOOR_Y, this.WIDTH, this.HEIGHT - this.FLOOR_Y);

        ctx.strokeStyle = 'rgba(52, 211, 153, 0.35)';
        ctx.lineWidth = 2;
        const gridOffset = (this.distance * 2) % 40;
        for (let x = -gridOffset; x <= this.WIDTH; x += 40) {
            ctx.beginPath();
            ctx.moveTo(x, this.FLOOR_Y);
            ctx.lineTo(x, this.HEIGHT);
            ctx.stroke();
        }

        ctx.strokeStyle = '#34D399';
        ctx.lineWidth = 3;
        ctx.beginPath();
        ctx.moveTo(0, this.FLOOR_Y);
        ctx.lineTo(this.WIDTH, this.FLOOR_Y);
        ctx.stroke();
    }

    renderPlayer(ctx) {
        const p = this.player;
        const skin = this.equippedSkin || 'mario';

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

        // 쉴드 아이템 에너지 배리어 아우라
        if (this.activeItems.shield) {
            ctx.save();
            ctx.strokeStyle = 'rgba(56, 189, 248, 0.9)';
            ctx.lineWidth = 3.5;
            ctx.shadowColor = '#38BDF8';
            ctx.shadowBlur = 10;
            ctx.beginPath();
            ctx.arc(0, -p.height / 2, p.height * 0.85, 0, Math.PI * 2);
            ctx.stroke();

            ctx.fillStyle = 'rgba(56, 189, 248, 0.18)';
            ctx.fill();
            ctx.restore();
        }

        // 스타 대시 무적 돌진 황금 아우라
        if (this.activeItems.starDashTimer > 0) {
            ctx.save();
            ctx.strokeStyle = 'rgba(251, 191, 36, 0.95)';
            ctx.lineWidth = 4;
            ctx.shadowColor = '#F59E0B';
            ctx.shadowBlur = 15;
            ctx.beginPath();
            ctx.arc(0, -p.height / 2, p.height * 0.95, 0, Math.PI * 2);
            ctx.stroke();
            ctx.restore();
        }

        // 3단 점프 날개 연출
        if (p.jumpCount === 3) {
            ctx.save();
            ctx.fillStyle = 'rgba(56, 189, 248, 0.85)';
            ctx.shadowColor = '#38BDF8';
            ctx.shadowBlur = 8;
            ctx.beginPath();
            ctx.moveTo(-12, -p.height / 2);
            ctx.lineTo(-26, -p.height / 2 - 12);
            ctx.lineTo(-18, -p.height / 2 + 6);
            ctx.closePath();
            ctx.fill();

            ctx.beginPath();
            ctx.moveTo(12, -p.height / 2);
            ctx.lineTo(26, -p.height / 2 - 12);
            ctx.lineTo(18, -p.height / 2 + 6);
            ctx.closePath();
            ctx.fill();
            ctx.restore();
        }

        // 캐릭터 스킨별 컬러 & 형태 정의
        let capColor = '#DC2626'; // 마리오 기본 빨강
        let shirtColor = '#DC2626';
        let pantsColor = '#1E3A8A'; // 오버올 청색
        let buttonColor = '#FBBF24'; // 노란 단추
        let shoeColor = '#92400E';

        if (skin === 'luigi') {
            capColor = '#16A34A'; // 루이지 초록
            shirtColor = '#16A34A';
            pantsColor = '#1E3A8A';
            shoeColor = '#78350F';
        } else if (skin === 'wario') {
            capColor = '#EAB308'; // 와리오 노랑
            shirtColor = '#EAB308';
            pantsColor = '#7E22CE'; // 보라 오버올
            shoeColor = '#15803D'; // 초록 신발
        } else if (skin === 'fire_mario') {
            capColor = '#FFFFFF'; // 파이어 화이트
            shirtColor = '#FFFFFF';
            pantsColor = '#DC2626'; // 빨간 오버올
            shoeColor = '#7F1D1D';
        }

        const legPhase = p.onGround ? Math.sin(p.runFrame) * 10 : -4;

        if (skin === 'toad') {
            // 🍄 키노피오 렌더링
            // 하얀 바지 & 갈색 신발
            ctx.fillStyle = '#78350F';
            ctx.fillRect(-10, -4 + legPhase * 0.4, 8, 4);
            ctx.fillRect(2, -4 - legPhase * 0.4, 8, 4);

            ctx.fillStyle = '#F8FAFC';
            ctx.fillRect(-9, -14, 18, 11);

            // 파란 조끼 & 금빛 테두리
            ctx.fillStyle = '#0284C7';
            ctx.fillRect(-10, -26, 20, 13);
            ctx.fillStyle = '#FBBF24';
            ctx.fillRect(-10, -26, 3, 13);
            ctx.fillRect(7, -26, 3, 13);

            // 얼굴
            ctx.fillStyle = '#FED7AA';
            ctx.fillRect(-9, -38, 18, 13);
            // 눈
            ctx.fillStyle = '#0F172A';
            ctx.fillRect(2, -35, 3, 5);

            // 커다란 흰 버섯 모자 & 빨간 도트
            ctx.fillStyle = '#FFFFFF';
            ctx.beginPath();
            ctx.arc(0, -44, 18, 0, Math.PI * 2);
            ctx.fill();

            ctx.fillStyle = '#DC2626';
            ctx.beginPath();
            ctx.arc(0, -49, 7, 0, Math.PI * 2);
            ctx.arc(-11, -43, 6, 0, Math.PI * 2);
            ctx.arc(11, -43, 6, 0, Math.PI * 2);
            ctx.fill();
        } else if (skin === 'peach') {
            // 👑 피치 공주 렌더링
            // 핑크 롱 드레스
            ctx.fillStyle = '#F472B6';
            ctx.beginPath();
            ctx.moveTo(-14, -4);
            ctx.lineTo(-6, -26);
            ctx.lineTo(6, -26);
            ctx.lineTo(14, -4);
            ctx.closePath();
            ctx.fill();

            // 드레스 하단 프릴
            ctx.fillStyle = '#FBCFE8';
            ctx.fillRect(-14, -4, 28, 4);

            // 상의 코르셋
            ctx.fillStyle = '#EC4899';
            ctx.fillRect(-8, -32, 16, 8);
            // 브로치 (푸른 보석)
            ctx.fillStyle = '#38BDF8';
            ctx.beginPath();
            ctx.arc(0, -28, 3, 0, Math.PI * 2);
            ctx.fill();

            // 얼굴 & 금발 머리
            ctx.fillStyle = '#FDE047';
            ctx.fillRect(-11, -46, 22, 16); // 풍성한 금발

            ctx.fillStyle = '#FED7AA';
            ctx.fillRect(-7, -44, 15, 12); // 얼굴

            // 눈 & 입술
            ctx.fillStyle = '#0284C7';
            ctx.fillRect(2, -41, 3, 4);
            ctx.fillStyle = '#F43F5E';
            ctx.fillRect(4, -35, 3, 2);

            // 금빛 왕관 & 보석
            ctx.fillStyle = '#FBBF24';
            ctx.fillRect(-7, -49, 14, 5);
            ctx.fillStyle = '#DC2626';
            ctx.fillRect(-5, -48, 2, 2);
            ctx.fillStyle = '#2563EB';
            ctx.fillRect(0, -48, 2, 2);
            ctx.fillStyle = '#DC2626';
            ctx.fillRect(5, -48, 2, 2);
        } else {
            // 🔴 마리오 / 🟢 루이지 / 🟡 와리오 / 🔥 파이어 마리오
            const isTall = (skin === 'luigi');
            const isWide = (skin === 'wario');

            const hOffset = isTall ? -3 : 0;
            const wScale = isWide ? 1.2 : 1.0;

            // 다리
            ctx.fillStyle = pantsColor;
            ctx.fillRect(-10 * wScale, -12, 7 * wScale, 12 + legPhase * 0.5);
            ctx.fillRect(3 * wScale, -12, 7 * wScale, 12 - legPhase * 0.5);

            // 신발
            ctx.fillStyle = shoeColor;
            ctx.fillRect(-12 * wScale, -2 + legPhase * 0.5, 9 * wScale, 4);
            ctx.fillRect(3 * wScale, -2 - legPhase * 0.5, 9 * wScale, 4);

            // 몸통
            ctx.fillStyle = shirtColor;
            ctx.fillRect(-11 * wScale, -32 + hOffset, 22 * wScale, 20);

            // 오버올 멜빵
            ctx.fillStyle = pantsColor;
            ctx.fillRect(-8 * wScale, -26 + hOffset, 4 * wScale, 14);
            ctx.fillRect(4 * wScale, -26 + hOffset, 4 * wScale, 14);

            // 노란 단추
            ctx.fillStyle = buttonColor;
            ctx.fillRect(-7 * wScale, -24 + hOffset, 2, 2);
            ctx.fillRect(5 * wScale, -24 + hOffset, 2, 2);

            // 머리 & 얼굴 살색
            ctx.fillStyle = '#FED7AA';
            ctx.fillRect(-9 * wScale, -46 + hOffset, 18 * wScale, 14);

            // 눈
            ctx.fillStyle = '#0F172A';
            ctx.fillRect(2 * wScale, -42 + hOffset, 3, 4);

            // 코
            ctx.fillStyle = '#FDBA74';
            ctx.fillRect(5 * wScale, -39 + hOffset, 4, 4);

            // 콧수염
            ctx.fillStyle = (skin === 'wario' ? '#000000' : '#451A03');
            if (skin === 'wario') {
                // 와리오 지그재그 수염
                ctx.fillRect(1 * wScale, -37 + hOffset, 9 * wScale, 3);
            } else {
                ctx.fillRect(2 * wScale, -37 + hOffset, 8 * wScale, 3);
            }

            // 모자
            ctx.fillStyle = capColor;
            ctx.fillRect(-11 * wScale, -48 + hOffset, 22 * wScale, 6);
            ctx.fillRect(1 * wScale, -44 + hOffset, 12 * wScale, 4); // 챙

            // 파이어 마리오 손 불꽃 오라
            if (skin === 'fire_mario') {
                ctx.fillStyle = 'rgba(249, 115, 22, 0.8)';
                ctx.beginPath();
                ctx.arc(10 * wScale, -20 + hOffset, 5, 0, Math.PI * 2);
                ctx.fill();
            }
        }

        ctx.restore();
        ctx.globalAlpha = 1.0;
    }

    renderGoalFlag(ctx, x, y) {
        ctx.save();
        ctx.translate(x, y);

        ctx.fillStyle = '#E2E8F0';
        ctx.fillRect(-2, -90, 4, 90);

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
