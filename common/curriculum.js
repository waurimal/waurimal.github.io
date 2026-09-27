/**
 * 초등 영어 교과과정 공통 어휘 및 문장 데이터 모듈 (Waurimal Curriculum Module)
 * 2026 Eui Elementary School - Waurimal Project
 */
(function() {
    'use strict';

    const RAW_LESSONS = [
        {
            code: "3G_L07",
            grade: 3,
            name: "3학년 Lesson 07 - 학용품",
            words: [
                { en: "crayon", kr: "크레용" },
                { en: "eraser", kr: "지우개" },
                { en: "glue stick", kr: "풀" },
                { en: "notebook", kr: "공책" },
                { en: "pencil", kr: "연필" },
                { en: "pencil case", kr: "필통" },
                { en: "ruler", kr: "자" },
                { en: "scissors", kr: "가위" }
            ],
            sentences: [
                { en: "Do you have a pencil?", kr: "연필 있니?" },
                { en: "Yes, I do.", kr: "응, 있어." },
                { en: "No, I don't.", kr: "아니, 없어." },
                { en: "Here you are.", kr: "여기 있어." }
            ]
        },
        {
            code: "6G_L01",
            grade: 6,
            name: "6학년 Lesson 01 - 서수와 학교생활",
            words: [
                { en: "first", kr: "첫 번째의" },
                { en: "second", kr: "두 번째의" },
                { en: "third", kr: "세 번째의" },
                { en: "fourth", kr: "네 번째의" },
                { en: "fifth", kr: "다섯 번째의" },
                { en: "sixth", kr: "여섯 번째의" },
                { en: "grade", kr: "학년" },
                { en: "spell", kr: "철자를 쓰다" },
                { en: "join", kr: "가입하다" },
                { en: "club", kr: "동아리" },
                { en: "dance", kr: "춤추다, 춤" },
                { en: "draw", kr: "그리다" },
                { en: "play", kr: "놀다, 연주하다" },
                { en: "listen", kr: "듣다" },
                { en: "read", kr: "읽다" },
                { en: "ride", kr: "(자전거 등을) 타다" }
            ],
            sentences: [
                { en: "What grade are you in?", kr: "몇 학년이니?" },
                { en: "I'm in the sixth grade.", kr: "나는 6학년이야." },
                { en: "How do you spell your name?", kr: "이름 철자가 어떻게 되니?" },
                { en: "I want to join the dance club.", kr: "나는 댄스 동아리에 들고 싶어." }
            ]
        },
        {
            code: "6G_L02",
            grade: 6,
            name: "6학년 Lesson 02 - 계절과 날씨",
            words: [
                { en: "season", kr: "계절" },
                { en: "spring", kr: "봄" },
                { en: "summer", kr: "여름" },
                { en: "fall", kr: "가을" },
                { en: "winter", kr: "겨울" },
                { en: "beautiful", kr: "아름다운" },
                { en: "flower", kr: "꽃" },
                { en: "cold", kr: "추운" },
                { en: "warm", kr: "따뜻한" },
                { en: "delicious", kr: "맛있는" },
                { en: "also", kr: "또한" },
                { en: "colorful", kr: "다채로운" },
                { en: "swim", kr: "수영하다" },
                { en: "field trip", kr: "현장학습" },
                { en: "clear", kr: "맑은" },
                { en: "bright", kr: "밝은" }
            ],
            sentences: [
                { en: "What's your favorite season?", kr: "네가 가장 좋아하는 계절은 뭐니?" },
                { en: "My favorite season is spring.", kr: "내가 가장 좋아하는 계절은 봄이야." },
                { en: "Because I can see beautiful flowers.", kr: "아름다운 꽃들을 볼 수 있기 때문이야." },
                { en: "It is warm and sunny.", kr: "따뜻하고 맑아." }
            ]
        },
        {
            code: "6G_L03",
            grade: 6,
            name: "6학년 Lesson 03 - 환경과 지구 보호",
            words: [
                { en: "campaign", kr: "홍보활동" },
                { en: "earth", kr: "지구" },
                { en: "paper", kr: "종이" },
                { en: "poster", kr: "포스터" },
                { en: "save", kr: "구하다, 절약하다" },
                { en: "shirt", kr: "셔츠" },
                { en: "turn off", kr: "끄다" },
                { en: "when", kr: "언제" },
                { en: "world", kr: "세계" },
                { en: "protect", kr: "보호하다" },
                { en: "recycle", kr: "재활용하다" }
            ],
            sentences: [
                { en: "We should save energy.", kr: "우리는 에너지를 절약해야 해." },
                { en: "Turn off the lights.", kr: "전등을 끄자." },
                { en: "Let's make a poster for the Earth.", kr: "지구를 위한 포스터를 만들자." },
                { en: "Don't waste paper.", kr: "종이를 낭비하지 마." }
            ]
        },
        {
            code: "6G_L04",
            grade: 6,
            name: "6학년 Lesson 04 - 감정과 이유 묻기",
            words: [
                { en: "because", kr: "~ 때문에" },
                { en: "belt", kr: "띠, 벨트" },
                { en: "birdhouse", kr: "새집" },
                { en: "break", kr: "부수다, 깨다" },
                { en: "congratulations", kr: "축하" },
                { en: "test", kr: "시험" },
                { en: "why", kr: "왜" },
                { en: "worry", kr: "걱정하다" },
                { en: "wrong", kr: "틀린, 잘못된" },
                { en: "happy", kr: "행복한" },
                { en: "sad", kr: "슬픈" }
            ],
            sentences: [
                { en: "Why are you so happy?", kr: "너 왜 그렇게 기분이 좋니?" },
                { en: "Because today is my birthday!", kr: "오늘이 내 생일이기 때문이야!" },
                { en: "What's wrong?", kr: "무슨 일 있니?" },
                { en: "Don't worry, you can do it.", kr: "걱정하지 마, 넌 할 수 있어." }
            ]
        },
        {
            code: "6G_L05",
            grade: 6,
            name: "6학년 Lesson 05 - 길 찾기와 장소",
            words: [
                { en: "bank", kr: "은행" },
                { en: "block", kr: "구역, 블록" },
                { en: "hospital", kr: "병원" },
                { en: "hungry", kr: "배고픈" },
                { en: "restaurant", kr: "식당" },
                { en: "left", kr: "왼쪽" },
                { en: "restroom", kr: "화장실" },
                { en: "stop", kr: "정류장, 멈추다" },
                { en: "store", kr: "가게" },
                { en: "straight", kr: "똑바로, 곧장" },
                { en: "town", kr: "마을" },
                { en: "right", kr: "오른쪽" }
            ],
            sentences: [
                { en: "Where is the library?", kr: "도서관이 어디에 있나요?" },
                { en: "Go straight two blocks and turn left.", kr: "두 블록 직진해서 왼쪽으로 도세요." },
                { en: "It's on your right.", kr: "오른쪽에 있습니다." },
                { en: "You can't miss it.", kr: "쉽게 찾을 수 있을 거예요." }
            ]
        },
        {
            code: "6G_L06",
            grade: 6,
            name: "6학년 Lesson 06 - 가족과 묘사하기",
            words: [
                { en: "aunt", kr: "고모, 이모" },
                { en: "daughter", kr: "딸" },
                { en: "dress", kr: "드레스, 원피스" },
                { en: "eye", kr: "눈" },
                { en: "hair", kr: "머리카락" },
                { en: "long", kr: "긴" },
                { en: "same", kr: "같은" },
                { en: "sand", kr: "모래" },
                { en: "short", kr: "짧은, 키가 작은" },
                { en: "curly", kr: "곱슬머리의" }
            ],
            sentences: [
                { en: "What does she look like?", kr: "그녀는 어떻게 생겼니?" },
                { en: "She has long straight hair.", kr: "그녀는 긴 생머리를 가지고 있어." },
                { en: "He is wearing glasses.", kr: "그는 안경을 쓰고 있어." }
            ]
        },
        {
            code: "6G_L07",
            grade: 6,
            name: "6학년 Lesson 07 - 경험과 과거",
            words: [
                { en: "after", kr: "~후에" },
                { en: "biscuit", kr: "비스킷" },
                { en: "laser", kr: "레이저" },
                { en: "light", kr: "빛, 가벼운" },
                { en: "number", kr: "숫자" },
                { en: "soft drink", kr: "청량음료" },
                { en: "tell", kr: "말하다" },
                { en: "visit", kr: "방문하다" }
            ],
            sentences: [
                { en: "Have you ever visited Jeju-do?", kr: "제주도에 가본 적 있니?" },
                { en: "Yes, I have.", kr: "응, 가본 적 있어." },
                { en: "It was a wonderful experience.", kr: "멋진 경험이었어." }
            ]
        },
        {
            code: "6G_L08",
            grade: 6,
            name: "6학년 Lesson 08 - 비교하기",
            words: [
                { en: "always", kr: "항상, 언제나" },
                { en: "baby", kr: "아기" },
                { en: "catch", kr: "잡다" },
                { en: "clock", kr: "시계" },
                { en: "dribble", kr: "공을 몰다" },
                { en: "fast", kr: "빠른" },
                { en: "giraffe", kr: "기린" },
                { en: "give", kr: "주다" },
                { en: "goal", kr: "득점, 골" },
                { en: "heavy", kr: "무거운" },
                { en: "race", kr: "경주" },
                { en: "strong", kr: "힘센" },
                { en: "than", kr: "~보다" },
                { en: "tower", kr: "탑" },
                { en: "win", kr: "이기다" }
            ],
            sentences: [
                { en: "A giraffe is taller than an elephant.", kr: "기린은 코끼리보다 키가 커." },
                { en: "He is faster than me.", kr: "그는 나보다 빨라." },
                { en: "Who is stronger?", kr: "누가 더 힘이 세니?" }
            ]
        },
        {
            code: "6G_L09",
            grade: 6,
            name: "6학년 Lesson 09 - 방학 계획과 미래",
            words: [
                { en: "any", kr: "어떤" },
                { en: "children", kr: "아이들" },
                { en: "forest", kr: "숲" },
                { en: "hamburger", kr: "햄버거" },
                { en: "plan", kr: "계획" },
                { en: "program", kr: "프로그램" },
                { en: "room", kr: "방" },
                { en: "stay", kr: "머무르다" },
                { en: "tent", kr: "텐트" },
                { en: "village", kr: "마을" },
                { en: "zoo", kr: "동물원" }
            ],
            sentences: [
                { en: "What will you do this summer?", kr: "이번 여름에 무엇을 할 거니?" },
                { en: "I will go camping in the forest.", kr: "숲으로 캠핑을 갈 거야." },
                { en: "I plan to visit my grandparents.", kr: "조부모님 댁을 방문할 계획이야." }
            ]
        }
    ];

    window.WaurimalCurriculum = {
        lessons: RAW_LESSONS,

        // 단원 코드로 조회
        getLesson: function(code) {
            return RAW_LESSONS.find(l => l.code === code) || null;
        },

        // 학년별 단원 목록
        getLessonsByGrade: function(grade) {
            return RAW_LESSONS.filter(l => l.grade === Number(grade));
        },

        // 전체 단어 평탄화 목록 반환
        getAllWords: function() {
            const list = [];
            RAW_LESSONS.forEach(l => {
                l.words.forEach(w => {
                    list.push({ ...w, lessonCode: l.code, lessonName: l.name, grade: l.grade });
                });
            });
            return list;
        },

        // 텍스트 기반 단어 목록 파싱 (예: "apple : 사과\nbanana : 바나나")
        parseWords: function(rawText) {
            if (!rawText) return [];
            return rawText.split('\n')
                .map(line => line.trim())
                .filter(line => line && line.includes(':'))
                .map(line => {
                    const parts = line.split(':');
                    return {
                        en: parts[0].trim(),
                        kr: parts.slice(1).join(':').trim()
                    };
                });
        },

        // 랜덤 단어 N개 추출
        getRandomWords: function(count, grade) {
            let pool = grade ? this.getLessonsByGrade(grade).flatMap(l => l.words) : this.getAllWords();
            const shuffled = [...pool].sort(() => Math.random() - 0.5);
            return shuffled.slice(0, count);
        }
    };
})();
