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
        },
        {
            code: "6G_L09_CANVA",
            grade: 6,
            name: "6학년 Lesson 09 - 방학 계획 (Canva Sparkle Standard)",
            words: [
                { en: "plan", kr: "계획" },
                { en: "children", kr: "아이들" },
                { en: "soccer", kr: "축구" },
                { en: "hiking", kr: "하이킹" },
                { en: "forest", kr: "숲" },
                { en: "hamburger", kr: "햄버거" },
                { en: "program", kr: "프로그램" },
                { en: "clean", kr: "청소하다" },
                { en: "tent", kr: "텐트" },
                { en: "village", kr: "마을" },
                { en: "zoo", kr: "동물원" }
            ],
            sentences: [
                { en: "I do not have any plans.", kr: "나는 아무 계획이 없어." },
                { en: "They play soccer with children.", kr: "그들은 아이들과 축구를 해." },
                { en: "I like hiking in a forest.", kr: "나는 숲에서 하이킹하는 것을 좋아해." },
                { en: "I like hamburgers.", kr: "나는 햄버거를 좋아해." },
                { en: "Choose your favorite program, please.", kr: "가장 좋아하는 프로그램을 골라주세요." },
                { en: "I am going to clean my room.", kr: "나는 방을 청소할 예정이야." },
                { en: "I am going to stay home.", kr: "나는 집에 머무를 예정이야." },
                { en: "We are going to sleep in a tent.", kr: "우리는 텐트에서 잘 거야." },
                { en: "We are going to visit a small village.", kr: "우리는 작은 마을을 방문할 거야." },
                { en: "I am going to clean the zoo.", kr: "나는 동물원을 청소할 거야." }
            ]
        }
    ];

    const STORAGE_KEY = 'waurimal_custom_curriculum';

    // 로컬 스토리지에 캐시된 커스텀 단원 확인 (오프라인 0ms 즉시 응답)
    let currentLessons = [...RAW_LESSONS];
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const parsed = JSON.parse(saved);
            if (Array.isArray(parsed) && parsed.length > 0) {
                currentLessons = parsed;
            }
        }
    } catch (e) {
        console.warn("Curriculum local storage error:", e);
    }

    const WaurimalCurriculum = {
        // 기본 제공 원본 데이터
        defaultLessons: RAW_LESSONS,

        // 현재 활성화된 교육과정 목록 (기본 + 교사 커스텀)
        lessons: currentLessons,

        // 전체 단원 목록 조회
        getLessons: function() {
            return this.lessons;
        },

        // 단원 코드로 조회
        getLesson: function(code) {
            if (!code) return null;
            const target = String(code).trim().toLowerCase();
            return this.lessons.find(l => 
                (l.code && l.code.toLowerCase() === target) || 
                (l.id && l.id.toLowerCase() === target)
            ) || null;
        },

        // 학년별 단원 목록 조회
        getLessonsByGrade: function(grade) {
            if (!grade || grade === 'all' || grade === 'ALL') return this.lessons;
            return this.lessons.filter(l => Number(l.grade) === Number(grade));
        },

        // 특정 단원의 단어 목록 ({ en, kr } 객체 배열) 반환
        getLessonWords: function(code) {
            const l = this.getLesson(code);
            return (l && Array.isArray(l.words)) ? l.words : [];
        },

        // 특정 단원의 단어 목록 텍스트 ("en : kr\n...") 반환
        getLessonWordsText: function(code) {
            const words = this.getLessonWords(code);
            return this.wordsToText(words);
        },

        // 특정 단원의 문장 목록 ({ en, kr } 객체 배열) 반환
        getLessonSentences: function(code) {
            const l = this.getLesson(code);
            if (!l) return [];
            if (Array.isArray(l.sentences)) {
                return l.sentences.map(s => {
                    if (typeof s === 'string') return { en: s, kr: '' };
                    return { en: s.en || s.sentence || '', kr: s.kr || s.meaning || '' };
                });
            }
            return [];
        },

        // 특정 단원의 문장 문자열 배열 (["I am happy.", ...]) 반환 (Sparkle 등 간편 사용)
        getLessonSentencesList: function(code) {
            const sentences = this.getLessonSentences(code);
            return sentences.map(s => s.en).filter(s => s && s.length > 0);
        },

        // 특정 단원의 문장 목록 텍스트 ("en : kr\n...") 반환
        getLessonSentencesText: function(code) {
            const sentences = this.getLessonSentences(code);
            return this.sentencesToText(sentences);
        },

        // 전체 단어 평탄화 목록 반환
        getAllWords: function() {
            const list = [];
            this.lessons.forEach(l => {
                if (Array.isArray(l.words)) {
                    l.words.forEach(w => {
                        list.push({ ...w, lessonCode: l.code, lessonName: l.name, grade: l.grade });
                    });
                }
            });
            return list;
        },

        // 전체 문장 평탄화 목록 반환
        getAllSentences: function() {
            const list = [];
            this.lessons.forEach(l => {
                if (Array.isArray(l.sentences)) {
                    l.sentences.forEach(s => {
                        const item = typeof s === 'string' ? { en: s, kr: '' } : { en: s.en || '', kr: s.kr || '' };
                        list.push({ ...item, lessonCode: l.code, lessonName: l.name, grade: l.grade });
                    });
                }
            });
            return list;
        },

        // 랜덤 단어 추출
        getRandomWords: function(count, grade) {
            let pool = grade ? this.getLessonsByGrade(grade).flatMap(l => l.words || []) : this.getAllWords();
            const shuffled = [...pool].sort(() => Math.random() - 0.5);
            return shuffled.slice(0, count);
        },

        // ── 텍스트 파싱 & 포맷팅 헬퍼 ──
        parseWords: function(rawText) {
            if (!rawText) return [];
            return rawText.split('\n')
                .map(line => line.trim())
                .filter(line => line.length > 0)
                .map(line => {
                    if (line.includes(':')) {
                        const parts = line.split(':');
                        return { en: parts[0].trim(), kr: parts.slice(1).join(':').trim() };
                    }
                    return { en: line.trim(), kr: '' };
                });
        },

        wordsToText: function(wordsArray) {
            if (!Array.isArray(wordsArray)) return '';
            return wordsArray.map(w => w.kr ? `${w.en} : ${w.kr}` : w.en).join('\n');
        },

        parseSentences: function(rawText) {
            if (!rawText) return [];
            return rawText.split('\n')
                .map(line => line.trim())
                .filter(line => line.length > 0)
                .map(line => {
                    if (line.includes(':')) {
                        const parts = line.split(':');
                        return { en: parts[0].trim(), kr: parts.slice(1).join(':').trim() };
                    }
                    return { en: line.trim(), kr: '' };
                });
        },

        sentencesToText: function(sentencesArray) {
            if (!Array.isArray(sentencesArray)) return '';
            return sentencesArray.map(s => {
                const en = typeof s === 'string' ? s : (s.en || s.sentence || '');
                const kr = typeof s === 'string' ? '' : (s.kr || s.meaning || '');
                return kr ? `${en} : ${kr}` : en;
            }).join('\n');
        },

        // ── 로컬 및 클라우드 동기화 메서드 ──
        // 단원 목록 업데이트 및 로컬/이벤트 통지
        setLessons: function(newLessons, syncToCloud) {
            if (!Array.isArray(newLessons)) return false;
            this.lessons = newLessons;
            try {
                localStorage.setItem(STORAGE_KEY, JSON.stringify(newLessons));
            } catch (e) {
                console.warn("Storage save error:", e);
            }
            window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: newLessons }));

            if (syncToCloud) {
                return this.saveToCloud();
            }
            return Promise.resolve(true);
        },

        // Firebase Firestore 중앙 클라우드 저장
        saveToCloud: async function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    await db.collection('settings').doc('curriculum').set({
                        lessons: this.lessons,
                        updatedAt: firebase.firestore.FieldValue.serverTimestamp()
                    }, { merge: true });
                    console.log("WaurimalCurriculum: Firestore synced successfully!");
                    return true;
                } else {
                    console.warn("Firebase Firestore not initialized, stored in localStorage only.");
                    return true;
                }
            } catch (e) {
                console.error("WaurimalCurriculum cloud sync error:", e);
                return false;
            }
        },

        // Firebase Firestore 중앙 클라우드에서 최신 데이터 로드
        loadFromCloud: async function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    const docSnap = await db.collection('settings').doc('curriculum').get();
                    if (docSnap.exists && docSnap.data().lessons && Array.isArray(docSnap.data().lessons)) {
                        const cloudLessons = docSnap.data().lessons;
                        if (cloudLessons.length > 0) {
                            this.lessons = cloudLessons;
                            localStorage.setItem(STORAGE_KEY, JSON.stringify(cloudLessons));
                            window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: cloudLessons }));
                            return true;
                        }
                    }
                }
            } catch (e) {
                console.warn("WaurimalCurriculum cloud load notice (offline fallback):", e);
            }
            return false;
        },

        // 실시간 클라우드 리스너 등록
        listenToCloud: function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    return db.collection('settings').doc('curriculum').onSnapshot(docSnap => {
                        if (docSnap.exists && docSnap.data().lessons && Array.isArray(docSnap.data().lessons)) {
                            const cloudLessons = docSnap.data().lessons;
                            if (cloudLessons.length > 0) {
                                this.lessons = cloudLessons;
                                localStorage.setItem(STORAGE_KEY, JSON.stringify(cloudLessons));
                                window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: cloudLessons }));
                            }
                        }
                    });
                }
            } catch (e) {
                console.warn("WaurimalCurriculum listen error:", e);
            }
            return null;
        },

        // 초기 기본 교육과정으로 리셋
        resetToDefault: async function(syncToCloud) {
            return this.setLessons([...RAW_LESSONS], syncToCloud);
        },

        // JSON 내보내기 & 불러오기
        exportJson: function() {
            return JSON.stringify(this.lessons, null, 2);
        },

        importJson: function(jsonString, syncToCloud) {
            try {
                const parsed = JSON.parse(jsonString);
                if (Array.isArray(parsed) && parsed.length > 0) {
                    return this.setLessons(parsed, syncToCloud);
                }
                return false;
            } catch (e) {
                console.error("JSON import error:", e);
                return false;
            }
        }
    };

    // 전역 등록 및 백그라운드 클라우드 동기화 시도
    if (typeof window !== 'undefined') {
        window.WaurimalCurriculum = WaurimalCurriculum;
        setTimeout(() => {
            if (typeof WaurimalCurriculum.loadFromCloud === 'function') {
                WaurimalCurriculum.loadFromCloud();
            }
        }, 500);
    }
})();
