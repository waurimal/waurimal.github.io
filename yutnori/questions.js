/**
 * 와우리말 영어 윷놀이 (English Yutnori Challenge) - 문제 생성 & 어휘/문장 데이터베이스
 * 2026 Eui Elementary School - Waurimal Project
 */

(function(window) {
    'use strict';

    // 1. 내장 기본 단어 데이터베이스 (초등 3~6 필수 어휘 200+)
    const DEFAULT_WORDS = [
        // 3~4학년 기초
        { en: "apple", kr: "사과", grade: 3, cat: "음식" },
        { en: "banana", kr: "바나나", grade: 3, cat: "음식" },
        { en: "cat", kr: "고양이", grade: 3, cat: "동물" },
        { en: "dog", kr: "개, 강아지", grade: 3, cat: "동물" },
        { en: "book", kr: "책", grade: 3, cat: "학용품" },
        { en: "pencil", kr: "연필", grade: 3, cat: "학용품" },
        { en: "eraser", kr: "지우개", grade: 3, cat: "학용품" },
        { en: "school", kr: "학교", grade: 3, cat: "장소" },
        { en: "teacher", kr: "선생님", grade: 3, cat: "사람" },
        { en: "friend", kr: "친구", grade: 3, cat: "사람" },
        { en: "happy", kr: "행복한", grade: 3, cat: "감정" },
        { en: "sad", kr: "슬픈", grade: 3, cat: "감정" },
        { en: "big", kr: "큰", grade: 3, cat: "상태" },
        { en: "small", kr: "작은", grade: 3, cat: "상태" },
        { en: "red", kr: "빨간색", grade: 3, cat: "색깔" },
        { en: "blue", kr: "파란색", grade: 3, cat: "색깔" },
        { en: "yellow", kr: "노란색", grade: 3, cat: "색깔" },
        { en: "green", kr: "초록색", grade: 3, cat: "색깔" },
        { en: "run", kr: "달리다", grade: 3, cat: "동작" },
        { en: "jump", kr: "뛰다, 점프하다", grade: 3, cat: "동작" },
        { en: "swim", kr: "수영하다", grade: 3, cat: "동작" },
        { en: "sing", kr: "노래하다", grade: 3, cat: "동작" },
        { en: "dance", kr: "춤추다", grade: 3, cat: "동작" },
        { en: "family", kr: "가족", grade: 3, cat: "사람" },
        { en: "father", kr: "아버지", grade: 3, cat: "가족" },
        { en: "mother", kr: "어머니", grade: 3, cat: "가족" },
        { en: "brother", kr: "형제, 남동생", grade: 3, cat: "가족" },
        { en: "sister", kr: "자매, 여동생", grade: 3, cat: "가족" },
        { en: "water", kr: "물", grade: 3, cat: "음식" },
        { en: "milk", kr: "우유", grade: 3, cat: "음식" },
        { en: "bread", kr: "빵", grade: 3, cat: "음식" },
        { en: "egg", kr: "달걀, 계란", grade: 3, cat: "음식" },
        { en: "sun", kr: "태양, 해", grade: 3, cat: "자연" },
        { en: "moon", kr: "달", grade: 3, cat: "자연" },
        { en: "star", kr: "별", grade: 3, cat: "자연" },
        { en: "tree", kr: "나무", grade: 3, cat: "자연" },
        { en: "flower", kr: "꽃", grade: 3, cat: "자연" },
        { en: "bird", kr: "새", grade: 3, cat: "동물" },
        { en: "fish", kr: "물고기", grade: 3, cat: "동물" },
        { en: "bear", kr: "곰", grade: 3, cat: "동물" },
        { en: "lion", kr: "사자", grade: 3, cat: "동물" },
        { en: "monkey", kr: "원숭이", grade: 3, cat: "동물" },
        { en: "desk", kr: "책상", grade: 3, cat: "사물" },
        { en: "chair", kr: "의자", grade: 3, cat: "사물" },
        { en: "clock", kr: "시계", grade: 3, cat: "사물" },
        { en: "door", kr: "문", grade: 3, cat: "사물" },
        { en: "window", kr: "창문", grade: 3, cat: "사물" },
        { en: "ball", kr: "공", grade: 3, cat: "스포츠" },
        { en: "soccer", kr: "축구", grade: 4, cat: "스포츠" },
        { en: "baseball", kr: "야구", grade: 4, cat: "스포츠" },
        { en: "basketball", kr: "농구", grade: 4, cat: "스포츠" },
        { en: "spring", kr: "봄", grade: 4, cat: "계절" },
        { en: "summer", kr: "여름", grade: 4, cat: "계절" },
        { en: "fall", kr: "가을", grade: 4, cat: "계절" },
        { en: "winter", kr: "겨울", grade: 4, cat: "계절" },
        { en: "sunny", kr: "화창한, 맑은", grade: 4, cat: "날씨" },
        { en: "rainy", kr: "비가 오는", grade: 4, cat: "날씨" },
        { en: "cloudy", kr: "구름 낀, 흐린", grade: 4, cat: "날씨" },
        { en: "snowy", kr: "눈이 오는", grade: 4, cat: "날씨" },
        { en: "cold", kr: "추운", grade: 4, cat: "날씨" },
        { en: "hot", kr: "더운, 뜨거운", grade: 4, cat: "날씨" },
        { en: "warm", kr: "따뜻한", grade: 4, cat: "날씨" },
        { en: "breakfast", kr: "아침 식사", grade: 4, cat: "음식" },
        { en: "lunch", kr: "점심 식사", grade: 4, cat: "음식" },
        { en: "dinner", kr: "저녁 식사", grade: 4, cat: "음식" },
        { en: "kitchen", kr: "주방, 부엌", grade: 4, cat: "장소" },
        { en: "bedroom", kr: "침실", grade: 4, cat: "장소" },
        { en: "bathroom", kr: "화장실, 욕실", grade: 4, cat: "장소" },
        { en: "living room", kr: "거실", grade: 4, cat: "장소" },
        { en: "park", kr: "공원", grade: 4, cat: "장소" },
        { en: "hospital", kr: "병원", grade: 4, cat: "장소" },
        { en: "library", kr: "도서관", grade: 4, cat: "장소" },
        { en: "museum", kr: "박물관", grade: 4, cat: "장소" },
        { en: "store", kr: "가게, 상점", grade: 4, cat: "장소" },
        { en: "clean", kr: "청소하다, 깨끗한", grade: 4, cat: "동작" },
        { en: "cook", kr: "요리하다", grade: 4, cat: "동작" },
        { en: "wash", kr: "씻다", grade: 4, cat: "동작" },
        { en: "sleep", kr: "잠자다", grade: 4, cat: "동작" },
        { en: "study", kr: "공부하다", grade: 4, cat: "동작" },
        { en: "draw", kr: "그리다", grade: 4, cat: "동작" },
        { en: "listen", kr: "듣다", grade: 4, cat: "동작" },
        { en: "read", kr: "읽다", grade: 4, cat: "동작" },
        { en: "write", kr: "쓰다", grade: 4, cat: "동작" },
        { en: "speak", kr: "말하다", grade: 4, cat: "동작" },

        // 5~6학년 심화
        { en: "grade", kr: "학년, 성적", grade: 5, cat: "학교" },
        { en: "subject", kr: "과목, 주제", grade: 5, cat: "학교" },
        { en: "science", kr: "과학", grade: 5, cat: "과목" },
        { en: "math", kr: "수학", grade: 5, cat: "과목" },
        { en: "history", kr: "역사", grade: 5, cat: "과목" },
        { en: "music", kr: "음악", grade: 5, cat: "과목" },
        { en: "art", kr: "미술, 예술", grade: 5, cat: "과목" },
        { en: "birthday", kr: "생일", grade: 5, cat: "행사" },
        { en: "vacation", kr: "방학, 휴가", grade: 5, cat: "생활" },
        { en: "yesterday", kr: "어제", grade: 5, cat: "시간" },
        { en: "tomorrow", kr: "내일", grade: 5, cat: "시간" },
        { en: "weekend", kr: "주말", grade: 5, cat: "시간" },
        { en: "season", kr: "계절", grade: 5, cat: "자연" },
        { en: "weather", kr: "날씨", grade: 5, cat: "날씨" },
        { en: "favorite", kr: "가장 좋아하는", grade: 5, cat: "감정" },
        { en: "delicious", kr: "아주 맛있는", grade: 5, cat: "상태" },
        { en: "exciting", kr: "신나는, 흥미진진한", grade: 5, cat: "상태" },
        { en: "interesting", kr: "흥미로운, 재미있는", grade: 5, cat: "상태" },
        { en: "difficult", kr: "어려운, 힘든", grade: 5, cat: "상태" },
        { en: "important", kr: "중요한", grade: 5, cat: "상태" },
        { en: "protect", kr: "보호하다, 지키다", grade: 5, cat: "동작" },
        { en: "recycle", kr: "재활용하다", grade: 5, cat: "환경" },
        { en: "earth", kr: "지구, 땅", grade: 5, cat: "환경" },
        { en: "nature", kr: "자연", grade: 5, cat: "자연" },
        { en: "future", kr: "미래", grade: 5, cat: "시간" },
        { en: "dream", kr: "꿈, 희망", grade: 5, cat: "마음" },
        { en: "health", kr: "건강", grade: 5, cat: "건강" },
        { en: "exercise", kr: "운동하다, 운동", grade: 5, cat: "건강" },
        { en: "medicine", kr: "약, 의학", grade: 5, cat: "건강" },
        { en: "headache", kr: "두통, 머리 아픔", grade: 5, cat: "건강" },
        { en: "stomachache", kr: "복통, 배 아픔", grade: 5, cat: "건강" },
        { en: "fever", kr: "열", grade: 5, cat: "건강" },
        { en: "doctor", kr: "의사", grade: 5, cat: "직업" },
        { en: "nurse", kr: "간호사", grade: 5, cat: "직업" },
        { en: "pilot", kr: "조종사, 파일럿", grade: 5, cat: "직업" },
        { en: "police officer", kr: "경찰관", grade: 5, cat: "직업" },
        { en: "firefighter", kr: "소방관", grade: 5, cat: "직업" },
        { en: "scientist", kr: "과학자", grade: 5, cat: "직업" },
        { en: "chef", kr: "요리사", grade: 5, cat: "직업" },
        { en: "artist", kr: "예술가, 화가", grade: 5, cat: "직업" },
        { en: "singer", kr: "가수", grade: 5, cat: "직업" },
        { en: "astronaut", kr: "우주비행사", grade: 6, cat: "직업" },
        { en: "inventor", kr: "발명가", grade: 6, cat: "직업" },
        { en: "invent", kr: "발명하다", grade: 6, cat: "동작" },
        { en: "discover", kr: "발견하다", grade: 6, cat: "동작" },
        { en: "travel", kr: "여행하다", grade: 6, cat: "동작" },
        { en: "visit", kr: "방문하다", grade: 6, cat: "동작" },
        { en: "tradition", kr: "전통", grade: 6, cat: "문화" },
        { en: "culture", kr: "문화", grade: 6, cat: "문화" },
        { en: "festival", kr: "축제", grade: 6, cat: "문화" },
        { en: "country", kr: "나라, 국가", grade: 6, cat: "사회" },
        { en: "world", kr: "세계, 세상", grade: 6, cat: "사회" },
        { en: "peace", kr: "평화", grade: 6, cat: "사회" },
        { en: "environment", kr: "환경", grade: 6, cat: "환경" },
        { en: "pollution", kr: "오염", grade: 6, cat: "환경" },
        { en: "energy", kr: "에너지", grade: 6, cat: "과학" },
        { en: "space", kr: "우주, 공간", grade: 6, cat: "과학" },
        { en: "planet", kr: "행성", grade: 6, cat: "과학" },
        { en: "robot", kr: "로봇", grade: 6, cat: "과학" },
        { en: "computer", kr: "컴퓨터", grade: 6, cat: "과학" },
        { en: "internet", kr: "인터넷", grade: 6, cat: "과학" },
        { en: "information", kr: "정보", grade: 6, cat: "사회" },
        { en: "communication", kr: "의사소통, 대화", grade: 6, cat: "사회" },
        { en: "experience", kr: "경험, 경험하다", grade: 6, cat: "생활" },
        { en: "memory", kr: "기억, 추억", grade: 6, cat: "생활" },
        { en: "promise", kr: "약속, 약속하다", grade: 6, cat: "생활" },
        { en: "honest", kr: "정직한", grade: 6, cat: "인성" },
        { en: "polite", kr: "예의 바른, 공손한", grade: 6, cat: "인성" },
        { en: "brave", kr: "용감한", grade: 6, cat: "인성" },
        { en: "kind", kr: "친절한, 종류", grade: 6, cat: "인성" },
        { en: "smart", kr: "똑똑한, 영리한", grade: 6, cat: "상태" },
        { en: "popular", kr: "인기 있는", grade: 6, cat: "상태" },
        { en: "famous", kr: "유명한", grade: 6, cat: "상태" },
        { en: "special", kr: "특별한", grade: 6, cat: "상태" },
        { en: "different", kr: "다른", grade: 6, cat: "상태" },
        { en: "same", kr: "같은", grade: 6, cat: "상태" },
        { en: "safe", kr: "안전한", grade: 6, cat: "안전" },
        { en: "dangerous", kr: "위험한", grade: 6, cat: "안전" }
    ];

    // 2. 내장 기본 문장 데이터베이스 (초등 필수 회화 & 핵심 문장 100+)
    const DEFAULT_SENTENCES = [
        // 3~4학년 기초 문장
        { en: "What is your name?", kr: "너의 이름은 무엇이니?", grade: 3 },
        { en: "My name is Minsu.", kr: "내 이름은 민수야.", grade: 3 },
        { en: "Nice to meet you.", kr: "만나서 반가워.", grade: 3 },
        { en: "How are you today?", kr: "오늘 기분이 어떠니?", grade: 3 },
        { en: "I am happy.", kr: "나는 행복해.", grade: 3 },
        { en: "What is this?", kr: "이것은 무엇이니?", grade: 3 },
        { en: "It is an apple.", kr: "그것은 사과야.", grade: 3 },
        { en: "Do you have a pencil?", kr: "너 연필 가지고 있니?", grade: 3 },
        { en: "Yes, I do.", kr: "응, 가지고 있어.", grade: 3 },
        { en: "No, I don't.", kr: "아니, 안 가지고 있어.", grade: 3 },
        { en: "Here you are.", kr: "여기 있어.", grade: 3 },
        { en: "Thank you very much.", kr: "정말 고마워.", grade: 3 },
        { en: "You're welcome.", kr: "천만에.", grade: 3 },
        { en: "Can you swim?", kr: "너 수영할 수 있니?", grade: 3 },
        { en: "Yes, I can.", kr: "응, 할 수 있어.", grade: 3 },
        { en: "No, I can't.", kr: "아니, 할 수 없어.", grade: 3 },
        { en: "I can dance well.", kr: "나는 춤을 잘 출 수 있어.", grade: 3 },
        { en: "How many apples?", kr: "사과가 몇 개 있니?", grade: 3 },
        { en: "Three apples, please.", kr: "사과 세 개 주세요.", grade: 3 },
        { en: "Put on your coat.", kr: "너의 코트를 입어라.", grade: 3 },
        { en: "Take off your shoes.", kr: "너의 신발을 벗어라.", grade: 3 },
        { en: "Open the door, please.", kr: "문을 열어 주세요.", grade: 3 },
        { en: "Close the window.", kr: "창문을 닫아라.", grade: 3 },
        { en: "What time is it now?", kr: "지금 몇 시인가요?", grade: 4 },
        { en: "It is ten o'clock.", kr: "10시 정각이야.", grade: 4 },
        { en: "It is time for lunch.", kr: "점심 먹을 시간이야.", grade: 4 },
        { en: "How's the weather today?", kr: "오늘 날씨가 어때?", grade: 4 },
        { en: "It is sunny and warm.", kr: "날씨가 맑고 따뜻해.", grade: 4 },
        { en: "It is raining outside.", kr: "밖에 비가 오고 있어.", grade: 4 },
        { en: "Don't run in the hallway.", kr: "복도에서 뛰지 마라.", grade: 4 },
        { en: "Where is the library?", kr: "도서관이 어디에 있나요?", grade: 4 },
        { en: "It is on the second floor.", kr: "그것은 2층에 있어.", grade: 4 },
        { en: "Go straight and turn right.", kr: "직진해서 오른쪽으로 돌아.", grade: 4 },
        { en: "What do you do on weekends?", kr: "너는 주말에 무엇을 하니?", grade: 4 },
        { en: "I usually ride my bike.", kr: "나는 보통 자전거를 타.", grade: 4 },
        { en: "I like playing soccer.", kr: "나는 축구하는 것을 좋아해.", grade: 4 },
        { en: "She likes reading books.", kr: "그녀는 책 읽는 것을 좋아해.", grade: 4 },

        // 5~6학년 심화 문장
        { en: "What grade are you in?", kr: "너는 몇 학년이니?", grade: 5 },
        { en: "I am in the fifth grade.", kr: "나는 5학년이야.", grade: 5 },
        { en: "What is your favorite subject?", kr: "너가 가장 좋아하는 과목은 무엇이니?", grade: 5 },
        { en: "My favorite subject is science.", kr: "내가 가장 좋아하는 과목은 과학이야.", grade: 5 },
        { en: "When is your birthday?", kr: "너의 생일은 언제니?", grade: 5 },
        { en: "My birthday is May fifth.", kr: "내 생일은 5월 5일이야.", grade: 5 },
        { en: "What did you do yesterday?", kr: "너는 어제 무엇을 했니?", grade: 5 },
        { en: "I visited my grandparents.", kr: "나는 조부모님 댁을 방문했어.", grade: 5 },
        { en: "We played baseball in the park.", kr: "우리는 공원에서 야구를 했어.", grade: 5 },
        { en: "What is the matter with you?", kr: "어디가 아프니? 무슨 일이니?", grade: 5 },
        { en: "I have a bad headache.", kr: "머리가 몹시 아파요.", grade: 5 },
        { en: "Take this medicine and rest.", kr: "이 약을 먹고 쉬세요.", grade: 5 },
        { en: "Where did you go on vacation?", kr: "방학 때 어디에 다녀왔니?", grade: 5 },
        { en: "I went to Jeju Island.", kr: "나는 제주도에 갔었어.", grade: 5 },
        { en: "It was very exciting.", kr: "정말 신나고 즐거웠어.", grade: 5 },
        { en: "How often do you exercise?", kr: "너는 얼마나 자주 운동하니?", grade: 5 },
        { en: "I exercise three times a week.", kr: "나는 일주일에 세 번 운동해.", grade: 5 },
        { en: "Why are you so happy?", kr: "너 왜 그렇게 기분이 좋아?", grade: 6 },
        { en: "Because tomorrow is my birthday.", kr: "내일이 내 생일이기 때문이야.", grade: 6 },
        { en: "What do you want to be in the future?", kr: "너는 미래에 무엇이 되고 싶니?", grade: 6 },
        { en: "I want to be a famous scientist.", kr: "나는 유명한 과학자가 되고 싶어.", grade: 6 },
        { en: "We should protect our environment.", kr: "우리는 환경을 보호해야 해.", grade: 6 },
        { en: "You should not waste water.", kr: "물을 낭비해서는 안 돼.", grade: 6 },
        { en: "Have you ever been to Canada?", kr: "너 캐나다에 가본 적 있니?", grade: 6 },
        { en: "Yes, I have been there twice.", kr: "응, 거기에 두 번 가봤어.", grade: 6 },
        { en: "I am taller than my brother.", kr: "나는 내 남동생보다 키가 커.", grade: 6 },
        { en: "This book is more interesting than that one.", kr: "이 책이 저 책보다 더 재미있어.", grade: 6 },
        { en: "Could you please help me?", kr: "저를 좀 도와주시겠어요?", grade: 6 },
        { en: "Sure, what can I do for you?", kr: "물론이죠, 무엇을 도와드릴까요?", grade: 6 }
    ];

    /**
     * WaurimalCurriculum 과 병합된 활성 단어 & 문장 목록 가져오기
     */
    function getActiveDataset(gradeFilter) {
        let words = [...DEFAULT_WORDS];
        let sentences = [...DEFAULT_SENTENCES];

        if (window.WaurimalCurriculum && typeof window.WaurimalCurriculum.getAllWords === 'function') {
            try {
                const currWords = window.WaurimalCurriculum.getAllWords();
                if (Array.isArray(currWords) && currWords.length > 0) {
                    // 중복 배제 병합
                    const existMap = new Set(words.map(w => w.en.toLowerCase().trim()));
                    currWords.forEach(cw => {
                        if (cw && cw.en && cw.kr && !existMap.has(cw.en.toLowerCase().trim())) {
                            words.push({
                                en: cw.en.trim(),
                                kr: cw.kr.trim(),
                                grade: cw.grade || 5,
                                cat: "교과서"
                            });
                            existMap.add(cw.en.toLowerCase().trim());
                        }
                    });
                }

                const currSentences = window.WaurimalCurriculum.getAllSentences();
                if (Array.isArray(currSentences) && currSentences.length > 0) {
                    const existSentMap = new Set(sentences.map(s => s.en.toLowerCase().trim()));
                    currSentences.forEach(cs => {
                        if (cs && cs.en && cs.kr && !existSentMap.has(cs.en.toLowerCase().trim())) {
                            sentences.push({
                                en: cs.en.trim(),
                                kr: cs.kr.trim(),
                                grade: cs.grade || 5
                            });
                            existSentMap.add(cs.en.toLowerCase().trim());
                        }
                    });
                }
            } catch (e) {
                console.warn("WaurimalCurriculum sync warning:", e);
            }
        }

        // 학년 필터 (all, 3-4, 5-6)
        if (gradeFilter === '3-4') {
            words = words.filter(w => w.grade <= 4);
            sentences = sentences.filter(s => s.grade <= 4);
        } else if (gradeFilter === '5-6') {
            words = words.filter(w => w.grade >= 5);
            sentences = sentences.filter(s => s.grade >= 5);
        }

        return { words, sentences };
    }

    /**
     * 유틸리티: 배열 셔플 (Fisher-Yates)
     */
    function shuffleArray(arr) {
        const copy = [...arr];
        for (let i = copy.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [copy[i], copy[j]] = [copy[j], copy[i]];
        }
        return copy;
    }

    /**
     * 윷놀이 퀴즈 생성기
     * 문제 유형:
     * 1) word_to_kr: 영단어 -> 뜻 맞추기 (객관식 4선)
     * 2) kr_to_word: 뜻 -> 영단어 맞추기 (객관식 4선)
     * 3) word_spelling: 스펠링 빈칸 채우기 (객관식 4선)
     * 4) sent_to_kr: 영어 문장 해석 고르기 (객관식 4선)
     * 5) sent_blank: 문장 속 핵심 빈칸 채우기 (객관식 4선)
     * 6) sent_scramble: 단어 블록 순서대로 맞추기 (어순 배열)
     */
    function generateQuestion(options = {}) {
        const gradeFilter = options.grade || 'all'; // 'all', '3-4', '5-6'
        const categoryPref = options.category || 'mixed'; // 'mixed', 'word_only', 'sentence_only'
        const { words, sentences } = getActiveDataset(gradeFilter);

        let questionType;
        if (categoryPref === 'word_only') {
            const wordTypes = ['word_to_kr', 'kr_to_word', 'word_spelling'];
            questionType = wordTypes[Math.floor(Math.random() * wordTypes.length)];
        } else if (categoryPref === 'sentence_only') {
            const sentTypes = ['sent_to_kr', 'sent_blank', 'sent_scramble'];
            questionType = sentTypes[Math.floor(Math.random() * sentTypes.length)];
        } else {
            // 종합 모드: 단어 50% 문장 50%
            const allTypes = [
                'word_to_kr', 'kr_to_word', 'word_spelling',
                'sent_to_kr', 'sent_blank', 'sent_scramble'
            ];
            questionType = allTypes[Math.floor(Math.random() * allTypes.length)];
        }

        switch (questionType) {
            case 'word_to_kr':
                return makeWordToKr(words);
            case 'kr_to_word':
                return makeKrToWord(words);
            case 'word_spelling':
                return makeWordSpelling(words);
            case 'sent_to_kr':
                return makeSentToKr(sentences);
            case 'sent_blank':
                return makeSentBlank(sentences, words);
            case 'sent_scramble':
                return makeSentScramble(sentences);
            default:
                return makeWordToKr(words);
        }
    }

    // 1) 영단어 보고 한국어 뜻 맞추기
    function makeWordToKr(words) {
        const target = words[Math.floor(Math.random() * words.length)];
        const distractors = shuffleArray(words.filter(w => w.en !== target.en && w.kr !== target.kr)).slice(0, 3);
        const choices = shuffleArray([target.kr, ...distractors.map(d => d.kr)]);

        return {
            type: 'word_to_kr',
            category: '단어 퀴즈',
            prompt: '다음 영어 단어의 올바른 뜻을 고르세요.',
            questionText: target.en,
            subText: target.cat ? `[분류: ${target.cat}]` : '',
            ttsText: target.en,
            choices: choices,
            answer: target.kr,
            answerIndex: choices.indexOf(target.kr),
            explanation: `"${target.en}"의 뜻은 "${target.kr}"입니다.`
        };
    }

    // 2) 한국어 뜻 보고 영어 단어 맞추기
    function makeKrToWord(words) {
        const target = words[Math.floor(Math.random() * words.length)];
        const distractors = shuffleArray(words.filter(w => w.en !== target.en)).slice(0, 3);
        const choices = shuffleArray([target.en, ...distractors.map(d => d.en)]);

        return {
            type: 'kr_to_word',
            category: '단어 퀴즈',
            prompt: '다음 뜻에 알맞은 영어 단어를 고르세요.',
            questionText: target.kr,
            subText: '',
            ttsText: target.en,
            choices: choices,
            answer: target.en,
            answerIndex: choices.indexOf(target.en),
            explanation: `"${target.kr}"은(는) 영어로 "${target.en}"입니다.`
        };
    }

    // 3) 스펠링 빈칸 채우기
    function makeWordSpelling(words) {
        // 길이가 4글자 이상인 단어 선택
        const candidateWords = words.filter(w => w.en.length >= 4 && !w.en.includes(' '));
        const target = (candidateWords.length > 0)
            ? candidateWords[Math.floor(Math.random() * candidateWords.length)]
            : words[0];

        const word = target.en;
        // 중간 글자 하나를 빈칸으로
        const blankIdx = Math.floor(Math.random() * (word.length - 2)) + 1;
        const answerLetter = word[blankIdx].toLowerCase();

        const displayArr = word.split('');
        displayArr[blankIdx] = '_';
        const maskedWord = displayArr.join(' ');

        // 오답 알파벳 생성
        const alphabet = 'abcdefghijklmnopqrstuvwxyz'.split('').filter(ch => ch !== answerLetter);
        const distractors = shuffleArray(alphabet).slice(0, 3);
        const choices = shuffleArray([answerLetter, ...distractors]);

        return {
            type: 'word_spelling',
            category: '스펠링 퀴즈',
            prompt: '빈칸(_)에 들어갈 알맞은 알파벳을 고르세요.',
            questionText: maskedWord,
            subText: `뜻: ${target.kr}`,
            ttsText: target.en,
            choices: choices,
            answer: answerLetter,
            answerIndex: choices.indexOf(answerLetter),
            explanation: `정답은 "${answerLetter}"입니다. (완성된 단어: ${target.en} - ${target.kr})`
        };
    }

    // 4) 영어 문장 해석 고르기
    function makeSentToKr(sentences) {
        const target = sentences[Math.floor(Math.random() * sentences.length)];
        const distractors = shuffleArray(sentences.filter(s => s.en !== target.en && s.kr !== target.kr)).slice(0, 3);
        const choices = shuffleArray([target.kr, ...distractors.map(d => d.kr)]);

        return {
            type: 'sent_to_kr',
            category: '문장 해석 퀴즈',
            prompt: '다음 문장의 올바른 우리말 해석을 고르세요.',
            questionText: target.en,
            subText: '',
            ttsText: target.en,
            choices: choices,
            answer: target.kr,
            answerIndex: choices.indexOf(target.kr),
            explanation: `정답: "${target.kr}"`
        };
    }

    // 5) 문장 속 핵심 빈칸 채우기
    function makeSentBlank(sentences, words) {
        // 문장에서 단어 1개를 빈칸으로 만듦
        const target = sentences[Math.floor(Math.random() * sentences.length)];
        const cleanWords = target.en.split(/\s+/);

        // 빈칸으로 쓸 만한 단어 고르기 (길이 3 이상, 기호 제거)
        let candidates = cleanWords.map((w, idx) => ({
            raw: w,
            clean: w.replace(/[^a-zA-Z]/g, ''),
            idx: idx
        })).filter(item => item.clean.length >= 3);

        if (candidates.length === 0) {
            return makeSentToKr(sentences);
        }

        const chosen = candidates[Math.floor(Math.random() * candidates.length)];
        const answerWord = chosen.clean;

        // 문장 재구성
        const maskedWords = [...cleanWords];
        maskedWords[chosen.idx] = '_______' + (chosen.raw.match(/[^a-zA-Z]+$/) ? chosen.raw.match(/[^a-zA-Z]+$/)[0] : '');
        const maskedSentence = maskedWords.join(' ');

        // 오답 보기 생성 (비슷한 품사나 일반 어휘)
        let distractors = shuffleArray(words.filter(w => w.en.toLowerCase() !== answerWord.toLowerCase() && !w.en.includes(' ')))
            .slice(0, 3)
            .map(d => d.en);

        if (distractors.length < 3) {
            distractors = ['school', 'time', 'friend'];
        }

        const choices = shuffleArray([answerWord, ...distractors]);

        return {
            type: 'sent_blank',
            category: '문장 빈칸 퀴즈',
            prompt: '문장의 빈칸에 들어갈 알맞은 단어를 고르세요.',
            questionText: maskedSentence,
            subText: `해석: ${target.kr}`,
            ttsText: target.en,
            choices: choices,
            answer: answerWord,
            answerIndex: choices.indexOf(answerWord),
            explanation: `정답: "${answerWord}" (전체 문장: ${target.en})`
        };
    }

    // 6) 문장 어순 배열 (Scramble)
    function makeSentScramble(sentences) {
        // 길이가 3~6단어 사이인 문장 선택
        const candidateSentences = sentences.filter(s => {
            const count = s.en.split(/\s+/).length;
            return count >= 3 && count <= 6;
        });

        const target = (candidateSentences.length > 0)
            ? candidateSentences[Math.floor(Math.random() * candidateSentences.length)]
            : sentences[0];

        const originalTokens = target.en.split(/\s+/);
        // 구두점 정리 또는 그대로 유지
        let shuffledTokens = shuffleArray(originalTokens);
        // 혹시 원래 순서와 완전히 같으면 다시 셔플
        if (shuffledTokens.join(' ') === originalTokens.join(' ') && originalTokens.length > 1) {
            shuffledTokens = [shuffledTokens[1], shuffledTokens[0], ...shuffledTokens.slice(2)];
        }

        return {
            type: 'sent_scramble',
            category: '어순 배열 퀴즈',
            prompt: '단어들을 알맞은 순서대로 클릭하여 문장을 완성하세요.',
            questionText: target.kr, // 우리말 뜻을 제시
            subText: '아래 단어 카드를 순서대로 눌러 문장을 완성해 보세요!',
            ttsText: target.en,
            tokens: shuffledTokens, // 섞인 토큰들
            answerSentence: target.en,
            answerTokens: originalTokens,
            explanation: `완성된 문장: "${target.en}" (${target.kr})`
        };
    }

    // 전역 노출
    window.YutnoriQuestions = {
        DEFAULT_WORDS,
        DEFAULT_SENTENCES,
        getActiveDataset,
        generateQuestion
    };

})(window);
