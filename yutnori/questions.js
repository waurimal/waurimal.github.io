/**
 * 와우리말 영어 윷놀이 (English Yutnori Challenge) - 학년별 문제 생성 & 어휘/문장 데이터베이스
 * 2026 Eui Elementary School - Waurimal Project
 * 초등 3학년, 4학년, 5학년, 6학년 교육과정 성취기준 100% 정합
 */

(function(window) {
    'use strict';

    // ────────────────────────────────────────────────────────
    // 1. 내장 기본 단어 데이터베이스 (초등 3~6학년별 필수 어휘 300+)
    // ────────────────────────────────────────────────────────
    const DEFAULT_WORDS = [
        // ====================================================
        // [초등 3학년 수준] 기초 인사, 가족, 학교, 동물, 색깔, 숫자, 신체, 음식, 기초 동작 (약 75단어)
        // ====================================================
        { en: "hello", kr: "안녕, 안녕하세요", grade: 3, cat: "인사" },
        { en: "bye", kr: "잘 가, 안녕", grade: 3, cat: "인사" },
        { en: "good", kr: "좋은, 착한", grade: 3, cat: "상태" },
        { en: "morning", kr: "아침", grade: 3, cat: "시간" },
        { en: "happy", kr: "행복한, 기쁜", grade: 3, cat: "감정" },
        { en: "sad", kr: "슬픈", grade: 3, cat: "감정" },
        { en: "fine", kr: "건강한, 좋은", grade: 3, cat: "상태" },
        { en: "tired", kr: "피곤한, 지친", grade: 3, cat: "상태" },
        { en: "apple", kr: "사과", grade: 3, cat: "음식" },
        { en: "banana", kr: "바나나", grade: 3, cat: "음식" },
        { en: "water", kr: "물", grade: 3, cat: "음식" },
        { en: "milk", kr: "우유", grade: 3, cat: "음식" },
        { en: "bread", kr: "빵", grade: 3, cat: "음식" },
        { en: "egg", kr: "달걀, 계란", grade: 3, cat: "음식" },
        { en: "cake", kr: "케이크", grade: 3, cat: "음식" },
        { en: "orange", kr: "오렌지, 주황색", grade: 3, cat: "음식" },
        { en: "cat", kr: "고양이", grade: 3, cat: "동물" },
        { en: "dog", kr: "개, 강아지", grade: 3, cat: "동물" },
        { en: "bird", kr: "새", grade: 3, cat: "동물" },
        { en: "fish", kr: "물고기", grade: 3, cat: "동물" },
        { en: "bear", kr: "곰", grade: 3, cat: "동물" },
        { en: "lion", kr: "사자", grade: 3, cat: "동물" },
        { en: "tiger", kr: "호랑이", grade: 3, cat: "동물" },
        { en: "monkey", kr: "원숭이", grade: 3, cat: "동물" },
        { en: "duck", kr: "오리", grade: 3, cat: "동물" },
        { en: "pig", kr: "돼지", grade: 3, cat: "동물" },
        { en: "book", kr: "책", grade: 3, cat: "학용품" },
        { en: "pencil", kr: "연필", grade: 3, cat: "학용품" },
        { en: "eraser", kr: "지우개", grade: 3, cat: "학용품" },
        { en: "ruler", kr: "자", grade: 3, cat: "학용품" },
        { en: "bag", kr: "가방", grade: 3, cat: "학용품" },
        { en: "crayon", kr: "크레용", grade: 3, cat: "학용품" },
        { en: "desk", kr: "책상", grade: 3, cat: "사물" },
        { en: "chair", kr: "의자", grade: 3, cat: "사물" },
        { en: "clock", kr: "시계", grade: 3, cat: "사물" },
        { en: "door", kr: "문", grade: 3, cat: "사물" },
        { en: "window", kr: "창문", grade: 3, cat: "사물" },
        { en: "box", kr: "상자", grade: 3, cat: "사물" },
        { en: "ball", kr: "공", grade: 3, cat: "사물" },
        { en: "cap", kr: "모자", grade: 3, cat: "의류" },
        { en: "red", kr: "빨간색", grade: 3, cat: "색깔" },
        { en: "blue", kr: "파란색", grade: 3, cat: "색깔" },
        { en: "yellow", kr: "노란색", grade: 3, cat: "색깔" },
        { en: "green", kr: "초록색", grade: 3, cat: "색깔" },
        { en: "black", kr: "검은색", grade: 3, cat: "색깔" },
        { en: "white", kr: "하얀색", grade: 3, cat: "색깔" },
        { en: "pink", kr: "분홍색", grade: 3, cat: "색깔" },
        { en: "one", kr: "하나, 1", grade: 3, cat: "숫자" },
        { en: "two", kr: "둘, 2", grade: 3, cat: "숫자" },
        { en: "three", kr: "셋, 3", grade: 3, cat: "숫자" },
        { en: "four", kr: "넷, 4", grade: 3, cat: "숫자" },
        { en: "five", kr: "다섯, 5", grade: 3, cat: "숫자" },
        { en: "six", kr: "여섯, 6", grade: 3, cat: "숫자" },
        { en: "seven", kr: "일곱, 7", grade: 3, cat: "숫자" },
        { en: "eight", kr: "여덟, 8", grade: 3, cat: "숫자" },
        { en: "nine", kr: "아홉, 9", grade: 3, cat: "숫자" },
        { en: "ten", kr: "열, 10", grade: 3, cat: "숫자" },
        { en: "eye", kr: "눈", grade: 3, cat: "신체" },
        { en: "nose", kr: "코", grade: 3, cat: "신체" },
        { en: "mouth", kr: "입", grade: 3, cat: "신체" },
        { en: "ear", kr: "귀", grade: 3, cat: "신체" },
        { en: "hand", kr: "손", grade: 3, cat: "신체" },
        { en: "head", kr: "머리", grade: 3, cat: "신체" },
        { en: "foot", kr: "발", grade: 3, cat: "신체" },
        { en: "leg", kr: "다리", grade: 3, cat: "신체" },
        { en: "family", kr: "가족", grade: 3, cat: "사람" },
        { en: "father", kr: "아버지", grade: 3, cat: "가족" },
        { en: "mother", kr: "어머니", grade: 3, cat: "가족" },
        { en: "brother", kr: "형제, 남동생", grade: 3, cat: "가족" },
        { en: "sister", kr: "자매, 여동생", grade: 3, cat: "가족" },
        { en: "friend", kr: "친구", grade: 3, cat: "사람" },
        { en: "teacher", kr: "선생님", grade: 3, cat: "사람" },
        { en: "run", kr: "달리다", grade: 3, cat: "동작" },
        { en: "jump", kr: "뛰다, 점프하다", grade: 3, cat: "동작" },
        { en: "swim", kr: "수영하다", grade: 3, cat: "동작" },
        { en: "sing", kr: "노래하다", grade: 3, cat: "동작" },
        { en: "dance", kr: "춤추다", grade: 3, cat: "동작" },
        { en: "walk", kr: "걷다", grade: 3, cat: "동작" },
        { en: "stop", kr: "멈추다", grade: 3, cat: "동작" },
        { en: "sit", kr: "앉다", grade: 3, cat: "동작" },
        { en: "stand", kr: "서다", grade: 3, cat: "동작" },
        { en: "big", kr: "큰", grade: 3, cat: "상태" },
        { en: "small", kr: "작은", grade: 3, cat: "상태" },
        { en: "sun", kr: "태양, 해", grade: 3, cat: "자연" },
        { en: "moon", kr: "달", grade: 3, cat: "자연" },
        { en: "star", kr: "별", grade: 3, cat: "자연" },
        { en: "tree", kr: "나무", grade: 3, cat: "자연" },
        { en: "flower", kr: "꽃", grade: 3, cat: "자연" },

        // ====================================================
        // [초등 4학년 수준] 일상생활, 시간/요일, 날씨, 계절, 집/장소, 식사, 스포츠, 취미, 옷차림 (약 75단어)
        // ====================================================
        { en: "time", kr: "시간", grade: 4, cat: "시간" },
        { en: "today", kr: "오늘", grade: 4, cat: "시간" },
        { en: "afternoon", kr: "오후", grade: 4, cat: "시간" },
        { en: "night", kr: "밤", grade: 4, cat: "시간" },
        { en: "Sunday", kr: "일요일", grade: 4, cat: "요일" },
        { en: "Monday", kr: "월요일", grade: 4, cat: "요일" },
        { en: "Tuesday", kr: "화요일", grade: 4, cat: "요일" },
        { en: "Wednesday", kr: "수요일", grade: 4, cat: "요일" },
        { en: "Thursday", kr: "목요일", grade: 4, cat: "요일" },
        { en: "Friday", kr: "금요일", grade: 4, cat: "요일" },
        { en: "Saturday", kr: "토요일", grade: 4, cat: "요일" },
        { en: "spring", kr: "봄", grade: 4, cat: "계절" },
        { en: "summer", kr: "여름", grade: 4, cat: "계절" },
        { en: "fall", kr: "가을", grade: 4, cat: "계절" },
        { en: "winter", kr: "겨울", grade: 4, cat: "계절" },
        { en: "weather", kr: "날씨", grade: 4, cat: "날씨" },
        { en: "sunny", kr: "화창한, 맑은", grade: 4, cat: "날씨" },
        { en: "rainy", kr: "비가 오는", grade: 4, cat: "날씨" },
        { en: "cloudy", kr: "구름 낀, 흐린", grade: 4, cat: "날씨" },
        { en: "snowy", kr: "눈이 오는", grade: 4, cat: "날씨" },
        { en: "windy", kr: "바람 부는", grade: 4, cat: "날씨" },
        { en: "warm", kr: "따뜻한", grade: 4, cat: "날씨" },
        { en: "hot", kr: "더운, 뜨거운", grade: 4, cat: "날씨" },
        { en: "cool", kr: "시원한", grade: 4, cat: "날씨" },
        { en: "cold", kr: "추운", grade: 4, cat: "날씨" },
        { en: "breakfast", kr: "아침 식사", grade: 4, cat: "음식" },
        { en: "lunch", kr: "점심 식사", grade: 4, cat: "음식" },
        { en: "dinner", kr: "저녁 식사", grade: 4, cat: "음식" },
        { en: "snack", kr: "간식", grade: 4, cat: "음식" },
        { en: "pizza", kr: "피자", grade: 4, cat: "음식" },
        { en: "chicken", kr: "치킨, 닭고기", grade: 4, cat: "음식" },
        { en: "salad", kr: "샐러드", grade: 4, cat: "음식" },
        { en: "juice", kr: "주스", grade: 4, cat: "음식" },
        { en: "kitchen", kr: "주방, 부엌", grade: 4, cat: "장소" },
        { en: "bedroom", kr: "침실", grade: 4, cat: "장소" },
        { en: "bathroom", kr: "화장실, 욕실", grade: 4, cat: "장소" },
        { en: "living room", kr: "거실", grade: 4, cat: "장소" },
        { en: "house", kr: "집", grade: 4, cat: "장소" },
        { en: "school", kr: "학교", grade: 4, cat: "장소" },
        { en: "park", kr: "공원", grade: 4, cat: "장소" },
        { en: "library", kr: "도서관", grade: 4, cat: "장소" },
        { en: "hospital", kr: "병원", grade: 4, cat: "장소" },
        { en: "museum", kr: "박물관", grade: 4, cat: "장소" },
        { en: "store", kr: "가게, 상점", grade: 4, cat: "장소" },
        { en: "restroom", kr: "화장실", grade: 4, cat: "장소" },
        { en: "soccer", kr: "축구", grade: 4, cat: "스포츠" },
        { en: "baseball", kr: "야구", grade: 4, cat: "스포츠" },
        { en: "basketball", kr: "농구", grade: 4, cat: "스포츠" },
        { en: "tennis", kr: "테니스", grade: 4, cat: "스포츠" },
        { en: "bike", kr: "자전거", grade: 4, cat: "사물" },
        { en: "piano", kr: "피아노", grade: 4, cat: "악기" },
        { en: "guitar", kr: "기타", grade: 4, cat: "악기" },
        { en: "coat", kr: "외투, 코트", grade: 4, cat: "의류" },
        { en: "jacket", kr: "재킷", grade: 4, cat: "의류" },
        { en: "pants", kr: "바지", grade: 4, cat: "의류" },
        { en: "shirt", kr: "셔츠", grade: 4, cat: "의류" },
        { en: "shoes", kr: "신발", grade: 4, cat: "의류" },
        { en: "hat", kr: "모자", grade: 4, cat: "의류" },
        { en: "socks", kr: "양말", grade: 4, cat: "의류" },
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
        { en: "play", kr: "놀다, 연주하다", grade: 4, cat: "동작" },
        { en: "ride", kr: "타다", grade: 4, cat: "동작" },
        { en: "dollar", kr: "달러", grade: 4, cat: "경제" },
        { en: "price", kr: "가격", grade: 4, cat: "경제" },
        { en: "corner", kr: "모퉁이, 구석", grade: 4, cat: "위치" },
        { en: "straight", kr: "곧바로, 직진", grade: 4, cat: "위치" },

        // ====================================================
        // [초등 5학년 수준] 교과목, 월/서수, 생일/날짜, 취미/여가, 과거 시제, 건강/증상, 직업, 감정 상태 (약 75단어)
        // ====================================================
        { en: "grade", kr: "학년, 성적", grade: 5, cat: "학교" },
        { en: "subject", kr: "과목, 주제", grade: 5, cat: "학교" },
        { en: "science", kr: "과학", grade: 5, cat: "과목" },
        { en: "math", kr: "수학", grade: 5, cat: "과목" },
        { en: "English", kr: "영어", grade: 5, cat: "과목" },
        { en: "Korean", kr: "국어, 한국어", grade: 5, cat: "과목" },
        { en: "history", kr: "역사", grade: 5, cat: "과목" },
        { en: "music", kr: "음악", grade: 5, cat: "과목" },
        { en: "art", kr: "미술, 예술", grade: 5, cat: "과목" },
        { en: "birthday", kr: "생일", grade: 5, cat: "행사" },
        { en: "party", kr: "파티, 모임", grade: 5, cat: "행사" },
        { en: "January", kr: "1월", grade: 5, cat: "월" },
        { en: "February", kr: "2월", grade: 5, cat: "월" },
        { en: "March", kr: "3월", grade: 5, cat: "월" },
        { en: "April", kr: "4월", grade: 5, cat: "월" },
        { en: "May", kr: "5월", grade: 5, cat: "월" },
        { en: "June", kr: "6월", grade: 5, cat: "월" },
        { en: "July", kr: "7월", grade: 5, cat: "월" },
        { en: "August", kr: "8월", grade: 5, cat: "월" },
        { en: "September", kr: "9월", grade: 5, cat: "월" },
        { en: "October", kr: "10월", grade: 5, cat: "월" },
        { en: "November", kr: "11월", grade: 5, cat: "월" },
        { en: "December", kr: "12월", grade: 5, cat: "월" },
        { en: "first", kr: "첫 번째의", grade: 5, cat: "서수" },
        { en: "second", kr: "두 번째의", grade: 5, cat: "서수" },
        { en: "third", kr: "세 번째의", grade: 5, cat: "서수" },
        { en: "fourth", kr: "네 번째의", grade: 5, cat: "서수" },
        { en: "fifth", kr: "다섯 번째의", grade: 5, cat: "서수" },
        { en: "vacation", kr: "방학, 휴가", grade: 5, cat: "생활" },
        { en: "yesterday", kr: "어제", grade: 5, cat: "시간" },
        { en: "tomorrow", kr: "내일", grade: 5, cat: "시간" },
        { en: "weekend", kr: "주말", grade: 5, cat: "시간" },
        { en: "favorite", kr: "가장 좋아하는", grade: 5, cat: "감정" },
        { en: "delicious", kr: "아주 맛있는", grade: 5, cat: "상태" },
        { en: "exciting", kr: "신나는, 흥미진진한", grade: 5, cat: "상태" },
        { en: "interesting", kr: "흥미로운, 재미있는", grade: 5, cat: "상태" },
        { en: "difficult", kr: "어려운, 힘든", grade: 5, cat: "상태" },
        { en: "important", kr: "중요한", grade: 5, cat: "상태" },
        { en: "busy", kr: "바쁜", grade: 5, cat: "상태" },
        { en: "famous", kr: "유명한", grade: 5, cat: "상태" },
        { en: "special", kr: "특별한", grade: 5, cat: "상태" },
        { en: "wonderful", kr: "멋진, 훌륭한", grade: 5, cat: "상태" },
        { en: "health", kr: "건강", grade: 5, cat: "건강" },
        { en: "healthy", kr: "건강한", grade: 5, cat: "건강" },
        { en: "exercise", kr: "운동하다, 운동", grade: 5, cat: "건강" },
        { en: "medicine", kr: "약, 의학", grade: 5, cat: "건강" },
        { en: "headache", kr: "두통, 머리 아픔", grade: 5, cat: "건강" },
        { en: "stomachache", kr: "복통, 배 아픔", grade: 5, cat: "건강" },
        { en: "toothache", kr: "치통, 이 아픔", grade: 5, cat: "건강" },
        { en: "fever", kr: "열", grade: 5, cat: "건강" },
        { en: "cough", kr: "기침", grade: 5, cat: "건강" },
        { en: "rest", kr: "휴식, 쉬다", grade: 5, cat: "건강" },
        { en: "doctor", kr: "의사", grade: 5, cat: "직업" },
        { en: "nurse", kr: "간호사", grade: 5, cat: "직업" },
        { en: "pilot", kr: "조종사, 파일럿", grade: 5, cat: "직업" },
        { en: "police officer", kr: "경찰관", grade: 5, cat: "직업" },
        { en: "firefighter", kr: "소방관", grade: 5, cat: "직업" },
        { en: "scientist", kr: "과학자", grade: 5, cat: "직업" },
        { en: "chef", kr: "요리사", grade: 5, cat: "직업" },
        { en: "artist", kr: "예술가, 화가", grade: 5, cat: "직업" },
        { en: "singer", kr: "가수", grade: 5, cat: "직업" },
        { en: "actor", kr: "배우", grade: 5, cat: "직업" },
        { en: "visited", kr: "방문했다", grade: 5, cat: "과거" },
        { en: "watched", kr: "보았다", grade: 5, cat: "과거" },
        { en: "played", kr: "놀았다, 경기했다", grade: 5, cat: "과거" },
        { en: "went", kr: "갔다", grade: 5, cat: "과거" },
        { en: "ate", kr: "먹었다", grade: 5, cat: "과거" },
        { en: "had", kr: "가졌다, 먹었다", grade: 5, cat: "과거" },
        { en: "always", kr: "항상, 언제나", grade: 5, cat: "빈도" },
        { en: "usually", kr: "보통, 대개", grade: 5, cat: "빈도" },
        { en: "often", kr: "자주, 종종", grade: 5, cat: "빈도" },
        { en: "sometimes", kr: "때때로, 가끔", grade: 5, cat: "빈도" },
        { en: "never", kr: "결코 ~않다", grade: 5, cat: "빈도" },

        // ====================================================
        // [초등 6학년 수준] 진로/꿈, 비교급/최상급, 환경/지구, 세계 문화/여행, 인성/태도, 과학/기술 (약 75단어)
        // ====================================================
        { en: "dream", kr: "꿈, 희망", grade: 6, cat: "진로" },
        { en: "future", kr: "미래", grade: 6, cat: "시간" },
        { en: "astronaut", kr: "우주비행사", grade: 6, cat: "직업" },
        { en: "inventor", kr: "발명가", grade: 6, cat: "직업" },
        { en: "programmer", kr: "프로그래머", grade: 6, cat: "직업" },
        { en: "author", kr: "작가", grade: 6, cat: "직업" },
        { en: "reporter", kr: "기자", grade: 6, cat: "직업" },
        { en: "designer", kr: "디자이너", grade: 6, cat: "직업" },
        { en: "invent", kr: "발명하다", grade: 6, cat: "동작" },
        { en: "discover", kr: "발견하다", grade: 6, cat: "동작" },
        { en: "travel", kr: "여행하다, 여행", grade: 6, cat: "생활" },
        { en: "visit", kr: "방문하다", grade: 6, cat: "동작" },
        { en: "experience", kr: "경험, 경험하다", grade: 6, cat: "생활" },
        { en: "memory", kr: "기억, 추억", grade: 6, cat: "생활" },
        { en: "tradition", kr: "전통", grade: 6, cat: "문화" },
        { en: "culture", kr: "문화", grade: 6, cat: "문화" },
        { en: "festival", kr: "축제", grade: 6, cat: "문화" },
        { en: "country", kr: "나라, 국가", grade: 6, cat: "사회" },
        { en: "world", kr: "세계, 세상", grade: 6, cat: "사회" },
        { en: "peace", kr: "평화", grade: 6, cat: "사회" },
        { en: "international", kr: "국제적인", grade: 6, cat: "사회" },
        { en: "environment", kr: "환경", grade: 6, cat: "환경" },
        { en: "protect", kr: "보호하다, 지키다", grade: 6, cat: "환경" },
        { en: "recycle", kr: "재활용하다", grade: 6, cat: "환경" },
        { en: "earth", kr: "지구, 땅", grade: 6, cat: "환경" },
        { en: "nature", kr: "자연", grade: 6, cat: "자연" },
        { en: "pollution", kr: "오염", grade: 6, cat: "환경" },
        { en: "trash", kr: "쓰레기", grade: 6, cat: "환경" },
        { en: "waste", kr: "낭비하다, 쓰레기", grade: 6, cat: "환경" },
        { en: "plastic", kr: "플라스틱", grade: 6, cat: "환경" },
        { en: "energy", kr: "에너지", grade: 6, cat: "과학" },
        { en: "save", kr: "구하다, 절약하다", grade: 6, cat: "동작" },
        { en: "space", kr: "우주, 공간", grade: 6, cat: "과학" },
        { en: "planet", kr: "행성", grade: 6, cat: "과학" },
        { en: "robot", kr: "로봇", grade: 6, cat: "과학" },
        { en: "computer", kr: "컴퓨터", grade: 6, cat: "과학" },
        { en: "internet", kr: "인터넷", grade: 6, cat: "과학" },
        { en: "information", kr: "정보", grade: 6, cat: "사회" },
        { en: "communication", kr: "의사소통, 대화", grade: 6, cat: "사회" },
        { en: "promise", kr: "약속, 약속하다", grade: 6, cat: "인성" },
        { en: "honest", kr: "정직한", grade: 6, cat: "인성" },
        { en: "polite", kr: "예의 바른, 공손한", grade: 6, cat: "인성" },
        { en: "brave", kr: "용감한", grade: 6, cat: "인성" },
        { en: "kind", kr: "친절한, 종류", grade: 6, cat: "인성" },
        { en: "smart", kr: "똑똑한, 영리한", grade: 6, cat: "상태" },
        { en: "popular", kr: "인기 있는", grade: 6, cat: "상태" },
        { en: "different", kr: "다른", grade: 6, cat: "상태" },
        { en: "same", kr: "같은", grade: 6, cat: "상태" },
        { en: "safe", kr: "안전한", grade: 6, cat: "안전" },
        { en: "dangerous", kr: "위험한", grade: 6, cat: "안전" },
        { en: "rules", kr: "규칙들", grade: 6, cat: "사회" },
        { en: "taller", kr: "더 키가 큰", grade: 6, cat: "비교" },
        { en: "shorter", kr: "더 키가 작은", grade: 6, cat: "비교" },
        { en: "bigger", kr: "더 큰", grade: 6, cat: "비교" },
        { en: "smaller", kr: "더 작은", grade: 6, cat: "비교" },
        { en: "faster", kr: "더 빠른", grade: 6, cat: "비교" },
        { en: "slower", kr: "더 느린", grade: 6, cat: "비교" },
        { en: "stronger", kr: "더 힘센, 더 강한", grade: 6, cat: "비교" },
        { en: "heavier", kr: "더 무거운", grade: 6, cat: "비교" },
        { en: "better", kr: "더 좋은", grade: 6, cat: "비교" },
        { en: "worse", kr: "더 나쁜", grade: 6, cat: "비교" },
        { en: "more", kr: "더 많이", grade: 6, cat: "비교" },
        { en: "most", kr: "가장 많은", grade: 6, cat: "비교" },
        { en: "reason", kr: "이유, 까닭", grade: 6, cat: "이유" },
        { en: "because", kr: "~때문에, 왜냐하면", grade: 6, cat: "이유" },
        { en: "excited", kr: "신이 난, 흥분된", grade: 6, cat: "감정" },
        { en: "surprised", kr: "놀란", grade: 6, cat: "감정" },
        { en: "proud", kr: "자랑스러운", grade: 6, cat: "감정" },
        { en: "worried", kr: "걱정스러운", grade: 6, cat: "감정" },
        { en: "nervous", kr: "긴장한, 초조한", grade: 6, cat: "감정" }
    ];

    // ────────────────────────────────────────────────────────
    // 2. 내장 기본 문장 데이터베이스 (초등 3~6학년별 필수 회화 130+)
    // ────────────────────────────────────────────────────────
    const DEFAULT_SENTENCES = [
        // ====================================================
        // [초등 3학년 수준] 기초 인사, 이름/사물 묻기, 소유, 능력, 명령/요청 (34문장)
        // ====================================================
        { en: "What is your name?", kr: "너의 이름은 무엇이니?", grade: 3 },
        { en: "My name is Minsu.", kr: "내 이름은 민수야.", grade: 3 },
        { en: "Nice to meet you.", kr: "만나서 반가워.", grade: 3 },
        { en: "How are you today?", kr: "오늘 기분이 어떠니?", grade: 3 },
        { en: "I am happy.", kr: "나는 행복해.", grade: 3 },
        { en: "I am very fine.", kr: "나는 아주 잘 지내.", grade: 3 },
        { en: "What is this?", kr: "이것은 무엇이니?", grade: 3 },
        { en: "It is an apple.", kr: "그것은 사과야.", grade: 3 },
        { en: "What is that?", kr: "저것은 무엇이니?", grade: 3 },
        { en: "It is a big box.", kr: "그것은 큰 상자야.", grade: 3 },
        { en: "Do you have a pencil?", kr: "너 연필 가지고 있니?", grade: 3 },
        { en: "Yes, I do.", kr: "응, 가지고 있어.", grade: 3 },
        { en: "No, I don't.", kr: "아니, 안 가지고 있어.", grade: 3 },
        { en: "Here you are.", kr: "여기 있어.", grade: 3 },
        { en: "Thank you very much.", kr: "정말 고마워.", grade: 3 },
        { en: "You are welcome.", kr: "천만에.", grade: 3 },
        { en: "Can you swim?", kr: "너 수영할 수 있니?", grade: 3 },
        { en: "Yes, I can.", kr: "응, 할 수 있어.", grade: 3 },
        { en: "No, I cannot.", kr: "아니, 할 수 없어.", grade: 3 },
        { en: "I can dance well.", kr: "나는 춤을 잘 춰.", grade: 3 },
        { en: "I can jump high.", kr: "나는 높이 뛸 수 있어.", grade: 3 },
        { en: "Touch your nose.", kr: "너의 코를 만져봐.", grade: 3 },
        { en: "Touch your ears.", kr: "너의 귀를 만져봐.", grade: 3 },
        { en: "Touch your head.", kr: "너의 머리를 만져봐.", grade: 3 },
        { en: "Open the door, please.", kr: "문을 열어 주세요.", grade: 3 },
        { en: "Close the window.", kr: "창문을 닫아라.", grade: 3 },
        { en: "Sit down, please.", kr: "자리에 앉아 주세요.", grade: 3 },
        { en: "Stand up, please.", kr: "일어서 주세요.", grade: 3 },
        { en: "How many apples?", kr: "사과가 몇 개 있니?", grade: 3 },
        { en: "Three apples, please.", kr: "사과 세 개 주세요.", grade: 3 },
        { en: "I like green apples.", kr: "나는 초록색 사과를 좋아해.", grade: 3 },
        { en: "Who is he?", kr: "그는 누구니?", grade: 3 },
        { en: "He is my father.", kr: "그는 나의 아버지셔.", grade: 3 },
        { en: "Put on your coat.", kr: "너의 코트를 입어라.", grade: 3 },
        { en: "Take off your shoes.", kr: "너의 신발을 벗어라.", grade: 3 },

        // ====================================================
        // [초등 4학년 수준] 시간, 날씨, 길 안내, 규칙/금지, 주말 일과, 가격, 소유 (33문장)
        // ====================================================
        { en: "What time is it now?", kr: "지금 몇 시인가요?", grade: 4 },
        { en: "It is ten o'clock.", kr: "10시 정각이야.", grade: 4 },
        { en: "It is time for lunch.", kr: "점심 먹을 시간이야.", grade: 4 },
        { en: "It is time for bed.", kr: "잠잘 시간이야.", grade: 4 },
        { en: "It is time for school.", kr: "학교 갈 시간이야.", grade: 4 },
        { en: "How is the weather today?", kr: "오늘 날씨가 어때?", grade: 4 },
        { en: "It is sunny and warm.", kr: "날씨가 맑고 따뜻해.", grade: 4 },
        { en: "It is raining outside.", kr: "밖에 비가 오고 있어.", grade: 4 },
        { en: "It is snowy and cold.", kr: "눈이 오고 날씨가 추워.", grade: 4 },
        { en: "It is windy today.", kr: "오늘은 바람이 불어.", grade: 4 },
        { en: "Where is the library?", kr: "도서관이 어디에 있나요?", grade: 4 },
        { en: "It is on the second floor.", kr: "그것은 2층에 있어.", grade: 4 },
        { en: "Where is the restroom?", kr: "화장실이 어디에 있나요?", grade: 4 },
        { en: "Go straight and turn right.", kr: "직진해서 오른쪽으로 돌아.", grade: 4 },
        { en: "Go straight and turn left.", kr: "직진해서 왼쪽으로 돌아.", grade: 4 },
        { en: "Don't run in the hallway.", kr: "복도에서 뛰지 마라.", grade: 4 },
        { en: "Don't push your friends.", kr: "친구들을 밀지 마라.", grade: 4 },
        { en: "Don't touch that, please.", kr: "그것을 만지지 마세요.", grade: 4 },
        { en: "What do you do on weekends?", kr: "너는 주말에 무엇을 하니?", grade: 4 },
        { en: "I usually ride my bike.", kr: "나는 보통 자전거를 타.", grade: 4 },
        { en: "I like playing soccer.", kr: "나는 축구하는 것을 좋아해.", grade: 4 },
        { en: "She likes reading books.", kr: "그녀는 책 읽는 것을 좋아해.", grade: 4 },
        { en: "Let's play basketball together.", kr: "우리 함께 농구하자.", grade: 4 },
        { en: "Sounds like a good idea.", kr: "좋은 생각이야.", grade: 4 },
        { en: "How much is this ruler?", kr: "이 자는 얼마인가요?", grade: 4 },
        { en: "It is two dollars.", kr: "2달러입니다.", grade: 4 },
        { en: "Whose cap is this?", kr: "이 모자는 누구 것이니?", grade: 4 },
        { en: "It is my cap.", kr: "그것은 내 모자야.", grade: 4 },
        { en: "Whose jacket is that?", kr: "저 재킷은 누구 것이니?", grade: 4 },
        { en: "What are you doing now?", kr: "너는 지금 무엇을 하고 있니?", grade: 4 },
        { en: "I am drawing a picture.", kr: "나는 그림을 그리고 있어.", grade: 4 },
        { en: "I am washing my hands.", kr: "나는 손을 씻고 있어.", grade: 4 },
        { en: "Help yourself to some snacks.", kr: "간식을 마음껏 먹으렴.", grade: 4 },

        // ====================================================
        // [초등 5학년 수준] 과목, 생일/날짜, 과거 경험, 건강/증상, 방학, 빈도, 외모 (33문장)
        // ====================================================
        { en: "What grade are you in?", kr: "너는 몇 학년이니?", grade: 5 },
        { en: "I am in fifth grade.", kr: "나는 5학년이야.", grade: 5 },
        { en: "What is your favorite subject?", kr: "너가 가장 좋아하는 과목은 무엇이니?", grade: 5 },
        { en: "My favorite subject is science.", kr: "내가 가장 좋아하는 과목은 과학이야.", grade: 5 },
        { en: "I like English and art.", kr: "나는 영어와 미술을 좋아해.", grade: 5 },
        { en: "When is your birthday?", kr: "너의 생일은 언제니?", grade: 5 },
        { en: "My birthday is May fifth.", kr: "내 생일은 5월 5일이야.", grade: 5 },
        { en: "It is on October ninth.", kr: "그것은 10월 9일이야.", grade: 5 },
        { en: "What did you do yesterday?", kr: "너는 어제 무엇을 했니?", grade: 5 },
        { en: "I visited my grandparents.", kr: "나는 조부모님 댁을 방문했어.", grade: 5 },
        { en: "We played baseball in the park.", kr: "우리는 공원에서 야구를 했어.", grade: 5 },
        { en: "I watched an exciting movie.", kr: "나는 신나는 영화를 보았어.", grade: 5 },
        { en: "I ate pizza with my family.", kr: "나는 가족과 함께 피자를 먹었어.", grade: 5 },
        { en: "What is the matter with you?", kr: "어디가 아프니? 무슨 일이니?", grade: 5 },
        { en: "I have a bad headache.", kr: "머리가 몹시 아파요.", grade: 5 },
        { en: "I have a stomachache.", kr: "배가 아파요.", grade: 5 },
        { en: "I have a high fever.", kr: "열이 많이 나요.", grade: 5 },
        { en: "Take this medicine and rest.", kr: "이 약을 먹고 쉬세요.", grade: 5 },
        { en: "You should go see a doctor.", kr: "너는 병원에 가보는 것이 좋겠어.", grade: 5 },
        { en: "Where did you go on vacation?", kr: "방학 때 어디에 다녀왔니?", grade: 5 },
        { en: "I went to Jeju Island.", kr: "나는 제주도에 갔었어.", grade: 5 },
        { en: "It was very exciting.", kr: "정말 신나고 즐거웠어.", grade: 5 },
        { en: "How often do you exercise?", kr: "너는 얼마나 자주 운동하니?", grade: 5 },
        { en: "I exercise three times a week.", kr: "나는 일주일에 세 번 운동해.", grade: 5 },
        { en: "I brush my teeth three times.", kr: "나는 하루 세 번 이를 닦아.", grade: 5 },
        { en: "What does he look like?", kr: "그는 어떻게 생겼니?", grade: 5 },
        { en: "He is tall and handsome.", kr: "그는 키가 크고 잘생겼어.", grade: 5 },
        { en: "She has long black hair.", kr: "그녀는 긴 검은 머리를 가지고 있어.", grade: 5 },
        { en: "What would you like to eat?", kr: "무엇을 드시고 싶으신가요?", grade: 5 },
        { en: "I would like a chicken sandwich.", kr: "치킨 샌드위치를 먹고 싶어요.", grade: 5 },
        { en: "Can I take your order?", kr: "주문하시겠습니까?", grade: 5 },
        { en: "I usually wake up at seven.", kr: "나는 보통 7시에 일어나.", grade: 5 },
        { en: "I am looking for my bag.", kr: "나는 내 가방을 찾고 있어.", grade: 5 },

        // ====================================================
        // [초등 6학년 수준] 진로/꿈, 이유 묻기, 비교급, 환경 보호, 여행 경험, 제안, 규칙 (33문장)
        // ====================================================
        { en: "What grade are you in?", kr: "너는 몇 학년이니?", grade: 6 },
        { en: "I am in the sixth grade.", kr: "나는 6학년이야.", grade: 6 },
        { en: "What do you want to be in the future?", kr: "너는 미래에 무엇이 되고 싶니?", grade: 6 },
        { en: "I want to be a famous scientist.", kr: "나는 유명한 과학자가 되고 싶어.", grade: 6 },
        { en: "I want to be an astronaut.", kr: "나는 우주비행사가 되고 싶어.", grade: 6 },
        { en: "Why do you want to be a doctor?", kr: "너는 왜 의사가 되고 싶니?", grade: 6 },
        { en: "Because I want to help sick people.", kr: "아픈 사람들을 돕고 싶기 때문이야.", grade: 6 },
        { en: "Why are you so happy?", kr: "너 왜 그렇게 기분이 좋아?", grade: 6 },
        { en: "Because tomorrow is my birthday.", kr: "내일이 내 생일이기 때문이야.", grade: 6 },
        { en: "I am taller than my brother.", kr: "나는 내 남동생보다 키가 커.", grade: 6 },
        { en: "Cheetahs run faster than lions.", kr: "치타는 사자보다 더 빨리 달려.", grade: 6 },
        { en: "This dog is bigger than that cat.", kr: "이 개가 저 고양이보다 더 커.", grade: 6 },
        { en: "Which season do you like better?", kr: "너는 어떤 계절을 더 좋아하니?", grade: 6 },
        { en: "I like winter better than summer.", kr: "나는 여름보다 겨울을 더 좋아해.", grade: 6 },
        { en: "This book is more interesting than that one.", kr: "이 책이 저 책보다 더 재미있어.", grade: 6 },
        { en: "We should protect our environment.", kr: "우리는 환경을 보호해야 해.", grade: 6 },
        { en: "We should protect our earth.", kr: "우리는 지구를 보호해야 해.", grade: 6 },
        { en: "You should not waste water.", kr: "물을 낭비해서는 안 돼.", grade: 6 },
        { en: "Don't throw away trash here.", kr: "여기에 쓰레기를 버리지 마세요.", grade: 6 },
        { en: "We can recycle paper and plastic.", kr: "우리는 종이와 플라스틱을 재활용할 수 있어.", grade: 6 },
        { en: "Turn off the light when you leave.", kr: "나갈 때 전등을 끄세요.", grade: 6 },
        { en: "Have you ever been to Canada?", kr: "너 캐나다에 가본 적 있니?", grade: 6 },
        { en: "Yes, I have been there twice.", kr: "응, 거기에 두 번 가봤어.", grade: 6 },
        { en: "No, I have never been there.", kr: "아니, 한 번도 가본 적 없어.", grade: 6 },
        { en: "Could you please help me?", kr: "저를 좀 도와주시겠어요?", grade: 6 },
        { en: "Sure, what can I do for you?", kr: "물론이죠, 무엇을 도와드릴까요?", grade: 6 },
        { en: "Why don't we go to the museum?", kr: "우리 박물관에 가는 게 어때?", grade: 6 },
        { en: "Sounds like a great idea!", kr: "정말 좋은 생각이야!", grade: 6 },
        { en: "What will you do tomorrow?", kr: "너는 내일 무엇을 할 거니?", grade: 6 },
        { en: "I will join the music club.", kr: "나는 음악 동아리에 가입할 거야.", grade: 6 },
        { en: "You should follow the school rules.", kr: "너는 학교 규칙을 지켜야 해.", grade: 6 },
        { en: "It was the best day of my life.", kr: "내 인생에서 최고의 날이었어.", grade: 6 },
        { en: "I want to travel around the world.", kr: "나는 세계 일주를 여행하고 싶어.", grade: 6 }
    ];

    /**
     * WaurimalCurriculum 과 병합된 활성 단어 & 문장 목록 가져오기
     * @param {string|number} gradeFilter - '3', '4', '5', '6', '3-4', '5-6', 'all'
     */
    function getActiveDataset(gradeFilter) {
        let words = [...DEFAULT_WORDS];
        let sentences = [...DEFAULT_SENTENCES];

        if (window.WaurimalCurriculum && typeof window.WaurimalCurriculum.getAllWords === 'function') {
            try {
                const currWords = window.WaurimalCurriculum.getAllWords();
                if (Array.isArray(currWords) && currWords.length > 0) {
                    const existMap = new Set(words.map(w => w.en.toLowerCase().trim()));
                    currWords.forEach(cw => {
                        if (cw && cw.en && cw.kr && !existMap.has(cw.en.toLowerCase().trim())) {
                            words.push({
                                en: cw.en.trim(),
                                kr: cw.kr.trim(),
                                grade: Number(cw.grade) || 6,
                                cat: cw.cat || "교과서"
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
                                grade: Number(cs.grade) || 6
                            });
                            existSentMap.add(cs.en.toLowerCase().trim());
                        }
                    });
                }
            } catch (e) {
                console.warn("WaurimalCurriculum sync warning:", e);
            }
        }

        // 학년별 필터링 분기 (단일 학년: 3, 4, 5, 6 / 군별: 3-4, 5-6 / 전체: all)
        const filterKey = String(gradeFilter || 'all').trim();
        if (filterKey === '3' || filterKey === 'grade3') {
            words = words.filter(w => Number(w.grade) === 3);
            sentences = sentences.filter(s => Number(s.grade) === 3);
        } else if (filterKey === '4' || filterKey === 'grade4') {
            words = words.filter(w => Number(w.grade) === 4);
            sentences = sentences.filter(s => Number(s.grade) === 4);
        } else if (filterKey === '5' || filterKey === 'grade5') {
            words = words.filter(w => Number(w.grade) === 5);
            sentences = sentences.filter(s => Number(s.grade) === 5);
        } else if (filterKey === '6' || filterKey === 'grade6') {
            words = words.filter(w => Number(w.grade) === 6);
            sentences = sentences.filter(s => Number(s.grade) === 6);
        } else if (filterKey === '3-4') {
            words = words.filter(w => Number(w.grade) === 3 || Number(w.grade) === 4);
            sentences = sentences.filter(s => Number(s.grade) === 3 || Number(s.grade) === 4);
        } else if (filterKey === '5-6') {
            words = words.filter(w => Number(w.grade) === 5 || Number(w.grade) === 6);
            sentences = sentences.filter(s => Number(s.grade) === 5 || Number(s.grade) === 6);
        }

        // 데이터가 비정상적으로 비어있을 때 안전 대비책
        if (words.length === 0) words = [...DEFAULT_WORDS];
        if (sentences.length === 0) sentences = [...DEFAULT_SENTENCES];

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
     * 3학년, 4학년, 5학년, 6학년 완벽 분리 지원
     */
    function generateQuestion(options = {}) {
        if (typeof options === 'string' || typeof options === 'number') {
            options = { grade: options };
        }
        const gradeFilter = options.grade || 'all'; // '3', '4', '5', '6', '3-4', '5-6', 'all'
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
        let distractors = shuffleArray(words.filter(w => w.en !== target.en && w.kr !== target.kr)).slice(0, 3);
        if (distractors.length < 3) {
            const extra = shuffleArray(DEFAULT_WORDS.filter(w => w.en !== target.en && w.kr !== target.kr)).slice(0, 3 - distractors.length);
            distractors = distractors.concat(extra);
        }
        const choices = shuffleArray([target.kr, ...distractors.map(d => d.kr)]);

        return {
            type: 'word_to_kr',
            category: '단어 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
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
        let distractors = shuffleArray(words.filter(w => w.en !== target.en)).slice(0, 3);
        if (distractors.length < 3) {
            const extra = shuffleArray(DEFAULT_WORDS.filter(w => w.en !== target.en)).slice(0, 3 - distractors.length);
            distractors = distractors.concat(extra);
        }
        const choices = shuffleArray([target.en, ...distractors.map(d => d.en)]);

        return {
            type: 'kr_to_word',
            category: '단어 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
            prompt: '다음 뜻에 알맞은 영어 단어를 고르세요.',
            questionText: target.kr,
            subText: target.cat ? `[분류: ${target.cat}]` : '',
            ttsText: target.en,
            choices: choices,
            answer: target.en,
            answerIndex: choices.indexOf(target.en),
            explanation: `"${target.kr}"은(는) 영어로 "${target.en}"입니다.`
        };
    }

    // 3) 스펠링 빈칸 채우기
    function makeWordSpelling(words) {
        const candidateWords = words.filter(w => w.en.length >= 4 && !w.en.includes(' '));
        const target = (candidateWords.length > 0)
            ? candidateWords[Math.floor(Math.random() * candidateWords.length)]
            : words[0];

        const word = target.en;
        const blankIdx = Math.floor(Math.random() * (word.length - 2)) + 1;
        const answerLetter = word[blankIdx].toLowerCase();

        const displayArr = word.split('');
        displayArr[blankIdx] = '_';
        const maskedWord = displayArr.join(' ');

        const alphabet = 'abcdefghijklmnopqrstuvwxyz'.split('').filter(ch => ch !== answerLetter);
        const distractors = shuffleArray(alphabet).slice(0, 3);
        const choices = shuffleArray([answerLetter, ...distractors]);

        return {
            type: 'word_spelling',
            category: '스펠링 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
            prompt: '빈칸(_)에 들어갈 알맞은 알파벳을 고르세요.',
            questionText: maskedWord,
            subText: `뜻: ${target.kr}${target.cat ? ` [분류: ${target.cat}]` : ''}`,
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
        let distractors = shuffleArray(sentences.filter(s => s.en !== target.en && s.kr !== target.kr)).slice(0, 3);
        if (distractors.length < 3) {
            const extra = shuffleArray(DEFAULT_SENTENCES.filter(s => s.en !== target.en && s.kr !== target.kr)).slice(0, 3 - distractors.length);
            distractors = distractors.concat(extra);
        }
        const choices = shuffleArray([target.kr, ...distractors.map(d => d.kr)]);

        return {
            type: 'sent_to_kr',
            category: '문장 해석 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
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
        const target = sentences[Math.floor(Math.random() * sentences.length)];
        const cleanWords = target.en.split(/\s+/);

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

        const maskedWords = [...cleanWords];
        maskedWords[chosen.idx] = '_______' + (chosen.raw.match(/[^a-zA-Z]+$/) ? chosen.raw.match(/[^a-zA-Z]+$/)[0] : '');
        const maskedSentence = maskedWords.join(' ');

        let distractors = shuffleArray(words.filter(w => w.en.toLowerCase() !== answerWord.toLowerCase() && !w.en.includes(' ')))
            .slice(0, 3)
            .map(d => d.en);

        if (distractors.length < 3) {
            const extra = shuffleArray(DEFAULT_WORDS.filter(w => w.en.toLowerCase() !== answerWord.toLowerCase() && !w.en.includes(' ')))
                .slice(0, 3 - distractors.length)
                .map(d => d.en);
            distractors = distractors.concat(extra);
        }
        if (distractors.length < 3) {
            distractors = ['school', 'friend', 'time'];
        }

        const choices = shuffleArray([answerWord, ...distractors]);

        return {
            type: 'sent_blank',
            category: '문장 빈칸 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
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
        const candidateSentences = sentences.filter(s => {
            const count = s.en.split(/\s+/).length;
            return count >= 3 && count <= 7;
        });

        const target = (candidateSentences.length > 0)
            ? candidateSentences[Math.floor(Math.random() * candidateSentences.length)]
            : sentences[0];

        const originalTokens = target.en.split(/\s+/);
        let shuffledTokens = shuffleArray(originalTokens);
        if (shuffledTokens.join(' ') === originalTokens.join(' ') && originalTokens.length > 1) {
            shuffledTokens = [shuffledTokens[1], shuffledTokens[0], ...shuffledTokens.slice(2)];
        }

        return {
            type: 'sent_scramble',
            category: '어순 배열 퀴즈',
            grade: target.grade,
            gradeLabel: target.grade ? `${target.grade}학년` : '초등 종합',
            prompt: '단어들을 알맞은 순서대로 클릭하여 문장을 완성하세요.',
            questionText: target.kr,
            subText: '아래 단어 카드를 순서대로 눌러 문장을 완성해 보세요!',
            ttsText: target.en,
            tokens: shuffledTokens,
            answerSentence: target.en,
            answerTokens: originalTokens,
            explanation: `완성된 문장: "${target.en}" (${target.kr})`
        };
    }

    /**
     * 학년별 통계 조회 헬퍼
     */
    function getGradeStats() {
        const stats = {
            grade3: { words: 0, sentences: 0 },
            grade4: { words: 0, sentences: 0 },
            grade5: { words: 0, sentences: 0 },
            grade6: { words: 0, sentences: 0 },
            totalWords: DEFAULT_WORDS.length,
            totalSentences: DEFAULT_SENTENCES.length
        };
        DEFAULT_WORDS.forEach(w => {
            const g = Number(w.grade);
            if (g === 3) stats.grade3.words++;
            else if (g === 4) stats.grade4.words++;
            else if (g === 5) stats.grade5.words++;
            else if (g === 6) stats.grade6.words++;
        });
        DEFAULT_SENTENCES.forEach(s => {
            const g = Number(s.grade);
            if (g === 3) stats.grade3.sentences++;
            else if (g === 4) stats.grade4.sentences++;
            else if (g === 5) stats.grade5.sentences++;
            else if (g === 6) stats.grade6.sentences++;
        });
        return stats;
    }

    // 전역 노출 (브라우저 window 및 Node 환경 호환)
    const exportTarget = (typeof window !== 'undefined') ? window : global;
    exportTarget.YutnoriQuestions = {
        DEFAULT_WORDS,
        DEFAULT_SENTENCES,
        getActiveDataset,
        generateQuestion,
        getGradeStats
    };

})(typeof window !== 'undefined' ? window : global);
