/**
 * 스펠링 갤러그 (Spelling Galaga) - 단어 데이터베이스 및 관리 모듈
 * 초등 교육과정 연계 학년별/주제별 어휘 데이터, 와우리말 공통 교육과정(WaurimalCurriculum) 연동 및 교사 맞춤 단어
 */

const DEFAULT_VOCABULARY = {
  grade3: {
    name: "초등 3학년 필수",
    description: "기초 알파벳과 친숙한 3~4글자 기본 단어",
    words: [
      { word: "CAT", meaning: "고양이" },
      { word: "DOG", meaning: "개 / 강아지" },
      { word: "SUN", meaning: "태양 / 해" },
      { word: "BUS", meaning: "버스" },
      { word: "CUP", meaning: "컵" },
      { word: "PIG", meaning: "돼지" },
      { word: "RED", meaning: "빨간색" },
      { word: "BOX", meaning: "상자" },
      { word: "PEN", meaning: "펜" },
      { word: "EGG", meaning: "달걀" },
      { word: "BAG", meaning: "가방" },
      { word: "BOOK", meaning: "책" },
      { word: "FISH", meaning: "물고기" },
      { word: "BIRD", meaning: "새" },
      { word: "TREE", meaning: "나무" },
      { word: "STAR", meaning: "별" },
      { word: "BLUE", meaning: "파란색" },
      { word: "MILK", meaning: "우유" },
      { word: "DUCK", meaning: "오리" },
      { word: "BALL", meaning: "공" }
    ]
  },
  grade4: {
    name: "초등 4학년 필수",
    description: "자주 쓰이는 일상 사물과 동물 4~6글자 단어",
    words: [
      { word: "APPLE", meaning: "사과" },
      { word: "WATER", meaning: "물" },
      { word: "BREAD", meaning: "빵" },
      { word: "CLOCK", meaning: "시계" },
      { word: "PIANO", meaning: "피아노" },
      { word: "TIGER", meaning: "호랑이" },
      { word: "ZEBRA", meaning: "얼룩말" },
      { word: "GREEN", meaning: "초록색" },
      { word: "WHITE", meaning: "흰색" },
      { word: "HAPPY", meaning: "행복한" },
      { word: "SMILE", meaning: "미소 / 웃다" },
      { word: "TRAIN", meaning: "기차" },
      { word: "HOUSE", meaning: "집" },
      { word: "MOUSE", meaning: "쥐" },
      { word: "CANDY", meaning: "사탕" },
      { word: "RIVER", meaning: "강" },
      { word: "CLOUD", meaning: "구름" },
      { word: "HORSE", meaning: "말 (동물)" },
      { word: "PENCIL", meaning: "연필" },
      { word: "BANANA", meaning: "바나나" }
    ]
  },
  grade5: {
    name: "초등 5학년 필수",
    description: "학교생활과 자연, 직업 관련 5~7글자 단어",
    words: [
      { word: "SCHOOL", meaning: "학교" },
      { word: "FRIEND", meaning: "친구" },
      { word: "ANIMAL", meaning: "동물" },
      { word: "DOCTOR", meaning: "의사" },
      { word: "MONKEY", meaning: "원숭이" },
      { word: "RABBIT", meaning: "토끼" },
      { word: "SPRING", meaning: "봄" },
      { word: "SUMMER", meaning: "여름" },
      { word: "WINTER", meaning: "겨울" },
      { word: "GARDEN", meaning: "정원" },
      { word: "WINDOW", meaning: "창문" },
      { word: "FAMILY", meaning: "가족" },
      { word: "YELLOW", meaning: "노란색" },
      { word: "ORANGE", meaning: "오렌지 / 주황색" },
      { word: "SOCCER", meaning: "축구" },
      { word: "STREET", meaning: "거리 / 길" },
      { word: "SEASON", meaning: "계절" },
      { word: "PLANET", meaning: "행성" },
      { word: "ROCKET", meaning: "로켓" },
      { word: "GUITAR", meaning: "기타 (악기)" }
    ]
  },
  grade6: {
    name: "초등 6학년 필수",
    description: "추상적 개념과 과학, 사회 관련 6~9글자 단어",
    words: [
      { word: "BEAUTIFUL", meaning: "아름다운" },
      { word: "COMPUTER", meaning: "컴퓨터" },
      { word: "SCIENCE", meaning: "과학" },
      { word: "UNIVERSE", meaning: "우주" },
      { word: "ENGLISH", meaning: "영어" },
      { word: "QUESTION", meaning: "질문" },
      { word: "LIBRARY", meaning: "도서관" },
      { word: "TEACHER", meaning: "선생님" },
      { word: "TOMORROW", meaning: "내일" },
      { word: "DOLPHIN", meaning: "돌고래" },
      { word: "WEATHER", meaning: "날씨" },
      { word: "RAINBOW", meaning: "무지개" },
      { word: "CONCERT", meaning: "음악회 / 콘서트" },
      { word: "JOURNEY", meaning: "여행 / 여정" },
      { word: "SPECIAL", meaning: "특별한" },
      { word: "MORNING", meaning: "아침" },
      { word: "EVENING", meaning: "저녁" },
      { word: "BASEBALL", meaning: "야구" },
      { word: "TREASURE", meaning: "보물" },
      { word: "CHAMPION", meaning: "챔피언 / 우승자" }
    ]
  },
  animals: {
    name: "동물 탐험대",
    description: "다양한 동물 친구들의 영단어",
    words: [
      { word: "LION", meaning: "사자" },
      { word: "BEAR", meaning: "곰" },
      { word: "PENGUIN", meaning: "펭귄" },
      { word: "GIRAFFE", meaning: "기린" },
      { word: "ELEPHANT", meaning: "코끼리" },
      { word: "KANGAROO", meaning: "캥거루" },
      { word: "OCTOPUS", meaning: "문어" },
      { word: "TURTLE", meaning: "거북이" },
      { word: "BUTTERFLY", meaning: "나비" },
      { word: "HAMSTER", meaning: "햄스터" }
    ]
  },
  space: {
    name: "우주 모험 (스페셜)",
    description: "갤러그 우주 테마와 어울리는 천체와 우주 단어",
    words: [
      { word: "SPACE", meaning: "우주" },
      { word: "ROCKET", meaning: "로켓" },
      { word: "GALAXY", meaning: "은하" },
      { word: "COMET", meaning: "혜성" },
      { word: "METEOR", meaning: "유성 / 별똥별" },
      { word: "ASTRONAUT", meaning: "우주비행사" },
      { word: "ALIEN", meaning: "외계인" },
      { word: "JUPITER", meaning: "목성" },
      { word: "SATURN", meaning: "토성" },
      { word: "NEBULA", meaning: "성운" }
    ]
  }
};

class WordManager {
  constructor() {
    this.storageKey = 'spelling_galaga_custom_words_v1';
    this.categories = { ...DEFAULT_VOCABULARY };
    this.loadCustomWords();
    this.syncCurriculum();
  }

  // 와우리말 공통 교육과정 모듈(WaurimalCurriculum) 연동
  syncCurriculum() {
    try {
      if (typeof window !== 'undefined' && window.WaurimalCurriculum) {
        const lessons = window.WaurimalCurriculum.getAllLessons ? window.WaurimalCurriculum.getAllLessons() : null;
        if (lessons && Array.isArray(lessons)) {
          lessons.forEach(l => {
            if (l.words && l.words.length > 0) {
              const cleanedWords = l.words
                .map(w => ({
                  word: (w.en || '').trim().toUpperCase().replace(/[^A-Z]/g, ''),
                  meaning: (w.kr || '').trim()
                }))
                .filter(w => w.word.length >= 2 && w.meaning.length > 0);

              if (cleanedWords.length > 0) {
                const key = `curr_${l.code || l.grade + '_' + l.name}`;
                this.categories[key] = {
                  name: `📖 [교과] ${l.name}`,
                  description: `${l.grade || 0}학년 교과서 수록 필수 단어 (${cleanedWords.length}단어)`,
                  words: cleanedWords,
                  isCurriculum: true
                };
              }
            }
          });
        }
      }
    } catch (e) {
      console.warn("WaurimalCurriculum sync error:", e);
    }
  }

  loadCustomWords() {
    try {
      const saved = localStorage.getItem(this.storageKey);
      if (saved) {
        const parsed = JSON.parse(saved);
        if (Array.isArray(parsed) && parsed.length > 0) {
          this.categories.custom = {
            name: "✏️ 선생님 커스텀 단어",
            description: "직접 입력한 수업용 맞춤 단어 목록",
            words: parsed
          };
        }
      }
    } catch (e) {
      console.warn("Failed to load custom words from localStorage:", e);
    }
  }

  saveCustomWords(wordList) {
    const cleaned = wordList
      .map(item => ({
        word: (item.word || '').trim().toUpperCase().replace(/[^A-Z]/g, ''),
        meaning: (item.meaning || '').trim()
      }))
      .filter(item => item.word.length >= 2 && item.meaning.length > 0);

    if (cleaned.length === 0) {
      throw new Error("유효한 단어가 1개 이상 필요합니다 (단어 2글자 이상 영문, 뜻 필수).");
    }

    try {
      localStorage.setItem(this.storageKey, JSON.stringify(cleaned));
      this.categories.custom = {
        name: "✏️ 선생님 커스텀 단어",
        description: "직접 입력한 수업용 맞춤 단어 목록",
        words: cleaned
      };
      return cleaned;
    } catch (e) {
      console.error("Failed to save custom words:", e);
      throw e;
    }
  }

  getCustomWords() {
    return this.categories.custom ? this.categories.custom.words : [];
  }

  hasCustomWords() {
    return !!(this.categories.custom && this.categories.custom.words.length > 0);
  }

  clearCustomWords() {
    try {
      localStorage.removeItem(this.storageKey);
      delete this.categories.custom;
    } catch (e) {
      console.warn("Failed to clear custom words:", e);
    }
  }

  getCategoryList() {
    this.syncCurriculum(); // 최신 교육과정 반영
    return Object.keys(this.categories).map(key => ({
      key,
      name: this.categories[key].name,
      description: this.categories[key].description,
      count: this.categories[key].words.length,
      isCurriculum: !!this.categories[key].isCurriculum
    }));
  }

  getWordsForCategory(categoryKey) {
    this.syncCurriculum();
    const cat = this.categories[categoryKey] || this.categories.grade4;
    // Fisher-Yates 셔플 복사본 반환
    const list = [...cat.words];
    for (let i = list.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [list[i], list[j]] = [list[j], list[i]];
    }
    return list;
  }
}

window.WordManager = WordManager;
