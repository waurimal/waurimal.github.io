/**
 * 무궁화 꽃이 피었습니다: 영어 런 - 초등 3~6학년 영어 퀴즈 엔진
 * 2022 개정 교육과정 연동 및 다양한 4지선다 문제 동적 생성
 */

class MugunghwaQuizManager {
    constructor() {
        this.selectedGrade = 'all'; // 'all', 3, 4, 5, 6
        this.fallbackWords = [
            // 3학년 기본
            { en: 'run', kr: '달리다', grade: 3 },
            { en: 'jump', kr: '뛰다, 점프하다', grade: 3 },
            { en: 'stop', kr: '멈추다', grade: 3 },
            { en: 'fast', kr: '빠른', grade: 3 },
            { en: 'friend', kr: '친구', grade: 3 },
            { en: 'apple', kr: '사과', grade: 3 },
            { en: 'book', kr: '책', grade: 3 },
            { en: 'pencil', kr: '연필', grade: 3 },
            { en: 'star', kr: '별', grade: 3 },
            { en: 'flower', kr: '꽃', grade: 3 },
            // 4학년
            { en: 'green', kr: '초록색', grade: 4 },
            { en: 'red', kr: '빨간색', grade: 4 },
            { en: 'light', kr: '빛, 불빛', grade: 4 },
            { en: 'time', kr: '시간', grade: 4 },
            { en: 'line', kr: '선, 줄', grade: 4 },
            { en: 'finish', kr: '끝내다, 골인', grade: 4 },
            { en: 'start', kr: '시작하다', grade: 4 },
            { en: 'move', kr: '움직이다', grade: 4 },
            { en: 'ready', kr: '준비된', grade: 4 },
            { en: 'happy', kr: '행복한', grade: 4 },
            // 5학년
            { en: 'together', kr: '함께', grade: 5 },
            { en: 'shield', kr: '방패, 보호막', grade: 5 },
            { en: 'speed', kr: '속도', grade: 5 },
            { en: 'arrive', kr: '도착하다', grade: 5 },
            { en: 'danger', kr: '위험', grade: 5 },
            { en: 'safe', kr: '안전한', grade: 5 },
            { en: 'listen', kr: '듣다', grade: 5 },
            { en: 'watch', kr: '지켜보다', grade: 5 },
            { en: 'chance', kr: '기회', grade: 5 },
            { en: 'energy', kr: '에너지', grade: 5 },
            // 6학년
            { en: 'survive', kr: '살아남다', grade: 6 },
            { en: 'challenge', kr: '도전', grade: 6 },
            { en: 'victory', kr: '승리', grade: 6 },
            { en: 'distance', kr: '거리', grade: 6 },
            { en: 'goal', kr: '목표, 골인', grade: 6 },
            { en: 'protect', kr: '보호하다', grade: 6 },
            { en: 'position', kr: '위치', grade: 6 },
            { en: 'secret', kr: '비밀', grade: 6 },
            { en: 'champion', kr: '챔피언, 1등', grade: 6 },
            { en: 'cooperation', kr: '협동', grade: 6 }
        ];

        this.fallbackSentences = [
            { en: "Don't move!", kr: "움직이지 마!", grade: 4 },
            { en: "Red light, stop!", kr: "빨간불, 멈춰!", grade: 3 },
            { en: "Green light, go!", kr: "초록불, 출발!", grade: 3 },
            { en: "Run to the goal line.", kr: "골인지점으로 달려라.", grade: 5 },
            { en: "We did it together!", kr: "우리가 함께 해냈어!", grade: 5 },
            { en: "Hurry up, time is ticking.", kr: "서둘러, 시간이 가고 있어.", grade: 6 },
            { en: "Look at the tagger.", kr: "술래를 쳐다봐.", grade: 4 },
            { en: "You can make it!", kr: "넌 해낼 수 있어!", grade: 4 }
        ];
    }

    setGrade(grade) {
        this.selectedGrade = grade;
    }

    getWordPool() {
        if (typeof window !== 'undefined' && window.WaurimalCurriculum) {
            let words = [];
            if (!this.selectedGrade || this.selectedGrade === 'all') {
                words = window.WaurimalCurriculum.getAllWords() || [];
            } else {
                const lessons = window.WaurimalCurriculum.getLessonsByGrade(Number(this.selectedGrade)) || [];
                lessons.forEach(l => {
                    if (l && Array.isArray(l.words)) {
                        words = words.concat(l.words.map(w => ({ ...w, grade: l.grade })));
                    }
                });
            }
            if (words && words.length >= 6) {
                return words;
            }
        }
        if (this.selectedGrade !== 'all') {
            const filtered = this.fallbackWords.filter(w => w.grade === Number(this.selectedGrade));
            if (filtered.length >= 4) return filtered;
        }
        return this.fallbackWords;
    }

    getSentencePool() {
        if (typeof window !== 'undefined' && window.WaurimalCurriculum) {
            let sentences = [];
            if (!this.selectedGrade || this.selectedGrade === 'all') {
                sentences = window.WaurimalCurriculum.getAllSentences() || [];
            } else {
                const lessons = window.WaurimalCurriculum.getLessonsByGrade(Number(this.selectedGrade)) || [];
                lessons.forEach(l => {
                    if (l && Array.isArray(l.sentences)) {
                        sentences = sentences.concat(l.sentences.map(s => (
                            typeof s === 'string' ? { en: s, kr: '', grade: l.grade } : { ...s, grade: l.grade }
                        )));
                    }
                });
            }
            if (sentences && sentences.length >= 4) {
                return sentences;
            }
        }
        return this.fallbackSentences;
    }

    // 4지선다 퀴즈 문제 1개 생성
    generateQuestion(options = {}) {
        const words = this.getWordPool();
        const sentences = this.getSentencePool();

        // 80% 단어 퀴즈, 20% 실생활 문장 퀴즈
        const isSentence = sentences.length >= 4 && Math.random() < 0.20;
        // 단어 퀴즈일 때 40% 확률로 [한글 뜻 보고 영단어 맞히기] 역방향 출제
        const isReverse = !isSentence && Math.random() < 0.40;

        let questionText = '';
        let promptText = '';
        let targetItem = null;
        let choices = [];
        let ttsWord = '';

        if (isSentence) {
            targetItem = sentences[Math.floor(Math.random() * sentences.length)];
            questionText = targetItem.en;
            promptText = "다음 문장의 알맞은 우리말 뜻을 고르세요!";
            ttsWord = targetItem.en;

            const distractors = [];
            const pool = sentences.filter(s => s.en !== targetItem.en);
            while (distractors.length < 3 && pool.length > 0) {
                const rIdx = Math.floor(Math.random() * pool.length);
                distractors.push(pool.splice(rIdx, 1)[0]);
            }

            choices = [
                { text: targetItem.kr || targetItem.en, isCorrect: true },
                ...distractors.map(d => ({ text: d.kr || d.en, isCorrect: false }))
            ];
        } else if (isReverse) {
            targetItem = words[Math.floor(Math.random() * words.length)];
            questionText = targetItem.kr;
            promptText = "다음 우리말 뜻에 알맞은 영어 낱말은?";
            ttsWord = targetItem.en;

            const distractors = [];
            const pool = words.filter(w => w.en.toLowerCase() !== targetItem.en.toLowerCase());
            while (distractors.length < 3 && pool.length > 0) {
                const rIdx = Math.floor(Math.random() * pool.length);
                distractors.push(pool.splice(rIdx, 1)[0]);
            }

            choices = [
                { text: targetItem.en, isCorrect: true },
                ...distractors.map(d => ({ text: d.en, isCorrect: false }))
            ];
        } else {
            targetItem = words[Math.floor(Math.random() * words.length)];
            questionText = targetItem.en;
            promptText = "다음 영어 낱말의 올바른 우리말 뜻을 고르세요!";
            ttsWord = targetItem.en;

            const distractors = [];
            const pool = words.filter(w => w.en.toLowerCase() !== targetItem.en.toLowerCase());
            while (distractors.length < 3 && pool.length > 0) {
                const rIdx = Math.floor(Math.random() * pool.length);
                distractors.push(pool.splice(rIdx, 1)[0]);
            }

            choices = [
                { text: targetItem.kr, isCorrect: true },
                ...distractors.map(d => ({ text: d.kr, isCorrect: false }))
            ];
        }

        // 선택지 랜덤 셔플
        for (let i = choices.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [choices[i], choices[j]] = [choices[j], choices[i]];
        }

        return {
            id: 'q_' + Math.random().toString(36).substring(2, 9),
            prompt: promptText,
            question: questionText,
            ttsWord: ttsWord,
            targetItem: targetItem,
            choices: choices
        };
    }
}

window.MugunghwaQuizManager = new MugunghwaQuizManager();
