/**
 * 점프맨 게임 - 부활 영어 퀴즈 시스템 (Jumpman English Revive Quiz)
 * 초등학교 3~6학년 교육과정 기반 어휘/문장 연동 및 3회 부활 찬스 지원
 */

class JumpmanQuizManager {
    constructor() {
        this.selectedGrade = 'all'; // 'all', 3, 4, 5, 6
        this.selectedLesson = 'all';
        this.currentQuestion = null;
        this.fallbackWords = [
            { en: 'jump', kr: '뛰다, 점프하다', grade: 3 },
            { en: 'run', kr: '달리다', grade: 3 },
            { en: 'ball', kr: '공', grade: 3 },
            { en: 'fast', kr: '빠른', grade: 3 },
            { en: 'high', kr: '높은', grade: 4 },
            { en: 'star', kr: '별', grade: 3 },
            { en: 'shield', kr: '방패, 보호막', grade: 5 },
            { en: 'heart', kr: '하트, 심장', grade: 3 },
            { en: 'danger', kr: '위험', grade: 5 },
            { en: 'chance', kr: '기회, 찬스', grade: 4 },
            { en: 'survive', kr: '살아남다', grade: 6 },
            { en: 'speed', kr: '속도', grade: 5 },
            { en: 'escape', kr: '탈출하다', grade: 5 },
            { en: 'energy', kr: '에너지, 힘', grade: 4 },
            { en: 'power', kr: '힘, 파워', grade: 4 },
            { en: 'obstacle', kr: '장애물', grade: 6 },
            { en: 'monster', kr: '괴물, 몬스터', grade: 4 },
            { en: 'hero', kr: '영웅', grade: 4 },
            { en: 'friend', kr: '친구', grade: 3 },
            { en: 'protect', kr: '보호하다', grade: 6 }
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
            if (words && words.length >= 4) {
                return words;
            }
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
                        sentences = sentences.concat(l.sentences.map(s => (typeof s === 'string' ? { en: s, kr: '', grade: l.grade } : { ...s, grade: l.grade })));
                    }
                });
            }
            if (sentences && sentences.length >= 4) {
                return sentences;
            }
        }
        return [];
    }

    // 부활 퀴즈 1문제 무작위 생성
    generateQuestion() {
        const words = this.getWordPool();
        const sentences = this.getSentencePool();

        // 80% 확률로 단어 퀴즈, 20% 확률로 문장 퀴즈(문장이 있을 때)
        const useSentence = sentences.length >= 4 && Math.random() < 0.25;

        if (useSentence) {
            const target = sentences[Math.floor(Math.random() * sentences.length)];
            const distractors = [];
            const tempPool = sentences.filter(s => s.en !== target.en);
            while (distractors.length < 3 && tempPool.length > 0) {
                const randIdx = Math.floor(Math.random() * tempPool.length);
                distractors.push(tempPool.splice(randIdx, 1)[0]);
            }

            const choices = [
                { text: target.kr || target.en, isCorrect: true },
                ...distractors.map(d => ({ text: d.kr || d.en, isCorrect: false }))
            ];
            this.shuffle(choices);

            this.currentQuestion = {
                type: 'sentence',
                prompt: '다음 영어 문장의 알맞은 뜻을 고르세요!',
                questionText: target.en,
                ttsText: target.en,
                answer: target.kr || target.en,
                choices: choices
            };
        } else {
            const target = words[Math.floor(Math.random() * words.length)];
            const distractors = [];
            const tempPool = words.filter(w => w.en !== target.en);
            while (distractors.length < 3 && tempPool.length > 0) {
                const randIdx = Math.floor(Math.random() * tempPool.length);
                distractors.push(tempPool.splice(randIdx, 1)[0]);
            }

            const choices = [
                { text: target.kr, isCorrect: true },
                ...distractors.map(d => ({ text: d.kr, isCorrect: false }))
            ];
            this.shuffle(choices);

            this.currentQuestion = {
                type: 'word',
                prompt: '다음 영어 단어의 알맞은 뜻을 고르세요!',
                questionText: target.en,
                ttsText: target.en,
                answer: target.kr,
                choices: choices
            };
        }

        return this.currentQuestion;
    }

    shuffle(array) {
        for (let i = array.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
    }
}

window.JumpmanQuiz = new JumpmanQuizManager();
