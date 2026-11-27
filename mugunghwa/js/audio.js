/**
 * 무궁화 꽃이 피었습니다: 영어 런 - 전용 오디오 & 음성(TTS) 엔진
 * Web Audio API 신디사이저 + Web Speech Synthesis
 */

class MugunghwaAudio {
    constructor() {
        this.ctx = null;
        this.isMuted = false;
        this.ttsAvailable = typeof window !== 'undefined' && 'speechSynthesis' in window;
        this.koreanVoice = null;
        this.englishVoice = null;
        this.chantTimerId = null;
        this.syllableTimerIds = [];

        if (this.ttsAvailable) {
            this.initVoices();
            if (window.speechSynthesis.onvoiceschanged !== undefined) {
                window.speechSynthesis.onvoiceschanged = () => this.initVoices();
            }
        }
    }

    init() {
        if (!this.ctx) {
            const AudioCtx = window.AudioContext || window.webkitAudioContext;
            if (AudioCtx) {
                this.ctx = new AudioCtx();
            }
        }
        if (this.ctx && this.ctx.state === 'suspended') {
            this.ctx.resume();
        }
    }

    initVoices() {
        try {
            const voices = window.speechSynthesis.getVoices();
            this.koreanVoice = voices.find(v => v.lang.startsWith('ko')) || null;
            this.englishVoice = voices.find(v => v.lang === 'en-US' || v.lang.startsWith('en')) || null;
        } catch (e) {}
    }

    toggleMute() {
        this.isMuted = !this.isMuted;
        if (this.isMuted && this.ttsAvailable) {
            window.speechSynthesis.cancel();
        }
        return this.isMuted;
    }

    // 1. 앞으로 전진 발걸음 소리
    playStep() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'triangle';
            osc.frequency.setValueAtTime(320, now);
            osc.frequency.exponentialRampToValueAtTime(140, now + 0.08);

            gain.gain.setValueAtTime(0.2, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.09);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.1);
        } catch (e) {}
    }

    // 2. 터보 부스터 가속음 (슝~!)
    playBoost() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'sawtooth';
            osc.frequency.setValueAtTime(250, now);
            osc.frequency.exponentialRampToValueAtTime(950, now + 0.25);

            gain.gain.setValueAtTime(0.25, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.28);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.3);
        } catch (e) {}
    }

    // 3. 아이템 획득 챠링~ 소리
    playItemGet() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const notes = [587.33, 739.99, 880.00, 1174.66]; // D5, F#5, A5, D6
            notes.forEach((freq, idx) => {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                osc.type = 'sine';
                osc.frequency.setValueAtTime(freq, now + idx * 0.05);

                gain.gain.setValueAtTime(0.2, now + idx * 0.05);
                gain.gain.exponentialRampToValueAtTime(0.001, now + idx * 0.05 + 0.22);

                osc.connect(gain);
                gain.connect(this.ctx.destination);
                osc.start(now + idx * 0.05);
                osc.stop(now + idx * 0.05 + 0.24);
            });
        } catch (e) {}
    }

    // 4. 쉴드 방어 팅~ 소리
    playShieldDefend() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'sine';
            osc.frequency.setValueAtTime(987.77, now);
            osc.frequency.exponentialRampToValueAtTime(440, now + 0.22);

            gain.gain.setValueAtTime(0.35, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.24);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.25);
        } catch (e) {}
    }

    // 5. 오답 / 후퇴 쿵! 추락음
    playWrongFall() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'sawtooth';
            osc.frequency.setValueAtTime(400, now);
            osc.frequency.exponentialRampToValueAtTime(90, now + 0.35);

            gain.gain.setValueAtTime(0.3, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.38);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.4);
        } catch (e) {}
    }

    // 6. 술래 뒤돌아볼 때 빨간불 레이저 스캔 경고음 (삐-익!)
    playTagScan() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'square';
            osc.frequency.setValueAtTime(880, now);
            osc.frequency.setValueAtTime(1760, now + 0.08);

            gain.gain.setValueAtTime(0.2, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.2);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.22);
        } catch (e) {}
    }

    // 7. 골인 팡파레 (Victory Fanfare!)
    playGoal() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const melody = [
                { f: 523.25, d: 0.12 }, // C5
                { f: 659.25, d: 0.12 }, // E5
                { f: 783.99, d: 0.12 }, // G5
                { f: 1046.50, d: 0.35 } // C6
            ];
            let offset = 0;
            melody.forEach(m => {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                osc.type = 'triangle';
                osc.frequency.setValueAtTime(m.f, now + offset);

                gain.gain.setValueAtTime(0.3, now + offset);
                gain.gain.exponentialRampToValueAtTime(0.001, now + offset + m.d);

                osc.connect(gain);
                gain.connect(this.ctx.destination);
                osc.start(now + offset);
                osc.stop(now + offset + m.d + 0.02);
                offset += m.d;
            });
        } catch (e) {}
    }

    // 8. 카운트다운 째깍 비프
    playCountdown(isLast = false) {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();
            osc.type = 'sine';
            osc.frequency.setValueAtTime(isLast ? 880 : 440, now);

            gain.gain.setValueAtTime(0.15, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.1);

            osc.connect(gain);
            gain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.12);
        } catch (e) {}
    }

    // 9. 술래 TTS 구호 외치기 ("무궁화 꽃이 피었습니다" / 또는 "Red light, green light, one two three!")
    speakChant(text = '무궁화 꽃이 피었습니다!', rate = 1.0, callback = null) {
        if (this.chantTimerId) {
            clearTimeout(this.chantTimerId);
            this.chantTimerId = null;
        }
        if (this.syllableTimerIds && this.syllableTimerIds.length > 0) {
            this.syllableTimerIds.forEach(id => clearTimeout(id));
            this.syllableTimerIds = [];
        }

        const safeRate = Math.min(1.05, Math.max(0.85, rate));
        const totalDurationMs = Math.floor(8200 / safeRate);
        const syllableCount = 10;
        const sylInterval = Math.floor(totalDurationMs / syllableCount);

        // 10음절 글자 게이지 단계별 타이머 등록
        for (let i = 0; i < syllableCount; i++) {
            const tid = setTimeout(() => {
                if (typeof window !== 'undefined' && window.onChantSyllable) {
                    window.onChantSyllable(i, syllableCount);
                }
            }, i * sylInterval);
            this.syllableTimerIds.push(tid);
        }

        if (this.isMuted || !this.ttsAvailable) {
            if (callback) {
                this.chantTimerId = setTimeout(() => {
                    this.chantTimerId = null;
                    callback();
                }, totalDurationMs);
            }
            return;
        }
        try {
            window.speechSynthesis.cancel();
            const utter = new SpeechSynthesisUtterance(text);
            utter.rate = safeRate; // 편안한 발음 속도
            utter.pitch = 1.05;

            if (text.includes('flower') || text.includes('light') || text.includes('one')) {
                if (this.englishVoice) utter.voice = this.englishVoice;
                utter.lang = 'en-US';
            } else {
                if (this.koreanVoice) utter.voice = this.koreanVoice;
                utter.lang = 'ko-KR';
            }

            if (callback) {
                let hasFired = false;
                const safeCb = () => {
                    if (this.chantTimerId) {
                        clearTimeout(this.chantTimerId);
                        this.chantTimerId = null;
                    }
                    if (this.syllableTimerIds && this.syllableTimerIds.length > 0) {
                        this.syllableTimerIds.forEach(id => clearTimeout(id));
                        this.syllableTimerIds = [];
                    }
                    if (!hasFired) {
                        hasFired = true;
                        callback();
                    }
                };

                // 음성 구호 발화 후 약 5.5초 추가 여유를 주어 총 문제 풀이 골든타임을 약 8초로 여유롭게 제공
                const postDelay = Math.max(1000, totalDurationMs - 2200);
                utter.onend = () => {
                    this.chantTimerId = setTimeout(safeCb, postDelay);
                };
                utter.onerror = safeCb;
                // 안전 타이머: 브라우저가 음성 이벤트를 놓치더라도 턴이 멈추지 않도록 보장
                this.chantTimerId = setTimeout(safeCb, totalDurationMs + 500);
            }
            window.speechSynthesis.speak(utter);
        } catch (e) {
            if (callback) callback();
        }
    }

    // 10. 단어 발음 들려주기
    speakWord(word) {
        if (this.isMuted || !this.ttsAvailable || !word) return;
        try {
            const utter = new SpeechSynthesisUtterance(word);
            utter.lang = 'en-US';
            utter.rate = 0.9;
            if (this.englishVoice) utter.voice = this.englishVoice;
            window.speechSynthesis.speak(utter);
        } catch (e) {}
    }

    // 11. 게임 종료 / 일시정지 시 모든 음성 및 예약 타이머 즉시 중단
    stopAll() {
        if (this.chantTimerId) {
            clearTimeout(this.chantTimerId);
            this.chantTimerId = null;
        }
        if (this.syllableTimerIds && this.syllableTimerIds.length > 0) {
            this.syllableTimerIds.forEach(id => clearTimeout(id));
            this.syllableTimerIds = [];
        }
        try {
            if (typeof window !== 'undefined' && window.speechSynthesis) {
                window.speechSynthesis.cancel();
            }
        } catch (e) {}
    }
}

window.MugunghwaAudio = new MugunghwaAudio();
