/**
 * 점프맨 게임 (Jumpman Game) - 8-Bit Web Audio API 음향 및 TTS 음성 모듈
 * 외부 오디오 파일 다운로드 없이 100% 브라우저 자체 합성으로 즉각 재생
 */

class JumpmanAudio {
    constructor() {
        this.ctx = null;
        this.isMuted = false;
        this.bgmPlaying = false;
        this.bgmTimer = null;
        this.bgmStep = 0;
        this.ttsAvailable = typeof window !== 'undefined' && 'speechSynthesis' in window;
        this.englishVoice = null;
        this.koreanVoice = null;

        if (this.ttsAvailable) {
            this.initVoices();
        }
    }

    init() {
        if (!this.ctx) {
            const AudioContext = window.AudioContext || window.webkitAudioContext;
            if (AudioContext) {
                this.ctx = new AudioContext();
            }
        }
        if (this.ctx && this.ctx.state === 'suspended') {
            this.ctx.resume();
        }
    }

    initVoices() {
        const pickVoice = () => {
            const voices = window.speechSynthesis.getVoices();
            this.englishVoice = voices.find(v => v.lang.startsWith('en-US')) ||
                                voices.find(v => v.lang.startsWith('en')) ||
                                voices[0];
            this.koreanVoice = voices.find(v => v.lang.startsWith('ko')) || null;
        };
        pickVoice();
        if (window.speechSynthesis.onvoiceschanged !== undefined) {
            window.speechSynthesis.onvoiceschanged = pickVoice;
        }
    }

    toggleMute() {
        this.isMuted = !this.isMuted;
        if (this.isMuted && this.bgmPlaying) {
            this.stopBgm();
        } else if (!this.isMuted && !this.bgmPlaying) {
            this.startBgm();
        }
        return this.isMuted;
    }

    // 1단 점프음 (부드러운 주파수 상승 푱~)
    playJump() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();

            osc.type = 'sine';
            osc.frequency.setValueAtTime(180, now);
            osc.frequency.exponentialRampToValueAtTime(420, now + 0.12);

            gain.gain.setValueAtTime(0.3, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.14);

            osc.connect(gain);
            gain.connect(this.ctx.destination);

            osc.start(now);
            osc.stop(now + 0.15);
        } catch (e) {}
    }

    // 2단 점프음 (더 높은 옥타브의 통통 튀는 음)
    playDoubleJump() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();

            osc.type = 'triangle';
            osc.frequency.setValueAtTime(320, now);
            osc.frequency.exponentialRampToValueAtTime(680, now + 0.15);

            gain.gain.setValueAtTime(0.35, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.18);

            osc.connect(gain);
            gain.connect(this.ctx.destination);

            osc.start(now);
            osc.stop(now + 0.19);
        } catch (e) {}
    }

    // 코인 획득음 (경쾌한 2화음 띵링~)
    playCoin() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc1 = this.ctx.createOscillator();
            const osc2 = this.ctx.createOscillator();
            const gain = this.ctx.createGain();

            osc1.type = 'sine';
            osc2.type = 'sine';

            osc1.frequency.setValueAtTime(987.77, now); // B5
            osc1.frequency.setValueAtTime(1318.51, now + 0.08); // E6

            osc2.frequency.setValueAtTime(1318.51, now);
            osc2.frequency.setValueAtTime(1760.00, now + 0.08); // A6

            gain.gain.setValueAtTime(0.25, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.28);

            osc1.connect(gain);
            osc2.connect(gain);
            gain.connect(this.ctx.destination);

            osc1.start(now);
            osc2.start(now);
            osc1.stop(now + 0.3);
            osc2.stop(now + 0.3);
        } catch (e) {}
    }

    // 충돌 / 타격음 (쿵! 노이즈 버스트)
    playCrash() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            // 노이즈 버퍼
            const bufferSize = this.ctx.sampleRate * 0.2;
            const buffer = this.ctx.createBuffer(1, bufferSize, this.ctx.sampleRate);
            const data = buffer.getChannelData(0);
            for (let i = 0; i < bufferSize; i++) {
                data[i] = Math.random() * 2 - 1;
            }
            const noise = this.ctx.createBufferSource();
            noise.buffer = buffer;

            const filter = this.ctx.createBiquadFilter();
            filter.type = 'lowpass';
            filter.frequency.setValueAtTime(800, now);
            filter.frequency.exponentialRampToValueAtTime(100, now + 0.2);

            const gain = this.ctx.createGain();
            gain.gain.setValueAtTime(0.5, now);
            gain.gain.exponentialRampToValueAtTime(0.01, now + 0.25);

            noise.connect(filter);
            filter.connect(gain);
            gain.connect(this.ctx.destination);

            noise.start(now);
            noise.stop(now + 0.26);

            // 저음 충격 펄스
            const osc = this.ctx.createOscillator();
            const oscGain = this.ctx.createGain();
            osc.type = 'sawtooth';
            osc.frequency.setValueAtTime(120, now);
            osc.frequency.exponentialRampToValueAtTime(40, now + 0.2);
            oscGain.gain.setValueAtTime(0.4, now);
            oscGain.gain.exponentialRampToValueAtTime(0.001, now + 0.25);
            osc.connect(oscGain);
            oscGain.connect(this.ctx.destination);
            osc.start(now);
            osc.stop(now + 0.26);
        } catch (e) {}
    }

    // 퀴즈 정답 / 부활 성공음 (따라라란~ 경쾌한 팡파르)
    playCorrect() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const notes = [523.25, 659.25, 783.99, 1046.50]; // C5, E5, G5, C6
            const now = this.ctx.currentTime;

            notes.forEach((freq, idx) => {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                const startTime = now + idx * 0.08;

                osc.type = 'triangle';
                osc.frequency.setValueAtTime(freq, startTime);

                gain.gain.setValueAtTime(0.28, startTime);
                gain.gain.exponentialRampToValueAtTime(0.001, startTime + 0.25);

                osc.connect(gain);
                gain.connect(this.ctx.destination);

                osc.start(startTime);
                osc.stop(startTime + 0.26);
            });
        } catch (e) {}
    }

    // 퀴즈 오답음 (삐-익 땡!)
    playWrong() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();

            osc.type = 'sawtooth';
            osc.frequency.setValueAtTime(220, now);
            osc.frequency.setValueAtTime(160, now + 0.15);

            gain.gain.setValueAtTime(0.35, now);
            gain.gain.exponentialRampToValueAtTime(0.01, now + 0.35);

            osc.connect(gain);
            gain.connect(this.ctx.destination);

            osc.start(now);
            osc.stop(now + 0.36);
        } catch (e) {}
    }

    // 부활 무적 보호막 발동음 (신비로운 크리스탈 파워업)
    playReviveShield() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const now = this.ctx.currentTime;
            const osc = this.ctx.createOscillator();
            const gain = this.ctx.createGain();

            osc.type = 'sine';
            osc.frequency.setValueAtTime(261.63, now); // C4
            osc.frequency.exponentialRampToValueAtTime(880, now + 0.35);

            gain.gain.setValueAtTime(0.4, now);
            gain.gain.exponentialRampToValueAtTime(0.001, now + 0.4);

            osc.connect(gain);
            gain.connect(this.ctx.destination);

            osc.start(now);
            osc.stop(now + 0.42);
        } catch (e) {}
    }

    // 게임 오버 팡파르
    playGameOver() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const notes = [392.00, 369.99, 349.23, 311.13]; // G4, F#4, F4, Eb4
            const now = this.ctx.currentTime;

            notes.forEach((freq, idx) => {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                const startTime = now + idx * 0.16;

                osc.type = 'sawtooth';
                osc.frequency.setValueAtTime(freq, startTime);

                gain.gain.setValueAtTime(0.3, startTime);
                gain.gain.exponentialRampToValueAtTime(0.001, startTime + 0.28);

                osc.connect(gain);
                gain.connect(this.ctx.destination);

                osc.start(startTime);
                osc.stop(startTime + 0.3);
            });
        } catch (e) {}
    }

    // 스테이지 클리어 팡파르
    playStageClear() {
        if (this.isMuted) return;
        this.init();
        if (!this.ctx) return;
        try {
            const notes = [523.25, 659.25, 783.99, 1046.50, 1318.51];
            const now = this.ctx.currentTime;

            notes.forEach((freq, idx) => {
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                const startTime = now + idx * 0.12;

                osc.type = 'triangle';
                osc.frequency.setValueAtTime(freq, startTime);

                gain.gain.setValueAtTime(0.35, startTime);
                gain.gain.exponentialRampToValueAtTime(0.001, startTime + 0.4);

                osc.connect(gain);
                gain.connect(this.ctx.destination);

                osc.start(startTime);
                osc.stop(startTime + 0.45);
            });
        } catch (e) {}
    }

    // 레트로 8-비트 신스 BGM 루프
    startBgm() {
        if (this.isMuted || this.bgmPlaying) return;
        this.init();
        if (!this.ctx) return;
        this.bgmPlaying = true;
        this.bgmStep = 0;

        // 경쾌한 128 BPM 8-비트 멜로디 패턴 (16스텝)
        const bassNotes = [
            130.81, 0, 130.81, 164.81, 196.00, 0, 196.00, 220.00,
            174.61, 0, 174.61, 196.00, 220.00, 0, 246.94, 261.63
        ];
        const leadNotes = [
            523.25, 659.25, 783.99, 659.25, 523.25, 880.00, 783.99, 659.25,
            698.46, 783.99, 880.00, 783.99, 698.46, 659.25, 587.33, 523.25
        ];

        const stepDuration = 0.125; // 120ms (약 120BPM 16분음표)

        const playBgmStep = () => {
            if (!this.bgmPlaying || this.isMuted || !this.ctx) return;
            const now = this.ctx.currentTime;
            const bFreq = bassNotes[this.bgmStep % bassNotes.length];
            const lFreq = leadNotes[this.bgmStep % leadNotes.length];

            if (bFreq > 0) {
                const bOsc = this.ctx.createOscillator();
                const bGain = this.ctx.createGain();
                bOsc.type = 'triangle';
                bOsc.frequency.setValueAtTime(bFreq, now);
                bGain.gain.setValueAtTime(0.08, now);
                bGain.gain.exponentialRampToValueAtTime(0.001, now + stepDuration * 0.9);
                bOsc.connect(bGain);
                bGain.connect(this.ctx.destination);
                bOsc.start(now);
                bOsc.stop(now + stepDuration);
            }

            if (lFreq > 0 && this.bgmStep % 2 === 0) {
                const lOsc = this.ctx.createOscillator();
                const lGain = this.ctx.createGain();
                lOsc.type = 'square';
                lOsc.frequency.setValueAtTime(lFreq, now);
                lGain.gain.setValueAtTime(0.035, now);
                lGain.gain.exponentialRampToValueAtTime(0.001, now + stepDuration * 1.5);
                lOsc.connect(lGain);
                lGain.connect(this.ctx.destination);
                lOsc.start(now);
                lOsc.stop(now + stepDuration * 1.6);
            }

            this.bgmStep = (this.bgmStep + 1) % 16;
            this.bgmTimer = setTimeout(playBgmStep, stepDuration * 1000);
        };

        playBgmStep();
    }

    stopBgm() {
        this.bgmPlaying = false;
        if (this.bgmTimer) {
            clearTimeout(this.bgmTimer);
            this.bgmTimer = null;
        }
    }

    // TTS 음성 읽기
    speak(text, lang = 'en-US') {
        if (!this.ttsAvailable || !text) return;
        try {
            window.speechSynthesis.cancel();
            const utter = new SpeechSynthesisUtterance(text);
            utter.lang = lang;
            utter.rate = 0.9;
            if (lang.startsWith('en') && this.englishVoice) {
                utter.voice = this.englishVoice;
            } else if (lang.startsWith('ko') && this.koreanVoice) {
                utter.voice = this.koreanVoice;
            }
            window.speechSynthesis.speak(utter);
        } catch (e) {
            console.warn("TTS error:", e);
        }
    }
}

window.JumpmanAudio = new JumpmanAudio();
