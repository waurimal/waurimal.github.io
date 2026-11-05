/**
 * 스펠링 갤러그 (Spelling Galaga) - 8-Bit Web Audio API 음향 및 TTS 음성 모듈
 * 외부 오디오 파일 다운로드 없이 100% 브라우저 자체 합성으로 즉각 재생
 */

class SoundEffects {
  constructor() {
    this.ctx = null;
    this.isMuted = false;
    this.bgmPlaying = false;
    this.bgmTimer = null;
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

  // 레이저 발사음 (Galaga Laser Pew)
  playLaser() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();

    osc.type = 'sawtooth';
    osc.frequency.setValueAtTime(880, t);
    osc.frequency.exponentialRampToValueAtTime(110, t + 0.12);

    gain.gain.setValueAtTime(0.18, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.12);

    osc.connect(gain);
    gain.connect(this.ctx.destination);

    osc.start(t);
    osc.stop(t + 0.12);
  }

  // 듀얼 레이저 발사음 (풍성한 스테레오 더블 샷)
  playDualLaser() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    [920, 840].forEach((startFreq, i) => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'sawtooth';
      osc.frequency.setValueAtTime(startFreq, t);
      osc.frequency.exponentialRampToValueAtTime(120, t + 0.13);

      gain.gain.setValueAtTime(0.15, t);
      gain.gain.exponentialRampToValueAtTime(0.01, t + 0.13);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t);
      osc.stop(t + 0.13);
    });
  }

  // 정답 글자 타격음 (산뜻하고 기분 좋은 2음 아르페지오)
  playCorrect() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const notes = [523.25, 659.25, 783.99, 1046.50]; // C5, E5, G5, C6

    notes.forEach((freq, idx) => {
      const noteTime = t + idx * 0.05;
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'triangle';
      osc.frequency.setValueAtTime(freq, noteTime);

      gain.gain.setValueAtTime(0.2, noteTime);
      gain.gain.exponentialRampToValueAtTime(0.001, noteTime + 0.15);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(noteTime);
      osc.stop(noteTime + 0.15);
    });
  }

  // 오답 경고음 (삐-익 둔탁한 버저음)
  playWrong() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();

    osc.type = 'sawtooth';
    osc.frequency.setValueAtTime(150, t);
    osc.frequency.linearRampToValueAtTime(90, t + 0.25);

    gain.gain.setValueAtTime(0.28, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.25);

    osc.connect(gain);
    gain.connect(this.ctx.destination);

    osc.start(t);
    osc.stop(t + 0.25);
  }

  // 8비트 폭발음 (화이트 노이즈 + 필터 감쇄)
  playExplosion() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const bufferSize = this.ctx.sampleRate * 0.25;
    const buffer = this.ctx.createBuffer(1, bufferSize, this.ctx.sampleRate);
    const data = buffer.getChannelData(0);

    for (let i = 0; i < bufferSize; i++) {
      data[i] = Math.random() * 2 - 1;
    }

    const noise = this.ctx.createBufferSource();
    noise.buffer = buffer;

    const filter = this.ctx.createBiquadFilter();
    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(1200, t);
    filter.frequency.exponentialRampToValueAtTime(60, t + 0.25);

    const gain = this.ctx.createGain();
    gain.gain.setValueAtTime(0.3, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.25);

    noise.connect(filter);
    filter.connect(gain);
    gain.connect(this.ctx.destination);

    noise.start(t);
    noise.stop(t + 0.25);
  }

  // 거대 폭탄 폭발음 (Bomb Explosion)
  playBombExplosion() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const bufferSize = this.ctx.sampleRate * 0.6;
    const buffer = this.ctx.createBuffer(1, bufferSize, this.ctx.sampleRate);
    const data = buffer.getChannelData(0);

    for (let i = 0; i < bufferSize; i++) {
      data[i] = Math.random() * 2 - 1;
    }

    const noise = this.ctx.createBufferSource();
    noise.buffer = buffer;

    const filter = this.ctx.createBiquadFilter();
    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(600, t);
    filter.frequency.exponentialRampToValueAtTime(40, t + 0.6);

    const gain = this.ctx.createGain();
    gain.gain.setValueAtTime(0.5, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.6);

    noise.connect(filter);
    filter.connect(gain);
    gain.connect(this.ctx.destination);

    noise.start(t);
    noise.stop(t + 0.6);
  }

  // 아이템 획득음 (영롱한 3화음 파워업 챠임)
  playItemPickup() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const notes = [659.25, 783.99, 1046.50, 1318.51]; // E5, G5, C6, E6
    notes.forEach((freq, idx) => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();
      const startT = t + idx * 0.06;

      osc.type = 'sine';
      osc.frequency.setValueAtTime(freq, startT);

      gain.gain.setValueAtTime(0.22, startT);
      gain.gain.exponentialRampToValueAtTime(0.001, startT + 0.18);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(startT);
      osc.stop(startT + 0.18);
    });
  }

  // 쉴드 방어 타격음 (금속성 튕겨냄)
  playShieldDeflect() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();

    osc.type = 'triangle';
    osc.frequency.setValueAtTime(800, t);
    osc.frequency.exponentialRampToValueAtTime(300, t + 0.15);

    gain.gain.setValueAtTime(0.25, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.15);

    osc.connect(gain);
    gain.connect(this.ctx.destination);

    osc.start(t);
    osc.stop(t + 0.15);
  }

  // 트랙터 빔 음향 (우주 인력 광선)
  playTractorBeam() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const osc = this.ctx.createOscillator();
    const gain = this.ctx.createGain();

    osc.type = 'sine';
    osc.frequency.setValueAtTime(280, t);
    osc.frequency.linearRampToValueAtTime(450, t + 0.25);
    osc.frequency.linearRampToValueAtTime(280, t + 0.5);

    gain.gain.setValueAtTime(0.12, t);
    gain.gain.exponentialRampToValueAtTime(0.01, t + 0.5);

    osc.connect(gain);
    gain.connect(this.ctx.destination);

    osc.start(t);
    osc.stop(t + 0.5);
  }

  // 듀얼 파이터 도킹 구출 팡파르 (Dual Fighter Rescue)
  playDualRescue() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const notes = [440, 554.37, 659.25, 880, 1108.73]; // A4, C#5, E5, A5, C#6
    notes.forEach((freq, idx) => {
      const startT = t + idx * 0.08;
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'square';
      osc.frequency.setValueAtTime(freq, startT);

      gain.gain.setValueAtTime(0.16, startT);
      gain.gain.exponentialRampToValueAtTime(0.005, startT + 0.22);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(startT);
      osc.stop(startT + 0.22);
    });
  }

  // 갤러그 오리지널 스테이지 시작 팡파르 (Galaga Stage Start Fanfare)
  playStageStart() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const melody = [
      { f: 587.33, d: 0.1 },  // D5
      { f: 880.00, d: 0.1 },  // A5
      { f: 1046.50, d: 0.1 }, // C6
      { f: 1174.66, d: 0.15 },// D6
      { f: 880.00, d: 0.1 },  // A5
      { f: 1174.66, d: 0.28 } // D6
    ];

    let offset = 0;
    melody.forEach(item => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'square';
      osc.frequency.setValueAtTime(item.f, t + offset);

      gain.gain.setValueAtTime(0.18, t + offset);
      gain.gain.exponentialRampToValueAtTime(0.005, t + offset + item.d);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t + offset);
      osc.stop(t + offset + item.d);

      offset += item.d * 0.88;
    });
  }

  // 단어 완성 승리 팡파르
  playWordComplete() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const melody = [
      { f: 523.25, d: 0.1 },
      { f: 659.25, d: 0.1 },
      { f: 783.99, d: 0.1 },
      { f: 987.77, d: 0.12 },
      { f: 1046.50, d: 0.35 }
    ];

    let offset = 0;
    melody.forEach(item => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'square';
      osc.frequency.setValueAtTime(item.f, t + offset);

      gain.gain.setValueAtTime(0.18, t + offset);
      gain.gain.exponentialRampToValueAtTime(0.005, t + offset + item.d);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t + offset);
      osc.stop(t + offset + item.d);

      offset += item.d * 0.9;
    });
  }

  // 목숨 차감 경고음
  playLifeLost() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    [260, 220, 180].forEach((freq, i) => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'sawtooth';
      osc.frequency.setValueAtTime(freq, t + i * 0.09);

      gain.gain.setValueAtTime(0.25, t + i * 0.09);
      gain.gain.exponentialRampToValueAtTime(0.01, t + (i + 1) * 0.09);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t + i * 0.09);
      osc.stop(t + (i + 1) * 0.09);
    });
  }

  // 게임 오버 음악
  playGameOver() {
    if (this.isMuted) return;
    this.init();
    if (!this.ctx) return;

    const t = this.ctx.currentTime;
    const notes = [
      { f: 392.00, d: 0.2 }, // G4
      { f: 349.23, d: 0.2 }, // F4
      { f: 329.63, d: 0.2 }, // E4
      { f: 261.63, d: 0.5 }  // C4
    ];

    let offset = 0;
    notes.forEach(item => {
      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'triangle';
      osc.frequency.setValueAtTime(item.f, t + offset);

      gain.gain.setValueAtTime(0.22, t + offset);
      gain.gain.exponentialRampToValueAtTime(0.005, t + offset + item.d);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t + offset);
      osc.stop(t + offset + item.d);

      offset += item.d;
    });
  }

  // 영어 단어 음성 읽어주기 (TTS)
  speakWord(text) {
    if (!this.ttsAvailable || !text) return;
    try {
      window.speechSynthesis.cancel();
      const utter = new SpeechSynthesisUtterance(text.toLowerCase());
      utter.lang = 'en-US';
      utter.rate = 0.85; // 학생들이 정확히 들을 수 있도록 또렷한 속도
      utter.pitch = 1.05;
      if (this.englishVoice) {
        utter.voice = this.englishVoice;
      }
      window.speechSynthesis.speak(utter);
    } catch (e) {
      console.warn("TTS synthesis error:", e);
    }
  }

  // 8비트 아케이드 BGM 루프
  startBgm() {
    if (this.isMuted || this.bgmPlaying) return;
    this.init();
    if (!this.ctx) return;

    this.bgmPlaying = true;
    const bassline = [
      130.81, 130.81, 155.56, 174.61, // C3, C3, Eb3, F3
      196.00, 196.00, 174.61, 155.56  // G3, G3, F3, Eb3
    ];
    let step = 0;

    const playStep = () => {
      if (!this.bgmPlaying || this.isMuted || !this.ctx) return;

      const t = this.ctx.currentTime;
      const freq = bassline[step % bassline.length];

      const osc = this.ctx.createOscillator();
      const gain = this.ctx.createGain();

      osc.type = 'triangle';
      osc.frequency.setValueAtTime(freq, t);

      gain.gain.setValueAtTime(0.08, t);
      gain.gain.exponentialRampToValueAtTime(0.005, t + 0.18);

      osc.connect(gain);
      gain.connect(this.ctx.destination);

      osc.start(t);
      osc.stop(t + 0.18);

      step++;
      this.bgmTimer = setTimeout(playStep, 220);
    };

    playStep();
  }

  stopBgm() {
    this.bgmPlaying = false;
    if (this.bgmTimer) {
      clearTimeout(this.bgmTimer);
      this.bgmTimer = null;
    }
  }
}

window.SoundEffects = SoundEffects;
