/**
 * 에듀테크 영어 학습 놀이터 공통 상단 네비게이션 및 학생 통합 프로필 (Waurimal GNB & Student SSO)
 * 2026 Eui Elementary School - Waurimal Project
 * 19개 모든 게임에 원클릭 바로 시작, 통일된 인터페이스, 공통 GNB, 오디오 및 테마를 완벽 동기화합니다.
 */
(function() {
    'use strict';

    // 1. 공통 테마 CSS (waurimal-theme.css) 및 폰트 주입
    if (!document.getElementById('waurimal-theme-link')) {
        const themeLink = document.createElement('link');
        themeLink.id = 'waurimal-theme-link';
        themeLink.rel = 'stylesheet';
        themeLink.href = (window.location.protocol === 'file:') ? '../common/waurimal-theme.css' : 'https://waurimal.github.io/common/waurimal-theme.css';
        document.head.appendChild(themeLink);
    }
    if (!document.getElementById('waurimal-pretendard-font')) {
        const link = document.createElement('link');
        link.id = 'waurimal-pretendard-font';
        link.rel = 'stylesheet';
        link.href = 'https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/static/pretendard.min.css';
        document.head.appendChild(link);
    }

    // 1-2. 공통 Firebase 설정 및 통합 교육과정 모듈 자동 주입
    if (!window.WaurimalFirebase && !document.getElementById('waurimal-firebase-script')) {
        const fbScript = document.createElement('script');
        fbScript.id = 'waurimal-firebase-script';
        fbScript.src = (window.location.protocol === 'file:') ? '../common/firebase-config.js' : 'https://waurimal.github.io/common/firebase-config.js';
        document.head.appendChild(fbScript);
    }
    if (!window.WaurimalCurriculum && !document.getElementById('waurimal-curriculum-script')) {
        const currScript = document.createElement('script');
        currScript.id = 'waurimal-curriculum-script';
        currScript.src = (window.location.protocol === 'file:') ? '../common/curriculum.js' : 'https://waurimal.github.io/common/curriculum.js';
        document.head.appendChild(currScript);
    }

    // 1-1. 비속어 및 부적절한 단어 필터링 엔진 (WaurimalProfanity)
    const PROFANITY_PATTERNS = [
        /시[^\w가-힣]*[발벌빨바파펄팔8]|씨[^\w가-힣]*[발벌빨바파팔8]|ㅅ[^\w가-힣]*ㅂ|ㅆ[^\w가-힣]*ㅂ|shib[ao]l|sibal/i,
        /병[^\w가-힣]*[신씬]|븅[^\w가-힣]*[신씬]|ㅂ[^\w가-힣]*ㅅ|등신/i,
        /개[^\w가-힣]*[새세색쉐스][^\w가-힣]*[끼기키이]?|ㄱ[^\w가-힣]*ㅅ[^\w가-힣]*ㄲ/i,
        /미[^\w가-힣]*[친칭][^\w가-힣]*[놈년놈들]?|ㅁ[^\w가-힣]*ㅊ/i,
        /지[^\w가-힣]*[랄럴]|ㅈ[^\w가-힣]*ㄹ/i,
        /존[^\w가-힣]*[나낭내]|졸[^\w가-힣]*라|ㅈ[^\w가-힣]*ㄴ/i,
        /닥[^\w가-힣]*쳐|ㄷ[^\w가-힣]*ㅊ|꺼[^\w가-힣]*[져저]|ㄲ[^\w가-힣]*ㅈ/i,
        /느[^\w가-힣]*[금검]|느[^\w가-힣]*개[^\w가-힣]*비|니[^\w가-힣]*[애엄][^\w가-힣]*[미마]|엠[^\w가-힣]*창|엄[^\w가-힣]*창/i,
        /섹[^\w가-힣]*[스스]|s[^\w가-힣]*e[^\w가-힣]*x|야[^\w가-힣]*동|포[^\w가-힣]*르[^\w가-힣]*노/i,
        /자[^\w가-힣]*[지위]|보[^\w가-힣]*지|잠[^\w가-힣]*지|딸[^\w가-힣]*딸[^\w가-힣]*이/i,
        /창[^\w가-힣]*[녀남]|걸[^\w가-힣]*레/i,
        /뒤[^\w가-힣]*[져저]|뒈[^\w가-힣]*[져저]|자[^\w가-힣]*살|죽[^\w가-힣]*[어여]|살[^\w가-힣]*[인해]/i,
        /찐[^\w가-힣]*따|애[^\w가-힣]*자|장[^\w가-힣]*애|호[^\w가-힣]*[구로]/i,
        /새[^\w가-힣]*[끼키]|섀[^\w가-힣]*끼|쉐[^\w가-힣]*끼/i,
        /좆|좃|조[^\w가-힣]*까|ㅈ[^\w가-힣]*까|ㅗ{2,}|凸/i,
        /바[^\w가-힣]*[보]|멍[^\w가-힣]*청[^\w가-힣]*[이이]?|똥[^\w가-힣]*[개꼬]/i,
        /\b(fuck|fck|fuk|shit|bitch|bastard|asshole|dick|pussy|cunt|slut|retard)\b/i
    ];

    const WaurimalProfanity = {
        check: function(text) {
            if (!text) return false;
            const t = String(text).trim();
            const clean = t.replace(/[\s\-_.,~!@#$%^&*()+=\/\\0-9]/g, "");
            for (const pat of PROFANITY_PATTERNS) {
                if (pat.test(t) || pat.test(clean)) return true;
            }
            return false;
        }
    };
    window.WaurimalProfanity = WaurimalProfanity;

    // 2. 학생 정보 관리자 (WaurimalStudent SSO Engine)
    const STORAGE_KEY = 'waurimal_student_profile';

    const WaurimalStudent = {
        get: function() {
            // 1) URL 쿼리 파라미터 우선 확인
            const params = new URLSearchParams(window.location.search);
            const qGrade = params.get('grade');
            const qClass = params.get('class') || params.get('classNum');
            const qNum = params.get('num') || params.get('studentNum');
            const qName = params.get('name') || params.get('studentName');
            const qSchool = params.get('school');

            if (qName && (qGrade || qClass || qNum)) {
                const urlProfile = {
                    school: qSchool || "이의초등학교",
                    grade: qGrade || "6",
                    classNum: qClass || "1",
                    studentNum: qNum || "1",
                    name: qName
                };
                this.save(urlProfile);
                return urlProfile;
            }

            // 2) localStorage 확인
            try {
                const saved = localStorage.getItem(STORAGE_KEY);
                if (saved) return JSON.parse(saved);
            } catch (e) {
                console.warn("Storage access error:", e);
            }
            return null;
        },

        save: function(profile) {
            if (profile && profile.name && WaurimalProfanity.check(profile.name)) {
                console.warn("Profanity blocked in WaurimalStudent.save:", profile.name);
                return false;
            }
            try {
                localStorage.setItem(STORAGE_KEY, JSON.stringify(profile));
                window.dispatchEvent(new CustomEvent('waurimal_student_updated', { detail: profile }));
            } catch (e) {
                console.warn("Storage save error:", e);
            }
            this.autoFillForms();
            this.renderQuickStartBox();
            return true;
        },

        clear: function() {
            try {
                localStorage.removeItem(STORAGE_KEY);
                window.dispatchEvent(new CustomEvent('waurimal_student_updated', { detail: null }));
            } catch (e) {}
            this.renderQuickStartBox();
        },

        getDisplayText: function(profile) {
            if (!profile || !profile.name) return "학교명 / 학생 정보 등록";
            const school = (profile.school && profile.school.trim()) || "이의초등학교";
            const g = profile.grade ? `${profile.grade}학년 ` : '';
            const c = profile.classNum ? `${profile.classNum}반 ` : '';
            const n = profile.studentNum ? `${profile.studentNum}번 ` : '';
            return `${school} / ${g}${c}${n}${profile.name}`.trim();
        },

        // 19개 모든 게임 내 학적 폼 자동 채우기
        autoFillForms: function() {
            const profile = this.get();
            if (!profile || !profile.name) return;

            const selectors = {
                grade: [
                    '#grade', '#p-grade', '#loginGrade', '#student-grade', '#grade-select',
                    '#grade-input', '#st-grade', '#inp-grade', '#input-grade', '#host-st-grade',
                    '#guest-st-grade', '#filter-score-grade', 'select[name="grade"]'
                ],
                classNum: [
                    '#classNum', '#p-class', '#loginClass', '#student-class', '#class-select',
                    '#class-input', '#st-class', '#inp-class', '#input-class', '#host-st-class',
                    '#guest-st-class', '#classCodeInput', '#filter-score-class', 'select[name="class"]'
                ],
                studentNum: [
                    '#studentNum', '#p-num', '#st-num', '#loginNumber', '#student-num',
                    '#input-num', '#input-number', '#number-input', '#inp-num', '#host-st-num',
                    'input[name="num"]', 'input[name="studentNum"]'
                ],
                name: [
                    '#studentName', '#p-name', '#st-name', '#loginName', '#student-name',
                    '#input-name', '#name-input', '#inp-name', '#uploaderName', '#host-st-name',
                    'input[name="name"]', 'input[name="nickname"]'
                ],
                school: [
                    '#school-input', '#loginSchool', '#settingSchoolName', '#setting-school-name'
                ]
            };

            function fillField(list, val) {
                if (!val) return;
                list.forEach(sel => {
                    const elements = document.querySelectorAll(sel);
                    elements.forEach(el => {
                        if (el && el.value !== String(val)) {
                            el.value = val;
                            el.dispatchEvent(new Event('input', { bubbles: true }));
                            el.dispatchEvent(new Event('change', { bubbles: true }));
                        }
                    });
                });
            }

            fillField(selectors.grade, profile.grade);
            fillField(selectors.classNum, profile.classNum);
            fillField(selectors.studentNum, profile.studentNum);
            fillField(selectors.name, profile.name);
            fillField(selectors.school, profile.school);
        },

        // 원클릭 스마트 입장 실행 엔진
        triggerQuickStart: function() {
            this.autoFillForms();
            const profile = this.get();
            const pName = profile ? profile.name : "학생";

            try { WaurimalAudio.play('correct'); } catch(e) {}

            // 게임별 진입 트리거 탐색 및 실행
            const appId = detectApp();

            if (appId === 'baseball') {
                const introScreen = document.getElementById('intro-screen');
                const btnGoLogin = document.getElementById('btn-go-login');
                const btnLogin = document.getElementById('btn-login');

                if (introScreen && !introScreen.classList.contains('hidden') && btnGoLogin) {
                    btnGoLogin.click();
                }
                setTimeout(() => {
                    if (btnLogin) btnLogin.click();
                }, 80);
            } else if (appId === 'tictac') {
                const introScreen = document.getElementById('intro-screen');
                const btnGoto = document.getElementById('btn-goto-login');
                const btnLogin = document.getElementById('btn-login');

                if (introScreen && !introScreen.classList.contains('hidden') && btnGoto) {
                    btnGoto.click();
                }
                setTimeout(() => {
                    if (btnLogin) btnLogin.click();
                }, 80);
            } else if (appId === 'reading') {
                if (typeof window.login === 'function') {
                    window.login();
                }
            } else if (appId === 'where') {
                const mainBtn = document.querySelector('#lobby-state-main button');
                const nextBtn = document.getElementById('btn-next-step');
                if (mainBtn && !document.getElementById('lobby-state-main').classList.contains('hidden')) {
                    mainBtn.click();
                }
                setTimeout(() => {
                    if (nextBtn) nextBtn.click();
                }, 80);
            } else if (appId === 'chain') {
                const btn = document.getElementById('btn-login');
                if (btn) btn.click();
            } else if (appId === 'flip') {
                if (typeof window.loginAsStudent === 'function') {
                    window.loginAsStudent();
                }
            } else if (appId === 'earthworm') {
                if (typeof window.login === 'function') {
                    window.login();
                }
            } else if (appId === 'frog' || appId === 'street') {
                const startBtn = document.getElementById('start-btn');
                if (startBtn) startBtn.click();
            } else if (appId === 'wordtyping' || appId === 'sen_typing') {
                if (typeof window.enterStudent === 'function') {
                    window.enterStudent();
                }
            } else if (appId === 'hangman') {
                if (window.ui && typeof window.ui.selectMode === 'function') {
                    window.ui.selectMode('solo');
                }
            } else if (appId === 'bluemarble') {
                const singleBtn = document.getElementById('btn-single-play');
                if (singleBtn) singleBtn.scrollIntoView({ behavior: 'smooth', block: 'center' });
            } else if (appId === 'bingo') {
                const tab1v1 = document.querySelector('#tab-1v1');
                if (tab1v1) tab1v1.scrollIntoView({ behavior: 'smooth', block: 'center' });
            }

            WaurimalToast(`${pName} 학생, 환영합니다! 게임을 시작하세요 🚀`, '🎮');
        },

        // 상단 GNB가 학생 정보의 단일 창구(SSO)이므로, 화면 내 중복 프로필 카드는 렌더링하지 않고 제거함
        renderQuickStartBox: function() {
            const existing = document.querySelectorAll('.w-quick-start-box');
            existing.forEach(el => el.remove());

            this.setupAutoAdvance();
        },

        // GNB에 학생 정보가 등록되어 있을 경우, 게임 시작 버튼 클릭 시 중복 선수 등록 폼을 자동으로 건너뜀
        setupAutoAdvance: function() {
            const profile = this.get();
            if (!profile || !profile.name) return;

            const appId = detectApp();
            if (appId === 'baseball') {
                const btnGoLogin = document.getElementById('btn-go-login');
                if (btnGoLogin && !btnGoLogin.dataset.wBypassBound) {
                    btnGoLogin.dataset.wBypassBound = 'true';
                    btnGoLogin.addEventListener('click', () => {
                        this.autoFillForms();
                        setTimeout(() => {
                            const btnLogin = document.getElementById('btn-login');
                            if (btnLogin) btnLogin.click();
                        }, 50);
                    });
                }
            } else if (appId === 'tictac') {
                const btnGoto = document.getElementById('btn-goto-login');
                if (btnGoto && !btnGoto.dataset.wBypassBound) {
                    btnGoto.dataset.wBypassBound = 'true';
                    btnGoto.addEventListener('click', () => {
                        this.autoFillForms();
                        setTimeout(() => {
                            const btnLogin = document.getElementById('btn-login');
                            if (btnLogin) btnLogin.click();
                        }, 50);
                    });
                }
            } else if (appId === 'where') {
                const mainBtn = document.querySelector('#lobby-state-main button');
                if (mainBtn && !mainBtn.dataset.wBypassBound) {
                    mainBtn.dataset.wBypassBound = 'true';
                    mainBtn.addEventListener('click', () => {
                        this.autoFillForms();
                        setTimeout(() => {
                            const nextBtn = document.getElementById('btn-next-step');
                            if (nextBtn) nextBtn.click();
                        }, 50);
                    });
                }
            }
        }
    };

    window.WaurimalStudent = WaurimalStudent;

    // 2-1. 공통 웹 오디오 사운드 합성 엔진
    const WaurimalAudio = {
        ctx: null,
        muted: localStorage.getItem('waurimal_muted') === 'true',
        init: function() {
            if (!this.ctx && (window.AudioContext || window.webkitAudioContext)) {
                this.ctx = new (window.AudioContext || window.webkitAudioContext)();
            }
        },
        toggleMute: function() {
            this.muted = !this.muted;
            localStorage.setItem('waurimal_muted', this.muted);
            return this.muted;
        },
        play: function(type) {
            if (this.muted) return;
            try {
                this.init();
                if (!this.ctx) return;
                if (this.ctx.state === 'suspended') this.ctx.resume();
                const now = this.ctx.currentTime;
                const osc = this.ctx.createOscillator();
                const gain = this.ctx.createGain();
                osc.connect(gain);
                gain.connect(this.ctx.destination);

                if (type === 'click') {
                    osc.type = 'sine';
                    osc.frequency.setValueAtTime(600, now);
                    osc.frequency.exponentialRampToValueAtTime(800, now + 0.05);
                    gain.gain.setValueAtTime(0.06, now);
                    gain.gain.linearRampToValueAtTime(0.001, now + 0.06);
                    osc.start(now);
                    osc.stop(now + 0.06);
                } else if (type === 'correct') {
                    osc.type = 'triangle';
                    osc.frequency.setValueAtTime(523.25, now);
                    osc.frequency.setValueAtTime(659.25, now + 0.08);
                    gain.gain.setValueAtTime(0.1, now);
                    gain.gain.linearRampToValueAtTime(0.001, now + 0.22);
                    osc.start(now);
                    osc.stop(now + 0.22);
                }
            } catch(e) {}
        }
    };
    window.WaurimalAudio = WaurimalAudio;

    // 2-2. 공통 모던 토스트 알림
    function WaurimalToast(message, icon = '✨') {
        let container = document.getElementById('waurimal-toast-container');
        if (!container) {
            container = document.createElement('div');
            container.id = 'waurimal-toast-container';
            document.body.appendChild(container);
        }
        const item = document.createElement('div');
        item.className = 'w-toast-item';
        item.innerHTML = `<span>${icon}</span> <span>${message}</span>`;
        container.appendChild(item);
        setTimeout(() => {
            item.style.opacity = '0';
            item.style.transition = 'opacity 0.3s ease';
            setTimeout(() => item.remove(), 300);
        }, 2600);
    }
    window.WaurimalToast = WaurimalToast;

    // 3. 앱별 메타데이터 및 가이드 사전
    const APP_DATABASE = {
        "wordtyping": { 
            title: "단어 타자 연습", 
            icon: "⌨️", 
            teacher: "1. 단원 및 어휘 관리: 우측 하단 [🔒 교사 모드]에서 학습할 단원과 단어·뜻을 자유롭게 추가하거나 편집할 수 있습니다.\n2. 실시간 명예의 전당 & 성적 관리: 학생들의 플레이 기록이 Firebase 클라우드 DB에 실시간 저장되며, 단원별·학년별·반별·기간별 성적 조회 및 1인 1최고점수 조회가 가능합니다.\n3. 비속어 감지 및 기록 삭제: 비속어 닉네임 감지(🚨) 기능이 지원되며, 부적절한 성적은 [삭제] 버튼으로 데이터베이스에서 즉시 영구 삭제할 수 있습니다.\n4. 전교생 일괄 배포: 단원 편집 후 [설정 파일 (config.json) 다운로드]를 받아 깃허브 저장소에 업로드하면 전교생 기기에 즉시 일괄 적용됩니다.", 
            student: "1. 학생 정보 확인: 화면 상단(GNB)의 프로필을 눌러 학교, 학년, 반, 번호, 이름을 확인합니다. (이름에 비속어나 부적절한 단어는 등록할 수 없습니다.)\n2. 단원 선택: 메인 화면의 [게임 시작하기]를 누른 뒤, 학습할 단원(Lesson)을 선택하고 [다음 단계]로 이동합니다.\n3. 단어 타이핑: 화면 중앙에 제시되는 영어 단어와 뜻을 보고, 입력창에 타이핑한 뒤 [Enter]를 누릅니다.\n • 연속으로 빠르게 맞히면 콤보 점수와 보너스를 획득합니다.\n • 스피커 버튼(🔊)을 눌러 원어민 발음을 함께 들을 수 있습니다.\n4. 명예의 전당 도전: 타자 연습을 완료하면 점수, 소요시간, 정확도가 클라우드에 자동 등록됩니다. 초기 화면의 [명예의 전당]에서 내 순위를 확인해 보세요!" 
        },
        "hangman": { title: "행맨 게임", icon: "🎮", teacher: "단어의 철자 구조와 파닉스를 복습하기 좋은 실시간 멀티플레이어 행맨 게임입니다.", student: "알파벳을 하나씩 추리하여 숨겨진 단어를 완성해 보세요!" },
        "alchemist": { title: "단어 연금술사", icon: "🧪", teacher: "단어들을 조합하여 새로운 합성어를 만들어내는 창의적 어휘 학습 도구입니다.", student: "기본 단어들을 합치고 연구하여 숨겨진 신비로운 단어들을 모두 찾아보세요!" },
        "reading": { title: "영어 읽기", icon: "🏃‍♂️", teacher: "TTS 음성을 지원하며 단계별 문장 독해 및 발음 따라 읽기 훈련에 적합합니다.", student: "원어민 발음을 듣고 문장을 큰 소리로 따라 읽으며 빠르게 완주해 보세요!" },
        "sen_typing": { title: "영어 문장", icon: "⚡", teacher: "핵심 문장 구조를 타이핑하며 문장 작성 감각을 체화하도록 돕습니다.", student: "정확한 띄어쓰기와 구두점에 유의하여 문장을 빠르게 입력하세요!" },
        "swarm": { title: "Swarm 게임", icon: "🐝", teacher: "협동 학습을 유도하는 게임으로 팀원 간 빠른 소통과 단어 개수 파악이 핵심입니다.", student: "화면에 흩어진 영어 단어들을 빠르게 세고 팀원과 전략을 세워 정답을 맞히세요!" },
        "tictac": { title: "Tic Tac Toe", icon: "⭕", teacher: "영어 문장 퀴즈를 맞혀야 틱택토 판에 말을 놓을 수 있는 두뇌 보드게임입니다.", student: "문제를 맞히고 가로, 세로, 대각선으로 3칸을 먼저 연결하세요!" },
        "baseball": { title: "야구게임", icon: "⚾", teacher: "야구의 볼카운트 룰을 결합하여 긴장감 있게 문장 퀴즈를 푸는 게임입니다.", student: "정답을 맞혀 안타와 홈런을 치고 상대 팀을 상대로 승리하세요!" },
        "where": { title: "길 찾기 게임", icon: "🗺️", teacher: "방향 지시 표현(Go straight, Turn left/right)을 듣고 목적지를 찾는 게임입니다.", student: "원어민 음성 지시를 귀담아듣고 지도 속 올바른 목적지로 이동하세요!" },
        "bingo": { title: "학교 빙고 게임", icon: "🎯", teacher: "1:1 또는 반 전체 학생이 배운 단어로 빙고판을 채우고 함께 즐길 수 있습니다.", student: "불리는 영어 단어를 찾아 클릭하고 3줄 이상의 빙고를 완성하세요!" },
        "skribble": { title: "Skribble 게임", icon: "🎨", teacher: "친구가 그린 그림을 보고 영어 단어를 맞히는 창의적 상호작용 활동입니다.", student: "제시어를 그림으로 멋지게 표현하고, 친구들의 그림을 보고 단어를 맞혀보세요!" },
        "bluemarble": { title: "부루마불", icon: "🎲", teacher: "세계 도시를 여행하며 영어 퀴즈를 풀고 자산을 관리하는 부루마불 게임입니다.", student: "주사위를 굴려 세계를 누비고 랜드마크를 건설해 최고의 여행가가 되어보세요!" },
        "flip": { title: "카드 뒤집기", icon: "🃏", teacher: "단어와 뜻/이미지를 짝 맞추는 기억력 향상 카드 뒤집기 게임입니다.", student: "뒤집힌 카드들의 위치를 잘 기억하여 짝이 맞는 카드를 모두 찾아내세요!" },
        "sudoku": { title: "스도쿠 게임", icon: "🔢", teacher: "가로, 세로, 3x3 박스 내 숫자 중복 없이 논리적으로 빈칸을 채우는 사고력 게임입니다.", student: "주어진 힌트 숫자를 단서로 빈칸의 숫자를 논리적으로 채워보세요!" },
        "earthworm": { title: "지렁이 게임", icon: "🐛", teacher: "순발력을 기르며 정답 단어 글자를 차례대로 먹어 몸을 늘리는 아케이드 게임입니다.", student: "방향키로 지렁이를 조종해 올바른 철자 먹이를 먹고 최고 길이에 도전하세요!" },
        "street": { title: "Crossy Road", icon: "🚦", teacher: "차도를 건너며 상황별 올바른 영어 표현을 선택하는 박진감 넘치는 게임입니다.", student: "달려오는 차들을 피하며 도로를 건너고 영어 미션을 완수하세요!" },
        "frog": { title: "개구리 게임", icon: "🐸", teacher: "통나무와 연잎을 타고 강을 건너며 단어 퀴즈를 푸는 타이밍 액션 게임입니다.", student: "강물에 빠지지 않게 점프 타이밍을 맞추고 목적지까지 무사히 도착하세요!" },
        "chain": { title: "단어 체인", icon: "🔗", teacher: "제시된 영단어의 끝 글자로 이어지는 단어를 찾아내는 어휘력 배틀입니다.", student: "마지막 알파벳으로 시작하는 새로운 단어를 재빠르게 입력하세요!" },
        "file_upload": { title: "파일 제출", icon: "📤", teacher: "학생들의 과제 파일, 학습 결과물을 간편하게 수합하는 포털입니다.", student: "자신의 학년, 반, 이름을 입력하고 과제 파일을 손쉽게 제출하세요!" },
        "feedback": { title: "평가 및 의견", icon: "💬", teacher: "에듀테크 앱에 대한 의견과 건의사항을 남길 수 있습니다.", student: "좋았던 점이나 개선할 점을 자유롭게 남겨주세요!" },
        "portal": { title: "학습 놀이터", icon: "🏫", teacher: "전체 19종 이상의 에듀테크 영어 놀이를 한눈에 탐색하고 수업에 활용하세요.", student: "다양한 영어 놀이를 골라 재미있게 학습해 보세요!" }
    };

    function detectApp() {
        const path = window.location.pathname;
        if (path.includes('feedback.html')) return "feedback";
        if (path.includes('waurimal.github.io') && (path.endsWith('index.html') || path.endsWith('/'))) return "portal";
        for (const appId in APP_DATABASE) {
            if (path.includes('/' + appId + '/') || path.endsWith('/' + appId)) return appId;
        }
        const title = document.title;
        for (const appId in APP_DATABASE) {
            if (title.includes(APP_DATABASE[appId].title)) return appId;
        }
        return "wordtyping";
    }

    const currentAppId = detectApp();
    const appInfo = APP_DATABASE[currentAppId] || { title: document.title || "영어 놀이터", icon: "🎮", teacher: "수업에 즐겁게 활용해 보세요.", student: "열심히 도전해 보세요!" };

    function getHomeUrl() {
        if (window.location.hostname.includes("github.io")) {
            return "https://waurimal.github.io/";
        }
        if (window.location.pathname.includes("/waurimal.github.io/")) {
            return "./index.html";
        }
        return "../waurimal.github.io/index.html";
    }

    function getFeedbackUrl() {
        if (window.location.hostname.includes("github.io")) {
            return "https://waurimal.github.io/feedback.html";
        }
        if (window.location.pathname.includes("/waurimal.github.io/")) {
            return "./feedback.html";
        }
        return "../waurimal.github.io/feedback.html";
    }

    // 4. 스타일 주입
    const styleEl = document.createElement('style');
    styleEl.id = 'waurimal-gnb-styles';
    styleEl.textContent = `
        :root {
            --w-gnb-h: 48px;
        }
        #waurimal-gnb-wrapper {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            width: 100%;
            z-index: 999999;
            font-family: 'Pretendard Variable', 'Pretendard', -apple-system, sans-serif;
            user-select: none;
            transition: transform 0.25s cubic-bezier(0.16, 1, 0.3, 1);
        }
        #waurimal-gnb-wrapper.w-collapsed {
            transform: translateY(-100%);
        }
        #waurimal-gnb {
            position: relative;
            height: var(--w-gnb-h);
            background: linear-gradient(135deg, rgba(30, 41, 59, 0.96) 0%, rgba(15, 23, 42, 0.98) 100%);
            backdrop-filter: blur(12px);
            -webkit-backdrop-filter: blur(12px);
            border-bottom: 2px solid #3b82f6;
            color: #f8fafc;
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 0 0.8rem;
            box-sizing: border-box;
            box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
        }
        #waurimal-gnb a {
            text-decoration: none;
            color: inherit;
        }
        .w-gnb-left {
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }
        .w-gnb-logo {
            font-weight: 800;
            font-size: 0.9rem;
            letter-spacing: -0.02em;
            background: linear-gradient(135deg, #60a5fa, #38bdf8);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            display: flex;
            align-items: center;
            gap: 0.35rem;
        }
        .w-gnb-logo:hover { opacity: 0.85; }
        .w-gnb-divider {
            color: #475569;
            font-size: 0.75rem;
        }
        .w-gnb-badge {
            background: rgba(59, 130, 246, 0.2);
            border: 1px solid rgba(96, 165, 250, 0.35);
            color: #93c5fd;
            padding: 0.15rem 0.5rem;
            border-radius: 9999px;
            font-size: 0.78rem;
            font-weight: 600;
            display: flex;
            align-items: center;
            gap: 0.3rem;
            white-space: nowrap;
        }
        .w-gnb-center {
            position: absolute;
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 10;
            pointer-events: auto;
        }
        .w-gnb-student-chip {
            background: rgba(255, 255, 255, 0.1);
            border: 1px solid rgba(255, 255, 255, 0.2);
            color: #ffffff;
            padding: 0.25rem 0.75rem;
            border-radius: 9999px;
            font-size: 0.82rem;
            font-weight: 700;
            display: flex;
            align-items: center;
            gap: 0.4rem;
            cursor: pointer;
            transition: all 0.2s;
            max-width: 380px;
            white-space: nowrap;
        }
        #w-student-chip-name {
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            min-width: 0;
        }
        .w-gnb-student-chip:hover {
            background: rgba(59, 130, 246, 0.3);
            border-color: #60a5fa;
            transform: scale(1.02);
        }
        .w-gnb-student-chip.empty {
            background: rgba(245, 158, 11, 0.2);
            border-color: rgba(245, 158, 11, 0.4);
            color: #fde68a;
        }
        .w-gnb-right {
            display: flex;
            align-items: center;
            gap: 0.4rem;
        }
        .w-gnb-btn {
            background: rgba(255, 255, 255, 0.08);
            border: 1px solid rgba(255, 255, 255, 0.15);
            color: #e2e8f0;
            padding: 0.28rem 0.65rem;
            border-radius: 8px;
            font-size: 0.78rem;
            font-weight: 600;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 0.3rem;
            transition: all 0.18s;
            text-decoration: none;
            white-space: nowrap;
        }
        .w-gnb-btn:hover {
            background: rgba(255, 255, 255, 0.2);
            color: #ffffff;
        }
        .w-gnb-btn-feedback {
            background: linear-gradient(135deg, #10b981 0%, #059669 100%) !important;
            border-color: #34d399 !important;
            color: #ffffff !important;
            font-weight: 700 !important;
        }
        .w-gnb-btn-feedback:hover {
            background: linear-gradient(135deg, #059669 0%, #047857) !important;
            box-shadow: 0 0 10px rgba(16, 185, 129, 0.5);
            transform: translateY(-1px);
        }
        .w-feedback-text::after {
            content: " 남기기";
        }
        .w-gnb-btn-primary {
            background: linear-gradient(135deg, #2563eb, #3b82f6) !important;
            border-color: #60a5fa !important;
            color: #ffffff !important;
            font-weight: 700;
        }
        .w-gnb-btn-primary:hover {
            background: linear-gradient(135deg, #1d4ed8, #2563eb) !important;
            box-shadow: 0 0 10px rgba(59, 130, 246, 0.5);
        }
        #w-gnb-toggle-handle {
            position: absolute;
            bottom: -18px;
            right: 16px;
            background: #1e293b;
            color: #94a3b8;
            font-size: 0.65rem;
            font-weight: 700;
            padding: 0.15rem 0.5rem;
            border-radius: 0 0 6px 6px;
            cursor: pointer;
            border: 1px solid #334155;
            border-top: none;
            transition: all 0.2s;
        }
        #w-gnb-toggle-handle:hover {
            color: #ffffff;
            background: #0f172a;
        }
        .w-modal-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(15, 23, 42, 0.65);
            backdrop-filter: blur(4px);
            z-index: 1000000;
            display: none;
            align-items: center;
            justify-content: center;
            padding: 1rem;
            box-sizing: border-box;
        }
        .w-modal-card {
            background: #ffffff;
            color: #1e293b;
            border-radius: 20px;
            width: 100%;
            max-width: 460px;
            box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.35);
            overflow: hidden;
            animation: wPop 0.2s cubic-bezier(0.16, 1, 0.3, 1);
        }
        @keyframes wPop {
            from { transform: scale(0.94); opacity: 0; }
            to { transform: scale(1); opacity: 1; }
        }
        .w-modal-header {
            background: linear-gradient(135deg, #1e3a8a, #3b82f6);
            color: #ffffff;
            padding: 1rem 1.2rem;
            display: flex;
            align-items: center;
            justify-content: space-between;
        }
        .w-modal-title {
            font-size: 1.05rem;
            font-weight: 700;
            display: flex;
            align-items: center;
            gap: 0.4rem;
        }
        .w-modal-close {
            background: none;
            border: none;
            color: #ffffff;
            font-size: 1.3rem;
            cursor: pointer;
            opacity: 0.8;
            padding: 0;
            line-height: 1;
        }
        .w-modal-close:hover { opacity: 1; }
        .w-modal-body {
            padding: 1.2rem;
            max-height: 65vh;
            overflow-y: auto;
        }
        .w-form-group {
            margin-bottom: 0.85rem;
        }
        .w-form-label {
            display: block;
            font-size: 0.82rem;
            font-weight: 700;
            color: #475569;
            margin-bottom: 0.3rem;
        }
        .w-form-row {
            display: flex;
            gap: 0.5rem;
        }
        .w-form-input, .w-form-select {
            width: 100%;
            padding: 0.55rem 0.75rem;
            border: 1.5px solid #cbd5e1;
            border-radius: 8px;
            font-size: 0.9rem;
            font-family: inherit;
            color: #0f172a;
            outline: none;
            transition: border-color 0.2s;
            box-sizing: border-box;
        }
        .w-form-input:focus, .w-form-select:focus {
            border-color: #3b82f6;
            box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.15);
        }
        .w-modal-tip {
            font-size: 0.75rem;
            color: #64748b;
            background: #f1f5f9;
            padding: 0.6rem 0.8rem;
            border-radius: 8px;
            margin-top: 0.8rem;
            line-height: 1.4;
        }
        .w-modal-footer {
            padding: 0.75rem 1.2rem;
            background: #f8fafc;
            border-top: 1px solid #e2e8f0;
            display: flex;
            justify-content: flex-end;
            gap: 0.5rem;
        }
        @media (max-width: 1200px) {
            .w-feedback-text::after {
                content: "";
            }
            .w-fs-text {
                display: none;
            }
            .w-gnb-student-chip {
                max-width: 300px;
            }
        }
        @media (max-width: 992px) {
            .w-feedback-text {
                display: none;
            }
            .w-guide-text {
                display: none;
            }
            .w-gnb-student-chip {
                max-width: 230px;
                font-size: 0.78rem;
            }
        }
        @media (max-width: 768px) {
            .w-gnb-badge {
                max-width: 110px;
                overflow: hidden;
                text-overflow: ellipsis;
            }
            .w-gnb-student-chip {
                max-width: 170px;
                font-size: 0.75rem;
            }
        }
        @media (max-width: 650px) {
            .w-gnb-divider, .w-gnb-badge {
                display: none;
            }
            .w-gnb-student-chip {
                max-width: 135px;
                font-size: 0.72rem;
            }
            #waurimal-gnb {
                padding: 0 0.5rem;
            }
        }
        @media (max-width: 480px) {
            .w-gnb-student-chip {
                max-width: 105px;
                padding: 0.2rem 0.45rem;
            }
        }
    `;
    document.head.appendChild(styleEl);

    // 5. GNB DOM 생성 및 이벤트 등록
    function initGNB() {
        if (document.getElementById('waurimal-gnb-wrapper')) return;

        const homeUrl = getHomeUrl();
        const feedbackUrl = getFeedbackUrl();
        const profile = WaurimalStudent.get();
        const studentText = WaurimalStudent.getDisplayText(profile);
        const isEmpty = !profile || !profile.name;

        const wrapper = document.createElement('div');
        wrapper.id = 'waurimal-gnb-wrapper';
        wrapper.innerHTML = `
            <nav id="waurimal-gnb">
                <div class="w-gnb-left">
                    <a href="${homeUrl}" class="w-gnb-logo" id="w-logo-home" title="에듀테크 영어 놀이터 메인으로">
                        <span>🏠</span> 놀이터(홈)
                    </a>
                    <span class="w-gnb-divider">/</span>
                    <div class="w-gnb-badge">
                        <span>${appInfo.icon}</span> ${appInfo.title}
                    </div>
                </div>
                <div class="w-gnb-center">
                    <button type="button" class="w-gnb-student-chip ${isEmpty ? 'empty' : ''}" id="w-btn-student-profile" title="학생 정보 설정 (모든 게임에 자동 적용)">
                        <span>👤</span>
                        <span id="w-student-chip-name">${studentText}</span>
                        <span style="font-size:0.75rem; opacity:0.8;">✏️</span>
                    </button>
                </div>
                <div class="w-gnb-right">
                    <a href="${feedbackUrl}" class="w-gnb-btn w-gnb-btn-feedback" id="w-btn-feedback" title="앱 평가 및 의견 남기기">
                        <span>💬</span> <span class="w-feedback-text">앱 평가 및 의견</span>
                    </a>
                    <button type="button" class="w-gnb-btn" id="w-btn-guide" title="게임 및 활동 방법 안내">
                        <span>📖</span><span class="w-guide-text"> 방법</span>
                    </button>
                    <button type="button" class="w-gnb-btn" id="w-btn-sound" title="효과음 켜기/끄기">
                        <span id="w-sound-icon">${WaurimalAudio.muted ? '🔇' : '🔊'}</span>
                    </button>
                    <button type="button" class="w-gnb-btn" id="w-btn-fullscreen" title="전체화면 전환 (스마트보드용)">
                        <span id="w-fs-icon">⛶</span><span class="w-fs-text"> 전체화면</span>
                    </button>
                </div>
            </nav>
            <div id="w-gnb-toggle-handle" title="상단 바 접기/펼치기">▲ 접기</div>
        `;

        // 학생 정보 입력 모달
        const studentModal = document.createElement('div');
        studentModal.className = 'w-modal-overlay';
        studentModal.id = 'w-student-modal';
        studentModal.innerHTML = `
            <div class="w-modal-card">
                <div class="w-modal-header">
                    <div class="w-modal-title">
                        <span>👤</span> 내 학생 정보 설정
                    </div>
                    <button type="button" class="w-modal-close" id="w-student-modal-close">&times;</button>
                </div>
                <div class="w-modal-body">
                    <div class="w-form-group">
                        <label class="w-form-label">학교명</label>
                        <input type="text" class="w-form-input" id="w-input-school" value="${(profile && profile.school) || '이의초등학교'}" placeholder="예: 이의초등학교">
                    </div>
                    <div class="w-form-row">
                        <div class="w-form-group" style="flex:1;">
                            <label class="w-form-label">학년</label>
                            <select class="w-form-select" id="w-input-grade">
                                <option value="3" ${profile && profile.grade == '3' ? 'selected' : ''}>3학년</option>
                                <option value="4" ${profile && profile.grade == '4' ? 'selected' : ''}>4학년</option>
                                <option value="5" ${profile && profile.grade == '5' ? 'selected' : ''}>5학년</option>
                                <option value="6" ${(!profile || profile.grade == '6') ? 'selected' : ''}>6학년</option>
                            </select>
                        </div>
                        <div class="w-form-group" style="flex:1;">
                            <label class="w-form-label">반</label>
                            <select class="w-form-select" id="w-input-class">
                                ${Array.from({length: 15}, (_, i) => `<option value="${i+1}" ${profile && profile.classNum == String(i+1) ? 'selected' : ''}>${i+1}반</option>`).join('')}
                            </select>
                        </div>
                        <div class="w-form-group" style="flex:1;">
                            <label class="w-form-label">번호</label>
                            <input type="number" class="w-form-input" id="w-input-num" min="1" max="40" value="${(profile && profile.studentNum) || ''}" placeholder="번호">
                        </div>
                    </div>
                    <div class="w-form-group">
                        <label class="w-form-label">이름 (실명)</label>
                        <input type="text" class="w-form-input" id="w-input-name" value="${(profile && profile.name) || ''}" placeholder="이름을 입력하세요 (예: 홍길동)">
                    </div>
                    <div class="w-modal-tip">
                        💡 <strong>한 번만 입력하면 끝!</strong><br>
                        여기서 저장한 정보는 모든 영어 게임에 자동으로 로그인/입장됩니다.
                    </div>
                </div>
                <div class="w-modal-footer">
                    <button type="button" class="w-gnb-btn" id="w-btn-student-clear" style="color:#ef4444; border-color:#fca5a5;">초기화</button>
                    <button type="button" class="w-gnb-btn w-gnb-btn-primary" id="w-btn-student-save">저장하고 적용하기</button>
                </div>
            </div>
        `;

        // 가이드 모달
        const guideModal = document.createElement('div');
        guideModal.className = 'w-modal-overlay';
        guideModal.id = 'w-guide-modal';
        guideModal.innerHTML = `
            <div class="w-modal-card">
                <div class="w-modal-header">
                    <div class="w-modal-title">
                        <span>${appInfo.icon}</span> ${appInfo.title} 안내
                    </div>
                    <button type="button" class="w-modal-close" id="w-guide-modal-close">&times;</button>
                </div>
                <div class="w-modal-body">
                    <div style="margin-bottom:1rem; background:#f8fafc; border-radius:10px; padding:0.9rem; border-left:4px solid #3b82f6;">
                        <div style="font-weight:700; font-size:0.9rem; color:#1e40af; margin-bottom:0.35rem;">👨‍🏫 선생님 지도 팁</div>
                        <p style="font-size:0.85rem; line-height:1.55; color:#334155; margin:0; white-space:pre-line;">${appInfo.teacher}</p>
                    </div>
                    <div style="background:#f8fafc; border-radius:10px; padding:0.9rem; border-left:4px solid #10b981;">
                        <div style="font-weight:700; font-size:0.9rem; color:#047857; margin-bottom:0.35rem;">🧑‍🎓 학생 참여 방법</div>
                        <p style="font-size:0.85rem; line-height:1.55; color:#334155; margin:0; white-space:pre-line;">${appInfo.student}</p>
                    </div>
                </div>
                <div class="w-modal-footer">
                    <button type="button" class="w-gnb-btn w-gnb-btn-primary" id="w-guide-modal-ok">확인</button>
                </div>
            </div>
        `;

        // 교사 설정 플로팅 버튼 (우측 하단)
        const teacherBtn = document.createElement('button');
        teacherBtn.type = 'button';
        teacherBtn.className = 'w-floating-teacher-btn';
        teacherBtn.innerHTML = '<span>🔒</span> 교사 모드';
        teacherBtn.title = '교사 전용 설정 열기';
        teacherBtn.addEventListener('click', () => {
            const targets = [
                '#btn-teacher-entry', '#btn-show-teacher-auth', '.teacher-btn', '#open-admin-btn',
                '#btn-go-teacher-auth', '#btn-admin-open', 'button[onclick*="teacher"]',
                'button[onclick*="Teacher"]', 'button[onclick*="admin"]', 'button[onclick*="Admin"]'
            ];
            for (const sel of targets) {
                const el = document.querySelector(sel);
                if (el) {
                    el.click();
                    WaurimalToast('교사 설정 모드로 진입합니다.', '🔒');
                    return;
                }
            }
            WaurimalToast('이 게임의 교사 설정 메뉴를 찾을 수 없습니다.', 'ℹ️');
        });

        document.body.prepend(wrapper);
        document.body.appendChild(studentModal);
        document.body.appendChild(guideModal);
        document.body.appendChild(teacherBtn);

        // 프로필 모달 바인딩
        const profileBtn = document.getElementById('w-btn-student-profile');
        profileBtn.addEventListener('click', () => { studentModal.style.display = 'flex'; });
        document.getElementById('w-student-modal-close').addEventListener('click', () => { studentModal.style.display = 'none'; });
        studentModal.addEventListener('click', (e) => { if (e.target === studentModal) studentModal.style.display = 'none'; });

        document.getElementById('w-btn-student-save').addEventListener('click', () => {
            const name = document.getElementById('w-input-name').value.trim();
            if (!name) {
                alert("이름을 입력해 주세요!");
                document.getElementById('w-input-name').focus();
                return;
            }
            if (WaurimalProfanity.check(name)) {
                alert("⚠️ 바르고 고운 말을 사용해 주세요!\n비속어, 욕설 또는 부적절한 단어는 이름으로 등록할 수 없습니다.");
                document.getElementById('w-input-name').focus();
                return;
            }
            const newProfile = {
                school: document.getElementById('w-input-school').value.trim() || "이의초등학교",
                grade: document.getElementById('w-input-grade').value,
                classNum: document.getElementById('w-input-class').value,
                studentNum: document.getElementById('w-input-num').value.trim() || "1",
                name: name
            };
            if (!WaurimalStudent.save(newProfile)) return;
            studentModal.style.display = 'none';

            const chip = document.getElementById('w-btn-student-profile');
            chip.classList.remove('empty');
            document.getElementById('w-student-chip-name').innerText = WaurimalStudent.getDisplayText(newProfile);
            WaurimalToast(`환영합니다! ${newProfile.school} ${name} 학생 ✨`, '👤');
        });

        document.getElementById('w-btn-student-clear').addEventListener('click', () => {
            if (confirm("학생 정보를 초기화하시겠습니까?")) {
                WaurimalStudent.clear();
                studentModal.style.display = 'none';
                const chip = document.getElementById('w-btn-student-profile');
                chip.classList.add('empty');
                document.getElementById('w-student-chip-name').innerText = "학교명 / 학생 정보 등록";
                WaurimalToast('학생 정보가 초기화되었습니다.', '🔄');
            }
        });

        // 가이드 모달 바인딩
        const guideBtn = document.getElementById('w-btn-guide');
        guideBtn.addEventListener('click', () => { guideModal.style.display = 'flex'; });
        const closeGuide = () => { guideModal.style.display = 'none'; };
        document.getElementById('w-guide-modal-close').addEventListener('click', closeGuide);
        document.getElementById('w-guide-modal-ok').addEventListener('click', closeGuide);
        guideModal.addEventListener('click', (e) => { if (e.target === guideModal) closeGuide(); });

        // 사운드 토글
        const soundBtn = document.getElementById('w-btn-sound');
        soundBtn.addEventListener('click', () => {
            const isMuted = WaurimalAudio.toggleMute();
            document.getElementById('w-sound-icon').innerText = isMuted ? '🔇' : '🔊';
            WaurimalToast(isMuted ? '효과음이 꺼졌습니다.' : '효과음이 켜졌습니다.', isMuted ? '🔇' : '🔊');
        });

        // 홈으로 이동
        function handleNavigateHome(e) {
            if (e) {
                e.preventDefault();
                e.stopPropagation();
            }
            try { WaurimalAudio.play("click"); } catch(err) {}
            window.location.href = getHomeUrl();
        }

        const logoHome = document.getElementById("w-logo-home");
        if (logoHome) {
            logoHome.addEventListener("click", handleNavigateHome);
            logoHome.addEventListener("touchend", handleNavigateHome);
        }

        // 의견 및 피드백 페이지로 이동
        function handleNavigateFeedback(e) {
            if (e) {
                e.preventDefault();
                e.stopPropagation();
            }
            try { WaurimalAudio.play("click"); } catch(err) {}
            window.location.href = getFeedbackUrl();
        }

        const feedbackBtn = document.getElementById("w-btn-feedback");
        if (feedbackBtn) {
            feedbackBtn.addEventListener("click", handleNavigateFeedback);
            feedbackBtn.addEventListener("touchend", handleNavigateFeedback);
        }

        // 전체화면 토글
        document.getElementById('w-btn-fullscreen').addEventListener('click', () => {
            if (!document.fullscreenElement) {
                document.documentElement.requestFullscreen().catch(err => console.warn(err));
            } else {
                document.exitFullscreen().catch(err => console.warn(err));
            }
        });

        // 접기 토글
        const toggleHandle = document.getElementById('w-gnb-toggle-handle');
        toggleHandle.addEventListener('click', () => {
            const isCollapsed = wrapper.classList.toggle('w-collapsed');
            toggleHandle.innerText = isCollapsed ? '▼ 메뉴' : '▲ 접기';
        });

        // 사운드 바인딩
        document.querySelectorAll('.w-gnb-btn, .w-gnb-student-chip').forEach(btn => {
            btn.addEventListener('click', () => WaurimalAudio.play('click'));
        });

        // 폼 자동 채우기 및 원클릭 입장 카드 렌더링
        WaurimalStudent.autoFillForms();
        WaurimalStudent.renderQuickStartBox();

        // 지연 로딩 화면 대비 재시도
        setTimeout(() => {
            WaurimalStudent.autoFillForms();
            WaurimalStudent.renderQuickStartBox();
        }, 500);
        setTimeout(() => {
            WaurimalStudent.autoFillForms();
            WaurimalStudent.renderQuickStartBox();
        }, 1500);

        // 화면 전환 감지 (MutationObserver)
        const observer = new MutationObserver(() => {
            WaurimalStudent.autoFillForms();
            WaurimalStudent.renderQuickStartBox();
        });
        observer.observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['class', 'style'] });

        // 로컬 폼 제출 자동 캡처
        document.addEventListener('click', (e) => {
            const btn = e.target.closest('button, input[type="button"], input[type="submit"]');
            if (!btn || btn.closest('#waurimal-gnb-wrapper') || btn.closest('#w-student-modal')) return;

            const nameEl = document.querySelector('#studentName, #p-name, #st-name, #loginName, #student-name, #input-name, #name-input, #inp-name, #uploaderName');
            const gradeEl = document.querySelector('#grade, #p-grade, #loginGrade, #student-grade, #grade-select, #grade-input, #st-grade, #inp-grade, #input-grade');
            const classEl = document.querySelector('#classNum, #p-class, #loginClass, #student-class, #class-select, #class-input, #st-class, #inp-class, #input-class');
            const numEl = document.querySelector('#studentNum, #p-num, #st-num, #loginNumber, #student-num, #input-num, #input-number, #number-input, #inp-num');

            if (nameEl && nameEl.value && nameEl.value.trim().length > 0) {
                const rawName = nameEl.value.trim();
                if (WaurimalProfanity.check(rawName)) return;
                const current = WaurimalStudent.get() || {};
                const captured = {
                    school: current.school || "이의초등학교",
                    grade: (gradeEl && gradeEl.value) || current.grade || "6",
                    classNum: (classEl && classEl.value) || current.classNum || "1",
                    studentNum: (numEl && numEl.value) || current.studentNum || "1",
                    name: rawName
                };
                if (!current.name || current.name !== captured.name) {
                    WaurimalStudent.save(captured);
                    const chip = document.getElementById('w-btn-student-profile');
                    if (chip) chip.classList.remove('empty');
                    const chipName = document.getElementById('w-student-chip-name');
                    if (chipName) chipName.innerText = WaurimalStudent.getDisplayText(captured);
                }
            }
        }, true);
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initGNB);
    } else {
        initGNB();
    }
})();
