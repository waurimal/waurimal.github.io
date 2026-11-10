/**
 * 에듀테크 영어 놀이터 Firebase 통합 설정 및 프로젝트 샤딩 모듈 (Waurimal Firebase Sharding Engine)
 * 2026 Eui Elementary School - Waurimal Project
 * 
 * 429 RESOURCE_EXHAUSTED 할당량 초과 방지를 위한 앱별 전용 프로젝트 분산(Sharding) 및 지능형 폴백
 */
(function() {
    'use strict';

    // 1. 공통 통합 프로젝트 (기본값): eng-activity
    const PRIMARY_CONFIG = {
        apiKey: "AIzaSyDHCnRrVE2dVuJpf77P-1oftGpczR5WDd4",
        authDomain: "eng-activity.firebaseapp.com",
        projectId: "eng-activity",
        storageBucket: "eng-activity.firebasestorage.app",
        messagingSenderId: "528859753713",
        appId: "1:528859753713:web:a1ed4a054380d181c10d50"
    };

    // 2. 앱별 전용 샤딩 프로젝트 (각각 일일 50,000회 읽기 / 20,000회 쓰기 독립 쿼터 보유)
    const SHARDED_CONFIGS = {
        // [아케이드 & 슈팅] 스펠링 갤러그, 카드 뒤집기
        "spelling_galaga": {
            apiKey: "AIzaSyAaRTZlJ_fQAKl8j0lCJfaOdJyx8wMP5Zo",
            authDomain: "tournament-267ab.firebaseapp.com",
            projectId: "tournament-267ab",
            storageBucket: "tournament-267ab.firebasestorage.app",
            messagingSenderId: "401796751337",
            appId: "1:401796751337:web:6f3ba64fde25adf1a8f578"
        },
        "galaga": {
            apiKey: "AIzaSyAaRTZlJ_fQAKl8j0lCJfaOdJyx8wMP5Zo",
            authDomain: "tournament-267ab.firebaseapp.com",
            projectId: "tournament-267ab",
            storageBucket: "tournament-267ab.firebasestorage.app",
            messagingSenderId: "401796751337",
            appId: "1:401796751337:web:6f3ba64fde25adf1a8f578"
        },
        "flip": {
            apiKey: "AIzaSyAaRTZlJ_fQAKl8j0lCJfaOdJyx8wMP5Zo",
            authDomain: "tournament-267ab.firebaseapp.com",
            projectId: "tournament-267ab",
            storageBucket: "tournament-267ab.firebasestorage.app",
            messagingSenderId: "401796751337",
            appId: "1:401796751337:web:6f3ba64fde25adf1a8f578"
        },

        // [타이핑] 단어 타이핑, 문장 타이핑
        "wordtyping": {
            apiKey: "AIzaSyCer92ve-QWjTVKaoYbUJaoqqoTJKCz7yI",
            authDomain: "sentence-typing-race.firebaseapp.com",
            projectId: "sentence-typing-race",
            storageBucket: "sentence-typing-race.firebasestorage.app",
            messagingSenderId: "28750171446",
            appId: "1:28750171446:web:8686d2c698bd5536cb3797"
        },
        "sen_typing": {
            apiKey: "AIzaSyCer92ve-QWjTVKaoYbUJaoqqoTJKCz7yI",
            authDomain: "sentence-typing-race.firebaseapp.com",
            projectId: "sentence-typing-race",
            storageBucket: "sentence-typing-race.firebasestorage.app",
            messagingSenderId: "28750171446",
            appId: "1:28750171446:web:8686d2c698bd5536cb3797"
        },

        // [읽기 스프린트]
        "reading": {
            apiKey: "AIzaSyBjdUUBlXqEQApdujZ6kqAwoBisAjALNXV28",
            authDomain: "reading-c0510.firebaseapp.com",
            projectId: "reading-c0510",
            storageBucket: "reading-c0510.firebasestorage.app",
            messagingSenderId: "140234107393",
            appId: "1:140234107393:web:6ede13e43bf353a8b5ee00"
        },

        // [야구 게임]
        "baseball": {
            apiKey: "AIzaSyDGinyJXc0n39-E-mdWQlASRmOwSWgZeuI",
            authDomain: "baseball-6f677.firebaseapp.com",
            projectId: "baseball-6f677",
            storageBucket: "baseball-6f677.firebasestorage.app",
            messagingSenderId: "258235991761",
            appId: "1:258235991761:web:eb7201c775bad0c46fe7f0"
        },

        // [단어 스크리블 캐치마인드]
        "skribble": {
            apiKey: "AIzaSyB9or-HoTZaXsk7hx4OdBX8b6TVVfmLtzs",
            authDomain: "skribble-2713c.firebaseapp.com",
            projectId: "skribble-2713c",
            storageBucket: "skribble-2713c.firebasestorage.app",
            messagingSenderId: "206350924074",
            appId: "1:206350924074:web:a659dbd6fe13fc56cec653"
        },

        // [에듀마블]
        "bluemarble": {
            apiKey: "AIzaSyD6YK02DdhaSVN9mx3Acd5amiiphxcobG8",
            authDomain: "mapia-29b3a.firebaseapp.com",
            projectId: "mapia-29b3a",
            storageBucket: "mapia-29b3a.firebasestorage.app",
            messagingSenderId: "937487062003",
            appId: "1:937487062003:web:2d98688bc5d0db5bfab886"
        },

        // [단어 떼지어 몰려들기 Swarm]
        "swarm": {
            apiKey: "AIzaSyBM_93jHRAwHuxFYMlHpa7kqAVp0u2v7U0",
            authDomain: "swarm-c23b4.firebaseapp.com",
            projectId: "swarm-c23b4",
            storageBucket: "swarm-c23b4.firebasestorage.app",
            messagingSenderId: "514706604321",
            appId: "1:514706604321:web:ede3ef113bb74a50a67e60"
        },

        // [단어 탐정 Where]
        "where": {
            apiKey: "AIzaSyCAR_dSA_jViVY0RZAI3s41a39u9NufFj8",
            authDomain: "where-2599e.firebaseapp.com",
            projectId: "where-2599e",
            storageBucket: "where-2599e.firebasestorage.app",
            messagingSenderId: "1090101095033",
            appId: "1:1090101095033:web:b80a7311d8dff569b842a8"
        },

        // [단어 빙고]
        "bingo": {
            apiKey: "AIzaSyC9Qas3pt9DZITM5UZr2GxKEQvykJq_Kh0",
            authDomain: "bingo-1707f.firebaseapp.com",
            projectId: "bingo-1707f",
            storageBucket: "bingo-1707f.firebasestorage.app",
            messagingSenderId: "995876035744",
            appId: "1:995876035744:web:84917aa4beb54a86ab7dc5"
        },

        // [틱택토]
        "tictac": {
            apiKey: "AIzaSyDXp0gubAUnliOqznx37Xu4mrQ9NoHLhaA",
            authDomain: "tictac-c3d4f.firebaseapp.com",
            projectId: "tictac-c3d4f",
            storageBucket: "tictac-c3d4f.firebasestorage.app",
            messagingSenderId: "192437292015",
            appId: "1:192437292015:web:0cda453bc116146589841a"
        }
    };

    // 현재 실행 중인 앱 ID 자동 감지
    function detectAppId() {
        if (typeof window === 'undefined' || !window.location) return null;
        const path = (window.location.pathname || '').toLowerCase();
        for (const appId in SHARDED_CONFIGS) {
            if (path.includes('/' + appId + '/') || path.endsWith('/' + appId) || path.endsWith('/' + appId + '.html') || path.endsWith('/' + appId + '/index.html')) {
                return appId;
            }
        }
        return null;
    }

    // 회로 차단기 (Circuit Breaker) 상태 관리
    const CIRCUIT_STORAGE_KEY = 'waurimal_circuit_broken_';

    function isCircuitBroken(projectId) {
        try {
            if (typeof sessionStorage === 'undefined') return false;
            const raw = sessionStorage.getItem(CIRCUIT_STORAGE_KEY + projectId);
            if (raw) {
                const unblockTime = Number(raw);
                if (Date.now() < unblockTime) {
                    return true;
                }
                sessionStorage.removeItem(CIRCUIT_STORAGE_KEY + projectId);
            }
        } catch (e) {}
        return false;
    }

    function tripCircuitBreaker(projectId, durationMinutes = 15) {
        try {
            if (typeof sessionStorage === 'undefined') return;
            sessionStorage.setItem(CIRCUIT_STORAGE_KEY + projectId, String(Date.now() + durationMinutes * 60 * 1000));
            console.warn(`[WaurimalFirebase] Circuit breaker tripped for project '${projectId}'. Remote requests suppressed for ${durationMinutes}m.`);
        } catch (e) {}
    }

    window.WaurimalFirebase = {
        // 앱별 전용 샤딩 설정 반환 (미지정 시 URL 기반 자동 감지)
        getConfig: function(appId) {
            const targetApp = appId || detectAppId();
            if (targetApp && SHARDED_CONFIGS[targetApp]) {
                const conf = SHARDED_CONFIGS[targetApp];
                return { ...conf };
            }
            return { ...PRIMARY_CONFIG };
        },

        getShardedConfig: function(appId) {
            return this.getConfig(appId);
        },

        // 회로 차단기 API
        tripCircuitBreaker: tripCircuitBreaker,
        isCircuitBroken: isCircuitBroken,

        // 앱별 컬렉션 경로
        getAppCollectionPath: function(appId, subPath) {
            return `games/${appId}/${subPath}`;
        },

        // 레거시 설정 호환
        getLegacyConfig: function(projectId) {
            return SHARDED_CONFIGS[projectId] || null;
        }
    };
})();
