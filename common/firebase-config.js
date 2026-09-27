/**
 * 에듀테크 영어 놀이터 Firebase 통합 설정 모듈 (Waurimal Firebase Config)
 * 2026 Eui Elementary School - Waurimal Project
 */
(function() {
    'use strict';

    // 1. 최신 표준 통합 프로젝트: eng-activity (2026 통일 프로젝트)
    const PRIMARY_CONFIG = {
        apiKey: "AIzaSyDHCnRrVE2dVuJpf77P-1oftGpczR5WDd4",
        authDomain: "eng-activity.firebaseapp.com",
        projectId: "eng-activity",
        storageBucket: "eng-activity.firebasestorage.app",
        messagingSenderId: "528859753713",
        appId: "1:528859753713:web:a1ed4a054380d181c10d50"
    };

    // 2. 레거시 프로젝트별 백업 설정 (기존 데이터 유지용)
    const LEGACY_CONFIGS = {
        "sentence-typing-race": {
            apiKey: "AIzaSyD1x-placeholder",
            authDomain: "sentence-typing-race.firebaseapp.com",
            projectId: "sentence-typing-race"
        },
        "tournament-267ab": {
            apiKey: "AIzaSyCX-placeholder",
            authDomain: "tournament-267ab.firebaseapp.com",
            projectId: "tournament-267ab"
        },
        "alchemist-ef277": {
            projectId: "alchemist-ef277"
        },
        "hangman-e4a7b": {
            projectId: "hangman-e4a7b"
        }
    };

    window.WaurimalFirebase = {
        // 기본 표준 설정 반환
        getConfig: function() {
            return { ...PRIMARY_CONFIG };
        },

        // 특정 앱을 위한 네임스페이스 컬렉션 경로 반환 (충돌 방지)
        // 예: getAppCollectionPath('hangman', 'rooms') => 'games/hangman/rooms'
        getAppCollectionPath: function(appId, subPath) {
            return `games/${appId}/${subPath}`;
        },

        // 레거시 설정 조회
        getLegacyConfig: function(projectId) {
            return LEGACY_CONFIGS[projectId] || null;
        }
    };
})();
