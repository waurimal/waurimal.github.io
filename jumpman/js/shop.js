/**
 * 점프맨 코인 상점 시스템 (Jumpman Coin Shop & Customization)
 * - 코인 수집 및 누적 저장 (localStorage)
 * - 마리오 패밀리 캐릭터 스킨 구매 & 장착
 * - 게임 스타트 파워업 아이템 구매 & 인벤토리 관리
 * - 다이내믹 낮밤 순환 및 스페셜 월드 배경 테마 구매 & 장착
 */

const JUMPMAN_SHOP_DATA = {
    characters: [
        {
            id: 'mario',
            name: '점프맨 (마리오)',
            title: '원조 런앤점프 영웅',
            desc: '균형 잡힌 점프력과 달리기 속도를 지닌 마리오의 원조 영웅!',
            price: 0,
            icon: '🔴',
            color: '#DC2626',
            stats: { speed: 5.5, jump: -12.8, perkText: '표준 밸런스 능력치' }
        },
        {
            id: 'luigi',
            name: '루이지 (Luigi)',
            title: '하이 점퍼 & 롱다리',
            desc: '높은 점프력과 부드러운 체공력으로 까다로운 장애물도 시원하게 통과!',
            price: 150,
            icon: '🟢',
            color: '#16A34A',
            stats: { speed: 5.5, jump: -13.7, perkText: '점프력 +7% & 공중 체공 시간 증가' }
        },
        {
            id: 'wario',
            name: '와리오 (Wario)',
            title: '골드 마그넷 거인',
            desc: '황금과 보물을 사랑하는 괴력의 사나이! 주변의 코인을 훨씬 넓은 반경에서 끌어당깁니다.',
            price: 200,
            icon: '🟡',
            color: '#EAB308',
            stats: { speed: 5.4, jump: -12.5, perkText: '기본 코인 흡수 반경 +45px (상시 자석)' }
        },
        {
            id: 'toad',
            name: '키노피오 (Toad)',
            title: '질주하는 스피드 러너',
            desc: '작고 날렵한 버섯 왕국의 날쌘돌이! 좌우 조작 반응과 달리기 속도가 가장 빠릅니다.',
            price: 180,
            icon: '🍄',
            color: '#0284C7',
            stats: { speed: 6.3, jump: -12.7, perkText: '이동 및 반응 속도 +15%' }
        },
        {
            id: 'peach',
            name: '피치 공주 (Peach)',
            title: '우아한 플로트 점퍼',
            desc: '핑크 드레스를 살랑이며 우아하게 착지합니다. 공중에서 천천히 하강하여 안전하게 착지!',
            price: 250,
            icon: '👑',
            color: '#EC4899',
            stats: { speed: 5.5, jump: -12.8, perkText: '점프 낙하 시 중력 -20% (우아한 활공)' }
        },
        {
            id: 'fire_mario',
            name: '파이어 마리오 (Fire Mario)',
            title: '화염의 불꽃 마스터',
            desc: '화염의 힘을 다루는 순백의 영웅! 5.5초마다 전방으로 파이어볼을 발사해 장애물을 파괴합니다!',
            price: 300,
            icon: '🔥',
            color: '#F97316',
            stats: { speed: 5.6, jump: -12.8, perkText: '5.5초마다 전방 파이어볼 발사(장애물 격파)' }
        }
    ],
    items: [
        {
            id: 'start_shield',
            name: '스타트 쉴드 (Shield)',
            title: '충돌 1회 완벽 방어막',
            desc: '게임 시작 시 장애물 1회 충돌을 완벽하게 무효화하는 에너지 쉴드를 장착합니다.',
            price: 50,
            icon: '🛡️',
            badge: '인기',
            badgeColor: 'bg-blue-500/20 text-blue-300 border-blue-500/40'
        },
        {
            id: 'start_magnet',
            name: '슈퍼 자석 (Coin Magnet)',
            title: '15초 황금 코인 자동 흡수',
            desc: '게임 시작과 동시에 15초간 주변에 등장하는 모든 코인을 자동으로 쏙쏙 끌어당깁니다.',
            price: 40,
            icon: '🧲',
            badge: '파밍 추천',
            badgeColor: 'bg-amber-500/20 text-amber-300 border-amber-500/40'
        },
        {
            id: 'start_star',
            name: '스타 대시 (Star Dash)',
            title: '8초 무적 질주 & 장애물 격파',
            desc: '시작 8초간 황금 별의 힘으로 전속력 무적 돌진! 부딪히는 모든 장애물을 날려버립니다.',
            price: 60,
            icon: '⭐',
            badge: '돌파',
            badgeColor: 'bg-yellow-500/20 text-yellow-300 border-yellow-500/40'
        },
        {
            id: 'extra_heart',
            name: '보너스 하트 (+1 Life)',
            title: '부활 기회 4회로 확장',
            desc: '기본 3회의 부활 퀴즈 찬스에 1회를 추가하여 총 4번의 부활 찬스를 갖고 출발합니다.',
            price: 80,
            icon: '💖',
            badge: '고득점 필수',
            badgeColor: 'bg-rose-500/20 text-rose-300 border-rose-500/40'
        }
    ],
    backgrounds: [
        {
            id: 'cycle',
            name: '실시간 낮·노을·밤 순환',
            title: 'Day & Night Cycle (다이내믹)',
            desc: '시간과 거리가 흐름에 따라 낮 ☀️ -> 노을 🌅 -> 밤 🌙 -> 새벽 🌄이 실시간으로 전환됩니다!',
            price: 0,
            icon: '🌓',
            color: '#3B82F6',
            badge: '기본 탑재'
        },
        {
            id: 'desert',
            name: '사막의 피라미드 & 오아시스',
            title: 'Desert & Ancient Pyramids',
            desc: '황금빛 모래 언덕과 신비로운 고대 피라미드가 펼쳐진 이집트 사막 어드벤처!',
            price: 100,
            icon: '🏜️',
            color: '#F59E0B',
            badge: '이국적 테마'
        },
        {
            id: 'cyber',
            name: '사이버 네온 시티',
            title: 'Cyberpunk Neon City',
            desc: '화려한 네온사인과 홀로그램 빌딩이 반짝이는 퓨처리스틱 사이버 도시!',
            price: 150,
            icon: '🌃',
            color: '#A855F7',
            badge: '화려한 네온'
        },
        {
            id: 'lava',
            name: '쿠파 성 & 불꽃 용암',
            title: 'Bowser Lava Castle',
            desc: '부글부글 끓어오르는 붉은 용암과 웅장한 가시 성채의 극한 스릴 월드!',
            price: 200,
            icon: '🌋',
            color: '#EF4444',
            badge: '인기 테마'
        },
        {
            id: 'galaxy',
            name: '코스믹 갤럭시 & 스타월드',
            title: 'Cosmic Galaxy & Outer Space',
            desc: '반짝이는 성운과 별똥별, 거대한 고리 행성이 떠 있는 환상적인 우주 공간!',
            price: 250,
            icon: '🌌',
            color: '#6366F1',
            badge: '환상적 우주'
        }
    ]
};

class JumpmanShopManager {
    constructor() {
        this.storageKeyPrefix = 'jumpman_';
        this.currentTab = 'characters'; // 'characters' | 'items' | 'backgrounds'

        this.initStorage();
        this.bindEvents();
    }

    initStorage() {
        // 총 코인 초기화 (신규 방문자에게 웰컴 보너스 100 코인 증정)
        if (localStorage.getItem(this.storageKeyPrefix + 'total_coins') === null) {
            localStorage.setItem(this.storageKeyPrefix + 'total_coins', '100');
        }

        // 해금된 캐릭터 스킨 목록
        if (!localStorage.getItem(this.storageKeyPrefix + 'unlocked_skins')) {
            localStorage.setItem(this.storageKeyPrefix + 'unlocked_skins', JSON.stringify(['mario']));
        }

        // 장착된 캐릭터
        if (!localStorage.getItem(this.storageKeyPrefix + 'equipped_skin')) {
            localStorage.setItem(this.storageKeyPrefix + 'equipped_skin', 'mario');
        }

        // 해금된 배경 테마 목록
        if (!localStorage.getItem(this.storageKeyPrefix + 'unlocked_bgs')) {
            localStorage.setItem(this.storageKeyPrefix + 'unlocked_bgs', JSON.stringify(['cycle']));
        }

        // 장착된 배경 테마
        if (!localStorage.getItem(this.storageKeyPrefix + 'equipped_bg')) {
            localStorage.setItem(this.storageKeyPrefix + 'equipped_bg', 'cycle');
        }

        // 아이템 인벤토리
        if (!localStorage.getItem(this.storageKeyPrefix + 'inventory')) {
            localStorage.setItem(this.storageKeyPrefix + 'inventory', JSON.stringify({
                start_shield: 0,
                start_magnet: 0,
                start_star: 0,
                extra_heart: 0
            }));
        }

        // 다음 게임에 사용할 활성 부스트 설정
        if (!localStorage.getItem(this.storageKeyPrefix + 'active_boosts')) {
            localStorage.setItem(this.storageKeyPrefix + 'active_boosts', JSON.stringify({
                start_shield: true,
                start_magnet: true,
                start_star: true,
                extra_heart: true
            }));
        }
    }

    getTotalCoins() {
        const val = parseInt(localStorage.getItem(this.storageKeyPrefix + 'total_coins'), 10);
        return isNaN(val) ? 0 : val;
    }

    addCoins(amount) {
        if (!amount || amount <= 0) return this.getTotalCoins();
        const cur = this.getTotalCoins();
        const next = cur + amount;
        localStorage.setItem(this.storageKeyPrefix + 'total_coins', next.toString());
        this.updateAllCoinDisplays();
        return next;
    }

    spendCoins(amount) {
        const cur = this.getTotalCoins();
        if (cur < amount) return false;
        const next = cur - amount;
        localStorage.setItem(this.storageKeyPrefix + 'total_coins', next.toString());
        this.updateAllCoinDisplays();
        return true;
    }

    getUnlockedSkins() {
        try {
            return JSON.parse(localStorage.getItem(this.storageKeyPrefix + 'unlocked_skins')) || ['mario'];
        } catch (e) {
            return ['mario'];
        }
    }

    getEquippedSkin() {
        return localStorage.getItem(this.storageKeyPrefix + 'equipped_skin') || 'mario';
    }

    equipSkin(id) {
        const unlocked = this.getUnlockedSkins();
        if (!unlocked.includes(id)) return false;
        localStorage.setItem(this.storageKeyPrefix + 'equipped_skin', id);
        if (window.JumpmanAudio) window.JumpmanAudio.playEquip();
        this.updateHomeProfilePreview();
        return true;
    }

    buySkin(id) {
        const item = JUMPMAN_SHOP_DATA.characters.find(c => c.id === id);
        if (!item) return false;
        const unlocked = this.getUnlockedSkins();
        if (unlocked.includes(id)) return true; // 이미 보유

        if (this.spendCoins(item.price)) {
            unlocked.push(id);
            localStorage.setItem(this.storageKeyPrefix + 'unlocked_skins', JSON.stringify(unlocked));
            this.equipSkin(id);
            if (window.JumpmanAudio) window.JumpmanAudio.playShopBuy();
            this.showToast(`🎉 ${item.name} 캐릭터를 구매하여 바로 장착했습니다!`);
            this.renderShopModal();
            return true;
        } else {
            this.showToast(`코인이 부족합니다! (필요: ${item.price} 🪙, 보유: ${this.getTotalCoins()} 🪙)`, 'error');
            return false;
        }
    }

    getUnlockedBgs() {
        try {
            return JSON.parse(localStorage.getItem(this.storageKeyPrefix + 'unlocked_bgs')) || ['cycle'];
        } catch (e) {
            return ['cycle'];
        }
    }

    getEquippedBg() {
        return localStorage.getItem(this.storageKeyPrefix + 'equipped_bg') || 'cycle';
    }

    equipBg(id) {
        const unlocked = this.getUnlockedBgs();
        if (!unlocked.includes(id)) return false;
        localStorage.setItem(this.storageKeyPrefix + 'equipped_bg', id);
        if (window.JumpmanAudio) window.JumpmanAudio.playEquip();
        this.updateHomeProfilePreview();
        return true;
    }

    buyBg(id) {
        const item = JUMPMAN_SHOP_DATA.backgrounds.find(b => b.id === id);
        if (!item) return false;
        const unlocked = this.getUnlockedBgs();
        if (unlocked.includes(id)) return true;

        if (this.spendCoins(item.price)) {
            unlocked.push(id);
            localStorage.setItem(this.storageKeyPrefix + 'unlocked_bgs', JSON.stringify(unlocked));
            this.equipBg(id);
            if (window.JumpmanAudio) window.JumpmanAudio.playShopBuy();
            this.showToast(`🎉 [${item.name}] 배경 테마를 구매하여 적용했습니다!`);
            this.renderShopModal();
            return true;
        } else {
            this.showToast(`코인이 부족합니다! (필요: ${item.price} 🪙, 보유: ${this.getTotalCoins()} 🪙)`, 'error');
            return false;
        }
    }

    getInventory() {
        try {
            return JSON.parse(localStorage.getItem(this.storageKeyPrefix + 'inventory')) || {
                start_shield: 0,
                start_magnet: 0,
                start_star: 0,
                extra_heart: 0
            };
        } catch (e) {
            return { start_shield: 0, start_magnet: 0, start_star: 0, extra_heart: 0 };
        }
    }

    buyItem(id) {
        const item = JUMPMAN_SHOP_DATA.items.find(it => it.id === id);
        if (!item) return false;

        if (this.spendCoins(item.price)) {
            const inv = this.getInventory();
            inv[id] = (inv[id] || 0) + 1;
            localStorage.setItem(this.storageKeyPrefix + 'inventory', JSON.stringify(inv));
            if (window.JumpmanAudio) window.JumpmanAudio.playShopBuy();
            this.showToast(`✨ ${item.name}을(를) 1개 구매했습니다! (보유: ${inv[id]}개)`);
            this.renderShopModal();
            return true;
        } else {
            this.showToast(`코인이 부족합니다! (필요: ${item.price} 🪙, 보유: ${this.getTotalCoins()} 🪙)`, 'error');
            return false;
        }
    }

    getActiveBoostSettings() {
        try {
            return JSON.parse(localStorage.getItem(this.storageKeyPrefix + 'active_boosts')) || {
                start_shield: true,
                start_magnet: true,
                start_star: true,
                extra_heart: true
            };
        } catch (e) {
            return { start_shield: true, start_magnet: true, start_star: true, extra_heart: true };
        }
    }

    toggleBoostSetting(id) {
        const boosts = this.getActiveBoostSettings();
        boosts[id] = !boosts[id];
        localStorage.setItem(this.storageKeyPrefix + 'active_boosts', JSON.stringify(boosts));
        this.renderShopModal();
    }

    // 게임 시작 시 인벤토리에서 활성화된 부스트 아이템을 소모하고 적용할 버프 객체 반환
    consumeActiveBoosts() {
        const inv = this.getInventory();
        const settings = this.getActiveBoostSettings();
        const result = {
            shield: false,
            magnetSeconds: 0,
            starSeconds: 0,
            extraChances: 0
        };

        if (settings.start_shield && inv.start_shield > 0) {
            inv.start_shield--;
            result.shield = true;
        }

        if (settings.start_magnet && inv.start_magnet > 0) {
            inv.start_magnet--;
            result.magnetSeconds = 15;
        }

        if (settings.start_star && inv.start_star > 0) {
            inv.start_star--;
            result.starSeconds = 8;
        }

        if (settings.extra_heart && inv.extra_heart > 0) {
            inv.extra_heart--;
            result.extraChances = 1;
        }

        localStorage.setItem(this.storageKeyPrefix + 'inventory', JSON.stringify(inv));
        return result;
    }

    updateAllCoinDisplays() {
        const coins = this.getTotalCoins();
        const formatted = coins.toLocaleString();

        const targets = [
            'homeCoinDisplay',
            'shopModalCoinDisplay',
            'hudTotalCoinDisplay',
            'gameOverCurrentCoins'
        ];

        targets.forEach(id => {
            const el = document.getElementById(id);
            if (el) el.textContent = formatted;
        });
    }

    updateHomeProfilePreview() {
        const equippedSkinId = this.getEquippedSkin();
        const skin = JUMPMAN_SHOP_DATA.characters.find(c => c.id === equippedSkinId) || JUMPMAN_SHOP_DATA.characters[0];

        const equippedBgId = this.getEquippedBg();
        const bg = JUMPMAN_SHOP_DATA.backgrounds.find(b => b.id === equippedBgId) || JUMPMAN_SHOP_DATA.backgrounds[0];

        const skinEl = document.getElementById('homeEquippedSkinBadge');
        if (skinEl) {
            skinEl.innerHTML = `${skin.icon} <span class="font-bold">${skin.name}</span>`;
        }

        const bgEl = document.getElementById('homeEquippedBgBadge');
        if (bgEl) {
            bgEl.innerHTML = `${bg.icon} <span class="font-bold">${bg.name}</span>`;
        }
    }

    bindEvents() {
        // 팝업 열기 버튼 바인딩
        const openBtns = ['btnOpenShop', 'btnOpenShopFromHome', 'btnOpenShopFromGameOver'];
        openBtns.forEach(id => {
            const btn = document.getElementById(id);
            if (btn) {
                btn.addEventListener('click', () => this.openShopModal());
            }
        });

        // 팝업 닫기 버튼
        const closeBtn = document.getElementById('btnCloseShop');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => this.closeShopModal());
        }

        // 탭 버튼 바인딩
        ['characters', 'items', 'backgrounds'].forEach(tab => {
            const tabBtn = document.getElementById(`shopTab-${tab}`);
            if (tabBtn) {
                tabBtn.addEventListener('click', () => {
                    this.currentTab = tab;
                    this.renderShopModal();
                });
            }
        });
    }

    openShopModal(tab = null) {
        if (tab) this.currentTab = tab;
        this.updateAllCoinDisplays();
        this.renderShopModal();
        const modal = document.getElementById('shopModal');
        if (modal) modal.classList.remove('hidden');
    }

    closeShopModal() {
        const modal = document.getElementById('shopModal');
        if (modal) modal.classList.add('hidden');
    }

    renderShopModal() {
        this.updateAllCoinDisplays();

        // 탭 활성화 스타일
        ['characters', 'items', 'backgrounds'].forEach(tab => {
            const btn = document.getElementById(`shopTab-${tab}`);
            if (btn) {
                if (this.currentTab === tab) {
                    btn.className = 'shop-tab-btn flex-1 py-2 text-xs sm:text-sm font-black rounded-xl transition bg-gradient-to-r from-amber-500 to-yellow-500 text-slate-950 shadow-md';
                } else {
                    btn.className = 'shop-tab-btn flex-1 py-2 text-xs sm:text-sm font-bold rounded-xl transition text-slate-400 hover:text-white hover:bg-slate-800';
                }
            }
        });

        const container = document.getElementById('shopItemsContainer');
        if (!container) return;

        const totalCoins = this.getTotalCoins();

        if (this.currentTab === 'characters') {
            const unlocked = this.getUnlockedSkins();
            const equipped = this.getEquippedSkin();

            container.innerHTML = `
                <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3">
                    ${JUMPMAN_SHOP_DATA.characters.map(char => {
                        const isUnlocked = unlocked.includes(char.id);
                        const isEquipped = equipped === char.id;
                        const canAfford = totalCoins >= char.price;

                        return `
                            <div class="bg-slate-950/80 p-3.5 rounded-2xl border ${isEquipped ? 'border-amber-400 ring-2 ring-amber-400/30' : 'border-slate-800'} flex flex-col justify-between hover:border-slate-700 transition space-y-3">
                                <div>
                                    <div class="flex items-center justify-between mb-1.5">
                                        <div class="flex items-center gap-2">
                                            <span class="text-2xl">${char.icon}</span>
                                            <div>
                                                <h3 class="font-black text-sm text-white">${char.name}</h3>
                                                <span class="text-[10px] text-amber-300 font-bold">${char.title}</span>
                                            </div>
                                        </div>
                                        ${isEquipped ? `
                                            <span class="px-2 py-0.5 rounded-full bg-emerald-500/20 text-emerald-300 border border-emerald-500/40 text-[10px] font-black">
                                                ✓ 장착 중
                                            </span>
                                        ` : isUnlocked ? `
                                            <span class="px-2 py-0.5 rounded-full bg-blue-500/20 text-blue-300 border border-blue-500/40 text-[10px] font-bold">
                                                보유 중
                                            </span>
                                        ` : `
                                            <span class="px-2 py-0.5 rounded-full bg-amber-500/20 text-amber-300 border border-amber-500/40 text-[10px] font-black">
                                                🪙 ${char.price}
                                            </span>
                                        `}
                                    </div>
                                    <p class="text-[11px] text-slate-400 leading-relaxed min-h-[32px]">${char.desc}</p>
                                    <div class="mt-2 p-2 rounded-xl bg-slate-900 border border-slate-800 text-[10px] text-sky-300 font-medium flex items-center gap-1.5">
                                        <span>⚡</span>
                                        <span>${char.stats.perkText}</span>
                                    </div>
                                </div>

                                <div class="pt-1">
                                    ${isEquipped ? `
                                        <button type="button" disabled class="w-full py-2 rounded-xl bg-emerald-600/30 text-emerald-300 border border-emerald-500/40 font-bold text-xs cursor-default">
                                            현재 사용 중
                                        </button>
                                    ` : isUnlocked ? `
                                        <button type="button" onclick="JumpmanShop.equipSkin('${char.id}'); JumpmanShop.renderShopModal();" class="w-full py-2 rounded-xl bg-blue-600 hover:bg-blue-500 text-white font-black text-xs transition shadow-md cursor-pointer">
                                            착용하기
                                        </button>
                                    ` : `
                                        <button type="button" onclick="JumpmanShop.buySkin('${char.id}')" class="w-full py-2 rounded-xl ${canAfford ? 'bg-gradient-to-r from-amber-500 to-yellow-500 hover:from-amber-400 hover:to-yellow-400 text-slate-950 font-black' : 'bg-slate-800 text-slate-500 font-bold cursor-not-allowed'} text-xs transition shadow-md">
                                            🪙 ${char.price} 코인으로 구매
                                        </button>
                                    `}
                                </div>
                            </div>
                        `;
                    }).join('')}
                </div>
            `;
        } else if (this.currentTab === 'items') {
            const inv = this.getInventory();
            const activeBoosts = this.getActiveBoostSettings();

            container.innerHTML = `
                <div class="mb-3 p-3 bg-blue-500/10 border border-blue-500/30 rounded-xl text-xs text-blue-200 flex items-center gap-2">
                    <span class="text-lg">💡</span>
                    <span>구매한 파워업 아이템은 인벤토리에 보관되며, 다음 게임 시작 시 자동으로 장착되어 출발합니다!</span>
                </div>
                <div class="grid grid-cols-1 sm:grid-cols-2 gap-3">
                    ${JUMPMAN_SHOP_DATA.items.map(item => {
                        const count = inv[item.id] || 0;
                        const isAutoUse = !!activeBoosts[item.id];
                        const canAfford = totalCoins >= item.price;

                        return `
                            <div class="bg-slate-950/80 p-3.5 rounded-2xl border border-slate-800 flex flex-col justify-between hover:border-slate-700 transition space-y-3">
                                <div>
                                    <div class="flex items-center justify-between mb-1.5">
                                        <div class="flex items-center gap-2">
                                            <span class="text-2xl">${item.icon}</span>
                                            <div>
                                                <h3 class="font-black text-sm text-white">${item.name}</h3>
                                                <span class="text-[10px] text-amber-300 font-bold">${item.title}</span>
                                            </div>
                                        </div>
                                        <span class="px-2 py-0.5 rounded-full border text-[10px] font-black ${item.badgeColor || 'bg-amber-500/20 text-amber-300 border-amber-500/40'}">
                                            ${item.badge}
                                        </span>
                                    </div>
                                    <p class="text-[11px] text-slate-400 leading-relaxed min-h-[32px]">${item.desc}</p>
                                    
                                    <div class="mt-2.5 flex items-center justify-between p-2 rounded-xl bg-slate-900 border border-slate-800 text-xs">
                                        <span class="text-slate-300 font-bold">내 보유 수량:</span>
                                        <span class="text-sm font-black ${count > 0 ? 'text-amber-400' : 'text-slate-500'}">${count}개</span>
                                    </div>
                                </div>

                                <div class="space-y-2 pt-1">
                                    <button type="button" onclick="JumpmanShop.buyItem('${item.id}')" class="w-full py-2 rounded-xl ${canAfford ? 'bg-gradient-to-r from-amber-500 to-yellow-500 hover:from-amber-400 hover:to-yellow-400 text-slate-950 font-black' : 'bg-slate-800 text-slate-500 font-bold cursor-not-allowed'} text-xs transition shadow-md">
                                        🪙 ${item.price} 코인으로 구매 (+1개)
                                    </button>

                                    <label class="flex items-center justify-center gap-2 text-[11px] text-slate-400 cursor-pointer select-none">
                                        <input type="checkbox" ${isAutoUse ? 'checked' : ''} onchange="JumpmanShop.toggleBoostSetting('${item.id}')" class="rounded accent-amber-500">
                                        <span>다음 판에 자동 장착</span>
                                    </label>
                                </div>
                            </div>
                        `;
                    }).join('')}
                </div>
            `;
        } else if (this.currentTab === 'backgrounds') {
            const unlocked = this.getUnlockedBgs();
            const equipped = this.getEquippedBg();

            container.innerHTML = `
                <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3">
                    ${JUMPMAN_SHOP_DATA.backgrounds.map(bg => {
                        const isUnlocked = unlocked.includes(bg.id);
                        const isEquipped = equipped === bg.id;
                        const canAfford = totalCoins >= bg.price;

                        return `
                            <div class="bg-slate-950/80 p-3.5 rounded-2xl border ${isEquipped ? 'border-amber-400 ring-2 ring-amber-400/30' : 'border-slate-800'} flex flex-col justify-between hover:border-slate-700 transition space-y-3">
                                <div>
                                    <div class="flex items-center justify-between mb-1.5">
                                        <div class="flex items-center gap-2">
                                            <span class="text-2xl">${bg.icon}</span>
                                            <div>
                                                <h3 class="font-black text-sm text-white">${bg.name}</h3>
                                                <span class="text-[10px] text-sky-300 font-bold">${bg.title}</span>
                                            </div>
                                        </div>
                                        ${isEquipped ? `
                                            <span class="px-2 py-0.5 rounded-full bg-emerald-500/20 text-emerald-300 border border-emerald-500/40 text-[10px] font-black">
                                                ✓ 적용 중
                                            </span>
                                        ` : isUnlocked ? `
                                            <span class="px-2 py-0.5 rounded-full bg-blue-500/20 text-blue-300 border border-blue-500/40 text-[10px] font-bold">
                                                보유 중
                                            </span>
                                        ` : `
                                            <span class="px-2 py-0.5 rounded-full bg-amber-500/20 text-amber-300 border border-amber-500/40 text-[10px] font-black">
                                                🪙 ${bg.price}
                                            </span>
                                        `}
                                    </div>
                                    <p class="text-[11px] text-slate-400 leading-relaxed min-h-[32px]">${bg.desc}</p>
                                </div>

                                <div class="pt-1">
                                    ${isEquipped ? `
                                        <button type="button" disabled class="w-full py-2 rounded-xl bg-emerald-600/30 text-emerald-300 border border-emerald-500/40 font-bold text-xs cursor-default">
                                            현재 적용 중
                                        </button>
                                    ` : isUnlocked ? `
                                        <button type="button" onclick="JumpmanShop.equipBg('${bg.id}'); JumpmanShop.renderShopModal();" class="w-full py-2 rounded-xl bg-blue-600 hover:bg-blue-500 text-white font-black text-xs transition shadow-md cursor-pointer">
                                            배경 적용하기
                                        </button>
                                    ` : `
                                        <button type="button" onclick="JumpmanShop.buyBg('${bg.id}')" class="w-full py-2 rounded-xl ${canAfford ? 'bg-gradient-to-r from-amber-500 to-yellow-500 hover:from-amber-400 hover:to-yellow-400 text-slate-950 font-black' : 'bg-slate-800 text-slate-500 font-bold cursor-not-allowed'} text-xs transition shadow-md">
                                            🪙 ${bg.price} 코인으로 구매
                                        </button>
                                    `}
                                </div>
                            </div>
                        `;
                    }).join('')}
                </div>
            `;
        }
    }

    showToast(message, type = 'info') {
        const toast = document.createElement('div');
        toast.className = `fixed bottom-6 right-6 z-50 px-4 py-2.5 rounded-xl shadow-xl border text-xs font-black transition-all transform duration-300 ${
            type === 'error'
                ? 'bg-rose-950/90 text-rose-200 border-rose-500/50'
                : 'bg-emerald-950/90 text-emerald-200 border-emerald-500/50'
        }`;
        toast.textContent = message;
        document.body.appendChild(toast);

        setTimeout(() => {
            toast.style.opacity = '0';
            toast.style.transform = 'translateY(10px)';
            setTimeout(() => toast.remove(), 350);
        }, 2200);
    }
}

// 싱글톤 인스턴스 생성 및 전역 등록
window.JumpmanShop = new JumpmanShopManager();
document.addEventListener('DOMContentLoaded', () => {
    if (window.JumpmanShop) {
        window.JumpmanShop.updateAllCoinDisplays();
        window.JumpmanShop.updateHomeProfilePreview();
    }
});
