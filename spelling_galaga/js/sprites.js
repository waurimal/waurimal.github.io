/**
 * 스펠링 갤러그 (Spelling Galaga) - 캔버스 픽셀아트 그래픽 & 스프라이트 렌더러
 * 고전 갤러그(Galaga) 아케이드 스타일의 비행체, 외계인(파리/나방/보스), 파워업 아이템, 트랙터 빔, 조준 인디케이터
 */

class SpriteRenderer {
  constructor() {
    this.animTick = 0;
  }

  update() {
    this.animTick++;
  }

  // 플레이어 전투기 렌더링 (기체 롤링 뱅크각, 듀얼 파이터, 에너지 쉴드, 무적 깜빡임)
  drawPlayer(ctx, x, y, width, height, isDual = false, isInvincible = false, tilt = 0, hasShield = false) {
    if (isInvincible && Math.floor(this.animTick / 4) % 2 === 0) {
      return; // 무적 상태 깜빡임 효과
    }

    ctx.save();
    ctx.translate(x, y);

    // 좌우 이동 시 기체 롤링(기울기)
    if (tilt !== 0) {
      ctx.rotate(tilt);
    }

    if (isDual) {
      // 듀얼 파이터 (좌우 나란히 2대 + 중앙 연결 빔)
      ctx.save();
      ctx.strokeStyle = '#00e5ff';
      ctx.lineWidth = 3;
      ctx.beginPath();
      ctx.moveTo(-width * 0.35, 0);
      ctx.lineTo(width * 0.35, 0);
      ctx.stroke();
      ctx.restore();

      this._renderFighter(ctx, -width * 0.35, 0, width * 0.65, height);
      this._renderFighter(ctx, width * 0.35, 0, width * 0.65, height);
    } else {
      this._renderFighter(ctx, 0, 0, width, height);
    }

    // 에너지 쉴드 렌더링
    if (hasShield) {
      const shieldR = (width / 2) * (isDual ? 1.6 : 1.4);
      const pulse = Math.sin(this.animTick * 0.2) * 2;
      ctx.save();
      ctx.beginPath();
      ctx.arc(0, 0, shieldR + pulse, 0, Math.PI * 2);
      ctx.strokeStyle = '#38bdf8';
      ctx.lineWidth = 2.5;
      ctx.fillStyle = 'rgba(56, 189, 248, 0.2)';
      ctx.shadowColor = '#38bdf8';
      ctx.shadowBlur = 12;
      ctx.stroke();
      ctx.fill();

      // 쉴드 외곽 회전 궤도 입자
      const angle = (this.animTick * 0.08) % (Math.PI * 2);
      ctx.fillStyle = '#ffffff';
      ctx.beginPath();
      ctx.arc(Math.cos(angle) * shieldR, Math.sin(angle) * shieldR, 3, 0, Math.PI * 2);
      ctx.arc(Math.cos(angle + Math.PI) * shieldR, Math.sin(angle + Math.PI) * shieldR, 3, 0, Math.PI * 2);
      ctx.fill();
      ctx.restore();
    }

    ctx.restore();
  }

  _renderFighter(ctx, ox, oy, w, h) {
    ctx.save();
    ctx.translate(ox, oy);

    const hw = w / 2;
    const hh = h / 2;

    // 엔진 부스터 불꽃
    const flameH = (Math.sin(this.animTick * 0.4) + 1.5) * (h * 0.25);
    const grad = ctx.createLinearGradient(0, hh, 0, hh + flameH);
    grad.addColorStop(0, '#ffff00');
    grad.addColorStop(0.5, '#ff4400');
    grad.addColorStop(1, 'transparent');
    ctx.fillStyle = grad;
    ctx.beginPath();
    ctx.moveTo(-hw * 0.3, hh);
    ctx.lineTo(hw * 0.3, hh);
    ctx.lineTo(0, hh + flameH);
    ctx.closePath();
    ctx.fill();

    // 본체 (흰색/회색)
    ctx.fillStyle = '#ffffff';
    ctx.beginPath();
    ctx.moveTo(0, -hh); // 앞코
    ctx.lineTo(hw * 0.3, -hh * 0.2);
    ctx.lineTo(hw * 0.4, hh * 0.6);
    ctx.lineTo(hw * 0.2, hh);
    ctx.lineTo(-hw * 0.2, hh);
    ctx.lineTo(-hw * 0.4, hh * 0.6);
    ctx.lineTo(-hw * 0.3, -hh * 0.2);
    ctx.closePath();
    ctx.fill();

    // 조종석 캐노피 (네온 블루)
    ctx.fillStyle = '#00e5ff';
    ctx.beginPath();
    ctx.ellipse(0, -hh * 0.2, hw * 0.18, hh * 0.3, 0, 0, Math.PI * 2);
    ctx.fill();

    // 좌우 날개 (빨간색 & 흰색 갤러그 고유 배색)
    ctx.fillStyle = '#ff2244';
    ctx.beginPath();
    ctx.moveTo(-hw * 0.2, -hh * 0.1);
    ctx.lineTo(-hw, hh * 0.5);
    ctx.lineTo(-hw * 0.8, hh);
    ctx.lineTo(-hw * 0.2, hh * 0.5);
    ctx.closePath();
    ctx.fill();

    ctx.beginPath();
    ctx.moveTo(hw * 0.2, -hh * 0.1);
    ctx.lineTo(hw, hh * 0.5);
    ctx.lineTo(hw * 0.8, hh);
    ctx.lineTo(hw * 0.2, hh * 0.5);
    ctx.closePath();
    ctx.fill();

    // 날개 끝 미사일 발사대 (노란색 팁)
    ctx.fillStyle = '#ffd700';
    ctx.fillRect(-hw - 1, hh * 0.2, 3, hh * 0.6);
    ctx.fillRect(hw - 2, hh * 0.2, 3, hh * 0.6);

    ctx.restore();
  }

  // 외계인 적 유닛 렌더링 (type: 'zako' | 'goei' | 'boss')
  drawEnemy(ctx, x, y, width, height, type = 'zako', letter = '', isTargetLetter = false, rotation = 0, difficulty = 'easy', hasCapturedFighter = false) {
    ctx.save();
    ctx.translate(x, y);
    if (rotation !== 0) {
      ctx.rotate(rotation);
    }

    const wingFrame = Math.floor(this.animTick / 12) % 2;

    switch (type) {
      case 'boss':
        this._drawBoss(ctx, width, height, wingFrame, hasCapturedFighter);
        break;
      case 'goei':
        this._drawGoei(ctx, width, height, wingFrame);
        break;
      case 'zako':
      default:
        this._drawZako(ctx, width, height, wingFrame);
        break;
    }

    // 회전 복원 후 알파벳 뱃지 렌더링 (글자는 항상 똑바로 보이도록)
    if (rotation !== 0) {
      ctx.rotate(-rotation);
    }

    if (letter) {
      this._drawLetterBadge(ctx, 0, 0, width, letter, isTargetLetter, difficulty);
    }

    ctx.restore();
  }

  // 1. 자코 (초록 파리 - Zako Fly)
  _drawZako(ctx, w, h, frame) {
    const hw = w / 2;
    const hh = h / 2;

    ctx.fillStyle = frame === 0 ? '#00e676' : '#69f0ae';
    ctx.beginPath();
    ctx.moveTo(0, -hh * 0.3);
    ctx.lineTo(-hw, frame === 0 ? -hh * 0.6 : -hh * 0.2);
    ctx.lineTo(-hw * 0.7, hh * 0.6);
    ctx.lineTo(0, hh * 0.3);
    ctx.lineTo(hw * 0.7, hh * 0.6);
    ctx.lineTo(hw, frame === 0 ? -hh * 0.6 : -hh * 0.2);
    ctx.closePath();
    ctx.fill();

    ctx.fillStyle = '#ffb300';
    ctx.beginPath();
    ctx.ellipse(0, 0, hw * 0.45, hh * 0.55, 0, 0, Math.PI * 2);
    ctx.fill();

    ctx.fillStyle = '#ff1744';
    ctx.fillRect(-hw * 0.25, -hh * 0.4, 4, 4);
    ctx.fillRect(hw * 0.25 - 4, -hh * 0.4, 4, 4);
  }

  // 2. 고에이 (빨간 나방 - Goei Moth)
  _drawGoei(ctx, w, h, frame) {
    const hw = w / 2;
    const hh = h / 2;

    ctx.fillStyle = '#ff3d00';
    ctx.beginPath();
    ctx.moveTo(0, -hh * 0.2);
    ctx.lineTo(-hw * 1.1, frame === 0 ? -hh * 0.8 : -hh * 0.3);
    ctx.lineTo(-hw * 0.5, hh * 0.8);
    ctx.lineTo(0, hh * 0.2);
    ctx.lineTo(hw * 0.5, hh * 0.8);
    ctx.lineTo(hw * 1.1, frame === 0 ? -hh * 0.8 : -hh * 0.3);
    ctx.closePath();
    ctx.fill();

    ctx.strokeStyle = '#ffd600';
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(-hw * 0.2, -hh * 0.4);
    ctx.lineTo(-hw * 0.6, -hh);
    ctx.moveTo(hw * 0.2, -hh * 0.4);
    ctx.lineTo(hw * 0.6, -hh);
    ctx.stroke();

    ctx.fillStyle = '#2979ff';
    ctx.beginPath();
    ctx.ellipse(0, 0, hw * 0.4, hh * 0.6, 0, 0, Math.PI * 2);
    ctx.fill();
  }

  // 3. 보스 갤러그 (Boss Galaga)
  _drawBoss(ctx, w, h, frame, hasCapturedFighter = false) {
    const hw = w / 2;
    const hh = h / 2;

    if (hasCapturedFighter) {
      ctx.save();
      ctx.translate(0, -hh - 8);
      ctx.scale(0.7, 0.7);
      this._renderFighter(ctx, 0, 0, w * 0.7, h * 0.7);
      ctx.restore();
    }

    ctx.fillStyle = '#2979ff';
    ctx.beginPath();
    ctx.moveTo(0, -hh * 0.9);
    ctx.lineTo(-hw * 0.8, -hh * 0.4);
    ctx.lineTo(-hw * 1.1, hh * 0.5);
    ctx.lineTo(-hw * 0.4, hh * 0.9);
    ctx.lineTo(0, hh * 0.5);
    ctx.lineTo(hw * 0.4, hh * 0.9);
    ctx.lineTo(hw * 1.1, hh * 0.5);
    ctx.lineTo(hw * 0.8, -hh * 0.4);
    ctx.closePath();
    ctx.fill();

    const corePulse = Math.sin(this.animTick * 0.2) * 2;
    ctx.fillStyle = frame === 0 ? '#ffd600' : '#00e5ff';
    ctx.beginPath();
    ctx.arc(0, 0, hw * 0.45 + corePulse, 0, Math.PI * 2);
    ctx.fill();
  }

  // 보스 갤러그의 트랙터 빔 렌더링
  drawTractorBeam(ctx, bossX, bossY, beamHeight = 220) {
    ctx.save();
    ctx.translate(bossX, bossY);

    const beamTopW = 20;
    const beamBottomW = 90;

    const grad = ctx.createLinearGradient(0, 0, 0, beamHeight);
    grad.addColorStop(0, 'rgba(0, 229, 255, 0.6)');
    grad.addColorStop(0.5, 'rgba(0, 229, 255, 0.25)');
    grad.addColorStop(1, 'rgba(0, 229, 255, 0.05)');

    ctx.fillStyle = grad;
    ctx.beginPath();
    ctx.moveTo(-beamTopW / 2, 0);
    ctx.lineTo(beamTopW / 2, 0);
    ctx.lineTo(beamBottomW / 2, beamHeight);
    ctx.lineTo(-beamBottomW / 2, beamHeight);
    ctx.closePath();
    ctx.fill();

    const rings = 5;
    ctx.strokeStyle = '#00e5ff';
    ctx.lineWidth = 2;
    for (let i = 0; i < rings; i++) {
      const progress = ((this.animTick * 0.04 + i / rings) % 1);
      const ringY = progress * beamHeight;
      const ringW = beamTopW + (beamBottomW - beamTopW) * progress;

      ctx.beginPath();
      ctx.ellipse(0, ringY, ringW / 2, 8, 0, 0, Math.PI * 2);
      ctx.stroke();
    }

    ctx.restore();
  }

  // 알파벳 스펠링 뱃지 & 타깃 조준 인디케이터
  _drawLetterBadge(ctx, ox, oy, enemyWidth, letter, isTargetLetter, difficulty = 'easy') {
    const badgeR = enemyWidth * 0.48;
    const showHighlight = isTargetLetter && (difficulty !== 'hard');

    if (showHighlight) {
      const pulseMag = (difficulty === 'easy') ? 6 : 3;
      const pulse = Math.sin(this.animTick * 0.2) * pulseMag + pulseMag;
      ctx.save();
      ctx.beginPath();
      ctx.arc(ox, oy, badgeR + pulse, 0, Math.PI * 2);
      ctx.fillStyle = (difficulty === 'easy') ? 'rgba(255, 215, 0, 0.45)' : 'rgba(56, 189, 248, 0.25)';
      ctx.shadowColor = (difficulty === 'easy') ? '#ffd700' : '#38bdf8';
      ctx.shadowBlur = (difficulty === 'easy') ? 18 : 8;
      ctx.fill();
      ctx.restore();
    }

    ctx.save();
    ctx.beginPath();
    ctx.arc(ox, oy, badgeR, 0, Math.PI * 2);
    ctx.fillStyle = showHighlight ? '#111827' : '#0f172a';
    ctx.fill();

    ctx.lineWidth = showHighlight ? 3.5 : 2;
    ctx.strokeStyle = showHighlight
      ? ((difficulty === 'easy') ? '#ffd700' : '#38bdf8')
      : '#334155';
    ctx.stroke();

    ctx.fillStyle = showHighlight
      ? ((difficulty === 'easy') ? '#ffeb3b' : '#38bdf8')
      : '#ffffff';
    ctx.font = `bold ${Math.round(badgeR * 1.35)}px 'Press Start 2P', monospace, sans-serif`;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText(letter, ox, oy + 2);

    // 🎯 초급/중급 모드: 타깃 외계인 상단에 큼직한 조준 화살표(▼) 표시
    if (showHighlight) {
      const arrowBounce = Math.sin(this.animTick * 0.25) * 5;
      ctx.fillStyle = '#ffd700';
      ctx.shadowColor = '#ffd700';
      ctx.shadowBlur = 12;
      ctx.beginPath();
      ctx.moveTo(ox, oy - badgeR - 10 + arrowBounce);
      ctx.lineTo(ox - 7, oy - badgeR - 20 + arrowBounce);
      ctx.lineTo(ox + 7, oy - badgeR - 20 + arrowBounce);
      ctx.closePath();
      ctx.fill();

      if (difficulty === 'easy') {
        ctx.font = "bold 11px 'DungGeunMo', 'Pretendard', sans-serif";
        ctx.fillStyle = '#fef08a';
        ctx.shadowBlur = 8;
        ctx.textAlign = 'center';
        ctx.fillText('🎯조준!', ox, oy - badgeR - 24 + arrowBounce);
      }
    }

    ctx.restore();
  }

  // 드롭 아이템 캡슐 렌더링
  drawPowerup(ctx, x, y, type = 'heart') {
    ctx.save();
    ctx.translate(x, y);

    const bob = Math.sin(this.animTick * 0.2) * 3;
    const r = 16;

    ctx.beginPath();
    ctx.arc(0, bob, r + 2, 0, Math.PI * 2);
    let glowColor = '#ffd700';
    if (type === 'heart') glowColor = '#ff2244';
    else if (type === 'dual') glowColor = '#00e5ff';
    else if (type === 'shield') glowColor = '#38bdf8';
    else if (type === 'bomb') glowColor = '#f97316';
    else if (type === 'slow') glowColor = '#a855f7';

    ctx.fillStyle = glowColor;
    ctx.shadowColor = glowColor;
    ctx.shadowBlur = 10;
    ctx.fill();

    ctx.fillStyle = '#0f172a';
    ctx.beginPath();
    ctx.arc(0, bob, r, 0, Math.PI * 2);
    ctx.fill();

    ctx.font = "14px 'Pretendard', sans-serif";
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    let icon = '⭐';
    if (type === 'heart') icon = '❤️';
    else if (type === 'dual') icon = '⚡';
    else if (type === 'shield') icon = '🛡️';
    else if (type === 'bomb') icon = '💣';
    else if (type === 'slow') icon = '⏱️';

    ctx.fillText(icon, 0, bob + 1);
    ctx.restore();
  }

  // 플레이어 미사일 (더 넓고 시원하게)
  drawMissile(ctx, x, y, width, height) {
    ctx.save();
    ctx.fillStyle = '#ffeb3b';
    ctx.shadowColor = '#ff5722';
    ctx.shadowBlur = 8;
    ctx.fillRect(x - width / 2, y - height / 2, width, height);

    ctx.fillStyle = '#ffffff';
    ctx.fillRect(x - width / 4, y - height / 2 + 2, width / 2, height - 4);
    ctx.restore();
  }

  // 적 탄환
  drawEnemyBullet(ctx, x, y, size) {
    ctx.save();
    ctx.fillStyle = '#ff1744';
    ctx.shadowColor = '#ff1744';
    ctx.shadowBlur = 6;
    ctx.beginPath();
    ctx.moveTo(x, y - size);
    ctx.lineTo(x + size * 0.7, y);
    ctx.lineTo(x, y + size);
    ctx.lineTo(x - size * 0.7, y);
    ctx.closePath();
    ctx.fill();
    ctx.restore();
  }
}

window.SpriteRenderer = SpriteRenderer;
