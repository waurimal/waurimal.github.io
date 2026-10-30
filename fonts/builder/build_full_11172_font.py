import cv2
import numpy as np
import os
import sys
import time
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTFont

sys.stdout.reconfigure(encoding='utf-8')

CHO_NAMES = ["g", "gg", "n", "d", "dd", "r", "m", "b", "bb", "s", "ss", "ng", "j", "jj", "ch", "k", "t", "p", "h"]

def cv2_imread_gray(path):
    img_array = np.fromfile(path, np.uint8)
    return cv2.imdecode(img_array, cv2.IMREAD_GRAYSCALE)

def cv2_imwrite(path, img):
    ext = os.path.splitext(path)[1]
    _, buf = cv2.imencode(ext, img)
    buf.tofile(path)

def trim_bbox(img, pad=2):
    pts = cv2.findNonZero(img)
    if pts is None:
        return np.zeros((10, 10), dtype=np.uint8)
    x, y, w, h = cv2.boundingRect(pts)
    h_img, w_img = img.shape
    x0 = max(0, x - pad)
    y0 = max(0, y - pad)
    x1 = min(w_img, x + w + pad)
    y1 = min(h_img, y + h + pad)
    return img[y0:y1, x0:x1]

def clean_cell_noise(img):
    h, w = img.shape
    num_labels, labels, stats, centroids = cv2.connectedComponentsWithStats(img)
    cleaned = np.zeros_like(img)
    center_x, center_y = w / 2.0, h / 2.0
    
    for i in range(1, num_labels):
        area = stats[i, cv2.CC_STAT_AREA]
        x = stats[i, cv2.CC_STAT_LEFT]
        y = stats[i, cv2.CC_STAT_TOP]
        cw = stats[i, cv2.CC_STAT_WIDTH]
        ch = stats[i, cv2.CC_STAT_HEIGHT]
        cx, cy = centroids[i]

        dist_to_edge_x = min(x, w - (x + cw))
        dist_to_edge_y = min(y, h - (y + ch))
        dist_from_center = np.hypot(cx - center_x, cy - center_y)

        is_marker_artifact = (dist_to_edge_x < 32 or dist_to_edge_y < 32) and (area < 400) and (dist_from_center > 55)
        if not is_marker_artifact and area > 12:
            cleaned = cv2.bitwise_or(cleaned, (labels == i).astype(np.uint8) * 255)
            
    return cleaned

def clean_jamo_specks(img, min_area=20):
    if img is None: return None
    num_labels, labels, stats, _ = cv2.connectedComponentsWithStats(img)
    if num_labels <= 2:
        return img
    cleaned = np.zeros_like(img)
    for i in range(1, num_labels):
        if stats[i, cv2.CC_STAT_AREA] >= min_area:
            cleaned[labels == i] = 255
    return cleaned

def paste_centered(canvas, img, target_box):
    tx, ty, tw, th = target_box
    ih, iw = img.shape
    if iw == 0 or ih == 0: return
    scale = min(tw / iw, th / ih)
    nw, nh = max(1, int(iw * scale)), max(1, int(ih * scale))
    resized = cv2.resize(img, (nw, nh), interpolation=cv2.INTER_AREA)
    ox = tx + (tw - nw) // 2
    oy = ty + (th - nh) // 2
    canvas[oy:oy+nh, ox:ox+nw] = np.maximum(canvas[oy:oy+nh, ox:ox+nw], resized)

def normalize_ink_canvas(ink_img, target_size=200, has_jong=False):
    pts = cv2.findNonZero(ink_img)
    if pts is None:
        return np.zeros((target_size, target_size), dtype=np.uint8)

    x, y, w, h = cv2.boundingRect(pts)
    trimmed = ink_img[y:y+h, x:x+w]
    canvas = np.zeros((target_size, target_size), dtype=np.uint8)

    if has_jong:
        max_target_h = 152
        max_target_w = 145
    else:
        max_target_h = 140
        max_target_w = 140

    scale = min(max_target_w / w, max_target_h / h)
    scale = min(scale, 2.5)

    nw = max(1, int(w * scale))
    nh = max(1, int(h * scale))
    resized = cv2.resize(trimmed, (nw, nh), interpolation=cv2.INTER_AREA)

    ox = (target_size - nw) // 2
    oy = (target_size - nh) // 2
    canvas[oy:oy+nh, ox:ox+nw] = resized
    return canvas

# ==========================================
# 2. 마스터 한글 합성 엔진 (MasterHangulSynthesizer)
# ==========================================
class MasterHangulSynthesizer:
    def __init__(self, jamo_dir="jamo_library", extracted_dir="extracted_chars"):
        self.jamo_dir = jamo_dir
        self.extracted_dir = extracted_dir
        self.cache_cho_v = {}
        self.cache_cho_h = {}
        self.cache_jung = {}
        self.compound_vowels = {}
        self.cache_jong = {}
        self.direct_chars = {}
        self.load_resources()

    def load_resources(self):
        # 1. 직접 쓴 한글 글자 로드
        if os.path.exists(self.extracted_dir):
            for f in os.listdir(self.extracted_dir):
                if f.endswith(".png") and not f.startswith("char_"):
                    char = os.path.splitext(f)[0]
                    raw = cv2_imread_gray(os.path.join(self.extracted_dir, f))
                    if raw is not None:
                        self.direct_chars[char] = clean_cell_noise(raw)

        # 2. 초성 로드 (잡음 제거 및 정제) - 완전무결한 cho_v를 기본으로 통일하여 하단 끊김 근본 차단
        for i, name in enumerate(CHO_NAMES):
            pv = os.path.join(self.jamo_dir, "cho_v", f"cho_{i:02d}_{name}.png")
            if os.path.exists(pv):
                img_v = cv2_imread_gray(pv)
                if img_v is not None and cv2.countNonZero(img_v) > 20:
                    self.cache_cho_v[i] = trim_bbox(clean_jamo_specks(img_v))
            # 가로모임 및 복합모임에도 온전한 형태의 초성을 사용하여 하단 잘림을 완전히 방지
            self.cache_cho_h[i] = self.cache_cho_v.get(i)

        # 3. 완벽한 단자모 라이브러리 확립
        # (1) pure_i (키 큰 온전한 수직 기둥)
        if os.path.exists("debug_tall_i.png"):
            pure_i = trim_bbox(cv2_imread_gray("debug_tall_i.png"))
        else:
            i_raw = cv2_imread_gray(os.path.join(self.jamo_dir, "jung/jung_20_i.png"))
            pure_i = trim_bbox(clean_jamo_specks(i_raw))

        # (2) a_img (ㅏ)
        a_img = trim_bbox(clean_jamo_specks(cv2_imread_gray(os.path.join(self.jamo_dir, "jung/jung_00_a.png"))))

        # (3) o_img (진짜 'ㅗ' - debug_clean_true_o.png), u_img (ㅜ)
        if os.path.exists("debug_clean_true_o.png"):
            o_img = trim_bbox(cv2_imread_gray("debug_clean_true_o.png"))
        else:
            o_img = trim_bbox(clean_jamo_specks(cv2_imread_gray(os.path.join(self.jamo_dir, "jung/jung_08_o.png"))))
        if os.path.exists("debug_pure_u.png"):
            u_img = trim_bbox(cv2_imread_gray("debug_pure_u.png"))
        else:
            u_img = trim_bbox(clean_jamo_specks(cv2_imread_gray(os.path.join(self.jamo_dir, "jung/jung_13_u.png"))))

        # (4) eu_img (진짜 'ㅡ' - debug_true_eu.png)
        if os.path.exists("debug_true_eu.png"):
            eu_img = trim_bbox(cv2_imread_gray("debug_true_eu.png"))
        else:
            eu_img = trim_bbox(clean_jamo_specks(cv2_imread_gray(os.path.join(self.jamo_dir, "jung/jung_18_eu.png"))))

        # (5) clean_e (진짜 'ㅔ' - debug_clean_e.png: 좌측 틱 + 제1기둥 + 간격 + 제2기둥)
        if os.path.exists("debug_clean_e.png"):
            clean_e = trim_bbox(cv2_imread_gray("debug_clean_e.png"))
        else:
            clean_e = pure_i

        # (6) clean_ae (진짜 'ㅐ' - debug_clean_ae.png: 제1기둥 + 우측 연결선 + 제2기둥)
        if os.path.exists("debug_clean_ae.png"):
            clean_ae = trim_bbox(cv2_imread_gray("debug_clean_ae.png"))
        else:
            clean_ae = pure_i

        # (7) clean_eo (진짜 'ㅓ' - 좌측 틱 + 수직 기둥)
        if os.path.exists("debug_clean_eo.png"):
            clean_eo = trim_bbox(cv2_imread_gray("debug_clean_eo.png"))
        else:
            clean_eo = pure_i

        # (8) clean_ye (진짜 'ㅖ') 및 clean_yeo (진짜 'ㅕ')
        if os.path.exists("debug_clean_ye.png"):
            clean_ye = trim_bbox(cv2_imread_gray("debug_clean_ye.png"))
        else:
            clean_ye = clean_e
        if os.path.exists("debug_clean_yeo.png"):
            clean_yeo = trim_bbox(cv2_imread_gray("debug_clean_yeo.png"))
        else:
            clean_yeo = clean_eo

        # (9) ya (ㅑ), yae (ㅒ)
        h_a, w_a = a_img.shape
        ya_canvas = np.zeros((int(h_a * 1.15), w_a), dtype=np.uint8)
        ya_canvas[int(h_a * 0.15):, :] = a_img
        tick_a = a_img[int(h_a * 0.3):int(h_a * 0.8), int(w_a * 0.4):]
        ya_canvas[:tick_a.shape[0], int(w_a * 0.4):int(w_a * 0.4) + tick_a.shape[1]] = np.maximum(
            ya_canvas[:tick_a.shape[0], int(w_a * 0.4):int(w_a * 0.4) + tick_a.shape[1]], tick_a
        )
        clean_ya = trim_bbox(ya_canvas)

        yae_canvas = np.zeros((max(clean_ya.shape[0], pure_i.shape[0]), clean_ya.shape[1] + pure_i.shape[1] + 8), dtype=np.uint8)
        paste_centered(yae_canvas, clean_ya, (0, 0, clean_ya.shape[1], yae_canvas.shape[0]))
        paste_centered(yae_canvas, pure_i, (clean_ya.shape[1] + 6, 0, pure_i.shape[1], yae_canvas.shape[0]))
        clean_yae = trim_bbox(yae_canvas)

        # (10) yo (ㅛ), yu (ㅠ)
        yo_canvas = np.zeros((int(eu_img.shape[0] + o_img.shape[0] * 0.7), int(eu_img.shape[1] * 1.1)), dtype=np.uint8)
        paste_centered(yo_canvas, eu_img, (0, int(o_img.shape[0] * 0.65), yo_canvas.shape[1], eu_img.shape[0]))
        stem_o = o_img[:int(o_img.shape[0]*0.7), :]
        paste_centered(yo_canvas, stem_o, (int(yo_canvas.shape[1]*0.25), 0, int(yo_canvas.shape[1]*0.2), int(o_img.shape[0]*0.65)))
        paste_centered(yo_canvas, stem_o, (int(yo_canvas.shape[1]*0.55), 0, int(yo_canvas.shape[1]*0.2), int(o_img.shape[0]*0.65)))
        clean_yo = trim_bbox(yo_canvas)

        yu_canvas = np.zeros((int(eu_img.shape[0] + u_img.shape[0] * 0.7), int(eu_img.shape[1] * 1.1)), dtype=np.uint8)
        paste_centered(yu_canvas, eu_img, (0, 0, yu_canvas.shape[1], eu_img.shape[0]))
        stem_u = u_img[int(u_img.shape[0]*0.3):, :]
        paste_centered(yu_canvas, stem_u, (int(yu_canvas.shape[1]*0.25), eu_img.shape[0], int(yu_canvas.shape[1]*0.2), int(u_img.shape[0]*0.65)))
        paste_centered(yu_canvas, stem_u, (int(yu_canvas.shape[1]*0.55), eu_img.shape[0], int(yu_canvas.shape[1]*0.2), int(u_img.shape[0]*0.65)))
        clean_yu = trim_bbox(yu_canvas)

        # 모음 21종 등록
        self.cache_jung[0] = a_img        # ㅏ
        self.cache_jung[1] = clean_ae     # ㅐ
        self.cache_jung[2] = clean_ya     # ㅑ
        self.cache_jung[3] = clean_yae    # ㅒ
        self.cache_jung[4] = clean_eo     # ㅓ
        self.cache_jung[5] = clean_e      # ㅔ
        self.cache_jung[6] = clean_yeo    # ㅕ
        self.cache_jung[7] = clean_ye     # ㅖ
        self.cache_jung[8] = o_img        # ㅗ
        self.cache_jung[12] = clean_yo    # ㅛ
        self.cache_jung[13] = u_img       # ㅜ
        self.cache_jung[17] = clean_yu    # ㅠ
        self.cache_jung[18] = eu_img      # ㅡ
        self.cache_jung[20] = pure_i      # ㅣ

        # 복합모음 (가로 성분 h_part, 세로 성분 v_part 분리 보관)
        self.compound_vowels[9] = (o_img, a_img)        # ㅘ = ㅗ + ㅏ
        self.compound_vowels[10] = (o_img, clean_ae)    # ㅙ = ㅗ + ㅐ
        self.compound_vowels[11] = (o_img, pure_i)      # ㅚ = ㅗ + ㅣ
        self.compound_vowels[14] = (u_img, clean_eo)    # ㅝ = ㅜ + ㅓ
        self.compound_vowels[15] = (u_img, clean_e)     # ㅞ = ㅜ + ㅔ
        self.compound_vowels[16] = (u_img, pure_i)      # ㅟ = ㅜ + ㅣ
        self.compound_vowels[19] = (eu_img, pure_i)     # ㅢ = ㅡ + ㅣ

        # 4. 종성 28종 확립 (훈민정음 종성부용초성)
        CHO_TO_JONG = {
            1: 0, 2: 1, 4: 2, 7: 3, 8: 5, 16: 6, 17: 7, 19: 9, 20: 10,
            21: 11, 22: 12, 23: 14, 24: 15, 25: 16, 26: 17, 27: 18
        }
        for jong_idx, cho_idx in CHO_TO_JONG.items():
            raw_cho = self.cache_cho_v.get(cho_idx)
            if raw_cho is not None:
                h, w = raw_cho.shape
                scale_w = 0.95
                scale_h = 0.82
                jong_resized = cv2.resize(raw_cho, (max(1, int(w * scale_w)), max(1, int(h * scale_h))), interpolation=cv2.INTER_AREA)
                self.cache_jong[jong_idx] = trim_bbox(jong_resized)

        # 복합 받침 (ㄳ, ㄵ, ㄶ, ㄺ, ㄻ, ㄼ, ㄽ, ㄾ, ㄿ, ㅀ, ㅄ)
        CLUSTER_MAP = {
            3: (1, 19),   # ㄳ: ㄱ + ㅅ
            5: (4, 22),   # ㄵ: ㄴ + ㅈ
            6: (4, 27),   # ㄶ: ㄴ + ㅎ
            9: (8, 1),    # ㄺ: ㄹ + ㄱ
            10: (8, 16),  # ㄻ: ㄹ + ㅁ
            11: (8, 17),  # ㄼ: ㄹ + ㅂ
            12: (8, 19),  # ㄽ: ㄹ + ㅅ
            13: (8, 25),  # ㄾ: ㄹ + ㅌ
            14: (8, 26),  # ㄿ: ㄹ + ㅍ
            15: (8, 27),  # ㅀ: ㄹ + ㅎ
            18: (17, 19)  # ㅄ: ㅂ + ㅅ
        }
        for c_idx, (j1, j2) in CLUSTER_MAP.items():
            img1 = self.cache_jong.get(j1)
            img2 = self.cache_jong.get(j2)
            if img1 is not None and img2 is not None:
                h1, w1 = img1.shape
                h2, w2 = img2.shape
                nw1 = max(1, int(w1 * 0.70))
                nw2 = max(1, int(w2 * 0.70))
                r1 = cv2.resize(img1, (nw1, h1), interpolation=cv2.INTER_AREA)
                r2 = cv2.resize(img2, (nw2, h2), interpolation=cv2.INTER_AREA)
                c_comb = np.zeros((max(h1, h2), nw1 + nw2 + 4), dtype=np.uint8)
                c_comb[:h1, :nw1] = r1
                c_comb[:h2, nw1+4:] = r2
                self.cache_jong[c_idx] = trim_bbox(c_comb)

    def render(self, char, size=200):
        code = ord(char) - 0xAC00
        if code < 0 or code > 11171:
            return None

        cho = code // (21 * 28)
        jung_idx = (code % (21 * 28)) // 28
        jong_idx = code % 28

        # 모든 한글 11,172자에 대해 정밀 마스터 엔진으로 100% 균일 조판 (원시 스캔 셀 잡음 및 잘림 방지)

        JUNG_TYPES = {
            0: 'V', 1: 'V', 2: 'V', 3: 'V', 4: 'V', 5: 'V', 6: 'V', 7: 'V', 20: 'V',
            8: 'H', 12: 'H', 13: 'H', 17: 'H', 18: 'H',
            9: 'C', 10: 'C', 11: 'C', 14: 'C', 15: 'C', 16: 'C', 19: 'C'
        }
        j_type = JUNG_TYPES.get(jung_idx, 'V')
        has_jong = (jong_idx > 0)

        canvas = np.zeros((size, size), dtype=np.uint8)

        if j_type == 'V':
            cho_img = self.cache_cho_v.get(cho)
        else:
            cho_img = self.cache_cho_h.get(cho)
            if cho_img is None or cv2.countNonZero(cho_img) < 20:
                cho_img = self.cache_cho_v.get(cho)

        jong_img = self.cache_jong.get(jong_idx) if has_jong else None

        if j_type == 'V':
            jung_img = self.cache_jung.get(jung_idx, self.cache_jung.get(20))
            if not has_jong:
                # 세로모임 무받침 (세, 에, 가, 배, 새, 대 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (18, 25, int(size * 0.36), int(size * 0.70)))
                if jung_img is not None:
                    paste_centered(canvas, jung_img, (int(size * 0.48), 18, int(size * 0.48), int(size * 0.82)))
            else:
                # 세로모임 유받침 (쳇, 입, 밥, 김, 빛 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (18, 12, int(size * 0.40), int(size * 0.44)))
                if jong_img is not None:
                    paste_centered(canvas, jong_img, (20, int(size * 0.58), int(size * 0.42), int(size * 0.36)))
                if jung_img is not None:
                    paste_centered(canvas, jung_img, (int(size * 0.50), 12, int(size * 0.46), int(size * 0.82)))

        elif j_type == 'H':
            jung_img = self.cache_jung.get(jung_idx, self.cache_jung.get(18))
            if not has_jong:
                # 가로모임 무받침 (고, 누, 드, 으, 트 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (20, 15, int(size * 0.68), int(size * 0.44)))
                if jung_img is not None:
                    paste_centered(canvas, jung_img, (15, int(size * 0.48), int(size * 0.80), int(size * 0.45)))
            else:
                # 가로모임 유받침 (손, 폰, 든, 뜻, 문, 글, 증 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (25, 10, int(size * 0.65), int(size * 0.35)))
                if jung_img is not None:
                    paste_centered(canvas, jung_img, (15, int(size * 0.40), int(size * 0.76), int(size * 0.26)))
                if jong_img is not None:
                    paste_centered(canvas, jong_img, (25, int(size * 0.64), int(size * 0.64), int(size * 0.34)))

        else: # 'C' 복합모임 (웹, 웨, 과, 와, 왜, 워, 위, 의 등)
            h_part, v_part = self.compound_vowels.get(jung_idx, (self.cache_jung.get(13), self.cache_jung.get(20)))
            if not has_jong:
                # 복합모임 무받침 (웨, 와, 왜, 워, 위, 의 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (18, 14, int(size * 0.40), int(size * 0.38)))
                if h_part is not None:
                    paste_centered(canvas, h_part, (16, int(size * 0.52), int(size * 0.44), int(size * 0.34)))
                if v_part is not None:
                    paste_centered(canvas, v_part, (int(size * 0.48), 12, int(size * 0.46), int(size * 0.80)))
            else:
                # 복합모임 유받침 (웹, 완, 원, 월 등)
                if cho_img is not None:
                    paste_centered(canvas, cho_img, (18, 10, int(size * 0.36), int(size * 0.28)))
                if h_part is not None:
                    paste_centered(canvas, h_part, (16, 64, int(size * 0.38), int(size * 0.22)))
                if v_part is not None:
                    paste_centered(canvas, v_part, (int(size * 0.48), 10, int(size * 0.44), int(size * 0.52)))
                if jong_img is not None:
                    paste_centered(canvas, jong_img, (int(size * 0.16), int(size * 0.58), int(size * 0.64), int(size * 0.36)))

        return normalize_ink_canvas(canvas, target_size=size, has_jong=has_jong)


# ==========================================
# 3. 비트맵 -> 트루타입 벡터 윤곽선 변환기
# ==========================================
def hangul_bitmap_to_ttglyph(bmp, margin_left=40, margin_right=40):
    pts = cv2.findNonZero(bmp)
    if pts is None:
        pen = TTGlyphPen(None)
        pen.moveTo((0, 0))
        pen.closePath()
        return pen.glyph(), 500

    # 획 두께 보강 및 굵기 선명화 (흐리게 나오거나 미세 끊김 방지)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
    bin_bmp = (bmp > 30).astype(np.uint8) * 255
    thick_bmp = cv2.dilate(bin_bmp, kernel, iterations=1)

    pts = cv2.findNonZero(thick_bmp)
    x, y, w, h = cv2.boundingRect(pts)
    crop = thick_bmp[y:y+h, x:x+w]

    scale = 800.0 / 200.0
    scaled_w = max(1, int(w * scale))
    scaled_h = max(1, int(h * scale))

    crop_scaled = cv2.resize(crop, (scaled_w, scaled_h), interpolation=cv2.INTER_NEAREST)
    contours, hierarchy = cv2.findContours(crop_scaled, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_TC89_KCOS)

    pen = TTGlyphPen(None)
    y_baseline = 100

    if hierarchy is not None:
        for i in range(len(contours)):
            cnt = contours[i]
            if cv2.contourArea(cnt) < 16:
                continue

            poly = cnt.reshape(-1, 2)
            first = True
            for pt in poly:
                gx = margin_left + int(pt[0])
                gy = y_baseline + (scaled_h - int(pt[1]))
                if first:
                    pen.moveTo((gx, gy))
                    first = False
                else:
                    pen.lineTo((gx, gy))
            pen.closePath()

    glyph = pen.glyph()
    adv_w = int(scaled_w + margin_left + margin_right)
    return glyph, adv_w

def clean_ascii_cell(img_cell):
    ch_h, ch_w = img_cell.shape
    num_labels, labels, stats, centroids = cv2.connectedComponentsWithStats(img_cell)
    cleaned = np.zeros_like(img_cell)
    for i in range(1, num_labels):
        area = stats[i, cv2.CC_STAT_AREA]
        x = stats[i, cv2.CC_STAT_LEFT]
        y = stats[i, cv2.CC_STAT_TOP]
        w = stats[i, cv2.CC_STAT_WIDTH]
        h = stats[i, cv2.CC_STAT_HEIGHT]
        dist_edge_x = min(x, ch_w - (x + w))
        dist_edge_y = min(y, ch_h - (y + h))
        if (dist_edge_x < 15 or dist_edge_y < 15) and area < 80:
            continue
        if area > 10:
            cleaned[labels == i] = 255
    return cleaned

def ascii_ink_to_ttglyph(char, img_cell, is_page1=True):
    cleaned = clean_ascii_cell(img_cell)
    pts = cv2.findNonZero(cleaned)
    if pts is None:
        pen = TTGlyphPen(None)
        pen.moveTo((0, 0))
        pen.closePath()
        return pen.glyph(), 500, 0

    # 영문/숫자/기호 획 두께 보강 (선명화 및 흐림 방지)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
    cleaned = cv2.dilate(cleaned, kernel, iterations=1)
    pts = cv2.findNonZero(cleaned)
    bx, by, bw, bh = cv2.boundingRect(pts)
    crop = cleaned[by:by+bh, bx:bx+bw]

    scale = 9.8
    scaled_w = max(1, int(bw * scale))
    scaled_h = max(1, int(bh * scale))

    crop_scaled = cv2.resize(crop, (scaled_w, scaled_h), interpolation=cv2.INTER_NEAREST)
    contours, hierarchy = cv2.findContours(crop_scaled, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_TC89_KCOS)

    y_base_pixel = 120.0 if is_page1 else 146.0
    gy_bottom = int((y_base_pixel - (by + bh)) * scale)

    margin_left = 60
    margin_right = 60

    if char in '1ilI|!':
        margin_left = 90
        margin_right = 90
    elif char in '.,:;':
        margin_left = 35
        margin_right = 55
    elif char in "'-`":
        margin_left = 35
        margin_right = 35

    pen = TTGlyphPen(None)
    if hierarchy is not None:
        for i in range(len(contours)):
            cnt = contours[i]
            if cv2.contourArea(cnt) < 12:
                continue
            poly = cnt.reshape(-1, 2)
            first = True
            for pt in poly:
                gx = margin_left + int(pt[0])
                gy = gy_bottom + (scaled_h - int(pt[1]))
                if first:
                    pen.moveTo((gx, gy))
                    first = False
                else:
                    pen.lineTo((gx, gy))
            pen.closePath()

    glyph = pen.glyph()
    adv_w = int(scaled_w + margin_left + margin_right)
    return glyph, adv_w, margin_left


# ==========================================
# 4. 전체 완성형 폰트 컴파일러 (한글 11,172자 + 영문/기호 94자)
# ==========================================
def build_full_font(output_ttf="MyHandwriting.ttf", output_woff2="MyHandwriting.woff2"):
    start_time = time.time()
    print("Step 1: Initializing Master Synthesizer & Resources...")
    syn = MasterHangulSynthesizer("jamo_library", "extracted_chars")

    fb = FontBuilder(unitsPerEm=1024, isTTF=True)
    glyph_order = ['.notdef', 'space']
    cmap = {0x20: 'space'}
    glyphs = {}
    metrics = {}

    pen = TTGlyphPen(None)
    pen.moveTo((0, 0)); pen.closePath()
    empty_glyph = pen.glyph()
    glyphs['.notdef'] = empty_glyph
    glyphs['space'] = empty_glyph
    metrics['.notdef'] = (500, 0)
    metrics['space'] = (380, 0)

    # 1. 영문 및 특수기호 (U+0021 ~ U+007E) 손글씨 아웃라인 등록
    ascii_count = 0
    print("Step 2: Compiling English (A-Z, a-z), Digits (0-9), and Symbols...")
    for code in range(0x21, 0x7F):
        ch = chr(code)
        fname = os.path.join("extracted_chars", f"char_{code:04X}.png")
        if not os.path.exists(fname): continue
        img = cv2_imread_gray(fname)
        if img is None: continue

        is_p1 = (('A' <= ch <= 'Z') or ('a' <= ch <= 'z'))
        gname = f"uni{code:04X}"
        glyph, adv_w, lsb = ascii_ink_to_ttglyph(ch, img, is_page1=is_p1)

        glyph_order.append(gname)
        cmap[code] = gname
        glyphs[gname] = glyph
        metrics[gname] = (adv_w, lsb)
        ascii_count += 1

    print(f"  Compiled {ascii_count} authentic English/Symbol glyphs.")

    # 2. 한글 전체 11,172자 (U+AC00 ~ U+D7A3)
    hangul_all = [chr(code) for code in range(0xAC00, 0xD7A4)]
    total_hangul = len(hangul_all)
    print(f"Step 3: Synthesizing full 11,172 Hangul syllables...")

    count = 0
    for char in hangul_all:
        count += 1
        if count % 2000 == 0 or count == total_hangul:
            print(f"  Processed {count}/{total_hangul} Hangul syllables ({count*100//total_hangul}%)...")

        code = ord(char)
        gname = f"uni{code:04X}"

        bmp = syn.render(char, size=200)
        if bmp is None: continue

        glyph, adv_w = hangul_bitmap_to_ttglyph(bmp, margin_left=40, margin_right=40)
        glyph_order.append(gname)
        cmap[code] = gname
        glyphs[gname] = glyph
        metrics[gname] = (adv_w, 40)

    print(f"Total glyphs in font: {len(glyph_order)}.")

    print("Step 4: Compiling OpenType / TrueType font tables...")
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap(cmap)
    fb.setupGlyf(glyphs)
    fb.setupHorizontalMetrics(metrics)
    fb.setupHorizontalHeader(ascent=824, descent=-200)
    fb.setupNameTable({
        'familyName': 'MyHandwriting',
        'styleName': 'Regular',
        'uniqueFontIdentifier': 'MyHandwriting:Antigravity:MasterFull:2026',
        'fullName': 'MyHandwriting Regular',
        'psName': 'MyHandwriting-Regular',
        'version': 'Version 4.000'
    })
    fb.setupOS2(sTypoAscender=824, sTypoDescender=-200, usWeightClass=400)
    fb.setupPost()

    fb.save(output_ttf)
    ttf_size = os.path.getsize(output_ttf)
    print(f"TTF build complete: {output_ttf} ({ttf_size / 1024 / 1024:.2f} MB)")

    print("Step 5: Compressing full master font to WOFF2...")
    f = TTFont(output_ttf)
    f.flavor = 'woff2'
    f.save(output_woff2)
    woff2_size = os.path.getsize(output_woff2)
    print(f"WOFF2 build complete: {output_woff2} ({woff2_size / 1024:.1f} KB)")
    print(f"Total time elapsed: {time.time() - start_time:.1f} seconds.")

if __name__ == "__main__":
    build_full_font()
