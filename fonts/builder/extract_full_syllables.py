import cv2
import numpy as np
import os
import sys

sys.stdout.reconfigure(encoding='utf-8')

def cv2_imread(path):
    img_array = np.fromfile(path, np.uint8)
    return cv2.imdecode(img_array, cv2.IMREAD_COLOR)

def cv2_imwrite(path, img):
    ext = os.path.splitext(path)[1]
    _, buf = cv2.imencode(ext, img)
    buf.tofile(path)

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

        # 모서리 마커 및 경계선 찌꺼기 제거
        is_marker_artifact = (dist_to_edge_x < 24 or dist_to_edge_y < 24) and (area < 350) and (dist_from_center > 50)
        if not is_marker_artifact and area > 15:
            cleaned = cv2.bitwise_or(cleaned, (labels == i).astype(np.uint8) * 255)

    return cleaned

def extract_sheet(img_path, chars, output_dir="extracted_chars"):
    os.makedirs(output_dir, exist_ok=True)
    img = cv2_imread(img_path)
    if img is None:
        raise ValueError(f"Cannot load image {img_path}")

    h, w = img.shape[:2]
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    _, dark_thresh = cv2.threshold(gray, 60, 255, cv2.THRESH_BINARY_INV)
    contours, _ = cv2.findContours(dark_thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    marker_centers = []
    for cnt in contours:
        area = cv2.contourArea(cnt)
        if 200 < area < 8000:
            x, y, cw, ch = cv2.boundingRect(cnt)
            ratio = float(cw) / ch
            if 0.7 < ratio < 1.3:
                marker_centers.append((x + cw / 2.0, y + ch / 2.0))

    if len(marker_centers) < 4:
        raise ValueError(f"Could not find 4 corner markers in {img_path}, found {len(marker_centers)}")

    tl = min(marker_centers, key=lambda p: p[0] + p[1])
    tr = min(marker_centers, key=lambda p: (w - p[0]) + p[1])
    bl = min(marker_centers, key=lambda p: p[0] + (h - p[1]))
    br = min(marker_centers, key=lambda p: (w - p[0]) + (h - p[1]))

    pts_src = np.array([tl, tr, br, bl], dtype=np.float32)

    target_w, target_h = 1240, 1754
    src_w_pt, src_h_pt = 595.27, 841.89
    mx = 40.0 / src_w_pt * target_w
    m_top = 80.0 / src_h_pt * target_h
    m_bot = 40.0 / src_h_pt * target_h

    dst_tl = [mx, m_top]
    dst_tr = [target_w - mx, m_top]
    dst_br = [target_w - mx, target_h - m_bot]
    dst_bl = [mx, target_h - m_bot]
    pts_dst = np.array([dst_tl, dst_tr, dst_br, dst_bl], dtype=np.float32)

    matrix = cv2.getPerspectiveTransform(pts_src, pts_dst)
    warped = cv2.warpPerspective(img, matrix, (target_w, target_h))

    cols, rows = 6, 8
    grid_w = target_w - mx * 2
    grid_h = target_h - m_top - m_bot
    cell_w = grid_w / cols
    cell_h = grid_h / rows

    extracted_count = 0
    for r in range(rows):
        for c in range(cols):
            idx = r * cols + c
            if idx >= len(chars): break
            char = chars[idx]

            cx = int(mx + c * cell_w)
            cy = int(m_top + r * cell_h)
            cw = int(cell_w)
            ch = int(cell_h)

            crop = warped[cy:cy+ch, cx:cx+cw]
            c_gray = cv2.cvtColor(crop, cv2.COLOR_BGR2GRAY)

            _, mask = cv2.threshold(c_gray, 160, 255, cv2.THRESH_BINARY_INV)

            # 테두리 여백 제거
            pad = 8
            mask[:pad, :] = 0
            mask[-pad:, :] = 0
            mask[:, :pad] = 0
            mask[:, -pad:] = 0

            # 좌상단 가이드 라벨 마스킹 제거
            mask[:int(ch * 0.20), :int(cw * 0.30)] = 0

            # 잡음 및 외곽 노이즈 정제
            cleaned = clean_cell_noise(mask)

            out_path = os.path.join(output_dir, f"{char}.png")
            cv2_imwrite(out_path, cleaned)
            extracted_count += 1

    print(f"Extracted {extracted_count} characters from {img_path}")

if __name__ == "__main__":
    page1_chars = [
        "백", "산", "님", "닳", "한", "강",
        "달", "말", "발", "살", "알", "칼",
        "단", "만", "판", "안", "잔", "찬",
        "맥", "택", "팩", "색", "객", "책",
        "김", "림", "심", "힘", "잎", "집",
        "건", "선", "천", "전", "번", "연",
        "센", "멘", "텐", "펜", "겐", "렌",
        "동", "해", "물", "두", "록", "국"
    ]

    page2_chars = [
        "닭", "맑", "삶", "넓", "없", "않",
        "값", "몫", "앉", "끊", "핥", "빛",
        "통", "홍", "공", "송", "봉", "농",
        "독", "곡", "목", "복", "속", "꽃",
        "불", "줄", "출", "풀", "꿀", "숲",
        "눈", "문", "순", "운", "준", "꿈",
        "글", "늘", "들", "슬", "을", "흙",
        "완", "원", "월", "광", "황", "환"
    ]

    print("Step 1: Extracting Page 1 (손글씨05.png)...")
    extract_sheet("손글씨05.png", page1_chars)

    print("Step 2: Extracting Page 2 (손글씨06.png)...")
    extract_sheet("손글씨06.png", page2_chars)
    print("Done extracting all 96 full syllables!")
