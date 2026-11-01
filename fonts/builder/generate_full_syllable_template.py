import os
import shutil
import fitz  # PyMuPDF
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

def generate_template(pdf_path="손글씨_온전한한글_작성양식.pdf"):
    # 폰트 등록 (Windows 맑은 고딕 사용)
    font_path = "C:\\Windows\\Fonts\\malgun.ttf"
    font_bold_path = "C:\\Windows\\Fonts\\malgunbd.ttf"
    pdfmetrics.registerFont(TTFont("Malgun", font_path))
    pdfmetrics.registerFont(TTFont("Malgun-Bold", font_bold_path))

    c = canvas.Canvas(pdf_path, pagesize=A4)
    width, height = A4  # 595.27 x 841.89 pt (210 x 297 mm)

    margin_x = 40
    margin_top = 80
    margin_bottom = 40
    cols = 6
    rows = 8

    grid_w = width - (margin_x * 2)
    grid_h = height - margin_top - margin_bottom

    cell_w = grid_w / cols
    cell_h = grid_h / rows

    # 페이지 1: 세로모음 유받침 집중 & 실전 핵심 음절 48자
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

    # 페이지 2: 겹받침, 가로/복합모음 받침 & 실전 문장 음절 48자
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

    def draw_marker(c, x, y, size=14):
        """자동 왜곡 보정용 정렬 마커 (정사각형)"""
        c.setFillColor(colors.black)
        c.rect(x - size/2, y - size/2, size, size, fill=1, stroke=0)
        c.setFillColor(colors.white)
        c.circle(x, y, 2.5, fill=1, stroke=0)

    def draw_page(chars, page_num, total_pages, title_sub):
        # 4개 모서리 마커 (정확한 기계 스캔 자동 크롭용)
        draw_marker(c, margin_x, height - margin_top)
        draw_marker(c, width - margin_x, height - margin_top)
        draw_marker(c, margin_x, margin_bottom)
        draw_marker(c, width - margin_x, margin_bottom)

        # 상단 제목 및 설명
        c.setFillColor(colors.HexColor("#0F172A"))
        c.setFont("Malgun-Bold", 15)
        c.drawString(margin_x, height - 38, "나만의 손글씨 완성형 온전한 한글 작성 양식")

        c.setFont("Malgun", 8.5)
        c.setFillColor(colors.HexColor("#475569"))
        c.drawString(margin_x, height - 52, f"[{page_num}/{total_pages}장: {title_sub}]  ※ 평소 필체대로 각 칸의 중앙 십자선에 자연스럽게 써주세요.")
        c.drawString(margin_x, height - 64, "※ 받침이 있는 글자의 모음 길이와 초성·받침 위치를 평소 습관대로 편안하게 작성해 주시면 됩니다.")

        # 페이지 번호 마커
        c.setFont("Malgun-Bold", 11)
        c.setFillColor(colors.HexColor("#334155"))
        c.drawRightString(width - margin_x, height - 38, f"PAGE {page_num}")

        start_y = height - margin_top

        for r in range(rows):
            for col in range(cols):
                idx = r * cols + col
                char = chars[idx]

                cx = margin_x + col * cell_w
                cy = start_y - (r + 1) * cell_h

                # 셀 외곽선
                c.setStrokeColor(colors.HexColor("#CBD5E1"))
                c.setLineWidth(0.8)
                c.rect(cx, cy, cell_w, cell_h, fill=0)

                # 중앙 십자 가이드선 (연한 점선)
                c.setStrokeColor(colors.HexColor("#E2E8F0"))
                c.setLineWidth(0.5)
                c.setDash([2, 3])
                c.line(cx + 4, cy + cell_h/2, cx + cell_w - 4, cy + cell_h/2)
                c.line(cx + cell_w/2, cy + 4, cx + cell_w/2, cy + cell_h - 4)
                c.setDash([])

                # 상단 좌측 가이드 글자
                c.setFillColor(colors.HexColor("#64748B"))
                c.setFont("Malgun-Bold", 9.5)
                c.drawString(cx + 6, cy + cell_h - 13, char)

                # 중앙 아주 연한 워터마크 안내 글자
                c.setFillColor(colors.HexColor("#F1F5F9"))
                c.setFont("Malgun", cell_h * 0.44)
                c.drawCentredString(cx + cell_w/2, cy + cell_h * 0.28, char)

        c.showPage()

    draw_page(page1_chars, 1, 2, "세로모음 유받침(백, 산, 님, 닳 등) 및 실전 핵심 48자")
    draw_page(page2_chars, 2, 2, "겹받침(닭, 맑, 삶 등) 및 가로/복합모음 받침 48자")
    c.save()
    print(f"Template successfully generated at: {os.path.abspath(pdf_path)}")

    # 렌더링 프리뷰 PNG 이미지 생성
    doc = fitz.open(pdf_path)
    for p_idx, page in enumerate(doc):
        pix = page.get_pixmap(dpi=150)
        out_png = f"template_full_page{p_idx+1}.png"
        pix.save(out_png)
        print(f"Saved preview: {out_png}")
    doc.close()

    # 웹 배포 폴더로도 복사
    web_fonts_dir = "01_edtech_games/waurimal.github.io/fonts"
    if os.path.exists(web_fonts_dir):
        dest_pdf = os.path.join(web_fonts_dir, os.path.basename(pdf_path))
        shutil.copy2(pdf_path, dest_pdf)
        print(f"Copied to web directory: {dest_pdf}")
        for p_idx in [1, 2]:
            src_png = f"template_full_page{p_idx}.png"
            if os.path.exists(src_png):
                shutil.copy2(src_png, os.path.join(web_fonts_dir, src_png))

if __name__ == "__main__":
    generate_template()
