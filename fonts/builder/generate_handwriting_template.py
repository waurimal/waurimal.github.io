import os
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

def generate_template(pdf_path="손글씨_작성양식.pdf"):
    # 폰트 등록 (Windows 맑은 고딕 사용)
    font_path = "C:\\Windows\\Fonts\\malgun.ttf"
    pdfmetrics.registerFont(TTFont("Malgun", font_path))
    pdfmetrics.registerFont(TTFont("Malgun-Bold", "C:\\Windows\\Fonts\\malgunbd.ttf"))

    c = canvas.Canvas(pdf_path, pagesize=A4)
    width, height = A4  # 595.27, 841.89 pt (210 x 297 mm)

    # 마커 및 그리드 설정
    margin_x = 40
    margin_top = 80
    margin_bottom = 40
    cols = 6
    rows = 8
    
    grid_w = width - (margin_x * 2)
    grid_h = height - margin_top - margin_bottom
    
    cell_w = grid_w / cols
    cell_h = grid_h / rows

    # 글자 목록 (페이지 1: 초성 및 기본 모음 48자)
    page1_chars = [
        "가", "나", "다", "라", "마", "바",
        "사", "아", "자", "차", "카", "타",
        "파", "하", "까", "따", "빠", "싸",
        "짜", "고", "노", "도", "로", "모",
        "보", "소", "오", "조", "초", "코",
        "토", "포", "호", "구", "누", "두",
        "루", "무", "부", "수", "우", "주",
        "으", "이", "그", "르", "브", "시"
    ]

    # 글자 목록 (페이지 2: 받침, 복합모음, 숫자 48자)
    page2_chars = [
        "각", "난", "닫", "랄", "맘", "밥",
        "삿", "앙", "잦", "낯", "앜", "밭",
        "앞", "닿", "깎", "았", "밝", "꽃",
        "꿈", "빛", "숲", "흙", "개", "내",
        "대", "래", "매", "배", "게", "네",
        "데", "레", "와", "왜", "외", "워",
        "웨", "위", "의", "1", "2", "3",
        "4", "5", "6", "7", "8", "9"
    ]

    def draw_marker(c, x, y, size=14):
        """자동 왜곡 보정용 정렬 마커 (정사각형)"""
        c.setFillColor(colors.black)
        c.rect(x - size/2, y - size/2, size, size, fill=1, stroke=0)
        # 내부에 흰색 점
        c.setFillColor(colors.white)
        c.circle(x, y, 2.5, fill=1, stroke=0)

    def draw_page(chars, page_num, total_pages, title_sub):
        # 4개 모서리 인식 마커
        draw_marker(c, margin_x, height - margin_top)
        draw_marker(c, width - margin_x, height - margin_top)
        draw_marker(c, margin_x, margin_bottom)
        draw_marker(c, width - margin_x, margin_bottom)

        # 상단 제목 및 설명
        c.setFillColor(colors.HexColor("#1A1A1A"))
        c.setFont("Malgun-Bold", 16)
        c.drawString(margin_x, height - 38, "나만의 손글씨 웹 폰트 작성 양식")

        c.setFont("Malgun", 9)
        c.setFillColor(colors.HexColor("#555555"))
        c.drawString(margin_x, height - 52, f"[{page_num}/{total_pages}장: {title_sub}]  ※ 굵기가 일정한 펜(0.5~0.7mm)으로 각 칸의 중앙 십자선에 맞춰 평소 필체로 편안하게 써주세요.")
        c.drawString(margin_x, height - 64, "※ 칸 테두리에 글씨가 닿지 않게 적어주시면 가장 깔끔하게 생성됩니다. (스캔 또는 스마트폰 촬영 업로드)")

        # 페이지 번호 마커 (우측 상단)
        c.setFont("Malgun-Bold", 11)
        c.setFillColor(colors.HexColor("#333333"))
        c.drawRightString(width - margin_x, height - 38, f"PAGE {page_num}")

        # 그리드 그리기
        start_y = height - margin_top

        for r in range(rows):
            for col in range(cols):
                idx = r * cols + col
                char = chars[idx]
                
                cx = margin_x + col * cell_w
                cy = start_y - (r + 1) * cell_h

                # 셀 외곽선 (부드러운 회색)
                c.setStrokeColor(colors.HexColor("#BBBBBB"))
                c.setLineWidth(0.8)
                c.rect(cx, cy, cell_w, cell_h, fill=0)

                # 중앙 십자 가이드선 (아주 연한 점선)
                c.setStrokeColor(colors.HexColor("#E2E2E2"))
                c.setLineWidth(0.5)
                c.setDash([2, 3])
                # 가로선
                c.line(cx + 4, cy + cell_h/2, cx + cell_w - 4, cy + cell_h/2)
                # 세로선
                c.line(cx + cell_w/2, cy + 4, cx + cell_w/2, cy + cell_h - 4)
                c.setDash([]) # 점선 리셋

                # 가이드 기준 글자 (상단 좌측 작게 표시)
                c.setFillColor(colors.HexColor("#888888"))
                c.setFont("Malgun-Bold", 10)
                c.drawString(cx + 6, cy + cell_h - 13, char)

                # 중앙 연한 워터마크 안내 (참고용 아주 연한 회색 글자)
                c.setFillColor(colors.HexColor("#F2F2F2"))
                c.setFont("Malgun", cell_h * 0.45)
                c.drawCentredString(cx + cell_w/2, cy + cell_h * 0.28, char)

        c.showPage()

    # 페이지 1 생성 (기본 자모)
    draw_page(page1_chars, 1, 2, "초성 및 기본 모음 48자 (필수)")
    # 페이지 2 생성 (받침 및 복합모음/숫자)
    draw_page(page2_chars, 2, 2, "받침, 복합모음, 숫자 48자 (권장)")

    c.save()
    print(f"Template successfully generated at: {os.path.abspath(pdf_path)}")

if __name__ == "__main__":
    generate_template()
