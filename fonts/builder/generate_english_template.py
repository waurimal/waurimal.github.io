import os
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

def generate_english_symbols_template(pdf_path="손글씨_영문_특수문자_작성양식.pdf"):
    font_path = "C:\\Windows\\Fonts\\malgun.ttf"
    pdfmetrics.registerFont(TTFont("Malgun", font_path))
    pdfmetrics.registerFont(TTFont("Malgun-Bold", "C:\\Windows\\Fonts\\malgunbd.ttf"))

    c = canvas.Canvas(pdf_path, pagesize=A4)
    width, height = A4 # 595.27 x 841.89 pt

    margin_x = 40
    margin_top = 80
    margin_bottom = 40

    def draw_marker(c, x, y, size=14):
        c.setFillColor(colors.black)
        c.rect(x - size/2, y - size/2, size, size, fill=1, stroke=0)
        c.setFillColor(colors.white)
        c.circle(x, y, 2.5, fill=1, stroke=0)

    # ----------------------------------------------------
    # Page 1: 영문 대문자 (A-Z) & 소문자 (a-z) (6열 x 9행 = 54칸)
    # ----------------------------------------------------
    cols1 = 6
    rows1 = 9
    grid_w1 = width - (margin_x * 2)
    grid_h1 = height - margin_top - margin_bottom
    cell_w1 = grid_w1 / cols1
    cell_h1 = grid_h1 / rows1

    page1_chars = [
        "A", "B", "C", "D", "E", "F",
        "G", "H", "I", "J", "K", "L",
        "M", "N", "O", "P", "Q", "R",
        "S", "T", "U", "V", "W", "X",
        "Y", "Z", "a", "b", "c", "d",
        "e", "f", "g", "h", "i", "j",
        "k", "l", "m", "n", "o", "p",
        "q", "r", "s", "t", "u", "v",
        "w", "x", "y", "z", "", ""
    ]

    # 모서리 마커
    draw_marker(c, margin_x, height - margin_top)
    draw_marker(c, width - margin_x, height - margin_top)
    draw_marker(c, margin_x, margin_bottom)
    draw_marker(c, width - margin_x, margin_bottom)

    # 상단 헤더
    c.setFillColor(colors.HexColor("#1A1A1A"))
    c.setFont("Malgun-Bold", 16)
    c.drawString(margin_x, height - 38, "나만의 손글씨 영문 & 특수문자 작성 양식")

    c.setFont("Malgun", 9)
    c.setFillColor(colors.HexColor("#555555"))
    c.drawString(margin_x, height - 52, "[1/2장: 영문 알파벳 52자 (A-Z, a-z)]  ※ 각 칸의 중심 가이드선에 맞춰 자연스러운 영문 필체로 작성해 주세요.")
    c.drawString(margin_x, height - 64, "※ 소문자는 기준선(베이스라인)에 유의하여 써주시면 디지털 폰트로 변환 시 가장 예쁘게 정렬됩니다.")

    c.setFont("Malgun-Bold", 11)
    c.drawRightString(width - margin_x, height - 38, "PAGE 1 (ENG)")

    start_y = height - margin_top
    for r in range(rows1):
        for col in range(cols1):
            idx = r * cols1 + col
            char = page1_chars[idx]
            if not char: continue

            cx = margin_x + col * cell_w1
            cy = start_y - (r + 1) * cell_h1

            # 테두리
            c.setStrokeColor(colors.HexColor("#BBBBBB"))
            c.setLineWidth(0.8)
            c.rect(cx, cy, cell_w1, cell_h1, fill=0)

            # 가이드선 (영문용 4선 가이드 느낌)
            c.setStrokeColor(colors.HexColor("#EAEAEA"))
            c.setLineWidth(0.5)
            c.setDash([2, 3])
            c.line(cx + 4, cy + cell_h1 * 0.35, cx + cell_w1 - 4, cy + cell_h1 * 0.35) # 베이스라인
            c.line(cx + 4, cy + cell_h1 * 0.65, cx + cell_w1 - 4, cy + cell_h1 * 0.65) # x-height
            c.setDash([])

            # 라벨
            c.setFillColor(colors.HexColor("#888888"))
            c.setFont("Malgun-Bold", 10)
            c.drawString(cx + 6, cy + cell_h1 - 13, char)

            # 연한 워터마크
            c.setFillColor(colors.HexColor("#F4F4F4"))
            c.setFont("Helvetica-Bold", cell_h1 * 0.50)
            c.drawCentredString(cx + cell_w1/2, cy + cell_h1 * 0.28, char)

    c.showPage()

    # ----------------------------------------------------
    # Page 2: 숫자 & 특수문자 42자 (6열 x 7행 = 42칸)
    # ----------------------------------------------------
    cols2 = 6
    rows2 = 7
    cell_w2 = grid_w1 / cols2
    cell_h2 = grid_h1 / rows2

    page2_chars = [
        "0", "1", "2", "3", "4", "5",
        "6", "7", "8", "9", ".", ",",
        "!", "?", "~", "-", "+", "=",
        "@", "#", "$", "%", "^", "&",
        "*", "(", ")", "[", "]", "{",
        "}", "<", ">", "/", "\\", "|",
        ";", ":", "'", '"', "`", "_"
    ]

    draw_marker(c, margin_x, height - margin_top)
    draw_marker(c, width - margin_x, height - margin_top)
    draw_marker(c, margin_x, margin_bottom)
    draw_marker(c, width - margin_x, margin_bottom)

    c.setFillColor(colors.HexColor("#1A1A1A"))
    c.setFont("Malgun-Bold", 16)
    c.drawString(margin_x, height - 38, "나만의 손글씨 영문 & 특수문자 작성 양식")

    c.setFont("Malgun", 9)
    c.setFillColor(colors.HexColor("#555555"))
    c.drawString(margin_x, height - 52, "[2/2장: 숫자 및 필수 특수기호 42자]  ※ 컴퓨터 키보드에 있는 모든 기호들을 내 손글씨로 완성합니다.")
    c.drawString(margin_x, height - 64, "※ 따옴표나 괄호, 특수기호의 크기와 높낮이를 칸의 중앙선에 맞춰 적어주세요.")

    c.setFont("Malgun-Bold", 11)
    c.drawRightString(width - margin_x, height - 38, "PAGE 2 (SYM)")

    for r in range(rows2):
        for col in range(cols2):
            idx = r * cols2 + col
            char = page2_chars[idx]

            cx = margin_x + col * cell_w2
            cy = start_y - (r + 1) * cell_h2

            c.setStrokeColor(colors.HexColor("#BBBBBB"))
            c.setLineWidth(0.8)
            c.rect(cx, cy, cell_w2, cell_h2, fill=0)

            c.setStrokeColor(colors.HexColor("#EAEAEA"))
            c.setLineWidth(0.5)
            c.setDash([2, 3])
            c.line(cx + 4, cy + cell_h2/2, cx + cell_w2 - 4, cy + cell_h2/2)
            c.line(cx + cell_w2/2, cy + 4, cx + cell_w2/2, cy + cell_h2 - 4)
            c.setDash([])

            c.setFillColor(colors.HexColor("#888888"))
            c.setFont("Malgun-Bold", 10)
            c.drawString(cx + 6, cy + cell_h2 - 13, char)

            c.setFillColor(colors.HexColor("#F4F4F4"))
            c.setFont("Helvetica-Bold", cell_h2 * 0.45)
            c.drawCentredString(cx + cell_w2/2, cy + cell_h2 * 0.28, char)

    c.showPage()
    c.save()
    print(f"English & Symbols Template generated at: {os.path.abspath(pdf_path)}")

if __name__ == "__main__":
    generate_english_symbols_template()
