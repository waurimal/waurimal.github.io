/**
 * 초등 영어 교과과정 공통 어휘 및 문장 데이터 모듈 (Waurimal Curriculum Module)
 * 2026 Eui Elementary School - Waurimal Project
 * 
 * 3, 4학년: YBM (최희경) 1~12단원
 * 5, 6학년: 천재 (함순애) 1~12단원
 * 총 48개 단원 전 학년 체계적 구축
 */
(function() {
    'use strict';

    const CURRICULUM_VERSION = '2026_V6_AUTHENTIC_44';

    const RAW_LESSONS = [
    {
        "code": "3G_L01",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 01 - Hello, ABC!",
        "words": [
            {
                "en": "hello",
                "kr": "안녕"
            },
            {
                "en": "hi",
                "kr": "안녕"
            },
            {
                "en": "I",
                "kr": "나"
            },
            {
                "en": "am",
                "kr": "~이다"
            },
            {
                "en": "my",
                "kr": "나의"
            },
            {
                "en": "name",
                "kr": "이름"
            },
            {
                "en": "thank",
                "kr": "고마워하다"
            },
            {
                "en": "you",
                "kr": "너, 당신"
            },
            {
                "en": "goodbye",
                "kr": "안녕히 가세요"
            },
            {
                "en": "bye",
                "kr": "잘 가"
            },
            {
                "en": "meet",
                "kr": "만나다"
            },
            {
                "en": "nice",
                "kr": "반가운, 좋은"
            }
        ],
        "sentences": [
            {
                "en": "Hello, I'm Olly.",
                "kr": "안녕, 나는 올리야."
            },
            {
                "en": "Hi, I'm Hana.",
                "kr": "안녕, 나는 하나야."
            },
            {
                "en": "Nice to meet you.",
                "kr": "만나서 반가워."
            },
            {
                "en": "Thank you.",
                "kr": "고마워."
            },
            {
                "en": "Goodbye!",
                "kr": "잘 가!"
            }
        ]
    },
    {
        "code": "3G_L02",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 02 - What's This?",
        "words": [
            {
                "en": "what",
                "kr": "무엇"
            },
            {
                "en": "this",
                "kr": "이것"
            },
            {
                "en": "that",
                "kr": "저것"
            },
            {
                "en": "it",
                "kr": "그것"
            },
            {
                "en": "is",
                "kr": "~이다"
            },
            {
                "en": "bag",
                "kr": "가방"
            },
            {
                "en": "ball",
                "kr": "공"
            },
            {
                "en": "chair",
                "kr": "의자"
            },
            {
                "en": "car",
                "kr": "자동차"
            },
            {
                "en": "doll",
                "kr": "인형"
            },
            {
                "en": "clock",
                "kr": "시계"
            },
            {
                "en": "cup",
                "kr": "컵"
            },
            {
                "en": "big",
                "kr": "큰"
            },
            {
                "en": "small",
                "kr": "작은"
            }
        ],
        "sentences": [
            {
                "en": "What's this?",
                "kr": "이것은 무엇이니?"
            },
            {
                "en": "It's a bag.",
                "kr": "그것은 가방이야."
            },
            {
                "en": "It's a ball.",
                "kr": "그것은 공이야."
            },
            {
                "en": "It's a cup.",
                "kr": "그것은 컵이야."
            },
            {
                "en": "It's big.",
                "kr": "그것은 커."
            }
        ]
    },
    {
        "code": "3G_L03",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 03 - Sit Down, Please",
        "words": [
            {
                "en": "stand",
                "kr": "서다"
            },
            {
                "en": "sit",
                "kr": "앉다"
            },
            {
                "en": "up",
                "kr": "위로"
            },
            {
                "en": "down",
                "kr": "아래로"
            },
            {
                "en": "open",
                "kr": "열다"
            },
            {
                "en": "close",
                "kr": "닫다"
            },
            {
                "en": "door",
                "kr": "문"
            },
            {
                "en": "book",
                "kr": "책"
            },
            {
                "en": "please",
                "kr": "부탁합니다, 제발"
            },
            {
                "en": "okay",
                "kr": "좋아, 알았어"
            },
            {
                "en": "come",
                "kr": "오다"
            },
            {
                "en": "here",
                "kr": "여기"
            }
        ],
        "sentences": [
            {
                "en": "Stand up, please.",
                "kr": "일어서 주세요."
            },
            {
                "en": "Sit down, please.",
                "kr": "앉아 주세요."
            },
            {
                "en": "Open the door, please.",
                "kr": "문을 열어 주세요."
            },
            {
                "en": "Close your book, please.",
                "kr": "책을 닫아 주세요."
            },
            {
                "en": "Come here, please.",
                "kr": "이리 오세요."
            }
        ]
    },
    {
        "code": "3G_L04",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 04 - How Many Cows?",
        "words": [
            {
                "en": "how",
                "kr": "얼마나, 어떻게"
            },
            {
                "en": "many",
                "kr": "많은"
            },
            {
                "en": "cow",
                "kr": "소"
            },
            {
                "en": "pig",
                "kr": "돼지"
            },
            {
                "en": "duck",
                "kr": "오리"
            },
            {
                "en": "cat",
                "kr": "고양이"
            },
            {
                "en": "dog",
                "kr": "개"
            },
            {
                "en": "zebra",
                "kr": "얼룩말"
            },
            {
                "en": "one",
                "kr": "하나 (1)"
            },
            {
                "en": "two",
                "kr": "둘 (2)"
            },
            {
                "en": "three",
                "kr": "셋 (3)"
            },
            {
                "en": "four",
                "kr": "넷 (4)"
            },
            {
                "en": "five",
                "kr": "다섯 (5)"
            }
        ],
        "sentences": [
            {
                "en": "How many cows?",
                "kr": "소가 몇 마리 있니?"
            },
            {
                "en": "Three cows.",
                "kr": "소 세 마리야."
            },
            {
                "en": "How many pigs?",
                "kr": "돼지가 몇 마리 있니?"
            },
            {
                "en": "Four pigs.",
                "kr": "돼지 네 마리야."
            },
            {
                "en": "Look at the ducks.",
                "kr": "오리들을 봐."
            }
        ]
    },
    {
        "code": "3G_L05",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 05 - Do You Like Apples?",
        "words": [
            {
                "en": "like",
                "kr": "좋아하다"
            },
            {
                "en": "apple",
                "kr": "사과"
            },
            {
                "en": "banana",
                "kr": "바나나"
            },
            {
                "en": "orange",
                "kr": "오렌지"
            },
            {
                "en": "grape",
                "kr": "포도"
            },
            {
                "en": "pizza",
                "kr": "피자"
            },
            {
                "en": "bread",
                "kr": "빵"
            },
            {
                "en": "soup",
                "kr": "수프"
            },
            {
                "en": "ice cream",
                "kr": "아이스크림"
            },
            {
                "en": "yes",
                "kr": "응, 그래"
            },
            {
                "en": "no",
                "kr": "아니요"
            }
        ],
        "sentences": [
            {
                "en": "Do you like apples?",
                "kr": "너는 사과를 좋아하니?"
            },
            {
                "en": "Yes, I do.",
                "kr": "응, 좋아해."
            },
            {
                "en": "No, I don't.",
                "kr": "아니, 안 좋아해."
            },
            {
                "en": "I like bananas.",
                "kr": "나는 바나나를 좋아해."
            },
            {
                "en": "I like pizza.",
                "kr": "나는 피자를 좋아해."
            }
        ]
    },
    {
        "code": "3G_L06",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 06 - I Can Jump",
        "words": [
            {
                "en": "can",
                "kr": "~할 수 있다"
            },
            {
                "en": "jump",
                "kr": "뛰다, 점프하다"
            },
            {
                "en": "swim",
                "kr": "수영하다"
            },
            {
                "en": "fly",
                "kr": "날다"
            },
            {
                "en": "kick",
                "kr": "차다"
            },
            {
                "en": "catch",
                "kr": "잡다"
            },
            {
                "en": "run",
                "kr": "달리다"
            },
            {
                "en": "dance",
                "kr": "춤추다"
            },
            {
                "en": "slide",
                "kr": "미끄러지다"
            },
            {
                "en": "climb",
                "kr": "오르다"
            },
            {
                "en": "ski",
                "kr": "스키 타다"
            }
        ],
        "sentences": [
            {
                "en": "Can you jump?",
                "kr": "너는 뛸 수 있니?"
            },
            {
                "en": "Yes, I can.",
                "kr": "응, 할 수 있어."
            },
            {
                "en": "No, I can't.",
                "kr": "아니, 못 해."
            },
            {
                "en": "I can swim.",
                "kr": "나는 수영할 수 있어."
            },
            {
                "en": "I can dance.",
                "kr": "나는 춤출 수 있어."
            }
        ]
    },
    {
        "code": "3G_L07",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 07 - I Have a Pencil",
        "words": [
            {
                "en": "have",
                "kr": "가지고 있다"
            },
            {
                "en": "pencil",
                "kr": "연필"
            },
            {
                "en": "ruler",
                "kr": "자"
            },
            {
                "en": "eraser",
                "kr": "지우개"
            },
            {
                "en": "crayon",
                "kr": "크레용"
            },
            {
                "en": "notebook",
                "kr": "공책"
            },
            {
                "en": "glue",
                "kr": "풀"
            },
            {
                "en": "scissors",
                "kr": "가위"
            },
            {
                "en": "pencil case",
                "kr": "필통"
            },
            {
                "en": "here",
                "kr": "여기"
            }
        ],
        "sentences": [
            {
                "en": "I have a pencil.",
                "kr": "나는 연필이 있어."
            },
            {
                "en": "Do you have a ruler?",
                "kr": "너는 자가 있니?"
            },
            {
                "en": "Yes, I do.",
                "kr": "응, 있어."
            },
            {
                "en": "No, I don't.",
                "kr": "아니, 없어."
            },
            {
                "en": "Here you are.",
                "kr": "여기 있어."
            }
        ]
    },
    {
        "code": "3G_L08",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 08 - I'm Happy",
        "words": [
            {
                "en": "happy",
                "kr": "행복한, 기쁜"
            },
            {
                "en": "sad",
                "kr": "슬픈"
            },
            {
                "en": "angry",
                "kr": "화난"
            },
            {
                "en": "hungry",
                "kr": "배고픈"
            },
            {
                "en": "tired",
                "kr": "피곤한"
            },
            {
                "en": "birthday",
                "kr": "생일"
            },
            {
                "en": "six",
                "kr": "여섯 (6)"
            },
            {
                "en": "seven",
                "kr": "일곱 (7)"
            },
            {
                "en": "eight",
                "kr": "여덟 (8)"
            },
            {
                "en": "nine",
                "kr": "아홉 (9)"
            },
            {
                "en": "ten",
                "kr": "열 (10)"
            }
        ],
        "sentences": [
            {
                "en": "I'm happy.",
                "kr": "나는 기뻐."
            },
            {
                "en": "I'm hungry.",
                "kr": "나는 배고파."
            },
            {
                "en": "Are you sad?",
                "kr": "너 슬프니?"
            },
            {
                "en": "Happy birthday!",
                "kr": "생일 축하해!"
            },
            {
                "en": "I'm ten years old.",
                "kr": "나는 열 살이야."
            }
        ]
    },
    {
        "code": "3G_L09",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 09 - What Color Is It?",
        "words": [
            {
                "en": "color",
                "kr": "색, 색깔"
            },
            {
                "en": "red",
                "kr": "빨간색"
            },
            {
                "en": "blue",
                "kr": "파란색"
            },
            {
                "en": "yellow",
                "kr": "노란색"
            },
            {
                "en": "green",
                "kr": "초록색"
            },
            {
                "en": "pink",
                "kr": "분홍색"
            },
            {
                "en": "purple",
                "kr": "보라색"
            },
            {
                "en": "white",
                "kr": "흰색"
            },
            {
                "en": "brown",
                "kr": "갈색"
            },
            {
                "en": "black",
                "kr": "검은색"
            },
            {
                "en": "orange",
                "kr": "주황색"
            }
        ],
        "sentences": [
            {
                "en": "What color is it?",
                "kr": "그것은 무슨 색이니?"
            },
            {
                "en": "It's red.",
                "kr": "그것은 빨간색이야."
            },
            {
                "en": "It's blue.",
                "kr": "그것은 파란색이야."
            },
            {
                "en": "It's yellow.",
                "kr": "그것은 노란색이야."
            },
            {
                "en": "It's green.",
                "kr": "그것은 초록색이야."
            }
        ]
    },
    {
        "code": "3G_L10",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 10 - How's the Weather?",
        "words": [
            {
                "en": "weather",
                "kr": "날씨"
            },
            {
                "en": "sunny",
                "kr": "화창한, 맑은"
            },
            {
                "en": "cloudy",
                "kr": "흐린"
            },
            {
                "en": "raining",
                "kr": "비가 오는"
            },
            {
                "en": "snowing",
                "kr": "눈이 오는"
            },
            {
                "en": "windy",
                "kr": "바람 부는"
            },
            {
                "en": "hot",
                "kr": "더운"
            },
            {
                "en": "cold",
                "kr": "추운"
            },
            {
                "en": "snowman",
                "kr": "눈사람"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            }
        ],
        "sentences": [
            {
                "en": "How's the weather?",
                "kr": "날씨가 어때?"
            },
            {
                "en": "It's sunny.",
                "kr": "날씨가 맑아."
            },
            {
                "en": "It's raining.",
                "kr": "비가 오고 있어."
            },
            {
                "en": "It's snowing.",
                "kr": "눈이 오고 있어."
            },
            {
                "en": "Let's make a snowman.",
                "kr": "눈사람을 만들자."
            }
        ]
    },
    {
        "code": "4G_L01",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 01 - How Are You?",
        "words": [
            {
                "en": "morning",
                "kr": "아침"
            },
            {
                "en": "afternoon",
                "kr": "오후"
            },
            {
                "en": "evening",
                "kr": "저녁"
            },
            {
                "en": "night",
                "kr": "밤"
            },
            {
                "en": "good",
                "kr": "좋은"
            },
            {
                "en": "great",
                "kr": "아주 좋은"
            },
            {
                "en": "fine",
                "kr": "잘 지내는"
            },
            {
                "en": "not bad",
                "kr": "그럭저럭 괜찮은"
            },
            {
                "en": "sleepy",
                "kr": "졸린"
            },
            {
                "en": "tired",
                "kr": "피곤한"
            }
        ],
        "sentences": [
            {
                "en": "Good morning.",
                "kr": "좋은 아침이야."
            },
            {
                "en": "How are you?",
                "kr": "어떻게 지내니?"
            },
            {
                "en": "I'm good.",
                "kr": "잘 지내."
            },
            {
                "en": "I'm great.",
                "kr": "아주 잘 지내."
            },
            {
                "en": "Not bad.",
                "kr": "그럭저럭 괜찮아."
            }
        ]
    },
    {
        "code": "4G_L02",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 02 - Who's She?",
        "words": [
            {
                "en": "who",
                "kr": "누구"
            },
            {
                "en": "she",
                "kr": "그녀"
            },
            {
                "en": "he",
                "kr": "그"
            },
            {
                "en": "friend",
                "kr": "친구"
            },
            {
                "en": "mother",
                "kr": "어머니"
            },
            {
                "en": "father",
                "kr": "아버지"
            },
            {
                "en": "brother",
                "kr": "남자 형제"
            },
            {
                "en": "sister",
                "kr": "여자 형제"
            },
            {
                "en": "teacher",
                "kr": "선생님"
            },
            {
                "en": "tall",
                "kr": "키가 큰"
            },
            {
                "en": "pretty",
                "kr": "예쁜"
            }
        ],
        "sentences": [
            {
                "en": "Who's she?",
                "kr": "그녀는 누구니?"
            },
            {
                "en": "She's my sister.",
                "kr": "그녀는 내 여동생이야."
            },
            {
                "en": "Who's he?",
                "kr": "그는 누구니?"
            },
            {
                "en": "He's my friend.",
                "kr": "그는 내 친구야."
            },
            {
                "en": "He's tall.",
                "kr": "그는 키가 커."
            }
        ]
    },
    {
        "code": "4G_L03",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 03 - What Time Is It?",
        "words": [
            {
                "en": "time",
                "kr": "시간, 때"
            },
            {
                "en": "o'clock",
                "kr": "~시 정각"
            },
            {
                "en": "eleven",
                "kr": "열하나 (11)"
            },
            {
                "en": "twelve",
                "kr": "열둘 (12)"
            },
            {
                "en": "thirty",
                "kr": "삼십 (30)"
            },
            {
                "en": "breakfast",
                "kr": "아침 식사"
            },
            {
                "en": "lunch",
                "kr": "점심 식사"
            },
            {
                "en": "dinner",
                "kr": "저녁 식사"
            },
            {
                "en": "bedtime",
                "kr": "취침 시간"
            },
            {
                "en": "wake up",
                "kr": "일어나다"
            }
        ],
        "sentences": [
            {
                "en": "What time is it?",
                "kr": "지금 몇 시니?"
            },
            {
                "en": "It's eight o'clock.",
                "kr": "8시 정각이야."
            },
            {
                "en": "It's time for breakfast.",
                "kr": "아침 먹을 시간이야."
            },
            {
                "en": "It's time for bed.",
                "kr": "잘 시간이야."
            },
            {
                "en": "It's twelve thirty.",
                "kr": "12시 30분이야."
            }
        ]
    },
    {
        "code": "4G_L04",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 04 - Let's Play Soccer",
        "words": [
            {
                "en": "soccer",
                "kr": "축구"
            },
            {
                "en": "baseball",
                "kr": "야구"
            },
            {
                "en": "basketball",
                "kr": "농구"
            },
            {
                "en": "badminton",
                "kr": "배드민턴"
            },
            {
                "en": "play",
                "kr": "놀다, 운동 경기를 하다"
            },
            {
                "en": "jump rope",
                "kr": "줄넘기"
            },
            {
                "en": "together",
                "kr": "함께"
            },
            {
                "en": "fun",
                "kr": "재미있는"
            },
            {
                "en": "sure",
                "kr": "물론이지"
            },
            {
                "en": "sorry",
                "kr": "미안한"
            }
        ],
        "sentences": [
            {
                "en": "Let's play soccer.",
                "kr": "축구 하자."
            },
            {
                "en": "Sounds good.",
                "kr": "좋은 생각이야."
            },
            {
                "en": "Let's play baseball.",
                "kr": "야구 하자."
            },
            {
                "en": "Sorry, I can't.",
                "kr": "미안하지만 난 못 해."
            },
            {
                "en": "Let's jump rope together.",
                "kr": "함께 줄넘기 하자."
            }
        ]
    },
    {
        "code": "4G_L05",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 05 - Where's My Bag?",
        "words": [
            {
                "en": "where",
                "kr": "어디에"
            },
            {
                "en": "in",
                "kr": "~안에"
            },
            {
                "en": "on",
                "kr": "~위에"
            },
            {
                "en": "under",
                "kr": "~아래에"
            },
            {
                "en": "by",
                "kr": "~옆에"
            },
            {
                "en": "desk",
                "kr": "책상"
            },
            {
                "en": "bed",
                "kr": "침대"
            },
            {
                "en": "box",
                "kr": "상자"
            },
            {
                "en": "table",
                "kr": "탁자"
            },
            {
                "en": "chair",
                "kr": "의자"
            },
            {
                "en": "cap",
                "kr": "모자"
            }
        ],
        "sentences": [
            {
                "en": "Where's my bag?",
                "kr": "내 가방이 어디 있지?"
            },
            {
                "en": "It's on the desk.",
                "kr": "책상 위에 있어."
            },
            {
                "en": "It's under the bed.",
                "kr": "침대 아래에 있어."
            },
            {
                "en": "It's in the box.",
                "kr": "상자 안에 있어."
            },
            {
                "en": "It's by the door.",
                "kr": "문 옆에 있어."
            }
        ]
    },
    {
        "code": "4G_L06",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 06 - What Are You Doing?",
        "words": [
            {
                "en": "doing",
                "kr": "하고 있는"
            },
            {
                "en": "reading",
                "kr": "읽고 있는"
            },
            {
                "en": "drawing",
                "kr": "그리고 있는"
            },
            {
                "en": "cooking",
                "kr": "요리하고 있는"
            },
            {
                "en": "cleaning",
                "kr": "청소하고 있는"
            },
            {
                "en": "singing",
                "kr": "노래하고 있는"
            },
            {
                "en": "dancing",
                "kr": "춤추고 있는"
            },
            {
                "en": "washing",
                "kr": "씻고 있는"
            },
            {
                "en": "eating",
                "kr": "먹고 있는"
            },
            {
                "en": "sleeping",
                "kr": "자고 있는"
            }
        ],
        "sentences": [
            {
                "en": "What are you doing?",
                "kr": "너 지금 뭐 하고 있니?"
            },
            {
                "en": "I'm reading a book.",
                "kr": "나는 책을 읽고 있어."
            },
            {
                "en": "I'm drawing a picture.",
                "kr": "나는 그림을 그리고 있어."
            },
            {
                "en": "I'm cooking dinner.",
                "kr": "나는 저녁을 요리하고 있어."
            },
            {
                "en": "I'm singing a song.",
                "kr": "나는 노래를 부르고 있어."
            }
        ]
    },
    {
        "code": "4G_L07",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 07 - Is This Your Jacket?",
        "words": [
            {
                "en": "jacket",
                "kr": "재킷"
            },
            {
                "en": "coat",
                "kr": "코트"
            },
            {
                "en": "skirt",
                "kr": "치마"
            },
            {
                "en": "shirt",
                "kr": "셔츠"
            },
            {
                "en": "pants",
                "kr": "바지"
            },
            {
                "en": "shoes",
                "kr": "신발"
            },
            {
                "en": "socks",
                "kr": "양말"
            },
            {
                "en": "hat",
                "kr": "모자"
            },
            {
                "en": "glove",
                "kr": "장갑"
            },
            {
                "en": "your",
                "kr": "너의"
            },
            {
                "en": "my",
                "kr": "나의"
            }
        ],
        "sentences": [
            {
                "en": "Is this your jacket?",
                "kr": "이것은 너의 재킷이니?"
            },
            {
                "en": "Yes, it is.",
                "kr": "응, 맞아."
            },
            {
                "en": "No, it isn't.",
                "kr": "아니, 아니야."
            },
            {
                "en": "Whose socks are these?",
                "kr": "이것은 누구의 양말이니?"
            },
            {
                "en": "They are mine.",
                "kr": "그것들은 내 것이야."
            }
        ]
    },
    {
        "code": "4G_L08",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 08 - Do You Want Some Chicken?",
        "words": [
            {
                "en": "chicken",
                "kr": "닭고기, 치킨"
            },
            {
                "en": "fish",
                "kr": "생선"
            },
            {
                "en": "milk",
                "kr": "우유"
            },
            {
                "en": "water",
                "kr": "물"
            },
            {
                "en": "juice",
                "kr": "주스"
            },
            {
                "en": "cake",
                "kr": "케이크"
            },
            {
                "en": "cookie",
                "kr": "쿠키"
            },
            {
                "en": "want",
                "kr": "원하다"
            },
            {
                "en": "some",
                "kr": "약간의, 조금"
            },
            {
                "en": "delicious",
                "kr": "맛있는"
            }
        ],
        "sentences": [
            {
                "en": "Do you want some chicken?",
                "kr": "치킨 좀 먹을래?"
            },
            {
                "en": "Yes, please.",
                "kr": "응, 부탁해."
            },
            {
                "en": "No, thanks.",
                "kr": "아니, 됐어."
            },
            {
                "en": "Do you want some milk?",
                "kr": "우유 좀 마실래?"
            },
            {
                "en": "It's delicious.",
                "kr": "정말 맛있어."
            }
        ]
    },
    {
        "code": "4G_L09",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 09 - What Day Is It Today?",
        "words": [
            {
                "en": "day",
                "kr": "요일, 날"
            },
            {
                "en": "today",
                "kr": "오늘"
            },
            {
                "en": "Monday",
                "kr": "월요일"
            },
            {
                "en": "Tuesday",
                "kr": "화요일"
            },
            {
                "en": "Wednesday",
                "kr": "수요일"
            },
            {
                "en": "Thursday",
                "kr": "목요일"
            },
            {
                "en": "Friday",
                "kr": "금요일"
            },
            {
                "en": "Saturday",
                "kr": "토요일"
            },
            {
                "en": "Sunday",
                "kr": "일요일"
            },
            {
                "en": "week",
                "kr": "주, 일주일"
            }
        ],
        "sentences": [
            {
                "en": "What day is it today?",
                "kr": "오늘 무슨 요일이니?"
            },
            {
                "en": "It's Monday.",
                "kr": "월요일이야."
            },
            {
                "en": "It's Friday.",
                "kr": "금요일이야."
            },
            {
                "en": "It's Sunday.",
                "kr": "일요일이야."
            },
            {
                "en": "Have a nice weekend.",
                "kr": "좋은 주말 보내."
            }
        ]
    },
    {
        "code": "4G_L10",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 10 - How Much Is It?",
        "words": [
            {
                "en": "much",
                "kr": "많은"
            },
            {
                "en": "hundred",
                "kr": "백 (100)"
            },
            {
                "en": "thousand",
                "kr": "천 (1000)"
            },
            {
                "en": "won",
                "kr": "원 (화폐)"
            },
            {
                "en": "dollar",
                "kr": "달러"
            },
            {
                "en": "cheap",
                "kr": "값싼"
            },
            {
                "en": "expensive",
                "kr": "비싼"
            },
            {
                "en": "buy",
                "kr": "사다"
            },
            {
                "en": "ruler",
                "kr": "자"
            },
            {
                "en": "notebook",
                "kr": "공책"
            }
        ],
        "sentences": [
            {
                "en": "How much is it?",
                "kr": "이것은 얼마인가요?"
            },
            {
                "en": "It's five hundred won.",
                "kr": "500원입니다."
            },
            {
                "en": "It's one thousand won.",
                "kr": "1,000원입니다."
            },
            {
                "en": "I want to buy this notebook.",
                "kr": "이 공책을 사고 싶어요."
            },
            {
                "en": "Here's the change.",
                "kr": "여기 거스름돈입니다."
            }
        ]
    },
    {
        "code": "5G_L01",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 01 - Where Are You From?",
        "words": [
            {
                "en": "country",
                "kr": "나라, 국가"
            },
            {
                "en": "Korea",
                "kr": "한국"
            },
            {
                "en": "Canada",
                "kr": "캐나다"
            },
            {
                "en": "Vietnam",
                "kr": "베트남"
            },
            {
                "en": "France",
                "kr": "프랑스"
            },
            {
                "en": "India",
                "kr": "인도"
            },
            {
                "en": "U.S.",
                "kr": "미국"
            },
            {
                "en": "from",
                "kr": "~출신의"
            },
            {
                "en": "friend",
                "kr": "친구"
            },
            {
                "en": "cousin",
                "kr": "사촌"
            },
            {
                "en": "beautiful",
                "kr": "아름다운"
            }
        ],
        "sentences": [
            {
                "en": "Where are you from?",
                "kr": "어느 나라에서 왔니?"
            },
            {
                "en": "I'm from Canada.",
                "kr": "나는 캐나다에서 왔어."
            },
            {
                "en": "I'm from India.",
                "kr": "나는 인도에서 왔어."
            },
            {
                "en": "This is my friend, Uju.",
                "kr": "이쪽은 내 친구 우주야."
            },
            {
                "en": "Nice to meet you.",
                "kr": "만나서 반가워."
            }
        ]
    },
    {
        "code": "5G_L02",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 02 - What Are These?",
        "words": [
            {
                "en": "these",
                "kr": "이것들"
            },
            {
                "en": "those",
                "kr": "저것들"
            },
            {
                "en": "glasses",
                "kr": "안경"
            },
            {
                "en": "gloves",
                "kr": "장갑"
            },
            {
                "en": "scissors",
                "kr": "가위"
            },
            {
                "en": "pants",
                "kr": "바지"
            },
            {
                "en": "shoes",
                "kr": "신발"
            },
            {
                "en": "socks",
                "kr": "양말"
            },
            {
                "en": "heavy",
                "kr": "무거운"
            },
            {
                "en": "light",
                "kr": "가벼운"
            }
        ],
        "sentences": [
            {
                "en": "What are these?",
                "kr": "이것들은 무엇이니?"
            },
            {
                "en": "They are glasses.",
                "kr": "그것들은 안경이야."
            },
            {
                "en": "What are those?",
                "kr": "저것들은 무엇이니?"
            },
            {
                "en": "They are gloves.",
                "kr": "그것들은 장갑이야."
            },
            {
                "en": "They are heavy.",
                "kr": "그것들은 무거워."
            }
        ]
    },
    {
        "code": "5G_L03",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 03 - Can I Take Pictures?",
        "words": [
            {
                "en": "take",
                "kr": "찍다, 취하다"
            },
            {
                "en": "picture",
                "kr": "사진, 그림"
            },
            {
                "en": "sit",
                "kr": "앉다"
            },
            {
                "en": "touch",
                "kr": "만지다"
            },
            {
                "en": "feed",
                "kr": "먹이를 주다"
            },
            {
                "en": "pet",
                "kr": "애완동물"
            },
            {
                "en": "museum",
                "kr": "박물관"
            },
            {
                "en": "camera",
                "kr": "카메라"
            },
            {
                "en": "sorry",
                "kr": "미안한"
            },
            {
                "en": "sure",
                "kr": "물론"
            }
        ],
        "sentences": [
            {
                "en": "Can I take pictures?",
                "kr": "사진을 찍어도 되나요?"
            },
            {
                "en": "Sure, you can.",
                "kr": "물론이지, 찍어도 돼."
            },
            {
                "en": "Sorry, you can't.",
                "kr": "미안하지만 안 돼."
            },
            {
                "en": "Can I sit here?",
                "kr": "여기 앉아도 되나요?"
            },
            {
                "en": "Can I feed the animals?",
                "kr": "동물들에게 먹이를 주어도 되나요?"
            }
        ]
    },
    {
        "code": "5G_L04",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 04 - Whose Pen Is This?",
        "words": [
            {
                "en": "whose",
                "kr": "누구의"
            },
            {
                "en": "pen",
                "kr": "펜"
            },
            {
                "en": "brush",
                "kr": "붓"
            },
            {
                "en": "umbrella",
                "kr": "우산"
            },
            {
                "en": "watch",
                "kr": "손목시계"
            },
            {
                "en": "cap",
                "kr": "모자"
            },
            {
                "en": "mine",
                "kr": "나의 것"
            },
            {
                "en": "yours",
                "kr": "너의 것"
            },
            {
                "en": "his",
                "kr": "그의 것"
            },
            {
                "en": "hers",
                "kr": "그녀의 것"
            }
        ],
        "sentences": [
            {
                "en": "Whose pen is this?",
                "kr": "이것은 누구의 펜이니?"
            },
            {
                "en": "It's mine.",
                "kr": "그것은 내 거야."
            },
            {
                "en": "It's Tom's.",
                "kr": "그것은 톰의 것이야."
            },
            {
                "en": "Whose watch is that?",
                "kr": "저것은 누구의 시계니?"
            },
            {
                "en": "It's not mine.",
                "kr": "그것은 내 것이 아니야."
            }
        ]
    },
    {
        "code": "5G_L05",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 05 - Let's Go Shopping",
        "words": [
            {
                "en": "shopping",
                "kr": "쇼핑"
            },
            {
                "en": "buy",
                "kr": "사다"
            },
            {
                "en": "store",
                "kr": "가게, 상점"
            },
            {
                "en": "market",
                "kr": "시장"
            },
            {
                "en": "shirt",
                "kr": "셔츠"
            },
            {
                "en": "hat",
                "kr": "모자"
            },
            {
                "en": "jacket",
                "kr": "재킷"
            },
            {
                "en": "price",
                "kr": "가격"
            },
            {
                "en": "cheap",
                "kr": "값싼"
            },
            {
                "en": "expensive",
                "kr": "비싼"
            }
        ],
        "sentences": [
            {
                "en": "Let's go shopping.",
                "kr": "쇼핑하러 가자."
            },
            {
                "en": "Sounds great.",
                "kr": "좋은 생각이야."
            },
            {
                "en": "I want to buy a hat.",
                "kr": "나는 모자를 사고 싶어."
            },
            {
                "en": "Look at this shirt.",
                "kr": "이 셔츠를 봐."
            },
            {
                "en": "It looks cool.",
                "kr": "정말 멋져 보여."
            }
        ]
    },
    {
        "code": "5G_L06",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 06 - What Will You Do This Summer?",
        "words": [
            {
                "en": "summer",
                "kr": "여름"
            },
            {
                "en": "vacation",
                "kr": "방학, 휴가"
            },
            {
                "en": "travel",
                "kr": "여행하다"
            },
            {
                "en": "visit",
                "kr": "방문하다"
            },
            {
                "en": "grandparents",
                "kr": "조부모님"
            },
            {
                "en": "camp",
                "kr": "캠프하다"
            },
            {
                "en": "swim",
                "kr": "수영하다"
            },
            {
                "en": "beach",
                "kr": "해변"
            },
            {
                "en": "mountain",
                "kr": "산"
            },
            {
                "en": "plan",
                "kr": "계획"
            }
        ],
        "sentences": [
            {
                "en": "What will you do this summer?",
                "kr": "이번 여름에 무엇을 할 거니?"
            },
            {
                "en": "I'll visit my grandparents.",
                "kr": "나는 조부모님 댁을 방문할 거야."
            },
            {
                "en": "I'll go to the beach.",
                "kr": "나는 해변에 갈 거야."
            },
            {
                "en": "I'll join a science camp.",
                "kr": "나는 과학 캠프에 참여할 거야."
            },
            {
                "en": "Have a nice vacation.",
                "kr": "즐거운 방학 보내."
            }
        ]
    },
    {
        "code": "5G_L07",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 07 - I Went to a Watermelon Festival",
        "words": [
            {
                "en": "went",
                "kr": "갔다 (go의 과거)"
            },
            {
                "en": "saw",
                "kr": "보았다 (see의 과거)"
            },
            {
                "en": "ate",
                "kr": "먹었다 (eat의 과거)"
            },
            {
                "en": "had",
                "kr": "가졌다 (have의 과거)"
            },
            {
                "en": "festival",
                "kr": "축제"
            },
            {
                "en": "watermelon",
                "kr": "수박"
            },
            {
                "en": "delicious",
                "kr": "맛있는"
            },
            {
                "en": "yesterday",
                "kr": "어제"
            },
            {
                "en": "weekend",
                "kr": "주말"
            },
            {
                "en": "exciting",
                "kr": "신나는"
            }
        ],
        "sentences": [
            {
                "en": "I went to a watermelon festival.",
                "kr": "나는 수박 축제에 갔어."
            },
            {
                "en": "What did you do yesterday?",
                "kr": "너 어제 무엇을 했니?"
            },
            {
                "en": "I ate delicious food.",
                "kr": "나는 맛있는 음식을 먹었어."
            },
            {
                "en": "I had a great time.",
                "kr": "나는 좋은 시간을 보냈어."
            },
            {
                "en": "It was wonderful.",
                "kr": "정말 멋졌어."
            }
        ]
    },
    {
        "code": "5G_L08",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 08 - What Would You Like?",
        "words": [
            {
                "en": "order",
                "kr": "주문하다"
            },
            {
                "en": "pizza",
                "kr": "피자"
            },
            {
                "en": "burger",
                "kr": "버거"
            },
            {
                "en": "salad",
                "kr": "샐러드"
            },
            {
                "en": "juice",
                "kr": "주스"
            },
            {
                "en": "water",
                "kr": "물"
            },
            {
                "en": "soup",
                "kr": "수프"
            },
            {
                "en": "sandwich",
                "kr": "샌드위치"
            },
            {
                "en": "hungry",
                "kr": "배고픈"
            },
            {
                "en": "thirsty",
                "kr": "목마른"
            }
        ],
        "sentences": [
            {
                "en": "What would you like?",
                "kr": "무엇을 드시겠어요?"
            },
            {
                "en": "I'd like a sandwich, please.",
                "kr": "샌드위치 하나 주세요."
            },
            {
                "en": "I'd like orange juice.",
                "kr": "오렌지 주스를 마시고 싶어요."
            },
            {
                "en": "Is that all?",
                "kr": "그게 전부인가요?"
            },
            {
                "en": "For here or to go?",
                "kr": "여기서 드시나요, 포장인가요?"
            }
        ]
    },
    {
        "code": "5G_L09",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 09 - They Are Twenty Thousand Won",
        "words": [
            {
                "en": "twenty",
                "kr": "이십 (20)"
            },
            {
                "en": "thirty",
                "kr": "삼십 (30)"
            },
            {
                "en": "forty",
                "kr": "사십 (40)"
            },
            {
                "en": "fifty",
                "kr": "오십 (50)"
            },
            {
                "en": "hundred",
                "kr": "백 (100)"
            },
            {
                "en": "thousand",
                "kr": "천 (1000)"
            },
            {
                "en": "won",
                "kr": "원 (화폐)"
            },
            {
                "en": "pants",
                "kr": "바지"
            },
            {
                "en": "shoes",
                "kr": "신발"
            },
            {
                "en": "gloves",
                "kr": "장갑"
            }
        ],
        "sentences": [
            {
                "en": "How much are they?",
                "kr": "이것들은 얼마인가요?"
            },
            {
                "en": "They are twenty thousand won.",
                "kr": "20,000원입니다."
            },
            {
                "en": "How much is this shirt?",
                "kr": "이 셔츠는 얼마인가요?"
            },
            {
                "en": "It is ten thousand won.",
                "kr": "10,000원입니다."
            },
            {
                "en": "I will take it.",
                "kr": "이것으로 할게요."
            }
        ]
    },
    {
        "code": "5G_L10",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 10 - What Time Do You Get Up?",
        "words": [
            {
                "en": "get up",
                "kr": "기상하다, 일어나다"
            },
            {
                "en": "wash",
                "kr": "씻다"
            },
            {
                "en": "breakfast",
                "kr": "아침 식사"
            },
            {
                "en": "school",
                "kr": "학교"
            },
            {
                "en": "lunch",
                "kr": "점심 식사"
            },
            {
                "en": "homework",
                "kr": "숙제"
            },
            {
                "en": "dinner",
                "kr": "저녁 식사"
            },
            {
                "en": "bed",
                "kr": "잠자리, 침대"
            },
            {
                "en": "daily",
                "kr": "매일의, 일상의"
            },
            {
                "en": "routine",
                "kr": "일과"
            }
        ],
        "sentences": [
            {
                "en": "What time do you get up?",
                "kr": "너 몇 시에 일어나니?"
            },
            {
                "en": "I get up at seven o'clock.",
                "kr": "나는 7시에 일어나."
            },
            {
                "en": "What time do you eat lunch?",
                "kr": "너 몇 시에 점심을 먹니?"
            },
            {
                "en": "I go to bed at ten o'clock.",
                "kr": "나는 10시에 잠자리에 들어."
            },
            {
                "en": "I always do my homework.",
                "kr": "나는 항상 숙제를 해."
            }
        ]
    },
    {
        "code": "5G_L11",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 11 - My Favorite Subject Is Math",
        "words": [
            {
                "en": "subject",
                "kr": "과목"
            },
            {
                "en": "favorite",
                "kr": "가장 좋아하는"
            },
            {
                "en": "math",
                "kr": "수학"
            },
            {
                "en": "science",
                "kr": "과학"
            },
            {
                "en": "English",
                "kr": "영어"
            },
            {
                "en": "music",
                "kr": "음악"
            },
            {
                "en": "art",
                "kr": "미술"
            },
            {
                "en": "P.E.",
                "kr": "체육"
            },
            {
                "en": "history",
                "kr": "역사"
            },
            {
                "en": "Korean",
                "kr": "국어"
            }
        ],
        "sentences": [
            {
                "en": "What's your favorite subject?",
                "kr": "가장 좋아하는 과목이 뭐니?"
            },
            {
                "en": "My favorite subject is math.",
                "kr": "내가 가장 좋아하는 과목은 수학이야."
            },
            {
                "en": "I like science and art.",
                "kr": "나는 과학과 미술을 좋아해."
            },
            {
                "en": "P.E. is fun and exciting.",
                "kr": "체육은 재미있고 신나."
            },
            {
                "en": "Why do you like English?",
                "kr": "너는 왜 영어를 좋아하니?"
            }
        ]
    },
    {
        "code": "5G_L12",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 12 - What Do You Want to Be?",
        "words": [
            {
                "en": "future",
                "kr": "미래"
            },
            {
                "en": "job",
                "kr": "직업"
            },
            {
                "en": "doctor",
                "kr": "의사"
            },
            {
                "en": "pilot",
                "kr": "조종사"
            },
            {
                "en": "cook",
                "kr": "요리사"
            },
            {
                "en": "scientist",
                "kr": "과학자"
            },
            {
                "en": "artist",
                "kr": "화가, 예술가"
            },
            {
                "en": "police officer",
                "kr": "경찰관"
            },
            {
                "en": "firefighter",
                "kr": "소방관"
            },
            {
                "en": "teacher",
                "kr": "선생님"
            }
        ],
        "sentences": [
            {
                "en": "What do you want to be?",
                "kr": "너는 장래에 무엇이 되고 싶니?"
            },
            {
                "en": "I want to be a doctor.",
                "kr": "나는 의사가 되고 싶어."
            },
            {
                "en": "I want to be a scientist.",
                "kr": "나는 과학자가 되고 싶어."
            },
            {
                "en": "Why do you want to be a cook?",
                "kr": "너는 왜 요리사가 되고 싶니?"
            },
            {
                "en": "Because I like cooking.",
                "kr": "요리하는 것을 좋아하기 때문이야."
            }
        ]
    },
    {
        "code": "6G_L01",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 01 - What Grade Are You In?",
        "words": [
            {
                "en": "grade",
                "kr": "학년"
            },
            {
                "en": "first",
                "kr": "첫 번째의 (1번째)"
            },
            {
                "en": "second",
                "kr": "두 번째의 (2번째)"
            },
            {
                "en": "third",
                "kr": "세 번째의 (3번째)"
            },
            {
                "en": "fourth",
                "kr": "네 번째의 (4번째)"
            },
            {
                "en": "fifth",
                "kr": "다섯 번째의 (5번째)"
            },
            {
                "en": "sixth",
                "kr": "여섯 번째의 (6번째)"
            },
            {
                "en": "spell",
                "kr": "철자를 말하다"
            },
            {
                "en": "club",
                "kr": "동아리"
            },
            {
                "en": "join",
                "kr": "가입하다, 함께하다"
            }
        ],
        "sentences": [
            {
                "en": "What grade are you in?",
                "kr": "너는 몇 학년이니?"
            },
            {
                "en": "I'm in the sixth grade.",
                "kr": "나는 6학년이야."
            },
            {
                "en": "How do you spell your name?",
                "kr": "네 이름 철자가 어떻게 되니?"
            },
            {
                "en": "Can I join the club?",
                "kr": "동아리에 가입해도 될까요?"
            },
            {
                "en": "I play the guitar every day.",
                "kr": "나는 매일 기타를 쳐."
            }
        ]
    },
    {
        "code": "6G_L02",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 02 - What Season Do You Like?",
        "words": [
            {
                "en": "season",
                "kr": "계절"
            },
            {
                "en": "spring",
                "kr": "봄"
            },
            {
                "en": "summer",
                "kr": "여름"
            },
            {
                "en": "fall",
                "kr": "가을"
            },
            {
                "en": "winter",
                "kr": "겨울"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            },
            {
                "en": "cool",
                "kr": "시원한"
            },
            {
                "en": "flower",
                "kr": "꽃"
            },
            {
                "en": "swim",
                "kr": "수영하다"
            },
            {
                "en": "skate",
                "kr": "스케이트 타다"
            }
        ],
        "sentences": [
            {
                "en": "What season do you like?",
                "kr": "너는 어떤 계절을 좋아하니?"
            },
            {
                "en": "I like spring.",
                "kr": "나는 봄을 좋아해."
            },
            {
                "en": "Why do you like winter?",
                "kr": "너는 왜 겨울을 좋아하니?"
            },
            {
                "en": "Because I can skate.",
                "kr": "스케이트를 탈 수 있기 때문이야."
            },
            {
                "en": "It's cool in fall.",
                "kr": "가을에는 시원해."
            }
        ]
    },
    {
        "code": "6G_L03",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 03 - When Is Your Birthday?",
        "words": [
            {
                "en": "birthday",
                "kr": "생일"
            },
            {
                "en": "January",
                "kr": "1월"
            },
            {
                "en": "February",
                "kr": "2월"
            },
            {
                "en": "March",
                "kr": "3월"
            },
            {
                "en": "April",
                "kr": "4월"
            },
            {
                "en": "May",
                "kr": "5월"
            },
            {
                "en": "June",
                "kr": "6월"
            },
            {
                "en": "July",
                "kr": "7월"
            },
            {
                "en": "August",
                "kr": "8월"
            },
            {
                "en": "September",
                "kr": "9월"
            },
            {
                "en": "October",
                "kr": "10월"
            },
            {
                "en": "November",
                "kr": "11월"
            },
            {
                "en": "December",
                "kr": "12월"
            }
        ],
        "sentences": [
            {
                "en": "When is your birthday?",
                "kr": "네 생일이 언제니?"
            },
            {
                "en": "It's on May fifth.",
                "kr": "5월 5일이야."
            },
            {
                "en": "It's on October third.",
                "kr": "10월 3일이야."
            },
            {
                "en": "Happy birthday to you!",
                "kr": "생일 축하해!"
            },
            {
                "en": "Let's celebrate together.",
                "kr": "함께 축하하자."
            }
        ]
    },
    {
        "code": "6G_L04",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 04 - Why Are You Happy?",
        "words": [
            {
                "en": "happy",
                "kr": "행복한, 기쁜"
            },
            {
                "en": "sad",
                "kr": "슬픈"
            },
            {
                "en": "excited",
                "kr": "신난, 들뜬"
            },
            {
                "en": "worried",
                "kr": "걱정스러운"
            },
            {
                "en": "angry",
                "kr": "화난"
            },
            {
                "en": "win",
                "kr": "이기다"
            },
            {
                "en": "lose",
                "kr": "잃다, 지다"
            },
            {
                "en": "prize",
                "kr": "상, 상품"
            },
            {
                "en": "test",
                "kr": "시험"
            },
            {
                "en": "because",
                "kr": "~때문에"
            }
        ],
        "sentences": [
            {
                "en": "Why are you happy?",
                "kr": "너는 왜 기쁘니?"
            },
            {
                "en": "Because I won first prize.",
                "kr": "1등 상을 탔기 때문이야."
            },
            {
                "en": "Why are you worried?",
                "kr": "너는 왜 걱정하니?"
            },
            {
                "en": "Because I have a math test.",
                "kr": "수학 시험이 있기 때문이야."
            },
            {
                "en": "Cheer up!",
                "kr": "힘내!"
            }
        ]
    },
    {
        "code": "6G_L05",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 05 - Where Is ABC Library?",
        "words": [
            {
                "en": "library",
                "kr": "도서관"
            },
            {
                "en": "bank",
                "kr": "은행"
            },
            {
                "en": "hospital",
                "kr": "병원"
            },
            {
                "en": "post office",
                "kr": "우체국"
            },
            {
                "en": "museum",
                "kr": "박물관"
            },
            {
                "en": "straight",
                "kr": "곧장, 직진하여"
            },
            {
                "en": "left",
                "kr": "왼쪽"
            },
            {
                "en": "right",
                "kr": "오른쪽"
            },
            {
                "en": "turn",
                "kr": "돌다"
            },
            {
                "en": "block",
                "kr": "블록, 구역"
            }
        ],
        "sentences": [
            {
                "en": "Where is ABC Library?",
                "kr": "ABC 도서관이 어디에 있나요?"
            },
            {
                "en": "Go straight two blocks.",
                "kr": "두 블록 직진하세요."
            },
            {
                "en": "Turn left at the bank.",
                "kr": "은행에서 좌회전하세요."
            },
            {
                "en": "It's on your right.",
                "kr": "당신의 오른쪽에 있어요."
            },
            {
                "en": "You can't miss it.",
                "kr": "쉽게 찾으실 수 있을 거예요."
            }
        ]
    },
    {
        "code": "6G_L06",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 06 - He Has Blue Eyes",
        "words": [
            {
                "en": "tall",
                "kr": "키가 큰"
            },
            {
                "en": "short",
                "kr": "키가 작은"
            },
            {
                "en": "hair",
                "kr": "머리카락"
            },
            {
                "en": "straight",
                "kr": "직모의, 곧은"
            },
            {
                "en": "curly",
                "kr": "곱슬곱슬한"
            },
            {
                "en": "long",
                "kr": "긴"
            },
            {
                "en": "eyes",
                "kr": "눈"
            },
            {
                "en": "glasses",
                "kr": "안경"
            },
            {
                "en": "wear",
                "kr": "착용하다, 입다"
            },
            {
                "en": "look like",
                "kr": "~처럼 보이다"
            }
        ],
        "sentences": [
            {
                "en": "What does he look like?",
                "kr": "그는 어떻게 생겼니?"
            },
            {
                "en": "He is tall and has blue eyes.",
                "kr": "그는 키가 크고 파란 눈을 가졌어."
            },
            {
                "en": "She has long curly hair.",
                "kr": "그녀는 긴 곱슬머리를 가지고 있어."
            },
            {
                "en": "He is wearing glasses.",
                "kr": "그는 안경을 쓰고 있어."
            },
            {
                "en": "That's my brother.",
                "kr": "저 사람이 내 남동생이야."
            }
        ]
    },
    {
        "code": "6G_L07",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 07 - Can You Come to the Movie Festival?",
        "words": [
            {
                "en": "invite",
                "kr": "초대하다"
            },
            {
                "en": "party",
                "kr": "파티"
            },
            {
                "en": "festival",
                "kr": "축제"
            },
            {
                "en": "movie",
                "kr": "영화"
            },
            {
                "en": "concert",
                "kr": "음악회, 콘서트"
            },
            {
                "en": "busy",
                "kr": "바쁜"
            },
            {
                "en": "free",
                "kr": "한가한, 자유로운"
            },
            {
                "en": "sure",
                "kr": "물론"
            },
            {
                "en": "sorry",
                "kr": "미안한"
            },
            {
                "en": "join",
                "kr": "함께하다"
            }
        ],
        "sentences": [
            {
                "en": "Can you come to the movie festival?",
                "kr": "영화 축제에 올 수 있니?"
            },
            {
                "en": "Sure, I'd love to.",
                "kr": "물론이지, 꼭 갈게."
            },
            {
                "en": "Sorry, I can't.",
                "kr": "미안하지만 갈 수 없어."
            },
            {
                "en": "I'm busy that day.",
                "kr": "그날은 바빠."
            },
            {
                "en": "See you at the festival.",
                "kr": "축제에서 만나자."
            }
        ]
    },
    {
        "code": "6G_L08",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 08 - It's Taller Than the Tree",
        "words": [
            {
                "en": "taller",
                "kr": "더 키가 큰"
            },
            {
                "en": "smaller",
                "kr": "더 작은"
            },
            {
                "en": "bigger",
                "kr": "더 큰"
            },
            {
                "en": "faster",
                "kr": "더 빠른"
            },
            {
                "en": "slower",
                "kr": "더 느린"
            },
            {
                "en": "stronger",
                "kr": "더 힘이 센"
            },
            {
                "en": "heavier",
                "kr": "더 무거운"
            },
            {
                "en": "than",
                "kr": "~보다"
            },
            {
                "en": "tower",
                "kr": "탑"
            },
            {
                "en": "tree",
                "kr": "나무"
            }
        ],
        "sentences": [
            {
                "en": "It's taller than the tree.",
                "kr": "그것은 나무보다 더 커."
            },
            {
                "en": "A cheetah is faster than a lion.",
                "kr": "치타는 사자보다 더 빨라."
            },
            {
                "en": "I'm stronger than you.",
                "kr": "내가 너보다 힘이 더 세."
            },
            {
                "en": "Which one is heavier?",
                "kr": "어느 것이 더 무겁니?"
            },
            {
                "en": "This box is heavier.",
                "kr": "이 상자가 더 무거워."
            }
        ]
    },
    {
        "code": "6G_L09",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 09 - What Are You Going to Do?",
        "words": [
            {
                "en": "plan",
                "kr": "계획"
            },
            {
                "en": "future",
                "kr": "미래"
            },
            {
                "en": "tomorrow",
                "kr": "내일"
            },
            {
                "en": "weekend",
                "kr": "주말"
            },
            {
                "en": "bake",
                "kr": "굽다"
            },
            {
                "en": "cookies",
                "kr": "쿠키"
            },
            {
                "en": "ride",
                "kr": "타다"
            },
            {
                "en": "bike",
                "kr": "자전거"
            },
            {
                "en": "visit",
                "kr": "방문하다"
            },
            {
                "en": "museum",
                "kr": "박물관"
            }
        ],
        "sentences": [
            {
                "en": "What are you going to do this weekend?",
                "kr": "이번 주말에 무엇을 할 예정이니?"
            },
            {
                "en": "I'm going to bake cookies.",
                "kr": "나는 쿠키를 구울 예정이야."
            },
            {
                "en": "I'm going to ride my bike.",
                "kr": "나는 자전거를 탈 거야."
            },
            {
                "en": "Are you going to visit the museum?",
                "kr": "박물관에 방문할 거니?"
            },
            {
                "en": "Yes, I am.",
                "kr": "응, 그럴 거야."
            }
        ]
    },
    {
        "code": "6G_L10",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 10 - How Often Do You Eat Vegetables?",
        "words": [
            {
                "en": "often",
                "kr": "자주, 종종"
            },
            {
                "en": "always",
                "kr": "항상"
            },
            {
                "en": "usually",
                "kr": "보통, 대개"
            },
            {
                "en": "sometimes",
                "kr": "때때로"
            },
            {
                "en": "never",
                "kr": "결코 ~않다"
            },
            {
                "en": "once",
                "kr": "한 번"
            },
            {
                "en": "twice",
                "kr": "두 번"
            },
            {
                "en": "exercise",
                "kr": "운동하다"
            },
            {
                "en": "vegetable",
                "kr": "채소"
            },
            {
                "en": "healthy",
                "kr": "건강한"
            }
        ],
        "sentences": [
            {
                "en": "How often do you eat vegetables?",
                "kr": "얼마나 자주 채소를 먹니?"
            },
            {
                "en": "I eat vegetables every day.",
                "kr": "나는 매일 채소를 먹어."
            },
            {
                "en": "How often do you exercise?",
                "kr": "얼마나 자주 운동하니?"
            },
            {
                "en": "I exercise three times a week.",
                "kr": "나는 일주일에 세 번 운동해."
            },
            {
                "en": "That is a healthy habit.",
                "kr": "그것은 건강한 습관이야."
            }
        ]
    },
    {
        "code": "6G_L11",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 11 - Do You Know Anything About Tuho?",
        "words": [
            {
                "en": "traditional",
                "kr": "전통적인"
            },
            {
                "en": "game",
                "kr": "놀이, 게임"
            },
            {
                "en": "Tuho",
                "kr": "투호"
            },
            {
                "en": "Jegichagi",
                "kr": "제기차기"
            },
            {
                "en": "Yutnori",
                "kr": "윷놀이"
            },
            {
                "en": "culture",
                "kr": "문화"
            },
            {
                "en": "throw",
                "kr": "던지다"
            },
            {
                "en": "arrow",
                "kr": "화살"
            },
            {
                "en": "jar",
                "kr": "항아리"
            },
            {
                "en": "rules",
                "kr": "규칙"
            }
        ],
        "sentences": [
            {
                "en": "Do you know anything about Tuho?",
                "kr": "투호에 대해 아는 것이 있니?"
            },
            {
                "en": "It is a traditional Korean game.",
                "kr": "그것은 한국의 전통 놀이야."
            },
            {
                "en": "You throw arrows into a jar.",
                "kr": "항아리에 화살을 던져 넣는 거야."
            },
            {
                "en": "It sounds very interesting.",
                "kr": "아주 흥미롭게 들린다."
            },
            {
                "en": "Let's play together.",
                "kr": "함께 놀자."
            }
        ]
    },
    {
        "code": "6G_L12",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 12 - We Made a Class Album",
        "words": [
            {
                "en": "made",
                "kr": "만들었다 (make의 과거)"
            },
            {
                "en": "album",
                "kr": "앨범"
            },
            {
                "en": "memory",
                "kr": "추억, 기억"
            },
            {
                "en": "graduation",
                "kr": "졸업"
            },
            {
                "en": "special",
                "kr": "특별한"
            },
            {
                "en": "remember",
                "kr": "기억하다"
            },
            {
                "en": "painted",
                "kr": "칠했다"
            },
            {
                "en": "helped",
                "kr": "도왔다"
            },
            {
                "en": "cleaned",
                "kr": "청소했다"
            },
            {
                "en": "wonderful",
                "kr": "멋진"
            }
        ],
        "sentences": [
            {
                "en": "We made a class album.",
                "kr": "우리는 학급 앨범을 만들었어."
            },
            {
                "en": "What did you do this year?",
                "kr": "올해 너희는 무엇을 했니?"
            },
            {
                "en": "We painted the wall together.",
                "kr": "우리는 함께 벽화를 그렸어."
            },
            {
                "en": "I'll miss my friends and teachers.",
                "kr": "친구들과 선생님들이 그리울 거야."
            },
            {
                "en": "It was a wonderful year.",
                "kr": "멋진 한 해였어."
            }
        ]
    }
];

    const STORAGE_KEY = 'waurimal_custom_curriculum';
    const VERSION_KEY = 'waurimal_curriculum_version';

    function isStrictValidCurriculum(list) {
        if (!Array.isArray(list) || list.length !== 44) return false;
        const g3 = list.filter(l => Number(l.grade) === 3).length;
        const g4 = list.filter(l => Number(l.grade) === 4).length;
        const g5 = list.filter(l => Number(l.grade) === 5).length;
        const g6 = list.filter(l => Number(l.grade) === 6).length;
        if (g3 !== 10 || g4 !== 10 || g5 !== 12 || g6 !== 12) return false;
        return list.every(l => Array.isArray(l.words) && l.words.length > 0);
    }

    // 로컬 스토리지에 캐시된 커스텀 단원 엄격 검증
    let currentLessons = [...RAW_LESSONS];
    try {
        const cachedVersion = localStorage.getItem(VERSION_KEY);
        const saved = localStorage.getItem(STORAGE_KEY);
        let validCache = false;
        if (cachedVersion === CURRICULUM_VERSION && saved) {
            try {
                const parsed = JSON.parse(saved);
                if (isStrictValidCurriculum(parsed)) {
                    currentLessons = parsed;
                    validCache = true;
                }
            } catch (err) {}
        }
        if (!validCache) {
            // 최신 48단원 원본 데이터셋으로 자동 동기화 및 캐시 갱신
            currentLessons = [...RAW_LESSONS];
            localStorage.setItem(STORAGE_KEY, JSON.stringify(RAW_LESSONS));
            localStorage.setItem(VERSION_KEY, CURRICULUM_VERSION);
        }
    } catch (e) {
        console.warn("Curriculum local storage error:", e);
    }

    const WaurimalCurriculum = {
        // 기본 제공 원본 데이터
        defaultLessons: RAW_LESSONS,

        // 현재 활성화된 교육과정 목록 (기본 + 교사 커스텀)
        lessons: currentLessons,

        // 전체 단원 목록 조회
        getLessons: function() {
            return this.lessons;
        },

        // 단원 코드로 조회
        getLesson: function(code) {
            if (!code) return null;
            const target = String(code).trim().toLowerCase();
            return this.lessons.find(l => 
                (l.code && l.code.toLowerCase() === target) || 
                (l.id && l.id.toLowerCase() === target)
            ) || null;
        },

        // 학년별 단원 목록 조회
        getLessonsByGrade: function(grade) {
            if (!grade || grade === 'all' || grade === 'ALL') return this.lessons;
            return this.lessons.filter(l => Number(l.grade) === Number(grade));
        },

        // 특정 단원의 단어 목록 ({ en, kr } 객체 배열) 반환
        getLessonWords: function(code) {
            const l = this.getLesson(code);
            return (l && Array.isArray(l.words)) ? l.words : [];
        },

        // 특정 단원의 단어 목록 텍스트 ("en : kr\n...") 반환
        getLessonWordsText: function(code) {
            const words = this.getLessonWords(code);
            return this.wordsToText(words);
        },

        // 특정 단원의 문장 목록 ({ en, kr } 객체 배열) 반환
        getLessonSentences: function(code) {
            const l = this.getLesson(code);
            if (!l) return [];
            if (Array.isArray(l.sentences)) {
                return l.sentences.map(s => {
                    if (typeof s === 'string') return { en: s, kr: '' };
                    return { en: s.en || s.sentence || '', kr: s.kr || s.meaning || '' };
                });
            }
            return [];
        },

        // 특정 단원의 문장 문자열 배열 (["I am happy.", ...]) 반환 (Sparkle 등 간편 사용)
        getLessonSentencesList: function(code) {
            const sentences = this.getLessonSentences(code);
            return sentences.map(s => s.en).filter(s => s && s.length > 0);
        },

        // 특정 단원의 문장 목록 텍스트 ("en : kr\n...") 반환
        getLessonSentencesText: function(code) {
            const sentences = this.getLessonSentences(code);
            return this.sentencesToText(sentences);
        },

        // 전체 단어 평탄화 목록 반환
        getAllWords: function() {
            const list = [];
            this.lessons.forEach(l => {
                if (Array.isArray(l.words)) {
                    l.words.forEach(w => {
                        list.push({ ...w, lessonCode: l.code, lessonName: l.name, grade: l.grade });
                    });
                }
            });
            return list;
        },

        // 전체 문장 평탄화 목록 반환
        getAllSentences: function() {
            const list = [];
            this.lessons.forEach(l => {
                if (Array.isArray(l.sentences)) {
                    l.sentences.forEach(s => {
                        const item = typeof s === 'string' ? { en: s, kr: '' } : { en: s.en || '', kr: s.kr || '' };
                        list.push({ ...item, lessonCode: l.code, lessonName: l.name, grade: l.grade });
                    });
                }
            });
            return list;
        },

        // 랜덤 단어 추출
        getRandomWords: function(count, grade) {
            let pool = grade ? this.getLessonsByGrade(grade).flatMap(l => l.words || []) : this.getAllWords();
            const shuffled = [...pool].sort(() => Math.random() - 0.5);
            return shuffled.slice(0, count);
        },

        // ── 텍스트 파싱 & 포맷팅 헬퍼 ──
        parseWords: function(rawText) {
            if (!rawText) return [];
            return rawText.split('\n')
                .map(line => line.trim())
                .filter(line => line.length > 0)
                .map(line => {
                    if (line.includes(':')) {
                        const parts = line.split(':');
                        return { en: parts[0].trim(), kr: parts.slice(1).join(':').trim() };
                    }
                    return { en: line.trim(), kr: '' };
                });
        },

        wordsToText: function(wordsArray) {
            if (!Array.isArray(wordsArray)) return '';
            return wordsArray.map(w => w.kr ? `${w.en} : ${w.kr}` : w.en).join('\n');
        },

        parseSentences: function(rawText) {
            if (!rawText) return [];
            return rawText.split('\n')
                .map(line => line.trim())
                .filter(line => line.length > 0)
                .map(line => {
                    if (line.includes(':')) {
                        const parts = line.split(':');
                        return { en: parts[0].trim(), kr: parts.slice(1).join(':').trim() };
                    }
                    return { en: line.trim(), kr: '' };
                });
        },

        sentencesToText: function(sentencesArray) {
            if (!Array.isArray(sentencesArray)) return '';
            return sentencesArray.map(s => {
                const en = typeof s === 'string' ? s : (s.en || s.sentence || '');
                const kr = typeof s === 'string' ? '' : (s.kr || s.meaning || '');
                return kr ? `${en} : ${kr}` : en;
            }).join('\n');
        },

        // ── 로컬 및 클라우드 동기화 메서드 ──
        setLessons: function(newLessons, syncToCloud) {
            if (!Array.isArray(newLessons)) return false;
            this.lessons = newLessons;
            try {
                localStorage.setItem(STORAGE_KEY, JSON.stringify(newLessons));
                localStorage.setItem(VERSION_KEY, CURRICULUM_VERSION);
            } catch (e) {
                console.warn("Storage save error:", e);
            }
            window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: newLessons }));

            if (syncToCloud) {
                return this.saveToCloud();
            }
            return Promise.resolve(true);
        },

        saveToCloud: async function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    await db.collection('settings').doc('curriculum').set({
                        lessons: this.lessons,
                        version: CURRICULUM_VERSION,
                        updatedAt: firebase.firestore.FieldValue.serverTimestamp()
                    }, { merge: true });
                    console.log("WaurimalCurriculum: Firestore synced successfully!");
                    return true;
                } else {
                    console.warn("Firebase Firestore not initialized, stored in localStorage only.");
                    return true;
                }
            } catch (e) {
                console.error("WaurimalCurriculum cloud sync error:", e);
                return false;
            }
        },

        loadFromCloud: async function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    const docSnap = await db.collection('settings').doc('curriculum').get();
                    if (docSnap.exists && docSnap.data().lessons && Array.isArray(docSnap.data().lessons)) {
                        const cloudLessons = docSnap.data().lessons;
                        if (isStrictValidCurriculum(cloudLessons)) {
                            this.lessons = cloudLessons;
                            localStorage.setItem(STORAGE_KEY, JSON.stringify(cloudLessons));
                            localStorage.setItem(VERSION_KEY, CURRICULUM_VERSION);
                            window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: cloudLessons }));
                            return true;
                        } else {
                            console.warn("WaurimalCurriculum: Cloud data incomplete, keeping strict 48 units.");
                        }
                    }
                }
            } catch (e) {
                console.warn("WaurimalCurriculum cloud load notice (offline fallback):", e);
            }
            return false;
        },

        listenToCloud: function() {
            try {
                if (window.firebase && typeof firebase.firestore === 'function') {
                    if (!firebase.apps.length && window.WaurimalFirebase) {
                        firebase.initializeApp(WaurimalFirebase.getConfig());
                    }
                    const db = firebase.firestore();
                    return db.collection('settings').doc('curriculum').onSnapshot(docSnap => {
                        if (docSnap.exists && docSnap.data().lessons && Array.isArray(docSnap.data().lessons)) {
                            const cloudLessons = docSnap.data().lessons;
                            if (isStrictValidCurriculum(cloudLessons)) {
                                this.lessons = cloudLessons;
                                localStorage.setItem(STORAGE_KEY, JSON.stringify(cloudLessons));
                                localStorage.setItem(VERSION_KEY, CURRICULUM_VERSION);
                                window.dispatchEvent(new CustomEvent('waurimal_curriculum_updated', { detail: cloudLessons }));
                            }
                        }
                    });
                }
            } catch (e) {
                console.warn("WaurimalCurriculum cloud listener warning:", e);
            }
            return null;
        }
    };

    window.WaurimalCurriculum = WaurimalCurriculum;

    // 클라우드 백업 자동 감지
    if (typeof window !== 'undefined' && typeof window.addEventListener === 'function') {
        window.addEventListener('DOMContentLoaded', () => {
            setTimeout(() => {
                if (typeof WaurimalCurriculum.loadFromCloud === 'function') {
                    WaurimalCurriculum.loadFromCloud();
                }
            }, 800);
        });
    }

})();
