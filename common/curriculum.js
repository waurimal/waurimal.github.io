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

    const CURRICULUM_VERSION = '2026_V5_FULL_48_STRICT';

    const RAW_LESSONS = [
    {
        "code": "3G_L01",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 01 - Hi, I'm Sena",
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
                "en": "bye",
                "kr": "잘 가"
            },
            {
                "en": "goodbye",
                "kr": "안녕히 가세요"
            },
            {
                "en": "meet",
                "kr": "만나다"
            },
            {
                "en": "nice",
                "kr": "좋은, 반가운"
            },
            {
                "en": "friend",
                "kr": "친구"
            },
            {
                "en": "you",
                "kr": "너, 당신"
            }
        ],
        "sentences": [
            {
                "en": "Hi, I'm Sena.",
                "kr": "안녕, 나는 세나야."
            },
            {
                "en": "Hello, my name is Minsoo.",
                "kr": "안녕, 내 이름은 민수야."
            },
            {
                "en": "Nice to meet you.",
                "kr": "만나서 반가워."
            },
            {
                "en": "Goodbye, see you tomorrow.",
                "kr": "잘 가, 내일 또 봐."
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
                "en": "book",
                "kr": "책"
            },
            {
                "en": "pen",
                "kr": "펜"
            },
            {
                "en": "pencil",
                "kr": "연필"
            },
            {
                "en": "cup",
                "kr": "컵"
            },
            {
                "en": "cap",
                "kr": "모자"
            },
            {
                "en": "bag",
                "kr": "가방"
            },
            {
                "en": "desk",
                "kr": "책상"
            },
            {
                "en": "chair",
                "kr": "의자"
            },
            {
                "en": "apple",
                "kr": "사과"
            }
        ],
        "sentences": [
            {
                "en": "What's this?",
                "kr": "이것은 무엇이니?"
            },
            {
                "en": "It's an apple.",
                "kr": "사과야."
            },
            {
                "en": "What's that?",
                "kr": "저것은 무엇이니?"
            },
            {
                "en": "It's a pencil case.",
                "kr": "필통이야."
            }
        ]
    },
    {
        "code": "3G_L03",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 03 - Open the Box, Please",
        "words": [
            {
                "en": "open",
                "kr": "열다"
            },
            {
                "en": "close",
                "kr": "닫다"
            },
            {
                "en": "box",
                "kr": "상자"
            },
            {
                "en": "door",
                "kr": "문"
            },
            {
                "en": "window",
                "kr": "창문"
            },
            {
                "en": "stand up",
                "kr": "일어서다"
            },
            {
                "en": "sit down",
                "kr": "앉다"
            },
            {
                "en": "please",
                "kr": "부디, 제발"
            },
            {
                "en": "look",
                "kr": "보다"
            },
            {
                "en": "listen",
                "kr": "듣다"
            },
            {
                "en": "read",
                "kr": "읽다"
            },
            {
                "en": "come",
                "kr": "오다"
            }
        ],
        "sentences": [
            {
                "en": "Open the box, please.",
                "kr": "상자를 열어주세요."
            },
            {
                "en": "Close the door, please.",
                "kr": "문을 닫아주세요."
            },
            {
                "en": "Stand up, please.",
                "kr": "일어나 주세요."
            },
            {
                "en": "Sit down, please.",
                "kr": "앉아 주세요."
            }
        ]
    },
    {
        "code": "3G_L04",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 04 - Do You Like Apples?",
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
                "en": "milk",
                "kr": "우유"
            },
            {
                "en": "cheese",
                "kr": "치즈"
            },
            {
                "en": "orange",
                "kr": "오렌지"
            },
            {
                "en": "strawberry",
                "kr": "딸기"
            },
            {
                "en": "chicken",
                "kr": "닭고기"
            },
            {
                "en": "pizza",
                "kr": "피자"
            },
            {
                "en": "melon",
                "kr": "멜론"
            },
            {
                "en": "grape",
                "kr": "포도"
            },
            {
                "en": "bread",
                "kr": "빵"
            }
        ],
        "sentences": [
            {
                "en": "Do you like apples?",
                "kr": "너 사과 좋아하니?"
            },
            {
                "en": "Yes, I do.",
                "kr": "응, 좋아해."
            },
            {
                "en": "No, I don't.",
                "kr": "아니, 좋아하지 않아."
            },
            {
                "en": "I like bananas.",
                "kr": "나는 바나나를 좋아해."
            }
        ]
    },
    {
        "code": "3G_L05",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 05 - How Many Dogs?",
        "words": [
            {
                "en": "how",
                "kr": "얼마나"
            },
            {
                "en": "many",
                "kr": "많은"
            },
            {
                "en": "one",
                "kr": "하나"
            },
            {
                "en": "two",
                "kr": "둘"
            },
            {
                "en": "three",
                "kr": "셋"
            },
            {
                "en": "four",
                "kr": "넷"
            },
            {
                "en": "five",
                "kr": "다섯"
            },
            {
                "en": "six",
                "kr": "여섯"
            },
            {
                "en": "seven",
                "kr": "일곱"
            },
            {
                "en": "eight",
                "kr": "여덟"
            },
            {
                "en": "nine",
                "kr": "아홉"
            },
            {
                "en": "ten",
                "kr": "열"
            },
            {
                "en": "dog",
                "kr": "개"
            },
            {
                "en": "cat",
                "kr": "고양이"
            }
        ],
        "sentences": [
            {
                "en": "How many dogs?",
                "kr": "개가 몇 마리니?"
            },
            {
                "en": "Three dogs.",
                "kr": "세 마리야."
            },
            {
                "en": "How many birds do you see?",
                "kr": "새가 몇 마리 보이니?"
            },
            {
                "en": "I see five birds.",
                "kr": "새 다섯 마리가 보여."
            }
        ]
    },
    {
        "code": "3G_L06",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 06 - Do You Have a Ruler?",
        "words": [
            {
                "en": "have",
                "kr": "가지고 있다"
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
                "en": "glue stick",
                "kr": "풀"
            },
            {
                "en": "notebook",
                "kr": "공책"
            },
            {
                "en": "scissors",
                "kr": "가위"
            },
            {
                "en": "pencil",
                "kr": "연필"
            },
            {
                "en": "pencil case",
                "kr": "필통"
            },
            {
                "en": "marker",
                "kr": "사인펜"
            }
        ],
        "sentences": [
            {
                "en": "Do you have a ruler?",
                "kr": "너 자 가지고 있니?"
            },
            {
                "en": "Yes, I do.",
                "kr": "응, 가지고 있어."
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
        "code": "3G_L07",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 07 - Can You Swim?",
        "words": [
            {
                "en": "can",
                "kr": "~할 수 있다"
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
                "en": "jump",
                "kr": "뛰다"
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
                "en": "sing",
                "kr": "노래하다"
            },
            {
                "en": "skate",
                "kr": "스케이트 타다"
            },
            {
                "en": "ski",
                "kr": "스키 타다"
            },
            {
                "en": "climb",
                "kr": "기어오르다"
            },
            {
                "en": "ride",
                "kr": "타다"
            }
        ],
        "sentences": [
            {
                "en": "Can you swim?",
                "kr": "너 수영할 수 있니?"
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
                "en": "I can dance well.",
                "kr": "나는 춤을 잘 춰."
            }
        ]
    },
    {
        "code": "3G_L08",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 08 - Don't Run, Please",
        "words": [
            {
                "en": "run",
                "kr": "달리다"
            },
            {
                "en": "touch",
                "kr": "만지다"
            },
            {
                "en": "push",
                "kr": "밀다"
            },
            {
                "en": "shout",
                "kr": "소리치다"
            },
            {
                "en": "eat",
                "kr": "먹다"
            },
            {
                "en": "enter",
                "kr": "들어가다"
            },
            {
                "en": "stop",
                "kr": "멈추다"
            },
            {
                "en": "quiet",
                "kr": "조용한"
            },
            {
                "en": "careful",
                "kr": "조심하는"
            },
            {
                "en": "sorry",
                "kr": "미안한"
            },
            {
                "en": "sleep",
                "kr": "자다"
            }
        ],
        "sentences": [
            {
                "en": "Don't run, please.",
                "kr": "뛰지 마세요."
            },
            {
                "en": "Don't touch, please.",
                "kr": "만지지 마세요."
            },
            {
                "en": "Don't push your friends.",
                "kr": "친구를 밀지 마."
            },
            {
                "en": "Okay, I'm sorry.",
                "kr": "알겠어요, 죄송합니다."
            }
        ]
    },
    {
        "code": "3G_L09",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 09 - Where Is the Ball?",
        "words": [
            {
                "en": "where",
                "kr": "어디"
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
                "en": "ball",
                "kr": "공"
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
                "en": "bed",
                "kr": "침대"
            },
            {
                "en": "room",
                "kr": "방"
            },
            {
                "en": "chair",
                "kr": "의자"
            },
            {
                "en": "desk",
                "kr": "책상"
            }
        ],
        "sentences": [
            {
                "en": "Where is the ball?",
                "kr": "공이 어디에 있니?"
            },
            {
                "en": "It's on the chair.",
                "kr": "의자 위에 있어."
            },
            {
                "en": "It's under the table.",
                "kr": "탁자 밑에 있어."
            },
            {
                "en": "It's in the box.",
                "kr": "상자 안에 있어."
            }
        ]
    },
    {
        "code": "3G_L10",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 10 - What Color Is It?",
        "words": [
            {
                "en": "color",
                "kr": "색깔"
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
                "en": "black",
                "kr": "검은색"
            },
            {
                "en": "white",
                "kr": "하얀색"
            },
            {
                "en": "pink",
                "kr": "분홍색"
            },
            {
                "en": "orange",
                "kr": "주황색"
            },
            {
                "en": "purple",
                "kr": "보라색"
            },
            {
                "en": "brown",
                "kr": "갈색"
            },
            {
                "en": "rainbow",
                "kr": "무지개"
            }
        ],
        "sentences": [
            {
                "en": "What color is it?",
                "kr": "무슨 색깔이니?"
            },
            {
                "en": "It's blue.",
                "kr": "파란색이야."
            },
            {
                "en": "I like yellow.",
                "kr": "나는 노란색을 좋아해."
            },
            {
                "en": "It's a red hat.",
                "kr": "빨간 모자야."
            }
        ]
    },
    {
        "code": "3G_L11",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 11 - How Old Are You?",
        "words": [
            {
                "en": "old",
                "kr": "나이 든"
            },
            {
                "en": "year",
                "kr": "살, 해"
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
                "en": "birthday",
                "kr": "생일"
            },
            {
                "en": "candle",
                "kr": "양초"
            },
            {
                "en": "party",
                "kr": "파티"
            },
            {
                "en": "present",
                "kr": "선물"
            },
            {
                "en": "cake",
                "kr": "케이크"
            },
            {
                "en": "card",
                "kr": "카드"
            },
            {
                "en": "happy",
                "kr": "행복한"
            },
            {
                "en": "today",
                "kr": "오늘"
            }
        ],
        "sentences": [
            {
                "en": "How old are you?",
                "kr": "몇 살이니?"
            },
            {
                "en": "I'm ten years old.",
                "kr": "열 살이야."
            },
            {
                "en": "Happy birthday to you!",
                "kr": "생일 축하해!"
            },
            {
                "en": "Thank you so much.",
                "kr": "정말 고마워."
            }
        ]
    },
    {
        "code": "3G_L12",
        "grade": 3,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 3학년 Lesson 12 - It's Sunny",
        "words": [
            {
                "en": "weather",
                "kr": "날씨"
            },
            {
                "en": "sunny",
                "kr": "화창한"
            },
            {
                "en": "rainy",
                "kr": "비 오는"
            },
            {
                "en": "snowy",
                "kr": "눈 오는"
            },
            {
                "en": "windy",
                "kr": "바람 부는"
            },
            {
                "en": "cloudy",
                "kr": "흐린"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            },
            {
                "en": "cold",
                "kr": "추운"
            },
            {
                "en": "hot",
                "kr": "더운"
            },
            {
                "en": "coat",
                "kr": "외투, 코트"
            },
            {
                "en": "umbrella",
                "kr": "우산"
            },
            {
                "en": "sky",
                "kr": "하늘"
            }
        ],
        "sentences": [
            {
                "en": "How's the weather today?",
                "kr": "오늘 날씨가 어떠니?"
            },
            {
                "en": "It's sunny and warm.",
                "kr": "화창하고 따뜻해."
            },
            {
                "en": "It's rainy. Take an umbrella.",
                "kr": "비가 와. 우산 챙겨."
            },
            {
                "en": "Put on your coat.",
                "kr": "코트를 입으렴."
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
                "en": "good",
                "kr": "좋은"
            },
            {
                "en": "great",
                "kr": "아주 좋은"
            },
            {
                "en": "fine",
                "kr": "괜찮은"
            },
            {
                "en": "not bad",
                "kr": "나쁘지 않은"
            },
            {
                "en": "tired",
                "kr": "피곤한"
            },
            {
                "en": "sad",
                "kr": "슬픈"
            },
            {
                "en": "happy",
                "kr": "행복한"
            },
            {
                "en": "sick",
                "kr": "아픈"
            },
            {
                "en": "morning",
                "kr": "아침"
            },
            {
                "en": "afternoon",
                "kr": "오후"
            },
            {
                "en": "today",
                "kr": "오늘"
            },
            {
                "en": "class",
                "kr": "수업, 학급"
            }
        ],
        "sentences": [
            {
                "en": "How are you today?",
                "kr": "오늘 기분 어떠니?"
            },
            {
                "en": "I'm great, thank you.",
                "kr": "아주 좋아요, 고마워요."
            },
            {
                "en": "I'm tired today.",
                "kr": "오늘 좀 피곤해요."
            },
            {
                "en": "Good morning, class!",
                "kr": "좋은 아침이에요, 여러분!"
            }
        ]
    },
    {
        "code": "4G_L02",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 02 - Who Is She?",
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
                "en": "father",
                "kr": "아버지"
            },
            {
                "en": "mother",
                "kr": "어머니"
            },
            {
                "en": "brother",
                "kr": "남자형제"
            },
            {
                "en": "sister",
                "kr": "여자형제"
            },
            {
                "en": "grandmother",
                "kr": "할머니"
            },
            {
                "en": "grandfather",
                "kr": "할아버지"
            },
            {
                "en": "teacher",
                "kr": "선생님"
            },
            {
                "en": "friend",
                "kr": "친구"
            },
            {
                "en": "cousin",
                "kr": "사촌"
            }
        ],
        "sentences": [
            {
                "en": "Who is she?",
                "kr": "그녀는 누구니?"
            },
            {
                "en": "She's my sister.",
                "kr": "내 여동생이야."
            },
            {
                "en": "Who is he?",
                "kr": "그는 누구니?"
            },
            {
                "en": "He's my English teacher.",
                "kr": "내 영어 선생님이셔."
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
                "kr": "시간"
            },
            {
                "en": "o'clock",
                "kr": "정각"
            },
            {
                "en": "hour",
                "kr": "시간 (시)"
            },
            {
                "en": "minute",
                "kr": "분"
            },
            {
                "en": "watch",
                "kr": "손목시계"
            },
            {
                "en": "clock",
                "kr": "벽시계"
            },
            {
                "en": "breakfast",
                "kr": "아침식사"
            },
            {
                "en": "lunch",
                "kr": "점심식사"
            },
            {
                "en": "dinner",
                "kr": "저녁식사"
            },
            {
                "en": "bedtime",
                "kr": "취침시간"
            },
            {
                "en": "school",
                "kr": "학교"
            },
            {
                "en": "late",
                "kr": "늦은"
            }
        ],
        "sentences": [
            {
                "en": "What time is it now?",
                "kr": "지금 몇 시니?"
            },
            {
                "en": "It's seven o'clock.",
                "kr": "7시 정각이야."
            },
            {
                "en": "It's time for lunch.",
                "kr": "점심 먹을 시간이야."
            },
            {
                "en": "It's time for bed.",
                "kr": "잠잘 시간이야."
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
                "en": "tennis",
                "kr": "테니스"
            },
            {
                "en": "play",
                "kr": "놀다, 운동하다"
            },
            {
                "en": "sound",
                "kr": "들리다"
            },
            {
                "en": "fun",
                "kr": "재미있는"
            },
            {
                "en": "sorry",
                "kr": "미안한"
            },
            {
                "en": "busy",
                "kr": "바쁜"
            },
            {
                "en": "together",
                "kr": "함께"
            },
            {
                "en": "team",
                "kr": "팀"
            }
        ],
        "sentences": [
            {
                "en": "Let's play soccer together.",
                "kr": "같이 축구하자."
            },
            {
                "en": "Sounds great!",
                "kr": "좋은 생각이야!"
            },
            {
                "en": "Let's play badminton.",
                "kr": "배드민턴 치자."
            },
            {
                "en": "Sorry, I can't. I'm busy.",
                "kr": "미안하지만 안 돼. 나 바빠."
            }
        ]
    },
    {
        "code": "4G_L05",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 05 - Where Is My Bag?",
        "words": [
            {
                "en": "bag",
                "kr": "가방"
            },
            {
                "en": "cap",
                "kr": "모자"
            },
            {
                "en": "shoes",
                "kr": "신발"
            },
            {
                "en": "glasses",
                "kr": "안경"
            },
            {
                "en": "next to",
                "kr": "~옆에"
            },
            {
                "en": "behind",
                "kr": "~뒤에"
            },
            {
                "en": "front",
                "kr": "앞"
            },
            {
                "en": "under",
                "kr": "~아래에"
            },
            {
                "en": "closet",
                "kr": "옷장"
            },
            {
                "en": "sofa",
                "kr": "소파"
            },
            {
                "en": "bed",
                "kr": "침대"
            },
            {
                "en": "desk",
                "kr": "책상"
            }
        ],
        "sentences": [
            {
                "en": "Where is my bag?",
                "kr": "내 가방 어디 있니?"
            },
            {
                "en": "It's under the desk.",
                "kr": "책상 아래에 있어."
            },
            {
                "en": "Where are my shoes?",
                "kr": "내 신발 어디 있니?"
            },
            {
                "en": "They are next to the door.",
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
                "kr": "하는 중인"
            },
            {
                "en": "cooking",
                "kr": "요리하는"
            },
            {
                "en": "reading",
                "kr": "읽는"
            },
            {
                "en": "drawing",
                "kr": "그리는"
            },
            {
                "en": "watching",
                "kr": "보는"
            },
            {
                "en": "sleeping",
                "kr": "자는"
            },
            {
                "en": "studying",
                "kr": "공부하는"
            },
            {
                "en": "playing",
                "kr": "노는"
            },
            {
                "en": "eating",
                "kr": "먹는"
            },
            {
                "en": "helping",
                "kr": "돕는"
            },
            {
                "en": "cleaning",
                "kr": "청소하는"
            },
            {
                "en": "washing",
                "kr": "씻는"
            }
        ],
        "sentences": [
            {
                "en": "What are you doing?",
                "kr": "너 지금 뭐 하니?"
            },
            {
                "en": "I'm reading a book.",
                "kr": "책을 읽고 있어."
            },
            {
                "en": "What is he doing?",
                "kr": "그는 뭐 하고 있니?"
            },
            {
                "en": "He is drawing a picture.",
                "kr": "그림을 그리고 있어."
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
                "en": "umbrella",
                "kr": "우산"
            },
            {
                "en": "glove",
                "kr": "장갑"
            },
            {
                "en": "coat",
                "kr": "외투, 코트"
            },
            {
                "en": "shirt",
                "kr": "셔츠"
            },
            {
                "en": "skirt",
                "kr": "치마"
            },
            {
                "en": "dress",
                "kr": "원피스"
            },
            {
                "en": "boots",
                "kr": "부츠"
            },
            {
                "en": "your",
                "kr": "너의"
            },
            {
                "en": "mine",
                "kr": "나의 것"
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
                "en": "Is this your jacket?",
                "kr": "이게 네 재킷이니?"
            },
            {
                "en": "Yes, it's mine.",
                "kr": "응, 내 거야."
            },
            {
                "en": "Whose umbrella is this?",
                "kr": "이건 누구 우산이니?"
            },
            {
                "en": "It's Minsoo's umbrella.",
                "kr": "민수 우산이야."
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
                "en": "sandwich",
                "kr": "샌드위치"
            },
            {
                "en": "noodle",
                "kr": "국수, 면"
            },
            {
                "en": "fruit",
                "kr": "과일"
            },
            {
                "en": "hungry",
                "kr": "배고픈"
            },
            {
                "en": "thirsty",
                "kr": "목마른"
            },
            {
                "en": "want",
                "kr": "원하다"
            },
            {
                "en": "full",
                "kr": "배부른"
            }
        ],
        "sentences": [
            {
                "en": "Do you want some chicken?",
                "kr": "치킨 좀 먹을래?"
            },
            {
                "en": "Yes, please.",
                "kr": "네, 주세요."
            },
            {
                "en": "No, thanks. I'm full.",
                "kr": "아니요, 괜찮아요. 배불러요."
            },
            {
                "en": "Help yourself!",
                "kr": "마음껏 드세요!"
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
                "en": "today",
                "kr": "오늘"
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
                "en": "class",
                "kr": "수업"
            },
            {
                "en": "day",
                "kr": "요일, 날"
            }
        ],
        "sentences": [
            {
                "en": "What day is it today?",
                "kr": "오늘 무슨 요일이니?"
            },
            {
                "en": "It's Friday.",
                "kr": "금요일이야."
            },
            {
                "en": "We have English class on Monday.",
                "kr": "우리는 월요일에 영어 수업이 있어."
            },
            {
                "en": "Have a great weekend!",
                "kr": "즐거운 주말 보내!"
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
                "en": "how much",
                "kr": "얼마"
            },
            {
                "en": "dollar",
                "kr": "달러"
            },
            {
                "en": "won",
                "kr": "원"
            },
            {
                "en": "buy",
                "kr": "사다"
            },
            {
                "en": "sell",
                "kr": "팔다"
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
                "en": "money",
                "kr": "돈"
            },
            {
                "en": "price",
                "kr": "가격"
            },
            {
                "en": "store",
                "kr": "가게, 상점"
            },
            {
                "en": "clerk",
                "kr": "점원"
            },
            {
                "en": "present",
                "kr": "선물"
            }
        ],
        "sentences": [
            {
                "en": "May I help you?",
                "kr": "도와드릴까요?"
            },
            {
                "en": "How much is this cap?",
                "kr": "이 모자 얼마인가요?"
            },
            {
                "en": "It's five dollars.",
                "kr": "5달러입니다."
            },
            {
                "en": "I'll take it.",
                "kr": "그것으로 살게요."
            }
        ]
    },
    {
        "code": "4G_L11",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 11 - I Have a Cold",
        "words": [
            {
                "en": "cold",
                "kr": "감기"
            },
            {
                "en": "fever",
                "kr": "열"
            },
            {
                "en": "headache",
                "kr": "두통"
            },
            {
                "en": "toothache",
                "kr": "치통"
            },
            {
                "en": "stomachache",
                "kr": "복통"
            },
            {
                "en": "medicine",
                "kr": "약"
            },
            {
                "en": "doctor",
                "kr": "의사"
            },
            {
                "en": "hospital",
                "kr": "병원"
            },
            {
                "en": "rest",
                "kr": "휴식, 쉬다"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            },
            {
                "en": "matter",
                "kr": "문제, 일"
            },
            {
                "en": "healthy",
                "kr": "건강한"
            }
        ],
        "sentences": [
            {
                "en": "What's the matter?",
                "kr": "무슨 일 있니?"
            },
            {
                "en": "I have a cold and a fever.",
                "kr": "감기 걸리고 열이 나요."
            },
            {
                "en": "You should see a doctor.",
                "kr": "의사 선생님 진료를 받아야 해."
            },
            {
                "en": "Take this medicine and get some rest.",
                "kr": "이 약을 먹고 좀 쉬렴."
            }
        ]
    },
    {
        "code": "4G_L12",
        "grade": 4,
        "publisher": "YBM (최희경)",
        "name": "[YBM (최희경)] 4학년 Lesson 12 - How Was Your Vacation?",
        "words": [
            {
                "en": "vacation",
                "kr": "방학"
            },
            {
                "en": "went",
                "kr": "갔다"
            },
            {
                "en": "visited",
                "kr": "방문했다"
            },
            {
                "en": "played",
                "kr": "놀았다"
            },
            {
                "en": "watched",
                "kr": "봤다"
            },
            {
                "en": "had",
                "kr": "가졌다"
            },
            {
                "en": "beach",
                "kr": "해변"
            },
            {
                "en": "trip",
                "kr": "여행"
            },
            {
                "en": "grandparents",
                "kr": "조부모님"
            },
            {
                "en": "exciting",
                "kr": "신나는"
            },
            {
                "en": "wonderful",
                "kr": "멋진"
            },
            {
                "en": "memory",
                "kr": "추억"
            }
        ],
        "sentences": [
            {
                "en": "How was your vacation?",
                "kr": "방학 어땠니?"
            },
            {
                "en": "It was wonderful!",
                "kr": "정말 멋졌어!"
            },
            {
                "en": "I went to the beach with my family.",
                "kr": "가족과 해변에 갔어."
            },
            {
                "en": "I visited my grandparents.",
                "kr": "조부모님 댁을 방문했어."
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
                "kr": "대한민국"
            },
            {
                "en": "Canada",
                "kr": "캐나다"
            },
            {
                "en": "the U.K.",
                "kr": "영국"
            },
            {
                "en": "the U.S.A.",
                "kr": "미국"
            },
            {
                "en": "Australia",
                "kr": "호주"
            },
            {
                "en": "France",
                "kr": "프랑스"
            },
            {
                "en": "China",
                "kr": "중국"
            },
            {
                "en": "Vietnam",
                "kr": "베트남"
            },
            {
                "en": "from",
                "kr": "~출신의"
            },
            {
                "en": "world",
                "kr": "세계"
            },
            {
                "en": "welcome",
                "kr": "환영하다"
            }
        ],
        "sentences": [
            {
                "en": "Where are you from?",
                "kr": "어디 출신이니?"
            },
            {
                "en": "I'm from Korea.",
                "kr": "나는 한국 출신이야."
            },
            {
                "en": "I'm from Canada.",
                "kr": "나는 캐나다에서 왔어."
            },
            {
                "en": "Welcome to our school!",
                "kr": "우리 학교에 온 걸 환영해!"
            }
        ]
    },
    {
        "code": "5G_L02",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 02 - I'd Like a Sandwich",
        "words": [
            {
                "en": "sandwich",
                "kr": "샌드위치"
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
                "en": "pizza",
                "kr": "피자"
            },
            {
                "en": "burger",
                "kr": "버거"
            },
            {
                "en": "order",
                "kr": "주문"
            },
            {
                "en": "menu",
                "kr": "메뉴"
            },
            {
                "en": "like",
                "kr": "원하다"
            },
            {
                "en": "eat",
                "kr": "먹다"
            },
            {
                "en": "drink",
                "kr": "마시다"
            },
            {
                "en": "dessert",
                "kr": "디저트"
            },
            {
                "en": "delicious",
                "kr": "맛있는"
            }
        ],
        "sentences": [
            {
                "en": "May I take your order?",
                "kr": "주문하시겠어요?"
            },
            {
                "en": "I'd like a sandwich, please.",
                "kr": "샌드위치 하나 주세요."
            },
            {
                "en": "What would you like to drink?",
                "kr": "음료는 무엇으로 하시겠어요?"
            },
            {
                "en": "Orange juice, please.",
                "kr": "오렌지 주스로 주세요."
            }
        ]
    },
    {
        "code": "5G_L03",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 03 - May I Sit Here?",
        "words": [
            {
                "en": "sit",
                "kr": "앉다"
            },
            {
                "en": "use",
                "kr": "사용하다"
            },
            {
                "en": "take",
                "kr": "찍다, 가져가다"
            },
            {
                "en": "open",
                "kr": "열다"
            },
            {
                "en": "borrow",
                "kr": "빌리다"
            },
            {
                "en": "bring",
                "kr": "가져오다"
            },
            {
                "en": "wash",
                "kr": "씻다"
            },
            {
                "en": "may",
                "kr": "~해도 되다"
            },
            {
                "en": "course",
                "kr": "물론"
            },
            {
                "en": "problem",
                "kr": "문제"
            },
            {
                "en": "go ahead",
                "kr": "어서 하렴"
            },
            {
                "en": "here",
                "kr": "여기"
            }
        ],
        "sentences": [
            {
                "en": "May I sit here?",
                "kr": "여기 앉아도 될까요?"
            },
            {
                "en": "Yes, you may.",
                "kr": "네, 앉으셔도 됩니다."
            },
            {
                "en": "May I borrow your pencil?",
                "kr": "연필 좀 빌려도 될까요?"
            },
            {
                "en": "Sure, go ahead.",
                "kr": "물론이지, 쓰렴."
            }
        ]
    },
    {
        "code": "5G_L04",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 04 - Whose Sock Is This?",
        "words": [
            {
                "en": "whose",
                "kr": "누구의"
            },
            {
                "en": "sock",
                "kr": "양말"
            },
            {
                "en": "glove",
                "kr": "장갑"
            },
            {
                "en": "watch",
                "kr": "손목시계"
            },
            {
                "en": "towel",
                "kr": "수건"
            },
            {
                "en": "brush",
                "kr": "솔, 빗"
            },
            {
                "en": "cap",
                "kr": "모자"
            },
            {
                "en": "umbrella",
                "kr": "우산"
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
                "en": "Whose sock is this?",
                "kr": "이 양말은 누구 것이니?"
            },
            {
                "en": "It's mine.",
                "kr": "내 거야."
            },
            {
                "en": "Whose cap is this?",
                "kr": "이 모자는 누구 것이니?"
            },
            {
                "en": "It's Tony's.",
                "kr": "토니 거야."
            }
        ]
    },
    {
        "code": "5G_L05",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 05 - What's Your Favorite Subject?",
        "words": [
            {
                "en": "subject",
                "kr": "과목"
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
                "en": "music",
                "kr": "음악"
            },
            {
                "en": "art",
                "kr": "미술"
            },
            {
                "en": "English",
                "kr": "영어"
            },
            {
                "en": "PE",
                "kr": "체육"
            },
            {
                "en": "Korean",
                "kr": "국어"
            },
            {
                "en": "history",
                "kr": "역사"
            },
            {
                "en": "why",
                "kr": "왜"
            },
            {
                "en": "because",
                "kr": "~때문에"
            },
            {
                "en": "interesting",
                "kr": "흥미로운"
            }
        ],
        "sentences": [
            {
                "en": "What's your favorite subject?",
                "kr": "가장 좋아하는 과목은 뭐니?"
            },
            {
                "en": "My favorite subject is science.",
                "kr": "가장 좋아하는 과목은 과학이야."
            },
            {
                "en": "Why do you like it?",
                "kr": "왜 그 과목을 좋아하니?"
            },
            {
                "en": "Because it is fun and exciting.",
                "kr": "재미있고 신나기 때문이야."
            }
        ]
    },
    {
        "code": "5G_L06",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 06 - Where Is the Ice Cream Store?",
        "words": [
            {
                "en": "bank",
                "kr": "은행"
            },
            {
                "en": "bakery",
                "kr": "빵집"
            },
            {
                "en": "bookstore",
                "kr": "서점"
            },
            {
                "en": "museum",
                "kr": "박물관"
            },
            {
                "en": "post office",
                "kr": "우체국"
            },
            {
                "en": "straight",
                "kr": "곧장, 똑바로"
            },
            {
                "en": "turn",
                "kr": "돌다"
            },
            {
                "en": "block",
                "kr": "블록, 구역"
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
                "en": "next to",
                "kr": "~옆에"
            },
            {
                "en": "between",
                "kr": "~사이에"
            }
        ],
        "sentences": [
            {
                "en": "Where is the bakery?",
                "kr": "빵집이 어디에 있나요?"
            },
            {
                "en": "Go straight one block and turn left.",
                "kr": "한 블록 곧장 가서 왼쪽으로 도세요."
            },
            {
                "en": "It's on your right.",
                "kr": "오른쪽에 있습니다."
            },
            {
                "en": "Thank you so much!",
                "kr": "정말 감사합니다!"
            }
        ]
    },
    {
        "code": "5G_L07",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 07 - I Visited My Uncle",
        "words": [
            {
                "en": "visited",
                "kr": "방문했다"
            },
            {
                "en": "went",
                "kr": "갔다"
            },
            {
                "en": "ate",
                "kr": "먹었다"
            },
            {
                "en": "saw",
                "kr": "보았다"
            },
            {
                "en": "rode",
                "kr": "탔다"
            },
            {
                "en": "swam",
                "kr": "수영했다"
            },
            {
                "en": "made",
                "kr": "만들었다"
            },
            {
                "en": "enjoyed",
                "kr": "즐겼다"
            },
            {
                "en": "uncle",
                "kr": "삼촌, 이모부"
            },
            {
                "en": "island",
                "kr": "섬"
            },
            {
                "en": "farm",
                "kr": "농장"
            },
            {
                "en": "delicious",
                "kr": "맛있는"
            }
        ],
        "sentences": [
            {
                "en": "What did you do on vacation?",
                "kr": "방학 때 무엇을 했니?"
            },
            {
                "en": "I visited my uncle in Jeju-do.",
                "kr": "제주도 삼촌 댁을 방문했어."
            },
            {
                "en": "I swam in the cool sea.",
                "kr": "시원한 바다에서 수영했어."
            },
            {
                "en": "We had a wonderful time.",
                "kr": "우리는 멋진 시간을 보냈어."
            }
        ]
    },
    {
        "code": "5G_L08",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 08 - I'm Taller Than You",
        "words": [
            {
                "en": "taller",
                "kr": "더 키가 큰"
            },
            {
                "en": "shorter",
                "kr": "더 키가 작은"
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
                "en": "bigger",
                "kr": "더 큰"
            },
            {
                "en": "smaller",
                "kr": "더 작은"
            },
            {
                "en": "heavier",
                "kr": "더 무거운"
            },
            {
                "en": "lighter",
                "kr": "더 가벼운"
            },
            {
                "en": "than",
                "kr": "~보다"
            },
            {
                "en": "cheetah",
                "kr": "치타"
            },
            {
                "en": "giraffe",
                "kr": "기린"
            },
            {
                "en": "compare",
                "kr": "비교하다"
            }
        ],
        "sentences": [
            {
                "en": "I'm taller than you.",
                "kr": "내가 너보다 키가 커."
            },
            {
                "en": "A cheetah is faster than a lion.",
                "kr": "치타는 사자보다 빨라."
            },
            {
                "en": "Which one is bigger?",
                "kr": "어느 것이 더 크니?"
            },
            {
                "en": "This box is heavier than that one.",
                "kr": "이 상자가 저것보다 더 무거워."
            }
        ]
    },
    {
        "code": "5G_L09",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 09 - What Do You Think?",
        "words": [
            {
                "en": "think",
                "kr": "생각하다"
            },
            {
                "en": "opinion",
                "kr": "의견"
            },
            {
                "en": "agree",
                "kr": "동의하다"
            },
            {
                "en": "disagree",
                "kr": "동의하지 않다"
            },
            {
                "en": "idea",
                "kr": "생각, 아이디어"
            },
            {
                "en": "wonderful",
                "kr": "멋진"
            },
            {
                "en": "difficult",
                "kr": "어려운"
            },
            {
                "en": "easy",
                "kr": "쉬운"
            },
            {
                "en": "boring",
                "kr": "지루한"
            },
            {
                "en": "important",
                "kr": "중요한"
            },
            {
                "en": "share",
                "kr": "나누다"
            },
            {
                "en": "together",
                "kr": "함께"
            }
        ],
        "sentences": [
            {
                "en": "What do you think about this?",
                "kr": "이것에 대해 어떻게 생각하니?"
            },
            {
                "en": "I think it's a great idea.",
                "kr": "아주 좋은 생각이라고 생각해."
            },
            {
                "en": "I agree with you.",
                "kr": "너에게 동의해."
            },
            {
                "en": "I have a different opinion.",
                "kr": "나는 다른 의견이 있어."
            }
        ]
    },
    {
        "code": "5G_L10",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 10 - Who Wrote the Book?",
        "words": [
            {
                "en": "wrote",
                "kr": "썼다"
            },
            {
                "en": "painted",
                "kr": "그렸다"
            },
            {
                "en": "invented",
                "kr": "발명했다"
            },
            {
                "en": "built",
                "kr": "지었다"
            },
            {
                "en": "discovered",
                "kr": "발견했다"
            },
            {
                "en": "author",
                "kr": "작가"
            },
            {
                "en": "scientist",
                "kr": "과학자"
            },
            {
                "en": "king",
                "kr": "왕"
            },
            {
                "en": "book",
                "kr": "책"
            },
            {
                "en": "painting",
                "kr": "그림"
            },
            {
                "en": "airplane",
                "kr": "비행기"
            },
            {
                "en": "history",
                "kr": "역사"
            }
        ],
        "sentences": [
            {
                "en": "Who wrote this book?",
                "kr": "이 책은 누가 썼니?"
            },
            {
                "en": "King Sejong created Hangeul.",
                "kr": "세종대왕께서 한글을 창제하셨어."
            },
            {
                "en": "Who painted the Mona Lisa?",
                "kr": "모나리자는 누가 그렸니?"
            },
            {
                "en": "Leonardo da Vinci painted it.",
                "kr": "레오나르도 다빈치가 그렸어."
            }
        ]
    },
    {
        "code": "5G_L11",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 11 - We Should Save the Earth",
        "words": [
            {
                "en": "earth",
                "kr": "지구"
            },
            {
                "en": "protect",
                "kr": "보호하다"
            },
            {
                "en": "recycle",
                "kr": "재활용하다"
            },
            {
                "en": "save",
                "kr": "절약하다, 구하다"
            },
            {
                "en": "turn off",
                "kr": "끄다"
            },
            {
                "en": "waste",
                "kr": "낭비하다"
            },
            {
                "en": "plastic",
                "kr": "플라스틱"
            },
            {
                "en": "energy",
                "kr": "에너지"
            },
            {
                "en": "nature",
                "kr": "자연"
            },
            {
                "en": "should",
                "kr": "~해야 한다"
            },
            {
                "en": "planet",
                "kr": "행성, 지구"
            },
            {
                "en": "clean",
                "kr": "깨끗한, 청소하다"
            }
        ],
        "sentences": [
            {
                "en": "We should save the Earth.",
                "kr": "우리는 지구를 아껴야 해."
            },
            {
                "en": "Turn off the lights, please.",
                "kr": "전등을 꺼주세요."
            },
            {
                "en": "Don't waste water.",
                "kr": "물을 낭비하지 마세요."
            },
            {
                "en": "Let's recycle plastic bottles.",
                "kr": "플라스틱 병을 재활용하자."
            }
        ]
    },
    {
        "code": "5G_L12",
        "grade": 5,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 5학년 Lesson 12 - What Are You Going to Do This Weekend?",
        "words": [
            {
                "en": "going to",
                "kr": "~할 예정이다"
            },
            {
                "en": "plan",
                "kr": "계획"
            },
            {
                "en": "weekend",
                "kr": "주말"
            },
            {
                "en": "camping",
                "kr": "캠핑"
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
                "en": "library",
                "kr": "도서관"
            },
            {
                "en": "picnic",
                "kr": "소풍"
            },
            {
                "en": "tomorrow",
                "kr": "내일"
            },
            {
                "en": "afternoon",
                "kr": "오후"
            },
            {
                "en": "stay",
                "kr": "머무르다"
            },
            {
                "en": "invite",
                "kr": "초대하다"
            }
        ],
        "sentences": [
            {
                "en": "What are you going to do this weekend?",
                "kr": "이번 주말에 무엇을 할 예정이니?"
            },
            {
                "en": "I'm going to go camping with my family.",
                "kr": "가족과 함께 캠핑을 갈 거야."
            },
            {
                "en": "Are you going to visit your grandmother?",
                "kr": "할머니 댁을 방문할 예정이니?"
            },
            {
                "en": "Yes, I am. I can't wait!",
                "kr": "응, 그럴 거야. 정말 기대돼!"
            }
        ]
    },
    {
        "code": "6G_L01",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 01 - What grade are you in?",
        "words": [
            {
                "en": "first",
                "kr": "첫 번째의"
            },
            {
                "en": "second",
                "kr": "두 번째의"
            },
            {
                "en": "third",
                "kr": "세 번째의"
            },
            {
                "en": "fourth",
                "kr": "네 번째의"
            },
            {
                "en": "fifth",
                "kr": "다섯 번째의"
            },
            {
                "en": "sixth",
                "kr": "여섯 번째의"
            },
            {
                "en": "grade",
                "kr": "학년"
            },
            {
                "en": "spell",
                "kr": "철자를 쓰다"
            },
            {
                "en": "join",
                "kr": "가입하다"
            },
            {
                "en": "club",
                "kr": "동아리"
            },
            {
                "en": "dance",
                "kr": "춤추다, 춤"
            },
            {
                "en": "draw",
                "kr": "그리다"
            },
            {
                "en": "play",
                "kr": "놀다, 연주하다"
            },
            {
                "en": "listen",
                "kr": "듣다"
            },
            {
                "en": "read",
                "kr": "읽다"
            },
            {
                "en": "ride",
                "kr": "(자전거 등을) 타다"
            }
        ],
        "sentences": [
            {
                "en": "What grade are you in?",
                "kr": "몇 학년이니?"
            },
            {
                "en": "I'm in the sixth grade.",
                "kr": "나는 6학년이야."
            },
            {
                "en": "Can you spell your name?",
                "kr": "이름의 철자를 말해줄 수 있니?"
            },
            {
                "en": "Do you want to join our dance club?",
                "kr": "우리 댄스 동아리에 가입할래?"
            }
        ]
    },
    {
        "code": "6G_L02",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 02 - What season do you like?",
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
                "en": "beautiful",
                "kr": "아름다운"
            },
            {
                "en": "flower",
                "kr": "꽃"
            },
            {
                "en": "cold",
                "kr": "추운"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            },
            {
                "en": "delicious",
                "kr": "맛있는"
            },
            {
                "en": "also",
                "kr": "또한"
            },
            {
                "en": "colorful",
                "kr": "다채로운"
            },
            {
                "en": "swim",
                "kr": "수영하다"
            },
            {
                "en": "field trip",
                "kr": "현장학습"
            },
            {
                "en": "clear",
                "kr": "맑은"
            },
            {
                "en": "bright",
                "kr": "밝은"
            }
        ],
        "sentences": [
            {
                "en": "What season do you like?",
                "kr": "어떤 계절을 좋아하니?"
            },
            {
                "en": "I like summer because I can swim.",
                "kr": "수영할 수 있어서 여름을 좋아해."
            },
            {
                "en": "How is the weather in winter?",
                "kr": "겨울 날씨는 어떠니?"
            },
            {
                "en": "It is very cold and snowy.",
                "kr": "매우 춥고 눈이 와."
            }
        ]
    },
    {
        "code": "6G_L03",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 03 - When is your birthday?",
        "words": [
            {
                "en": "campaign",
                "kr": "홍보활동"
            },
            {
                "en": "earth",
                "kr": "지구"
            },
            {
                "en": "paper",
                "kr": "종이"
            },
            {
                "en": "poster",
                "kr": "포스터"
            },
            {
                "en": "save",
                "kr": "구하다"
            },
            {
                "en": "shirt",
                "kr": "셔츠"
            },
            {
                "en": "turn off",
                "kr": "끄다"
            },
            {
                "en": "when",
                "kr": "언제"
            },
            {
                "en": "world",
                "kr": "세계"
            }
        ],
        "sentences": [
            {
                "en": "When is your birthday?",
                "kr": "네 생일이 언제니?"
            },
            {
                "en": "My birthday is May fifth.",
                "kr": "내 생일은 5월 5일이야."
            },
            {
                "en": "We should turn off the lights.",
                "kr": "우리는 전등을 꺼야 해."
            },
            {
                "en": "Let's protect our Earth.",
                "kr": "우리의 지구를 보호하자."
            }
        ]
    },
    {
        "code": "6G_L04",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 04 - Why are you happy?",
        "words": [
            {
                "en": "because",
                "kr": "~ 때문에"
            },
            {
                "en": "belt",
                "kr": "띠"
            },
            {
                "en": "birdhouse",
                "kr": "새집, 새장"
            },
            {
                "en": "break",
                "kr": "부수다, 깨다"
            },
            {
                "en": "congratulations",
                "kr": "축하"
            },
            {
                "en": "test",
                "kr": "시험"
            },
            {
                "en": "why",
                "kr": "왜"
            },
            {
                "en": "worry",
                "kr": "걱정하다"
            },
            {
                "en": "wrong",
                "kr": "나쁜, 틀린"
            }
        ],
        "sentences": [
            {
                "en": "Why are you happy?",
                "kr": "왜 행복하니?"
            },
            {
                "en": "Because I got a black belt.",
                "kr": "태권도 검은 띠를 땄기 때문이야."
            },
            {
                "en": "I passed the English test.",
                "kr": "나 영어 시험에 통과했어."
            },
            {
                "en": "Congratulations on your prize.",
                "kr": "상 받은 것을 축하해."
            }
        ]
    },
    {
        "code": "6G_L05",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 05 - Where is the post office?",
        "words": [
            {
                "en": "bank",
                "kr": "은행"
            },
            {
                "en": "block",
                "kr": "구역"
            },
            {
                "en": "hospital",
                "kr": "병원"
            },
            {
                "en": "hungry",
                "kr": "배고픈"
            },
            {
                "en": "restaurant",
                "kr": "식당"
            },
            {
                "en": "left",
                "kr": "왼쪽"
            },
            {
                "en": "restroom",
                "kr": "화장실"
            },
            {
                "en": "stop",
                "kr": "정류장"
            },
            {
                "en": "store",
                "kr": "가게"
            },
            {
                "en": "straight",
                "kr": "똑바로, 일직선으로"
            },
            {
                "en": "town",
                "kr": "마을"
            }
        ],
        "sentences": [
            {
                "en": "Where is the post office?",
                "kr": "우체국이 어디에 있나요?"
            },
            {
                "en": "Go straight and turn left.",
                "kr": "직진해서 왼쪽으로 도세요."
            },
            {
                "en": "It is on your right.",
                "kr": "그것은 당신의 오른쪽에 있어요."
            },
            {
                "en": "It is next to the bank.",
                "kr": "은행 옆에 있어요."
            }
        ]
    },
    {
        "code": "6G_L06",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 06 - What does he look like?",
        "words": [
            {
                "en": "aunt",
                "kr": "고모"
            },
            {
                "en": "daughter",
                "kr": "딸"
            },
            {
                "en": "dress",
                "kr": "드레스"
            },
            {
                "en": "eye",
                "kr": "눈"
            },
            {
                "en": "hair",
                "kr": "머리카락"
            },
            {
                "en": "long",
                "kr": "긴"
            },
            {
                "en": "of",
                "kr": "…의"
            },
            {
                "en": "same",
                "kr": "같은"
            },
            {
                "en": "sand",
                "kr": "모래"
            },
            {
                "en": "short",
                "kr": "짧은"
            }
        ],
        "sentences": [
            {
                "en": "What does he look like?",
                "kr": "그는 어떻게 생겼니?"
            },
            {
                "en": "He has short curly hair.",
                "kr": "그는 짧은 곱슬머리를 가지고 있어."
            },
            {
                "en": "She is wearing glasses.",
                "kr": "그녀는 안경을 쓰고 있어."
            },
            {
                "en": "He is tall and strong.",
                "kr": "그는 키가 크고 힘이 세."
            }
        ]
    },
    {
        "code": "6G_L07",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 07 - Can you come to my party?",
        "words": [
            {
                "en": "after",
                "kr": "~후에"
            },
            {
                "en": "biscuit",
                "kr": "비스킷"
            },
            {
                "en": "laser",
                "kr": "레이저"
            },
            {
                "en": "light",
                "kr": "빛"
            },
            {
                "en": "number",
                "kr": "숫자"
            },
            {
                "en": "soft drink",
                "kr": "청량음료"
            },
            {
                "en": "tell",
                "kr": "말하다"
            }
        ],
        "sentences": [
            {
                "en": "Can you come to my party?",
                "kr": "내 파티에 올 수 있니?"
            },
            {
                "en": "Sure, I'd love to.",
                "kr": "물론이지, 정말 가고 싶어."
            },
            {
                "en": "Sorry, but I can't.",
                "kr": "미안하지만 갈 수 없어."
            },
            {
                "en": "I have a piano lesson.",
                "kr": "피아노 레슨이 있어."
            }
        ]
    },
    {
        "code": "6G_L08",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 08 - I'm stronger than you.",
        "words": [
            {
                "en": "always",
                "kr": "항상, 언제나"
            },
            {
                "en": "baby",
                "kr": "아기"
            },
            {
                "en": "catch",
                "kr": "잡다"
            },
            {
                "en": "clock",
                "kr": "시계"
            },
            {
                "en": "dribble",
                "kr": "공을 몰다"
            },
            {
                "en": "fast",
                "kr": "빠른"
            },
            {
                "en": "giraffe",
                "kr": "기린"
            },
            {
                "en": "give",
                "kr": "주다"
            },
            {
                "en": "goal",
                "kr": "득점"
            },
            {
                "en": "heavy",
                "kr": "무거운"
            },
            {
                "en": "race",
                "kr": "경주"
            },
            {
                "en": "side",
                "kr": "쪽"
            },
            {
                "en": "strong",
                "kr": "힘 센"
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
                "en": "win",
                "kr": "이기다"
            }
        ],
        "sentences": [
            {
                "en": "Who is faster, Tom or Jerry?",
                "kr": "톰과 제리 중 누가 더 빠르니?"
            },
            {
                "en": "Tom is faster than Jerry.",
                "kr": "톰이 제리보다 더 빨라."
            },
            {
                "en": "The giraffe is taller than the zebra.",
                "kr": "기린이 얼룩말보다 더 커."
            },
            {
                "en": "This box is heavier than that one.",
                "kr": "이 상자가 저 상자보다 더 무거워."
            }
        ]
    },
    {
        "code": "6G_L09",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 09 - What will you do this weekend?",
        "words": [
            {
                "en": "any",
                "kr": "어떤"
            },
            {
                "en": "children",
                "kr": "아이들"
            },
            {
                "en": "forest",
                "kr": "숲"
            },
            {
                "en": "hamburger",
                "kr": "햄버거"
            },
            {
                "en": "plan",
                "kr": "계획"
            },
            {
                "en": "program",
                "kr": "프로그램"
            },
            {
                "en": "room",
                "kr": "방"
            },
            {
                "en": "stay",
                "kr": "머무르다"
            },
            {
                "en": "tent",
                "kr": "텐트"
            },
            {
                "en": "village",
                "kr": "마을"
            },
            {
                "en": "zoo",
                "kr": "동물원"
            }
        ],
        "sentences": [
            {
                "en": "What are you going to do this vacation?",
                "kr": "이번 방학에 무엇을 할 예정이니?"
            },
            {
                "en": "I'm going to visit Jeju-do.",
                "kr": "제주도에 방문할 예정이야."
            },
            {
                "en": "I'm going to camp with my family.",
                "kr": "가족과 캠핑을 갈 거야."
            },
            {
                "en": "That sounds very exciting!",
                "kr": "정말 신나겠다!"
            }
        ]
    },
    {
        "code": "6G_L10",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 10 - How often do you exercise?",
        "words": [
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
                "en": "dollar",
                "kr": "달러"
            },
            {
                "en": "cent",
                "kr": "센트"
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
                "en": "sell",
                "kr": "팔다"
            },
            {
                "en": "money",
                "kr": "돈"
            },
            {
                "en": "market",
                "kr": "시장"
            },
            {
                "en": "present",
                "kr": "선물"
            }
        ],
        "sentences": [
            {
                "en": "How much is this shirt?",
                "kr": "이 셔츠 얼마인가요?"
            },
            {
                "en": "It is ten dollars.",
                "kr": "10달러입니다."
            },
            {
                "en": "It is too expensive.",
                "kr": "너무 비싸요."
            },
            {
                "en": "Can you give me a discount?",
                "kr": "할인해 주실 수 있나요?"
            }
        ]
    },
    {
        "code": "6G_L11",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 11 - Do you know anything about jegichagi?",
        "words": [
            {
                "en": "headache",
                "kr": "두통, 머리 아픔"
            },
            {
                "en": "stomachache",
                "kr": "복통, 배 아픔"
            },
            {
                "en": "toothache",
                "kr": "치통, 이 아픔"
            },
            {
                "en": "fever",
                "kr": "열"
            },
            {
                "en": "cold",
                "kr": "감기, 추운"
            },
            {
                "en": "matter",
                "kr": "문제, 일"
            },
            {
                "en": "doctor",
                "kr": "의사"
            },
            {
                "en": "hospital",
                "kr": "병원"
            },
            {
                "en": "medicine",
                "kr": "약"
            },
            {
                "en": "rest",
                "kr": "휴식, 쉬다"
            },
            {
                "en": "warm",
                "kr": "따뜻한"
            },
            {
                "en": "healthy",
                "kr": "건강한"
            }
        ],
        "sentences": [
            {
                "en": "What's the matter?",
                "kr": "무슨 일이니? 어디 아프니?"
            },
            {
                "en": "I have a terrible headache.",
                "kr": "두통이 너무 심해요."
            },
            {
                "en": "Take some medicine and rest.",
                "kr": "약 좀 먹고 쉬어."
            },
            {
                "en": "You should see a doctor.",
                "kr": "병원에 가보는 게 좋겠어."
            }
        ]
    },
    {
        "code": "6G_L12",
        "grade": 6,
        "publisher": "천재 (함순애)",
        "name": "[천재 (함순애)] 6학년 Lesson 12 - Did you paint the wall?",
        "words": [
            {
                "en": "idea",
                "kr": "생각, 아이디어"
            },
            {
                "en": "together",
                "kr": "함께"
            },
            {
                "en": "park",
                "kr": "공원"
            },
            {
                "en": "movie",
                "kr": "영화"
            },
            {
                "en": "sound",
                "kr": "들리다"
            },
            {
                "en": "wonderful",
                "kr": "멋진"
            },
            {
                "en": "badminton",
                "kr": "배드민턴"
            },
            {
                "en": "afternoon",
                "kr": "오후"
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
                "en": "busy",
                "kr": "바쁜"
            },
            {
                "en": "finish",
                "kr": "끝내다"
            }
        ],
        "sentences": [
            {
                "en": "Did you paint the wall?",
                "kr": "벽에 페인트칠을 했니?"
            },
            {
                "en": "Yes, I painted it yesterday.",
                "kr": "응, 어제 칠했어."
            },
            {
                "en": "What did you do last weekend?",
                "kr": "지난 주말에 뭐 했니?"
            },
            {
                "en": "I watched a wonderful movie.",
                "kr": "멋진 영화를 봤어."
            }
        ]
    }
];

    const STORAGE_KEY = 'waurimal_custom_curriculum';
    const VERSION_KEY = 'waurimal_curriculum_version';

    function isStrictValidCurriculum(list) {
        if (!Array.isArray(list) || list.length !== 48) return false;
        const grades = [3, 4, 5, 6];
        const gradeCountsValid = grades.every(g => list.filter(l => Number(l.grade) === g).length === 12);
        if (!gradeCountsValid) return false;
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
