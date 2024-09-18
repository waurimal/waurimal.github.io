# 실습자 선정
name <- c("김남지","김아현","김예지","김현주","박준성","이유진","이주은","장현아","정재훈","조은혜")
sample(name,2,replace = F)



pkg_list <- c(
  "haven","tidyverse","modelsummary","lavaan",
  "lavaanPlot","knitr","epiDisplay","tinytex",
  "showtext","stargazer","tibble","magrittr",
  "semPlot","semTools"
)

lapply(pkg_list, require, character.only = T)

getwd()
# 파일 불러오기
e4_s <- read_sav("./DATA/1차년도_초등학교/1차년도_학생(초).sav")

## ID 변수 확인

names(e4_s)[1:5]
e4_s$HID[1:5]
e4_s$PID[1:5]

## ID 변수 만들기

e4_s$ID <- (e4_s$HID+10000)*10+e4_s$PID;e4_s$ID[1:3]
e4_p$ID <- (e4_p$HID+10000)*10+e4_p$PID;e4_p$ID[1:3]

## 3차년도 불참자 제외
  
e4_s <- e4_s %>% filter(SURVEY1w3==1); e4_s[1:4,]

## left_join()

e4_all <- left_join(e4_s,e4_p,by="ID")

## 특정값 결측치 만들기
  
e4_all$YINT1A00w3[e4_all$YINT1A00w3==6] <- NA
e4_all$YINT1B00w3[e4_all$YINT1B00w3==6] <- NA

## 케이스 결측치 확인

complete.cases(e4_all[,1:20])[1:50]


## 케이스 결측치 행 확인

which(!complete.cases(e4_all[,1:20]))

  
## 변수별 결측치 확인

is.na(e4_all)[1:5,11:15]
colSums(is.na(e4_all))[11:15]

## 결측치 비율 계산하기

e4_all_missing_ratio <- as.data.frame((colSums(is.na(e4_all))/length(e4_all$ID))*100)
e4_all_missing_ratio[1:50,]

## 결측치 제거하기
  
## 행 제거(list-wise deletion)

e4_all_nomiss <- na.omit(e4_all[,1:20]); glimpse(e4_all_nomiss)

## 역산문항 처리하기
  
## 자아존중감(2,5,6,8,9번)문항 / 4점 리커트 척도인 경우

e4_all <- e4_all %>% mutate(
  YPSY3A02w3R=5-YPSY3A02w3,
  YPSY3A05w3R=5-YPSY3A02w3,
  YPSY3A06w3R=5-YPSY3A02w3,
  YPSY3A08w3R=5-YPSY3A02w3,
  YPSY3A09w3R=5-YPSY3A02w3
)


## 더미변수 만들기
  
## 성별인 경우

e4_all <- e4_all %>% mutate(
  남자=ifelse(YGENDERw3==1,1,0)
)
e4_all$남자[1:10]

# 샘플크기

#Computation of minimum sample size for test of fit

rmsea0 <- 0.05 #null hypothesized RMSEA
rmseaa <- 0.08 #alternative hypothesized RMSEA
d <- 19 #degrees of freedom
alpha <- 0.05 #alpha level
desired <- 0.6 #desired power

#Code below need not be changed by user
#initialize values
pow <- 0.0
n <- 0
#begin loop for finding initial level of n
while (pow<desired) {
  n <- n+100
  ncp0 <- (n-1)*d*rmsea0^2
  ncpa <- (n-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
}

#begin loop for interval halving
foo <- -1
newn <- n
interval <- 200
powdiff <- pow - desired
while (powdiff>.001) {
  interval <- interval*.5
  newn <- newn + foo*interval*.5
  ncp0 <- (newn-1)*d*rmsea0^2
  ncpa <- (newn-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  powdiff <- abs(pow-desired)
  if (pow<desired) {
    foo <- 1
  }
  if (pow>desired) {
    foo <- -1
  }
}

minn <- newn
print(minn)



e4_s <- read_csv("./DATA/KCYPS2018e4Yw3.csv")

## 분석 관심 변수 추출 후 서브셋 데이터 만들기

e4_s_sub <- e4_s %>% 
  dplyr::select(c(ID,YGENDERw3,YEDU3A01w3:YEDU3A14w3,YINT2A01w3:YINT2A16w3,YINT1A00w3,YINT1B00w3))
e4_s_sub[1:2,]

e4_s_sub$YINT1A00w3[e4_s_sub$YINT1A00w3==6] <- NA
e4_s_sub$YINT1B00w3[e4_s_sub$YINT1B00w3==6] <- NA

## 결측치 확인 및 단일대체

colSums(is.na(e4_s_sub))

## 결측치 확인 및 단일대체

(colSums(is.na(e4_s_sub))/length(e4_s_sub$ID))*100


## 결측치 확인 및 단일대체
e4_s_sub_comp <- mice(e4_s_sub,seed=1234,m=1,method = "pmm") # Predictive Mean Matching

## 결측치 확인 및 단일대체
e4_s_sub <- complete(e4_s_sub_comp);colSums(is.na(e4_s_sub))

## 서브셋 데이터 파일로 저장하기

write.csv(e4_s_sub,"e4_s_sub.csv",row.names = F)

## 신뢰도 확인

### 교사 관계

# 접근가능성/민감성/신뢰성/수용성/전체
cronbach.te01 <- e4_s_sub %>% dplyr::select(YEDU3A01w3,YEDU3A11w3,YEDU3A12w3) %>% cronbach()
cronbach.te01$alpha
cronbach.te02 <- e4_s_sub %>% dplyr::select(YEDU3A02w3,YEDU3A03w3,YEDU3A05w3,YEDU3A13w3) %>% cronbach();cronbach.te02$alpha
cronbach.te03 <- e4_s_sub %>% dplyr::select(YEDU3A04w3,YEDU3A07w3,YEDU3A10w3,YEDU3A14w3) %>% cronbach();cronbach.te03$alpha
cronbach.te04 <- e4_s_sub %>% dplyr::select(YEDU3A06w3,YEDU3A08w3,YEDU3A09w3) %>% cronbach();cronbach.te04$alpha
cronbach.te <- e4_s_sub %>% dplyr::select(YEDU3A01w3:YEDU3A14w3) %>% cronbach();cronbach.te$alpha


### 학업 열의

# 헌신/활기/효능감/몰두/전체
cronbach.le01 <- e4_s_sub %>% dplyr::select(YINT2A01w3:YINT2A04w3) %>% cronbach();cronbach.le01$alpha
cronbach.le02 <- e4_s_sub %>% dplyr::select(YINT2A05w3:YINT2A08w3) %>% cronbach();cronbach.le02$alpha
cronbach.le03 <- e4_s_sub %>% dplyr::select(YINT2A09w3:YINT2A12w3) %>% cronbach();cronbach.le03$alpha
cronbach.le04 <- e4_s_sub %>% dplyr::select(YINT2A13w3:YINT2A16w3) %>% cronbach();cronbach.le04$alpha
cronbach.le <- e4_s_sub %>% dplyr::select(YINT2A01w3:YINT2A16w3) %>% cronbach();cronbach.le$alpha


### 학업성취도

cronbach.ach <- e4_s_sub %>% dplyr::select(YINT1A00w3,YINT1B00w3) %>% cronbach();cronbach.ach$alpha


### 신뢰도 표 만들어 파일로 저장하기

cronbach.all <- data.frame(cronbach.te01$alpha,
                           cronbach.te02$alpha,
                           cronbach.te03$alpha,
                           cronbach.te04$alpha,
                           cronbach.te$alpha,
                           cronbach.le01$alpha,
                           cronbach.le02$alpha,
                           cronbach.le03$alpha,
                           cronbach.le04$alpha,
                           cronbach.le$alpha,
                           cronbach.ach$alpha)
cronbach.all <- round(cronbach.all,3)
write.csv(cronbach.all,"cronbach.all.csv",row.names = F)
cronbach.all.t <- t(cronbach.all)

kable(cronbach.all.t,format = "markdown")
library(rrtable)
table2pptx("cronbach.all",echo=T,append=T)

## 변수 생성

e4_s_sub <- e4_s_sub %>% rowwise() %>% mutate(
  접근가능성 = mean(c(YEDU3A01w3,YEDU3A11w3,YEDU3A12w3)),
  민감성 = mean(c(YEDU3A02w3,YEDU3A03w3,YEDU3A05w3,YEDU3A13w3)),
  신뢰성 = mean(c(YEDU3A04w3,YEDU3A07w3,YEDU3A10w3,YEDU3A14w3)),
  수용성 = mean(c(YEDU3A06w3,YEDU3A08w3,YEDU3A09w3)),
  교사관계 = mean(c(접근가능성,민감성,신뢰성,수용성)),
  헌신 = mean(c(YINT2A01w3,YINT2A02w3,YINT2A03w3,YINT2A04w3)),
  활기 = mean(c(YINT2A05w3,YINT2A06w3,YINT2A07w3,YINT2A08w3)),
  효능감 = mean(c(YINT2A09w3,YINT2A10w3,YINT2A11w3,YINT2A12w3)),
  몰두 = mean(c(YINT2A13w3,YINT2A14w3,YINT2A15w3,YINT2A16w3)),
  학업열의 = mean(c(헌신,활기,효능감,몰두)),
  학업성취도 = mean(c(YINT1A00w3,YINT1B00w3))
)


## 서브셋 데이터 파일로 저장하기

write.csv(e4_s_sub,"e4_s_sub.csv",row.names = F)

## 변수 살펴보기(그래프)
par(mfrow=c(1,1))
Boxplot(e4_s_sub$접근가능성,id=list())

scatterplot(접근가능성~민감성,data=e4_s_sub,id=list())
scatterplot(접근가능성~신뢰성,data=e4_s_sub,id=list())
scatterplot(접근가능성~수용성,data=e4_s_sub,id=list())

## 다변량 정규성 검정

mvn.result1 <- mvn(data=e4_s_sub[,35:43],mvnTest="mardia",univariateTest="SW",univariatePlot="histogram",multivariatePlot="qq")
mvn.result1
mvn.result2 <- mvn(data=e4_s_sub[,35:43],mvnTest="hz",univariateTest="SW",univariatePlot="histogram",multivariatePlot="qq")
mvn.result2
mvn.result3 <- mvn(data=e4_s_sub[,35:43],mvnTest="dh",univariateTest="SW",univariatePlot="histogram",multivariatePlot="qq")
mvn.result3
mvn.result4 <- mvn(data=e4_s_sub[,35:43],mvnTest="energy",univariateTest="SW",univariatePlot="histogram",multivariatePlot="qq")
mvn.result4 

