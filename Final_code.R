### 분석전 필요 library 불러오기 (여기다 모든 라이브러리 불러와줘)
library(dplyr)
library(readxl)
library(ggplot2)

## working directory설정

#setwd(r'(C:\Users\hyeon\Desktop\Hyeon\Finance_competition\data)')
#setwd("C:/Users/hc07c/Documents/R/공모전데이터/")


# =================================== 1. 데이터 로드 ===================================

#신한은행 데이터 로드
#거리두기 단계를 19년 3월, 9월은 0 / 20년 3월은 1 / 20년 9월과 21년 3월은 3으로 줌
shinhan1903 <- read_excel("신한은행201903.xlsx")
shinhan1903$거리두기 <- 0
shinhan1909 <- read_excel("신한은행201909.xlsx")
shinhan1909$거리두기 <- 0
shinhan2003 <- read_excel("신한은행202003.xlsx")
shinhan2003$거리두기 <- 1
shinhan2009 <- read_excel("신한은행202009.xlsx")
shinhan2009$거리두기 <- 3
shinhan2103 <- read_excel("신한은행202103.xlsx")
shinhan2103$거리두기 <- 3
shinhan <- rbind(shinhan1903, shinhan1909, shinhan2003, shinhan2009, shinhan2103)
shinhan <- as.data.frame(shinhan)
#메모리 위해서 나머지 변수 제거
rm(shinhan1903); rm(shinhan1909); rm(shinhan2003); rm(shinhan2009);rm(shinhan2103)


#비씨카드 온라인 매출 데이터 로드
online <- read.csv("온라인 품목별 소비데이터.csv")



# =================================== 2. 신한은행 데이터 분석 ===================================

# -----------------1. 데이터 전처리 및 가공

#신한데이터에서 총소비금액이 0인 행 제외(246787건)
shinhan_d <- shinhan %>%
  filter(총소비금액 != 0) %>%
  select(지역구, 나이, 성별, 직장인여부, 총소비금액, 총수신금액, 거리두기)

#범주형 변수 factor 변환
shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)
shinhan_d$거리두기 = as.factor(shinhan_d$거리두기)
rm(shinhan)
str(shinhan_d)

# -----------------2. 기초 통계량 분석

summary(shinhan_d)

# 성별 소비금액 박스플랏
ggplot(data = shinhan_d,mapping = aes(x=성별,y = 총소비금액))+
  geom_boxplot()+
  ggtitle("성별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('남자','여자'))

# 연령별 소비금액 박스플랏
ggplot(data = shinhan_d,mapping = aes(x=나이,y = 총소비금액))+
  geom_boxplot()+
  ggtitle("연령별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('20대','30대','40대','50대','60대','70대'))

#성별 총소비금액 기초통계 
summary(shinhan_d[shinhan_d$성별==1,'총소비금액'])
summary(shinhan_d[shinhan_d$성별==2,'총소비금액'])

#연령별 총소비금액 기초통계
for(i in 2:7){
  print(summary(shinhan_d[shinhan_d$나이==i,'총소비금액']))
}

# -----------------2. 분산분석

#회귀 모델 생성전 거리두기, 성별, 연령과 지출금액의 분산분석
shinhan_aov_age = aov(총소비금액~나이*거리두기,data=shinhan_d)
shinhan_aov_sex = aov(총소비금액~성별*거리두기,data=shinhan_d)
summary(shinhan_aov_age)
summary(shinhan_aov_sex)

# -----------------3. 회귀분석

lm_shinhan_inter <- lm(총소비금액 ~ .+나이*거리두기+성별*거리두기,data=shinhan_d)
summary(lm_shinhan_inter)


#잔차 정규성 확인
hist(rstandard(lm_shinhan_inter),main='Hist of Residual')
qqnorm(rstandard(lm_shinhan_inter))
qqline(rstandard(lm_shinhan_inter))
#잔차의 등분산성.
plot(lm_shinhan_inter,3)


# =================================== 3. 비씨카드 데이터 분석 ===================================

# -------------------------------1. 데이터 전처리 및 가공
online <- online %>%
  filter(품목중분류명 != '다이어트')
#품목중분류 중 2019년 데이터가 없는 '다이어트' 행 제거
#나머지 품목들은 모두 2019년 3월 ~ 2021년 3월까지 데이터 존재

#거리두기 변수 추가
online$거리두기 <- ifelse(online$기준년월 %in% c(201903, 201909), 0, 
                      ifelse(online$기준년월 == 202003, 1, 3))
str(online)

#회귀분석에 사용할 변수 추출
online <- online %>%
  select(품목대분류명 ,성별, 연령, 매출금액, 거리두기)


#범주형 변수 factor 변환
online$품목대분류명 <- factor(online$품목대분류명)
online$성별 <- factor(online$성별)
online$연령 <- factor(online$연령)
online$거리두기 <- factor(online$거리두기)

# -------------------------------2. 기초 통계량 분석

summary(online)

#이상치 개수파악
which(online$매출금액 > summary(online$매출금액)[5] + 1.5*IQR(online$매출금액))
sum(online$매출금액 > summary(online$매출금액)[5] + 1.5*IQR(online$매출금액))

# 성별 소비금액 박스플랏
ggplot(data = online,mapping = aes(x=성별,y = 매출금액))+
  geom_boxplot()+
  ggtitle("성별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('남자','여자'))

# 연령별 소비금액 박스플랏
ggplot(data = online,mapping = aes(x=연령,y = 매출금액))+
  geom_boxplot()+
  ggtitle("연령별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('20대','20대미만','30대','40대','50대','60대','70대'))

#성별 총소비금액 기초통계 
summary(online[online$성별=='남성','매출금액'])
summary(online[online$성별=='여성','매출금액'])
#연령별 총소비금액 기초통계
agesummary = c()
for(i in levels(online$연령)){
  agesummary = rbind(agesummary,summary(online[online$연령==i,'매출금액']))
}
agesummary

#온라인 원래 데이터 히스토그램
ggplot(online, aes(x = 매출금액)) +
  geom_histogram(binwidth = 1000000)+ggtitle("Onilne Expenditure Histogram")+
  theme(plot.title = element_text(hjust=.5))

#이상치 검사
outlierInd = which(online$매출금액 > summary(online$매출금액)[5] + 1.5*IQR(online$매출금액))
length(outlierInd)
length(outlierInd)/nrow(online) #이상치 비율

out <- online[outlierInd,]


out_group <- out %>%
  group_by(품목대분류명) %>%
  summarise(총매출 = sum(매출금액),
               평균매출 = mean(매출금액))
out_group <- cbind(as.data.frame(out_group),out %>% count(품목대분류명))
names(out_group)[5] = "카운트"

#이상치 그룹 분석.
out_group = out_group[order(out_group$총매출,decreasing =T),]

#온라인데이터 매출금액에 로그 씌운 분석
online_log <- online
online_log$매출금액 <- log(online_log$매출금액)
names(online_log)[4] <- "log_매출금액"
str(online_log)
str(online)

# 온라인 로그 씌운 히스토그램
ggplot(online_log, aes(x = log_매출금액)) +
  geom_histogram(binwidth = .3)+ggtitle("Onilne log_Expenditure Histogram")+xlab("log_소비금액")+
  theme(plot.title = element_text(hjust=.5))

#회귀 모델 생성전 거리두기, 성별, 연령과 지출금액의 분산분석
online_aov_age = aov(log_매출금액~연령*거리두기,data=online_log)
online_aov_sex = aov(log_매출금액~성별*거리두기,data=online_log)
online_aov_goods = aov(log_매출금액~품목대분류명*거리두기,data=online_log)
summary(online_aov_age)
summary(online_aov_sex)
summary(online_aov_goods)


# -------------------------------3. 회귀분석
lm_online_log <- lm(log_매출금액 ~ . + 성별*거리두기 + 연령*거리두기+품목대분류명*거리두기, data = online_log)
summary(lm_online_log)

write.csv(lm_online_log$coefficients,file = "onilnecoef.csv")
#log_level의 경우 더미변수 해석
#exp(계수) - 1

#온라인 로그 데이터 잔차 검정
#잔차 정규성 확인
par(mfrow=c(1,1))
hist(rstandard(lm_online_log),main='Hist of Residual')
qqnorm(rstandard(lm_online_log))
qqline(rstandard(lm_online_log))
#잔차의 등분산성.
plot(lm_online_log,3)


# =================================== 4. 예측 모델링 ===================================

#============ 신한데이터 ===============
shinhan_d$거리두기 = as.numeric(shinhan_d$거리두기)

#training과 test를 위해 80:20으로 데이터를 나눔
set.seed(11)
sp <- sample(1:nrow(shinhan_d), ceiling(nrow(shinhan_d)*0.8))
shinhan_tr <- shinhan_d[sp, ]
shinhan_ts <- shinhan_d[-sp, ]
boxplot(shinhan_tr$총소비금액)

#회귀분석
lm_shinhan_tr <- lm(총소비금액 ~ . + 나이*거리두기 + 성별*거리두기, 
                         data = shinhan_tr)
summary(lm_shinhan_tr)

#test data를 이용해 lm_shinhan_tr 모델평가
pred_sh <- predict(lm_shinhan_tr, shinhan_ts)
error_sh <- pred_sh - shinhan_ts$총소비금액
plot(error_sh)
par(mfrow = c(1,1))
qqnorm(error_sh)
mean(shinhan_ts$총소비금액)
mean(error_sh^2) %>% sqrt()

#메모리 사용을 위해 필요없는 데이터 삭제
rm(lm_shinhan_inter, shinhan_tr, shinhan_ts)

#예측 데이터셋 만들기
str(shinhan_d)

region <- unique(shinhan_d$지역구)
age <- unique(shinhan_d$나이)
sex <- unique(shinhan_d$성별)
salary <- unique(shinhan_d$직장인여부)
fimoney <- summary(shinhan_d$총수신금액)[c(2,4,5)]
#총수신금액은 연속형 변수여서 값을 임의로 넣을 수 없기 때문에
#1/4분위값, 평균, 3/4분위값 세가지로 넣음

set_expect_sh <- expand.grid(region, age, sex, salary, fimoney)
str(set_expect_sh)

#거리두기를 4로 주고 변수명 shinhan_d 데이터와 같게 통일
set_expect_sh$거리두기 <- 4
names(set_expect_sh)[1:5] <- names(shinhan_d)[c(1:4, 6)]

#lm_shinhan_tr로 예측
shinhan_pred <- predict(lm_shinhan_tr, set_expect_sh)
set_expect_sh$예상소비금액 <- shinhan_pred
head(set_expect_sh, 50)

#비슷한 총수신금액을 보유한 사람들의 3단계 평균총소비금액과 비교(예시)
shinhan_d %>%
  filter(지역구 == '강남구') %>%
  filter(나이 == '2') %>%
  filter(성별 == '1') %>%
  filter(직장인여부 == '0') %>%
  filter(거리두기 == 3) %>%
  filter(총수신금액 > 2000000 & 총수신금액 < 2100000) %>%
  summarise('3단계 평균 총소비금액' = mean(총소비금액))


#예측데이터와 거리두기 3단계 시 평균 소비금액 비교

#연령별
age4 <- set_expect_sh %>%
  group_by(나이) %>%
  summarise(mean = mean(예상소비금액))
age4 <- as.data.frame(age4)

age3 <- shinhan_d %>%
  filter(거리두기 == 3) %>%
  group_by(나이) %>%
  summarise(mean = mean(총소비금액))
age3 <- as.data.frame(age3) 

age4$mean / age3$mean

#성별
sex4 <- set_expect_sh %>%
  group_by(성별) %>%
  summarise(mean = mean(예상소비금액))
sex4 <- as.data.frame(sex4)

sex3 <- shinhan_d %>%
  filter(거리두기 == 3) %>%
  group_by(성별) %>%
  summarise(mean = mean(총소비금액))
sex3 <- as.data.frame(sex3) 

sex4$mean / sex3$mean


#================= 온라인데이터 =================
online_log$거리두기 = as.numeric(online_log$거리두기)

#training과 test를 위해 80:20으로 데이터를 나눔
set.seed(11)
sp <- sample(1:nrow(online_log), ceiling(nrow(online_log)*0.8))

online_tr <- online_log[sp, ]
online_ts <- online_log[-sp, ]


#training data로 회귀분석
lm_online_tr <- lm(log_매출금액 ~ . + 성별 * 거리두기 + 연령 * 거리두기 +
                     품목대분류명*거리두기, data = online_tr)
summary(lm_online_tr)


#test data를 이용해 lm_online_tr 모델평가
pred <- predict(lm_online_tr, online_ts)
error <- pred - online_ts$log_매출금액
plot(error)
head(error, 30)
mean(error^2) %>% sqrt()
mean(online_ts$log_매출금액)
qqnorm(error)

#4단계 예측시 필요한 데이터 셋을 만듦
product = unique(online$품목대분류명)
sex = unique(online$성별)
age = unique(online$연령)
set_expect = expand.grid(product,sex,age)

#거리두기를 4로 줌
set_expect$거리두기 <- 4
names(set_expect)[1:3] <- names(online)[1:3]
head(set_expect)

#위에서 구한 lm_online_tr로 예측
online_pred <- predict(lm_online_tr, set_expect)
head(online_pred)

#예측 데이터 셋에 예측 금액을 합쳐줌
set_expect$log_매출금액 <- online_pred
head(set_expect)

#같은 데이터 셋의 3단계 평균매출금액과 비교해봄(예시)
online_log %>%
  filter(품목대분류명 == '의류') %>%
  filter(성별 == '여성') %>%
  filter(연령 == '40대') %>%
  filter(고객소재지_시군구 == '구로구') %>% 
  filter(거리두기 == 3) %>%
  summarise('3단계 평균매출금액' = mean(log_매출금액))

#예측데이터와 거리두기 3단계 시 평균 소비금액 비교
sex4_on <- set_expect %>%
  group_by(성별) %>%
  summarise(mean = mean(log_매출금액))
sex4_on <- as.data.frame(sex4_on)

sex3_on <- online_log %>%
  filter(거리두기 == 3) %>%
  group_by(성별) %>%
  summarise(mean = mean(log_매출금액))
sex3_on <- as.data.frame(sex3_on)

sex4_on$mean / sex3_on$mean

age4_on <- set_expect %>%
  group_by(연령) %>%
  summarise(mean = mean(log_매출금액))
age4_on <- as.data.frame(age4_on)

age3_on <- online_log %>%
  filter(거리두기 == 3) %>%
  group_by(연령) %>%
  summarise(mean = mean(log_매출금액))
age3_on <- as.data.frame(age3_on)

age4_on$mean / age3_on$mean