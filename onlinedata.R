
# -------------------------------1. 온라인 데이터 전처리 및 가공
online <- online %>%
  filter(품목중분류명 != '다이어트')
#품목중분류 중 2019년 데이터가 없는 '다이어트' 행 제거
#나머지 품목들은 모두 2019년 3월 ~ 2021년 3월까지 데이터 존재

#거리두기 변수 추가
online$거리두기 <- ifelse(online$기준년월 %in% c(201903, 201909), 0, 
                      ifelse(online$기준년월 == 202003, 1, 3))

#회귀분석에 사용할 변수 추출
online <- online %>%
  select(품목대분류명, 성별, 연령, 매출금액, 거리두기)


#범주형 변수 factor 변환
online$품목대분류명 <- factor(online$품목대분류명)
online$성별 <- factor(online$성별)
online$연령 <- factor(online$연령)
online$거리두기 <- factor(online$거리두기)

# -------------------------------2. 기초 통계량 분석

summary(online)



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
summary(online_aov_age)
summary(online_aov_sex)




# -------------------------------3. 회귀분석
lm_online_log <- lm(log_매출금액 ~ . + 성별*거리두기 + 연령*거리두기, data = online_log)
summary(lm_online_log)



#온라인 로그 데이터 잔차 검정
#잔차 정규성 확인
par(mfrow=c(1,1))
hist(rstandard(lm_online_log),main='Hist of Residual')
qqnorm(rstandard(lm_online_log))
qqline(rstandard(lm_online_log))
#잔차의 등분산성.
plot(lm_online_log,3)





#log_level의 경우 더미변수 해석
#exp(계수) - 1

exp(0.2491390) - 1 #여성
exp(0.0549541) - 1 #성별여성:거리두기1 
exp(0.0329318) - 1 #성별여성:거리두기3
exp(0.4964267) - 1 #연령30대
exp(0.2274273) - 1 #연령40대
exp(0.1107281) - 1 #연령50대
exp(-1.7300009) - 1 #연령20세 미만
exp(-0.1913269) - 1 #연령60대 이상
exp(-0.2678189) - 1 #연령20세 미만:거리두기1
exp(-0.1432789) - 1 #연령30대:거리두기1
exp(-0.0825936) - 1 #연령40대:거리두기1
exp(-0.0553533) - 1 #연령60대 이상:거리두기1
exp(-0.1677261) - 1 #연령20세 미만:거리두기3
exp(-0.1604924) - 1 #연령30대:거리두기3
exp(0.0297523) - 1 #연령40대:거리두기3
exp(0.0916623) - 1 #연령50대:거리두기3
exp(0.1065602) - 1 #연령60대 이상:거리두기3



##예측
online_log$거리두기 = as.numeric(online_log$거리두기)

#training과 test를 위해 80:20으로 데이터를 나눔
set.seed(11)
sp <- sample(1:nrow(online_log), ceiling(nrow(online_log)*0.8))

online_tr <- online_log[sp, ]
online_ts <- online_log[-sp, ]


#training data로 회귀분석
lm_online_tr <- lm(log_매출금액 ~ . + 성별 * 거리두기 + 연령 * 거리두기 +
                     품목대분류명 * 거리두기, data = online_tr)
summary(lm_online_tr)


#test data를 이용해 lm_online_tr 모델평가
pred <- predict(lm_online_tr, online_ts)
error <- pred - online_ts$log_매출금액
plot(error)
head(error, 30)
mean(error^2) %>% sqrt()
mean(online_ts$log_매출금액)


#4단계 예측시 필요한 데이터 셋을 만듦
product = unique(online$품목대분류명)
sex = unique(online$성별)
age = unique(online$연령)
region = unique(online$고객소재지_시군구)
set_expect = expand.grid(product,sex,age,region)

#거리두기를 4로 줌
set_expect$거리두기 <- 4
names(set_expect)[1:4] <- names(online)[1:4]

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
