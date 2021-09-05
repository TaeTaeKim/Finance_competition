##예측 모델링

#온라인데이터
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



##신한데이터
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


#메모리 사용을 위해 필요없는 데이터 삭제
rm(lm_shinhan_inter, shinhan_tr, shinhan_ts)

mean(shinhan_ts$총소비금액)
mean(error_sh^2) %>% sqrt()




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
head(set_expect_sh, 100)

#비슷한 총수신금액을 보유한 사람들의 3단계 평균총소비금액과 비교(예시)
shinhan_d %>%
  filter(지역구 == '강남구') %>%
  filter(나이 == '2') %>%
  filter(성별 == '1') %>%
  filter(직장인여부 == '0') %>%
  filter(거리두기 == 3) %>%
  filter(총수신금액 > 2000000 & 총수신금액 < 2100000) %>%
  summarise('3단계 평균 총소비금액' = mean(총소비금액))

shinhan_d %>%
  filter(지역구 == '중구') %>%
  filter(나이 == '5') %>%
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

