#실제로 분석에 사용할 모델들

#shinhanDA와 onlinedata에서 전처리한 데이터를 가지고 진행

#shinhan data
lm_shinhan_inter <- lm(총소비금액 ~ .+나이*거리두기+성별*거리두기,data=shinhan_d)
summary(lm_shinhan_inter)


#online data 
lm_online_inter <- lm(매출금액 ~ .+성별 * 거리두기+연령 * 거리두기 +
                            품목대분류명 *거리두기, data = online)
summary(lm_online_inter)



##예측 모델링

#거리두기 4단계 시의 지출과 소비를 예측하기 위해 기존에 factor로 사용했던
#거리두기 변수를 numeric으로 바꿔서 진행

#온라인 데이터 
online$거리두기 = as.numeric(online$거리두기)
str(online)
#training과 test를 위해 80:20으로 데이터를 나눔
set.seed(11)
sp <- sample(1:nrow(online), ceiling(nrow(online)*0.8))

online_tr <- online[sp, ]
boxplot(online_tr$매출금액,outline=F)
online_ts <- online[-sp, ]


#training data로 회귀분석
lm_online_tr <- lm(매출금액 ~ . + 성별 * 거리두기 + 연령 * 거리두기 +
                         품목대분류명 * 거리두기, data = online_tr)
summary(lm_online_tr)


#test data를 이용해 lm_online_tr 모델평가
pred <- predict(lm_online_tr, online_ts)
error <- pred - online_ts$매출금액
plot(error,ylim = c(min(error),-min(error)))
abline(a = 0,b=1,col='red')
abline()
head(error)
mean(error^2) %>% sqrt()


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
set_expect$매출금액 <- online_pred
head(set_expect)

#같은 데이터 셋의 3단계 평균매출금액과 비교해봄(예시)
online %>%
  filter(품목대분류명 == '의류') %>%
  filter(성별 == '여성') %>%
  filter(연령 == '40대') %>%
  filter(고객소재지_시군구 == '구로구') %>% 
  filter(거리두기 == 3) %>%
  summarise('3단계 평균매출금액' = mean(매출금액))




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
#메모리 사용을 위해 필요없는 데이터 삭제
rm(lm_shinhan_inter, shinhan_tr, shinhan_ts)


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
head(set_expect_sh)

#비슷한 총수신금액을 보유한 사람들의 3단계 평균총소비금액과 비교(예시)
shinhan_d %>%
  filter(지역구 == '강남구') %>%
  filter(나이 == '2') %>%
  filter(성별 == '1') %>%
  filter(직장인여부 == '0') %>%
  filter(거리두기 == 3) %>%
  filter(총수신금액 > 2000000 & 총수신금액 < 2100000) %>%
  summarise('3단계 평균 총소비금액' = mean(총소비금액))
