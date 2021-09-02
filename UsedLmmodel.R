### 시각화화 데이터들


#구별 소비금액의 평균
as.data.frame(shinhan %>% group_by(지역구) %>% summarise(expmean = mean(총소비금액),
                                                      salaryman = sum(직장인여부
                                                                      )))
#교차항을 넣은 선형회귀
shinhan_lm_cross <- lm(총소비금액 ~ 지역구 + 나이*거리두기 + 성별*거리두기 +
                              직장인여부 + 총수신금액, data = shinhan_d)
shinhan_lm_cross2 <- lm(총소비금액 ~ .+나이*거리두기+성별*거리두기,data=shinhan_d)
summary(shinhan_lm_cross2)



online$거리두기 = as.numeric(online$거리두기)
str(online)
lm_online_inter2 <- lm(매출금액 ~ . + 성별 * 거리두기 +
                            연령 * 거리두기+품목대분류명*거리두기, data = online)



summary(lm_online_inter2)
#### 보니까 기준이 되는 factor를 빼고 돌려야함
#### 그러면 해석을 어떻게 해야지?
product = unique(online$품목대분류명)
sex = unique(online$성별)
age = unique(online$연령)
region = unique(online$고객소재지_시군구)
a = expand.grid(product,sex,age,region)
b = for(i in 1:length(a)){
  predict(lm_online_inter2,a[i,])
}


##예측 모델링

#온라인 데이터 

#training과 test를 위해 80:20으로 데이터를 나눔
sp <- sample(1:nrow(online), ceiling(nrow(online)*0.8))

online_tr <- online[sp, ]
online_ts <- online[-sp, ]

#training data로 회귀분석
lm_online_tr <- lm(매출금액 ~ . + 성별 * 거리두기 +
                             연령 * 거리두기+품목대분류명*거리두기, data = online_tr)

summary(lm_online_tr)
#데이터 전체로 돌린 것과 training data로 돌린 것에 큰 차이가 없어
#lm_online_tr로 예측을 진행해도 될 것 같음
#어차피 예측에서 사용하는 회귀분석은 거리두기를 numeric으로 두고 한 거라
#우리가 factor로 분석한 회귀식과는 차이가 있기도 함

#test data를 이용해 lm_online_tr 모델평가
pred <- predict(lm_online_tr, online_ts)
error <- pred - online_ts$매출금액
plot(error)
head(error)
#오차가 십만단위로 나눈 것들은 꽤 잘 맞춘 것으로 보임


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

#같은 데이터 셋의 3단계 평균매출금액과 비교해봄(맨 위에것만 해봄)
online %>%
  filter(품목대분류명 == '의류') %>%
  filter(성별 == '여성') %>%
  filter(연령 == '40대') %>%
  filter(고객소재지_시군구 == '구로구') %>% 
  filter(거리두기 == 3) %>%
  summarise('3단계 평균매출금액' = mean(매출금액))


##신한데이터
#training과 test를 위해 80:20으로 데이터를 나눔
shinhan_d$거리두기 = as.numeric(shinhan_d$거리두기)
sp <- sample(1:nrow(shinhan_d), ceiling(nrow(shinhan_d)*0.8))
str(shinhan_d)
shinhan_tr <- shinhan_d[sp, ]
shinhan_ts <- shinhan_d[-sp, ]

lm_shinhan_tr <- lm(총소비금액 ~ . + 나이*거리두기 + 성별*거리두기, 
                         data = shinhan_tr)
summary(lm_shinhan_tr)
#이것도 전체 데이터로 돌린 회귀식과 큰 차이가 없어
#lm_shinhan_tr로 예측해도 될 듯

#test data를 이용해 lm_shinhan_tr 모델평가
pred_sh <- predict(lm_shinhan_tr, shinhan_ts)
error_sh <- pred_sh - shinhan_ts$총소비금액
plot(error_sh)
mean(error_sh)
#128777.6 등 꽤 잘맞추는 것도 있음

rm(shinhan_lm_cross2, shinhan_tr, shinhan_ts)

#예측 데이터셋 만들기
str(shinhan_d)

region <- unique(shinhan_d$지역구)
age <- unique(shinhan_d$나이)
sex <- unique(shinhan_d$성별)
salary <- unique(shinhan_d$직장인여부)
fimoney <- summary(shinhan_d$총수신금액)[c(2,4,5)]
#총수신금액은 연속형 변수로 값을 임의로 넣을 수 없어서
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

#비슷한 총수신금액을 보유한 사람들의 3단계 평균총소비금액과 비교
shinhan_d %>%
  filter(지역구 == '강남구') %>%
  filter(나이 == '2') %>%
  filter(성별 == '1') %>%
  filter(직장인여부 == '0') %>%
  filter(거리두기 == 3) %>%
  filter(총수신금액 > 2000000 & 총수신금액 < 2100000) %>%
  summarise('3단계 평균 총소비금액' = mean(총소비금액))

##비교를 몇 개 해보고 증가할 것으로 보인다/ 감소할 것으로 보인다 
##이런거 제시해보면 어떨까?
