names(shinhan)
shinhan_d <- shinhan %>%
  filter(총소비금액 != 0) %>%
  select(지역구, 나이, 성별, 직장인여부, 총소비금액, 총수신금액, 거리두기)
#신한데이터에서 총소비금액이 0인 행 제외(246787건)

shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)
shinhan_d$거리두기 = as.factor(shinhan_d$거리두기)
rm(shinhan)
str(shinhan_d)

'''
<예측모델은 일단 나중에>
#training과 test 데이터 나누기
#80% : 20%로 나눔
#1039568*0.8에서 올림해서 training 데이터에 넣음
set.seed(1)
random <- sample(1:1039568, 831655)
shinhan_tr <- shinhan_d[random, ]
shinhan_ts <- shinhan_d[-random, ]
'''

#단순선형회귀
shinhan_lm <- lm(총소비금액 ~ ., data = shinhan_d)
summary(shinhan_lm)

#lm_pr <- predict(shinhan_lm, shinhan_ts)
#head(lm_pr)

#교차항을 넣은 선형회귀
shinhan_lm_cross <- lm(총소비금액 ~ 지역구*거리두기 + 나이*거리두기 + 성별*거리두기 +
                              직장인여부*거리두기 + 총수신금액*거리두기, data = shinhan_d)
summary(shinhan_lm_cross)


#one-hot encoding을 이용한 회귀분석
onehot_shinhan <- one_hot(as.data.table(shinhan_d))
str(onehot_shinhan)

shinhan_lm_onehot <- lm(총소비금액 ~ ., data = onehot_shinhan)
summary(shinhan_lm_onehot)

#ond-hot encoding 한 상태에서 교차항 
shinhan_lm_oc <- lm(총소비금액 ~ 
                           지역구_강남구*거리두기_0 + 지역구_강동구*거리두기_0 + 지역구_강북구*거리두기_0 +
                           지역구_강서구*거리두기_0 + 지역구_관악구*거리두기_0 + 지역구_광진구*거리두기_0 +
                           지역구_구로구*거리두기_0 + 지역구_금천구*거리두기_0 + 지역구_노원구*거리두기_0 +
                           지역구_도봉구*거리두기_0 + 지역구_동대문구*거리두기_0 + 지역구_동작구*거리두기_0 +
                           지역구_마포구*거리두기_0 + 지역구_서대문구*거리두기_0 + 지역구_서초구*거리두기_0 +
                           지역구_성동구*거리두기_0 + 지역구_성북구*거리두기_0 + 지역구_송파구*거리두기_0 +
                           지역구_양천구*거리두기_0 + 지역구_영등포구*거리두기_0 + 지역구_용산구*거리두기_0 +
                           지역구_은평구*거리두기_0 + 지역구_종로구*거리두기_0 + 지역구_중구*거리두기_0 +
                           지역구_중랑구*거리두기_0 + 나이_2*거리두기_0 + 나이_3*거리두기_0 + 나이_4*거리두기_0 +
                           나이_5*거리두기_0 + 나이_6*거리두기_0 + 나이_7*거리두기_0 + 성별_1*거리두기_0 + 성별_2*거리두기_0 +
                           직장인여부_0*거리두기_1 + 직장인여부_1*거리두기_0 + 총수신금액*거리두기_0 + 
                           지역구_강남구*거리두기_1 + 지역구_강동구*거리두기_1 + 지역구_강북구*거리두기_1 +
                           지역구_강서구*거리두기_1 + 지역구_관악구*거리두기_1 + 지역구_광진구*거리두기_1 +
                           지역구_구로구*거리두기_1 + 지역구_금천구*거리두기_1 + 지역구_노원구*거리두기_1 +
                           지역구_도봉구*거리두기_1 + 지역구_동대문구*거리두기_1 + 지역구_동작구*거리두기_1 +
                           지역구_마포구*거리두기_1 + 지역구_서대문구*거리두기_1 + 지역구_서초구*거리두기_1 +
                           지역구_성동구*거리두기_1 + 지역구_성북구*거리두기_1 + 지역구_송파구*거리두기_1 +
                           지역구_양천구*거리두기_1 + 지역구_영등포구*거리두기_1 + 지역구_용산구*거리두기_1 +
                           지역구_은평구*거리두기_1 + 지역구_종로구*거리두기_1 + 지역구_중구*거리두기_1 +
                           지역구_중랑구*거리두기_1 + 나이_2*거리두기_1 + 나이_3*거리두기_1 + 나이_4*거리두기_1 +
                           나이_5*거리두기_1 + 나이_6*거리두기_1 + 나이_7*거리두기_1 + 성별_1*거리두기_1 + 성별_2*거리두기_1 +
                           직장인여부_0*거리두기_1 + 직장인여부_1*거리두기_1 + 총수신금액*거리두기_1 +
                           지역구_강남구*거리두기_3 + 지역구_강동구*거리두기_3 + 지역구_강북구*거리두기_3 +
                           지역구_강서구*거리두기_3 + 지역구_관악구*거리두기_3 + 지역구_광진구*거리두기_3 +
                           지역구_구로구*거리두기_3 + 지역구_금천구*거리두기_3 + 지역구_노원구*거리두기_3 +
                           지역구_도봉구*거리두기_3 + 지역구_동대문구*거리두기_3 + 지역구_동작구*거리두기_3 +
                           지역구_마포구*거리두기_3 + 지역구_서대문구*거리두기_3 + 지역구_서초구*거리두기_3 +
                           지역구_성동구*거리두기_3 + 지역구_성북구*거리두기_3 + 지역구_송파구*거리두기_3 +
                           지역구_양천구*거리두기_3 + 지역구_영등포구*거리두기_3 + 지역구_용산구*거리두기_3 +
                           지역구_은평구*거리두기_3 + 지역구_종로구*거리두기_3 + 지역구_중구*거리두기_3 +
                           지역구_중랑구*거리두기_3 + 나이_2*거리두기_3 + 나이_3*거리두기_3 + 나이_4*거리두기_3 +
                           나이_5*거리두기_3 + 나이_6*거리두기_3 + 나이_7*거리두기_3 + 성별_1*거리두기_3 + 성별_2*거리두기_3 +
                           직장인여부_0*거리두기_3 + 직장인여부_1*거리두기_3 + 총수신금액*거리두기_3, data = onehot_shinhan)

summary(shinhan_lm_oc)
#각 범주형 변수마다 하나씩 NA로 처리되서(NA뜨는 변수들이 기준인 듯)
#결국에는 그냥 범주형 변수로 돌렸을 때와 같아지는 것 같음


#ridge, lasso regression
##범주형변수 변환 후 더미변수 만들기

library(mltools)
library(data.table)
library(glmnet)

onehot_shinhan <- one_hot(as.data.table(shinhan_d))
#OnehotShinhan_tr <- one_hot(as.data.table(shinhan_tr))
#OnehotShinhan_ts <- one_hot(as.data.table(shinhan_ts))


shinhan_x <- as.matrix(onehot_shinhan %>% select(-'총소비금액'))
shinhan_y <- as.matrix(onehot_shinhan %>% select('총소비금액'))

#shinhan_tr_x <- as.matrix(OnehotShinhan_tr %>% select(-'총소비금액'))
#shinhan_tr_y <- as.matrix(OnehotShinhan_tr %>% select('총소비금액'))

shihan_lasso <- cv.glmnet(shinhan_x, shinhan_y, family = "gaussian")
shinhan_ridge <- cv.glmnet(shinhan_x, shinhan_y, family = "gaussian", alpha = 0)

coef(shihan_lasso)
coef(shinhan_ridge)

#지역변수를 어떻게 해석해야되나??

'''
pr_las <- predict(fit_lasso, as.matrix(OnehotShinhan_ts %>% select(-'총소비금액')))
pr_rid <- predict(fit_ridge, as.matrix(OnehotShinhan_ts %>% select(-"총소비금액")))
yy <- shinhan_ts$총소비금액 - pr_las
kk <- shinhan_ts$총소비금액 - pr_rid
plot(yy)
'''