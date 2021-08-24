names(shinhan)
shinhan_d <- shinhan[,c("지역구", "나이", "성별", "직장인여부", "총소비금액", "총수신금액", "거리두기")]
shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)
shinhan_d$거리두기 = as.factor(shinhan_d$거리두기)
rm(shinhan)
#training과 test 데이터 나누기
#80% : 20%로 나눔
#1039568*0.8에서 올림해서 training 데이터에 넣음
set.seed(1)
random <- sample(1:1039568, 831655)
shinhan_tr <- shinhan_d[random, ]
shinhan_ts <- shinhan_d[-random, ]

#단순선형회귀
shinhan_lm <- lm(총소비금액 ~ 지역구 + 나이 + 성별
                      + 직장인여부 + 총수신금액 + 거리두기, data = shinhan_tr)
summary(shinhan_lm)
lm_pr <- predict(shinhan_lm, shinhan_ts)
head(lm_pr)


#ridge, lasso regression
##범주형변수 변환 후 더미변수 만들기

library(mltools)
library(data.table)

OnehotShinhan_tr <- one_hot(as.data.table(shinhan_tr))

OnehotShinhan_ts <- one_hot(as.data.table(shinhan_ts))


#이전과 같이 training과 test data 나눔 ==> 어차피 나눴는데 그대로 사용하면 되지않나?
library(glmnet)
shinhan_tr_x <- as.matrix(OnehotShinhan_tr %>% select(-'총소비금액'))
shinhan_tr_y <- as.matrix(OnehotShinhan_tr %>% select('총소비금액'))

fit_lasso <- cv.glmnet(shinhan_tr_x, shinhan_tr_y, family = "gaussian")
fit_ridge <- cv.glmnet(shinhan_tr_x, shinhan_tr_y, family = "gaussian", alpha = 0)

coef(fit_lasso)
coef(fit_ridge)

pr_las <- predict(fit_lasso, as.matrix(OnehotShinhan_ts %>% select(-'총소비금액')))
pr_rid <- predict(fit_ridge, as.matrix(OnehotShinhan_ts %>% select(-"총소비금액")))
yy <- shinhan_ts$총소비금액 - pr_las
kk <- shinhan_ts$총소비금액 - pr_rid
plot(yy)
