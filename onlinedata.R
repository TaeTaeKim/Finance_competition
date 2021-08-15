#setwd(r'(C:\Users\hyeon\Desktop\Hyeon\Finance_competition\data)')
online <- read.csv("온라인 품목별 소비데이터.csv")

online$거리두기 <- ifelse(online$기준년월 %in% c(201903, 201909), 0, 
                      ifelse(online$기준년월 == 202003, 1, 3))
online <- online[, c("품목중분류명", "성별", "연령", "고객소재지_시군구", "매출금액","거리두기")]

online$품목중분류명 <- factor(online$품목중분류명)
online$성별 <- factor(online$성별)
online$연령 <- factor(online$연령)
online$고객소재지_시군구 <- factor(online$고객소재지_시군구)
online$거리두기 <- factor(online$거리두기)

#지역을 서울로 한정
online <- online[!(online$고객소재지_시군구 %in% c('용인시', '수원시', '고양시', '서구', '남동구', '부평구', '강화군', '계양구', '성남시', '미추홀구', '연수구', '동구', '옹진군')), ]

sp <- sample(1:nrow(online), ceiling(nrow(online)*0.8))

online_tr <- online[sp, ]
online_ts <- online[-sp, ]

#단순회귀분석
lm_online <- lm(매출금액 ~ ., data = online_tr)
summary(lm_online)

on_lm_pr <- predict(lm_online, online_ts)

dd <- online_ts$매출금액 - on_lm_pr
plot(dd)


#ridge, lasso
library(mltools)
library(data.table)
library(glmnet)
library(dplyr)

Onehotonline_tr <- one_hot(as.data.table(online_tr))

Onehotonline_ts <- one_hot(as.data.table(online_ts))


online_tr_x <- as.matrix(Onehotonline_tr %>% select(-'매출금액'))
online_tr_y <- as.matrix(Onehotonline_tr %>% select('매출금액'))

fit_lasso <- cv.glmnet(online_tr_x, online_tr_y, family = "gaussian")
fit_ridge <- cv.glmnet(online_tr_x, online_tr_y, family = "gaussian", alpha = 0)

coef(fit_lasso)
coef(fit_ridge)

pr_las <- predict(fit_lasso, as.matrix(Onehotonline_ts %>% select(-'매출금액')))
pr_rid <- predict(fit_ridge, as.matrix(Onehotonline_ts %>% select(-'매출금액')))
yy <- online_ts$매출금액 - pr_las
kk <- online_ts$매출금액 - pr_rid
plot(yy)
yy
