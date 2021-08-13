setwd(r'(C:\Users\hyeon\Desktop\Hyeon\Finance_competition\data\서울시 지역단위(집계구 업데이트))')
library(readxl)
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
head(shinhan)
tail(shinhan)
names(shinhan)
shinhan_d <- shinhan[,c("지역구", "나이", "성별", "직장인여부", "총소비금액", "총수신금액", "거리두기")]

#training과 test 데이터 나누기
#80% : 20%로 나눔
#1039568*0.8에서 올림해서 training 데이터에 넣음

set.seed(1)
random <- sample(1:1039568, 831655)
shinhan_tr <- shinhan_d[random, ]
shinhan_ts <- shinhan_d[-random, ]

#단순선형회귀
shinhan_lm <- lm(총소비금액 ~ factor(지역구) + factor(나이) + factor(성별)
                      + factor(직장인여부) + 총수신금액 + 거리두기, data = shinhan_tr)
summary(shinhan_lm)
lm_pr <- predict(shinhan_lm, shinhan_ts)
plot(shinhan_ts$총소비금액 - lm_pr) #오차


#ridge, lasso regression
##범주형변수 변환 후 더미변수 만들기
install.packages("mltools")
library(mltools)
library(data.table)
shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)

fac <- one_hot(as.data.table(shinhan_d))
head(fac)
str(fac)

#이전과 같이 training과 test data 나눔
set.seed(1)
random <- sample(1:1039568, 831655)
shinhan_tr <- fac[random, ]
shinhan_ts <- fac[-random, ]

library(glmnet)
shinhan_tr_x <- as.matrix(shinhan_tr[, -36])
shinhan_tr_y <- as.matrix(shinhan_tr[, 36])

fit_lasso <- cv.glmnet(shinhan_tr_x, shinhan_tr_y, family = "gaussian")
fit_ridge <- cv.glmnet(shinhan_tr_x, shinhan_tr_y, family = "gaussian", alpha = 0)

coef(fit_lasso)
coef(fit_ridge)

pr_las <- predict(fit_lasso, as.matrix(shinhan_ts[, -36]))
pr_rid <- predict(fit_ridge, as.matrix(shinhan_ts[, -36]))
yy <- shinhan_ts$총소비금액 - pr_las
kk <- shinhan_ts$총소비금액 - pr_rid
plot(yy)
