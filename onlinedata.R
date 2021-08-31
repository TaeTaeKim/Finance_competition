## 워킹디렉토리는 my.r에서 설정하기!!

online <- online %>%
  filter(품목중분류명 != '다이어트')
#품목중분류 중 2019년 데이터가 없는 '다이어트' 행 제거
#나머지 품목들은 모두 2019년 3월 ~ 2021년 3월까지 데이터 존재

online$거리두기 <- ifelse(online$기준년월 %in% c(201903, 201909), 0, 
                      ifelse(online$기준년월 == 202003, 1, 3))

online <- online %>%
  select(품목대분류명, 성별, 연령, 고객소재지_시군구, 매출금액, 거리두기)
#중분류에서 대분류로 변경

#지역을 서울로 한정
online <- online %>%
  filter(!(online$고객소재지_시군구 %in% c('용인시', '수원시', '고양시', '서구', '남동구', '부평구', '강화군', '계양구', '성남시', '미추홀구', '연수구', '동구', '옹진군')))

online$품목대분류명 <- factor(online$품목대분류명)
online$성별 <- factor(online$성별)
online$연령 <- factor(online$연령)
online$고객소재지_시군구 <- factor(online$고객소재지_시군구)
online$거리두기 <- factor(online$거리두기)


'''
예측은 나중에
sp <- sample(1:nrow(online), ceiling(nrow(online)*0.8))

online_tr <- online[sp, ]
online_ts <- online[-sp, ]
'''

#단순회귀분석
lm_online <- lm(매출금액 ~ ., data = online)
summary(lm_online)

#교차항 추가
lm_online_inter <- lm(매출금액 ~ 품목대분류명 * 거리두기 + 성별 * 거리두기 +
                            연령 * 거리두기 + 고객소재지_시군구 * 거리두기, data = online)

summary(lm_online_inter)

'''
on_lm_pr <- predict(lm_online, online_ts)

dd <- online_ts$매출금액 - on_lm_pr
plot(dd)
'''

#ridge, lasso
library(mltools)
library(data.table)
library(glmnet)
library(dplyr)

onehotonline <- one_hot(as.data.table(online))
#onehotonline_tr <- one_hot(as.data.table(online_tr))
#onehotonline_ts <- one_hot(as.data.table(online_ts))


online_x <- as.matrix(onehotonline %>% select(-'매출금액'))
online_y <- as.matrix(onehotonline %>% select('매출금액'))

online_lasso <- cv.glmnet(online_x, online_y, family = "gaussian")
online_ridge <- cv.glmnet(online_x, online_y, family = "gaussian", alpha = 0)

coef(online_lasso)
coef(online_ridge)

'''
pr_las <- predict(fit_lasso, as.matrix(Onehotonline_ts %>% select(-'매출금액')))
pr_rid <- predict(fit_ridge, as.matrix(Onehotonline_ts %>% select(-'매출금액')))
yy <- online_ts$매출금액 - pr_las
kk <- online_ts$매출금액 - pr_rid
plot(yy)
yy
'''

#----------------------------------#

##코로나전후  나눠서 회귀분석
online_bc <- online %>% filter(거리두기 %in% '0') %>% select(품목중분류명, 성별, 연령, 매출금액)
online_ac <- online %>% filter(거리두기 %in% c('1', '3')) %>% select(품목중분류명, 성별, 연령, 매출금액)

lm_online_bc <- lm(매출금액 ~ ., data = online_bc)
lm_online_ac <- lm(매출금액 ~ ., data = online_ac)

summary(lm_online_bc)
summary(lm_online_ac)
