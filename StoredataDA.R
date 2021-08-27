StoreData$거리두기 <- ifelse(StoreData$기준년월 %in% c(201903, 201909), 0,
                         ifelse(StoreData$기준년월 == 202003, 1, 3))

#원래 있던 일차원분석은 simple analysis에서 확인가능

StoreData <- StoreData %>%
  filter(!is.na(StoreData$업종대분류))
#업종자체가 표기되어 있지 않은 데이터 삭제(72개)

StoreData <- StoreData %>%
  filter(카드매출금액 != 0)
#카드매출금액이 아예 표기가 안된 행(R에서는 0으로 취급) 삭제

StoreData <- StoreData %>%
  filter(!(업종소분류 %in% c('웹툰/웹소설', '요식배달', '온라인게임', '대형 이커머스',
                      '온라인종합컨텐츠', '인터넷강의', '시험접수')))

#2019년 데이터가 없는 업종들 삭제

#터널/하이패스/유료도로 => 19년부터 데이터는 있으나 20년에 들어서 전국적으로 유료도로가 늘어남
#해외사용 => only 서울만 있음
#하나하나 다 찾아내기는 어려울 것 같아 분석하면서 이상한 점이 있으면 찾아서 확인해보는 것이 좋을듯



#대분류 회귀분석
#단순
StoreData_b <- StoreData %>%
  select(광역시도명, 업종대분류, 카드매출금액, 거리두기)

StoreData_b$광역시도명 <- as.factor(StoreData_b$광역시도명)
StoreData_b$업종대분류 <- as.factor(StoreData_b$업종대분류)
StoreData_b$거리두기 <- as.factor(StoreData_b$거리두기)

lm_storeb <- lm(카드매출금액 ~ ., data = StoreData_b)
summary(lm_storeb)

#lasso, ridge
onehot_store <- one_hot(as.data.table(StoreData_b))

store_x <- as.matrix(onehot_store %>% select(-'카드매출금액'))
store_y <- as.matrix(onehot_store %>% select('카드매출금액'))

store_lasso <- cv.glmnet(store_x, store_y, family = "gaussian")
store_ridge <- cv.glmnet(store_x, store_y, family = "gaussian", alpha = 0)

coef(store_lasso)
coef(store_ridge)


#중분류 회귀분석
StoreData_m <- StoreData %>%
  select(광역시도명, 업종중분류, 카드매출금액, 거리두기)

StoreData_m$광역시도명 <- as.factor(StoreData_m$광역시도명)
StoreData_m$업종중분류 <- as.factor(StoreData_m$업종중분류)
StoreData_m$거리두기 <- as.factor(StoreData_m$거리두기)

lm_storem <- lm(카드매출금액 ~ ., data = StoreData_m)
summary(lm_storem)
