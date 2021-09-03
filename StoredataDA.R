##my.r에서 Store data load 하고 올 것

StoreData$거리두기 <- ifelse(StoreData$기준년월 %in% c(201903, 201909), 0,
                         ifelse(StoreData$기준년월 == 202003, 1, 3))

StoreData <- StoreData %>%
  filter(!is.na(StoreData$업종대분류))
#업종자체가 표기되어 있지 않은 데이터 삭제(72개)

StoreData <- StoreData %>%
  filter(카드매출금액 != 0)
#카드매출금액이 아예 표기가 안된 행(R에서는 0으로 취급) 삭제

StoreData <- StoreData %>%
  filter(!(업종소분류 %in% c('웹툰/웹소설', '요식배달', '온라인게임', '대형 이커머스',
                      '온라인종합컨텐츠', '인터넷강의', '시험접수')))
#2020년만 있고 2019년 데이터가 없는 업종들 삭제

#회귀분석에 필요한 변수들 추출
StoreData_b <- StoreData %>%
  select(광역시도명, 업종대분류, 카드매출금액, 거리두기)

#범주형 변수 factor 변환
StoreData_b$광역시도명 <- as.factor(StoreData_b$광역시도명)
StoreData_b$업종대분류 <- as.factor(StoreData_b$업종대분류)
StoreData_b$거리두기 <- as.factor(StoreData_b$거리두기)

#단순회귀분석
lm_store <- lm(카드매출금액 ~ ., data = StoreData_b)
summary(lm_store)

#교차항 추가
lm_store_inter <- lm(카드매출금액 ~ .+업종대분류*거리두기, data = StoreData_b)
summary(lm_store_inter)