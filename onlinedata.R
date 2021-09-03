###my.r에서 online data load하고 올 것

online <- online %>%
  filter(품목중분류명 != '다이어트')
#품목중분류 중 2019년 데이터가 없는 '다이어트' 행 제거
#나머지 품목들은 모두 2019년 3월 ~ 2021년 3월까지 데이터 존재

#거리두기 변수 추가
online$거리두기 <- ifelse(online$기준년월 %in% c(201903, 201909), 0, 
                      ifelse(online$기준년월 == 202003, 1, 3))

#회귀분석에 사용할 변수 추출
online <- online %>%
  select(품목대분류명, 성별, 연령, 고객소재지_시군구, 매출금액, 거리두기)

#지역을 서울로 한정
online <- online %>%
  filter(!(online$고객소재지_시군구 %in% c('용인시', '수원시', '고양시', '서구', '남동구', '부평구', '강화군', '계양구', '성남시', '미추홀구', '연수구', '동구', '옹진군')))

#범주형 변수 factor 변환
online$품목대분류명 <- factor(online$품목대분류명)
online$성별 <- factor(online$성별)
online$연령 <- factor(online$연령)
online$고객소재지_시군구 <- factor(online$고객소재지_시군구)
online$거리두기 <- factor(online$거리두기)


#단순회귀분석
lm_online <- lm(매출금액 ~ ., data = online)
summary(lm_online)

#교차항 추가
lm_online_inter <- lm(매출금액 ~ . + 성별 * 거리두기 + 연령 * 거리두기 +
                            품목대분류명 * 거리두기, data = online)
summary(lm_online_inter)
