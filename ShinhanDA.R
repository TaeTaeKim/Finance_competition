###my.r에서 shinhan data load하고 올 것

shinhan_d <- shinhan %>%
  filter(총소비금액 != 0) %>%
  select(지역구, 나이, 성별, 직장인여부, 총소비금액, 총수신금액, 거리두기)
#신한데이터에서 총소비금액이 0인 행 제외(246787건)

#범주형 변수 factor 변환
shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)
shinhan_d$거리두기 = as.factor(shinhan_d$거리두기)
rm(shinhan)
str(shinhan_d)


#단순선형회귀
lm_shinhan <- lm(총소비금액 ~ ., data = shinhan_d)
summary(lm_shinhan)

#교차항을 넣은 선형회귀
lm_shinhan_inter <- lm(총소비금액 ~ 지역구*거리두기 + 나이*거리두기 + 성별*거리두기 +
                              직장인여부*거리두기 + 총수신금액*거리두기, data = shinhan_d)
summary(lm_shinhan_inter)