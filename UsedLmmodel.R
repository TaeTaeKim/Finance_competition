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




lm_online_inter2 <- lm(매출금액 ~ . + 성별 * 거리두기 +
                            연령 * 거리두기+품목대분류명*거리두기, data = online)



summary(lm_online_inter2)


unique(StoreData$업종대분류)
