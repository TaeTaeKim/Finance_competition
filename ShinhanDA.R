###my.r에서 shinhan data load하고 올 것

# -----------------1. 데이터 전처리 및 가공

#신한데이터에서 총소비금액이 0인 행 제외(246787건)
shinhan_d <- shinhan %>%
  filter(총소비금액 != 0) %>%
  select(지역구, 나이, 성별, 직장인여부, 총소비금액, 총수신금액, 거리두기)


#범주형 변수 factor 변환
shinhan_d$지역구 <- as.factor(shinhan_d$지역구) 
shinhan_d$나이 <- as.factor(shinhan_d$나이) 
shinhan_d$성별 <- as.factor(shinhan_d$성별)
shinhan_d$직장인여부 <- as.factor(shinhan_d$직장인여부)
shinhan_d$거리두기 = as.factor(shinhan_d$거리두기)
rm(shinhan)
str(shinhan_d)

# -----------------2. 기초 통계량 분석

summary(shinhan_d)

# 성별 소비금액 박스플랏
ggplot(data = shinhan_d,mapping = aes(x=성별,y = 총소비금액))+
  geom_boxplot()+
  ggtitle("성별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('남자','여자'))

# 연령별 소비금액 박스플랏
ggplot(data = shinhan_d,mapping = aes(x=나이,y = 총소비금액))+
  geom_boxplot()+
  ggtitle("연령별 소비금액")+xlab("")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "darkblue"))+
  scale_x_discrete(labels =c('20대','30대','40대','50대','60대','70대'))

#성별 총소비금액 기초통계 
summary(shinhan_d[shinhan_d$성별==1,'총소비금액'])
summary(shinhan_d[shinhan_d$성별==2,'총소비금액'])
#연령별 총소비금액 기초통계
for(i in 2:7){
  print(summary(shinhan_d[shinhan_d$나이==i,'총소비금액']))
}
# -----------------2. 분산분석

#회귀 모델 생성전 거리두기, 성별, 연령과 지출금액의 분산분석
shinhan_aov_age = aov(총소비금액~나이*거리두기,data=shinhan_d)
shinhan_aov_sex = aov(총소비금액~성별*거리두기,data=shinhan_d)
summary(shinhan_aov_age)
summary(shinhan_aov_sex)

# -----------------3. 회귀분석
#단순선형회귀
lm_shinhan <- lm(총소비금액 ~ ., data = shinhan_d)
summary(lm_shinhan)

#교차항을 넣은 선형회귀
lm_shinhan_inter <- lm(총소비금액 ~ 지역구*거리두기 + 나이*거리두기 + 성별*거리두기 +
                              직장인여부*거리두기 + 총수신금액*거리두기, data = shinhan_d)
summary(lm_shinhan_inter)
