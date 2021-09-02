library(ggplot2)

#shinhan data
sh <- shinhan_lm_cross2[["coefficients"]]

write.csv(sh, file = 'shinhan_coef.csv', fileEncoding = "UTF-8")
sh <- read.csv("shinhan_coef.csv")
names(sh) <- c('변수', '계수')

sh_age1 <- sh[36:40, ]
sh_age3 <- sh[41:45, ]
sh_sex <- sh[46:47, ]
sh_age <- rbind(sh_age1, sh_age3)

ggplot(sh_age1, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 1단계 연령 교차항 계수")

ggplot(sh_age3, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 3단계 연령 교차항 계수")

ggplot(sh_age, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 연령에 따른 교차항 계수") +
  scale_x_discrete(limits = c("나이3:거리두기1", "나이4:거리두기1", "나이5:거리두기1",
                              "나이6:거리두기1", "나이7:거리두기1",
                              "나이3:거리두기3", "나이4:거리두기3", "나이5:거리두기3",
                              "나이6:거리두기3", "나이7:거리두기3"))

ggplot(sh_sex, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', width = 0.3) +
  xlab("") + ylab("") + 
  ggtitle("거리두기 성별에 따른 교차항 계수")


#online data
on <- coef(lm_online_inter2)

write.csv(on, "online_coef.csv", fileEncoding = "UTF-8")

on <- read.csv("online_coef.csv", fileEncoding = "UTF-8")
names(on) <- c('변수', '계수')

on$변수 <- gsub("고객소재지_시군구", "", on$변수)
on$변수 <- gsub("품목대분류명", "", on$변수)
on$변수 <- gsub("성별", "", on$변수)
on$변수 <- gsub("연령", "", on$변수)


on_sex <- on[46:47, ]
on_age <- on[48:57, ]
on_age1 <- on[48:52, ]
on_age3 <- on[53:57, ]

ggplot(on_sex, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', width = 0.3) +
  xlab("") + ylab("") + 
  ggtitle("거리두기 성별에 따른 교차항 계수")

ggplot(on_age, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 연령에 따른 교차항 계수") +
  scale_x_discrete(limits = c("20세 미만:거리두기1", "30대:거리두기1", "40대:거리두기1",
                              "50대:거리두기1", "60대 이상:거리두기1", 
                              "20세 미만:거리두기3", "30대:거리두기3", "40대:거리두기3",
                              "50대:거리두기3", "60대 이상:거리두기3"))

ggplot(on_age1, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 1단계 연령에 따른 교차항 계수")

ggplot(on_age3, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 3단계 연령에 따른 교차항 계수")

##유의미한 계수만
#신한은 다 유의미함
on_sex_sig <- on[47, ]
on_age_sig <- on[c(49, 50, 53:56), ]

ggplot(on_sex_sig, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', width = 0.2) +
  xlab("") + ylab("") + 
  ggtitle("거리두기 성별에 따른 교차항 계수")


ggplot(on_age_sig, aes(x = 변수, y = 계수)) + 
  geom_bar(stat = 'identity', fill = 'steelblue')+
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("거리두기 연령에 따른 교차항 계수") +
  scale_x_discrete(limits = c('50대:거리두기3','40대:거리두기3', '30대:거리두기3', 
                              '20세 미만:거리두기3', '40대:거리두기1', '30대:거리두기1'))
