### 앞에 my.r에서 Investment 데이터를 load하고 올 것.
require(tidyverse)
library(stringr)
library(mltools)
library(data.table)
library(glmnet)
# 데이터 --> KRX정보데이터시스템.
KOSPI = read.csv("KOSPI.csv")
KOSDAC = read.csv("KOSDAC.csv")

name = Investment$상품명

# 전처리에 필요한 정규표현식들
except = c('보통주','[1-9]?우선주','[(](신형)?(전환)?(주)?[)]',
           'Inc[.]',"^ ",'주식회사'," $")


# 정규표현식에 해당하는 주식 상품명을 전처리
for(i in 1:length(name)){
  if(str_detect(name[i],'기업인수목적')){
    name[i] = gsub('기업인수목적','스팩',name[i])
  }
  for(j in 1:length(except)){
    if(str_detect(name[i],except[j])){
      name[i] = gsub(except[j],"",name[i])
    }
  }
}

# 원데이터의 이름을 치환하고 메모리를 위해 name변수 삭제
Investment$상품명 = name
rm(name)


# KOSPI와 KOSDAC 행병합후 코스피,코스닥데이터삭제
Inc_list = rbind(KOSPI,KOSDAC) %>% select('종목명','업종명')
rm(KOSDAC);rm(KOSPI)


# Investment 업종명 left join
Invest_subset = as.data.frame(Investment %>% 
  left_join(Inc_list,by=c('상품명'='종목명')) %>% 
  select('상품명','업종명'))

# 업종명이 NA인 상품을 따로 저장함
NAStocks = Invest_subset %>% filter(is.na(업종명)) %>% select('상품명','업종명')

# 중복없이 업종명이 없는 상품명을 저장
NAStocks_name = unique(NAStocks$상품명)
length(NAStocks_name) 
#약 274개의 종목이 업종명과 join되지 않음 --> 
#영어의 한글표기 혹은 풀네임으로 적힌 종목

# 오류를 방지하기 위해 복제군을 생성후 처리
NAStocks_modified = NAStocks_name

#종목명을 최대한 맞추기 위해 Inc_data의 값에 유사하게 맞춤.
# ex)이화전기공업 의 경우 이화전기 가 detect되어서 값을 이화전기로 바꿈.
for(i in 1:length(NAStocks_modified)){
  for(j in nrow(Inc_list):1){
    if(str_detect(NAStocks_modified[i],Inc_list[j,1])){
      NAStocks_modified[i] = Inc_list[j,1]
    }
  }
}

#### 최종 274개의 수작업 확인 후 수정
NAStocks_processed =cbind(NAStocks_name,NAStocks_modified)
write.csv(NAStocks_processed,file='NeedtoPostprocess.csv')
NAStocks_processed = read.csv("NeedtoPostprocess.csv")
head(NAStocks_processed)
head(Investment)
for(i in 1:nrow(Invest_subset)){
  if(is.na(Invest_subset$업종명[i])){
    Invest_subset$업종명[i] = 
      as.character(NAStocks_processed %>% 
                     filter(NAStocks_name==Invest_subset$상품명[i]) %>% 
                     select('업종명'))
  }
}

#데이터 처리 --> 분석에 필요한 변수 설정 및 data type정렬
InvestData = Investment %>% select(-c('주문시간대','상품번호','거래소구분코드','주문구분코드',
                                      '총체결수량','총체결금액','상한가여부','하한가여부'))

# 최종 전처리된 업종분류와 데이터분석을 위한 InvestData병합
InvestData = cbind(InvestData,Invest_subset$업종명)
names(InvestData)[16] = '업종명'
InvestData[InvestData$업종명=='character(0)','업종명'] = '제약'
str(InvestData)



#-------------------------------#
#위에서 처리한 InvestData를 가져와 진행 
InvestData <- read.csv("InvestData.csv")


#범주형 변수들을 factor형으로 변환
InvestData$고객성별구분코드 <- as.factor(InvestData$고객성별구분코드)
InvestData$동일나이군구분코드 <- as.factor(InvestData$동일나이군구분코드)
InvestData$주소.시도. <- as.factor(InvestData$주소.시도.)
InvestData$업종명 <- as.factor(InvestData$업종명)
InvestData$매도매수구분코드 = as.factor(InvestData$매도매수구분코드)
InvestData['주문총금액'] = InvestData$실주문단가 * InvestData$주문수량
# 주문총금액 z score표준화
InvestData$주문총금액 = scale(InvestData$주문총금액) 
str(InvestData)
Invest_sell = InvestData %>% filter(매도매수구분코드==1) %>% select('고객성별구분코드','동일나이군구분코드','업종명','주문총금액')
Invest_buy = InvestData %>% filter(매도매수구분코드==2) %>% select('고객성별구분코드','동일나이군구분코드','업종명','주문총금액')

##주문수량에 대한 회귀분석

#고객성별구분코드, 동일나이군구분코드, 업종명만 독립변수로 사용

#단순회귀분석
Invest_sell_lm <- lm(주문총금액 ~고객성별구분코드 + 동일나이군구분코드 + 업종명, data = Invest_sell)
summary(Invest_sell_lm)
Invest_buy_lm =  lm(주문총금액 ~고객성별구분코드 + 동일나이군구분코드 + 업종명, data = Invest_buy)
summary(Invest_buy_lm)

#Lasso, Ridge

OnehotInvest_sell <- one_hot(as.data.table(Invest_sell))
OnehotInvest_buy <- one_hot(as.data.table(Invest_buy))

str(OnehotInvest_sell)
####매도 릿지 라쏘
Invest_x <- as.matrix(OnehotInvest_sell %>% select(-'주문총금액'))
Invest_y <- as.matrix(OnehotInvest_sell %>% select('주문총금액'))
str(Invest_x)
sell_lasso_c <- cv.glmnet(Invest_x, Invest_y, family = "gaussian")
sell_ridge_c <- cv.glmnet(Invest_x, Invest_y, family = "gaussian", alpha = 0)

coef(sell_lasso_c)
coef(sell_ridge_c)
###매수 릿지 라쏘
Invest_x <- as.matrix(OnehotInvest_buy %>% select(-'주문총금액'))
Invest_y <- as.matrix(OnehotInvest_buy %>% select('주문총금액'))

buy_lasso_c <- cv.glmnet(Invest_x, Invest_y, family = "gaussian")
buy_ridge_c <- cv.glmnet(Invest_x, Invest_y, family = "gaussian", alpha = 0)

coef(buy_lasso_c)
coef(buy_ridge_c)



##총 주문금액으로 매도매수 구분없이 회귀분석

#단순선형회귀
Invest_p_lm <- lm(주문총금액 ~ 고객성별구분코드 + 동일나이군구분코드 + 업종명, data = InvestData)
summary(Invest_p_lm)


#Lasso, Ridge
InvestData_p<- InvestData %>% select('고객성별구분코드', '동일나이군구분코드', '업종명', '주문총금액')



OnehotInvest <- one_hot(as.data.table(InvestData_p))

Invest_x <- as.matrix(OnehotInvest %>% select(-'주문총금액'))
Invest_y <- as.matrix(OnehotInvest %>% select('주문총금액'))

fit_lasso_p <- cv.glmnet(Invest_x, Invest_y, family = "gaussian")
fit_ridge_p <- cv.glmnet(Invest_x, Invest_y, family = "gaussian", alpha = 0)

coef(fit_lasso_p)
coef(fit_ridge_p)

