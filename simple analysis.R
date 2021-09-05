###온라인 소비데이터에 대한 일차원 분석

#기준년월에 따른 품목별 매출금액 총합

sales_sum <- online %>%
  group_by(품목중분류명, 기준년월) %>%
  summarise(매출금액 = sum(매출금액))
sales_sum <- as.data.frame(sales_sum)

sales_sum$품목중분류명 = as.factor(sales_sum$품목중분류명)

sales_sum$매출금액 = log(sales_sum$매출금액)  
head(sales_sum)
p = ggplot(data = sales_sum,mapping = aes(factor(기준년월),매출금액,color = 품목중분류명))
p+geom_line()+labs(title = "온라인품목별 매출변화")
#기타결제, 기타교육비, 디지털, 레저, 신선요리재료, 취미/특기 등이 증가세를 보임

#코로나 전후로 데이터를 나눈 후 
#매출금액 상위 10개 품목 비교

online_bc <- online[online$기준년월 %in% c(201903, 201909), ]
online_ac <- online[online$기준년월 %in% c(202003, 202009, 202103), ]

sales_bc <- online_bc %>%
  group_by(품목중분류명) %>%
  summarise(매출금액 = sum(매출금액))
sales_bc <- as.data.frame(sales_bc)
sales_bc10 <- sales_bc %>%
  arrange(desc(), 매출금액) %>%
  head(10)

sales_ac <- online_ac %>%
  group_by(품목중분류명) %>%
  summarise(매출금액 = sum(매출금액))
sales_ac <- as.data.frame(sales_ac)
sales_ac10 <- sales_ac %>%
  arrange(desc(), 매출금액) %>%
  head(10)
rm(count_ac)

sales_bc10$매출금액평균 <- sales_bc10$매출금액 / 2  
sales_ac10$매출금액평균 <- sales_ac10$매출금액 / 3
#코로나 전은 두달치, 코로나 후는 세달치여서 평균으로 비교

sales_bc10
sales_ac10


###주식투자 데이터에 대한 일차원분석

#전처리한 데이터를 가져옴
InvestData <- read.csv("InvestData.csv")

#사람들이 가장 많이 투자한 상위 10개 업종 - 매수에서 주문총금액 상위 10개
InvestData['주문총금액'] = InvestData$실주문단가 * InvestData$주문수량
Invest_sell = InvestData %>% filter(매도매수구분코드==1) %>% select('고객성별구분코드','동일나이군구분코드','업종명','주문총금액')
Invest_buy = InvestData %>% filter(매도매수구분코드==2) %>% select('고객성별구분코드','동일나이군구분코드','업종명','주문총금액')

buy_sum <- Invest_buy %>%
  group_by(업종명) %>%
  summarise(주문총금액 = sum(주문총금액))

buy_sum <- as.data.frame(buy_sum)

buy_sum10 <- buy_sum %>%
  arrange(desc(), 주문총금액) %>%
  head(10)

rm(buy_sum)


###신한카드 가맹점 데이터에 대한 일차원 분석

#코로나 전후로 데이터를 나눈 후
#총매출금액의 변화 

store_bc <- StoreData[StoreData$기준년월 %in% c(201903, 201909), ]
store_ac <- StoreData[StoreData$기준년월 %in% c(202003, 202009), ]

sales_type_bc <- store_bc %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류) %>%
  summarise(카드총매출_bc = sum(카드매출금액))

sales_type_bc <- as.data.frame(sales_type_bc)
sales_type_bc

sales_type_ac <- store_ac %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류) %>%
  summarise(카드총매출_ac = sum(카드매출금액))

sales_type_ac <- as.data.frame(sales_type_ac)
sales_type_ac

sales_sum <- sales_type_bc %>%
  full_join(sales_type_ac, by = '업종중분류')

rm(sales_type_bc, sales_type_ac)

sales_sum$매출변화 <- (sales_sum$카드총매출_ac - sales_sum$카드총매출_bc)
sales_sum$매출변화_퍼센트 <- round(sales_sum$매출변화 / sales_sum$카드총매출_bc, 4)*100

#총매출변화 감소 상위 10개 업종
minus10 <- sales_sum %>%
  filter(매출변화 < 0) %>%
  arrange(., 매출변화_퍼센트) %>%
  head(10) %>%
  select(업종중분류, 매출변화, 매출변화_퍼센트)

#총매출변화 증가 상위 10개 업종
plus10 <- sales_sum %>%
  filter(매출변화 > 0) %>%
  arrange(desc(), 매출변화_퍼센트) %>%
  head(10) %>%
  select(업종중분류, 매출변화, 매출변화_퍼센트)

#음반/음원/영상의 온라인종합컨텐츠 삭제
#터널/유료도로/하이패스 삭제
#해외사용 삭제
str(StoreData)
StoreData <- StoreData %>%
  filter(!(업종소분류 == '온라인종합컨텐츠')) %>%
  filter(!(업종중분류 == '터널/유료도로/하이패스')) %>%
  filter(!(업종중분류 == '해외사용'))

#점당평균매출의 변화
sales_mean_bc <- store_bc %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류) %>%
  summarise(점당평균매출_bc = mean(점당매출금액))

sales_mean_bc <- as.data.frame(sales_mean_bc)
sales_mean_bc

sales_mean_ac <- store_ac %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류) %>%
  summarise(점당평균매출_ac = mean(점당매출금액))

sales_mean_ac <- as.data.frame(sales_mean_ac)
sales_mean_ac

sales_mean <- sales_mean_bc %>%
  full_join(sales_mean_ac, by = '업종중분류')

rm(sales_mean_bc, sales_mean_ac)

sales_mean$평균매출변화 <- (sales_mean$점당평균매출_ac - sales_mean$점당평균매출_bc)
sales_mean$평균매출변화_퍼센트 <- round(sales_mean$평균매출변화 / sales_mean$점당평균매출_bc, 4)*100

#점당평균매출 감소 상위 10개 업종
sales_mean %>%
  filter(평균매출변화 < 0) %>%
  arrange(., 평균매출변화_퍼센트) %>%
  head(10) %>%
  select(업종중분류, 평균매출변화, 평균매출변화_퍼센트)

#점당평균매출 증가 상위 10개 업종
sales_mean %>%
  filter(평균매출변화 > 0) %>%
  arrange(desc(), 평균매출변화_퍼센트) %>%
  head(10) %>%
  select(업종중분류, 평균매출변화, 평균매출변화_퍼센트)
