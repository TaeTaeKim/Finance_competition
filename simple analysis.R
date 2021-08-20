###온라인 소비데이터에 대한 일차원 분석

#기준년월에 따른 품목별 매출금액 총합

sales_sum <- online %>%
  group_by(품목중분류명, 기준년월) %>%
  summarise(매출금액 = sum(매출금액))
sales_sum <- as.data.frame(online_sum)

#기타결제, 기타교육비, 디지털, 레저, 신선요리재료, 취미/특기 등이 증가세를 보임


#코로나 전후로 데이터를 나눈 후 
#매출건수 상위 10개 품목 비교
online_bc <- online[online$기준년월 %in% c(201903, 201909), ]
online_ac <- online[online$기준년월 %in% c(202003, 202009, 202103), ]

count_bc <- online_bc %>%
  group_by(품목중분류명) %>%
  summarise(매출건수 = sum(매출건수))
count_bc <- as.data.frame(count_bc)
count_bc[order(count_bc$매출건수, decreasing = TRUE), ]
count_bc10 <- count_bc[order(count_bc$매출건수, decreasing = TRUE), ] %>%
  head(10)
rm(count_bc)

count_ac <- online_ac %>%
  group_by(품목중분류명) %>%
  summarise(매출건수 = sum(매출건수))
count_ac <- as.data.frame(count_ac)
count_ac10 <- count_ac[order(count_ac$매출건수, decreasing = TRUE), ] %>%
  head(10)
rm(count_ac)

count_bc10
count_ac10
#신선/요리재료, 가공식품,건강식품이 증가하고 여행이 감소함



###주식투자 데이터에 대한 일차원분석

#전처리한 데이터를 가져옴
InvestData <- read.csv("InvestData.csv")

#사람들이 가장 많이 투자한 상위 10개 업종
invest_count <- InvestData %>%
  group_by(업종명) %>%
  summarise(주문수량 = sum(주문수량))
invest_count <- as.data.frame(invest_count)

invest_count10 <- invest_count[order(invest_count$주문수량, decreasing = TRUE), ] %>%
  head(10)
rm(invest_count)



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
sales_sum %>%
  filter(매출변화 < 0) %>%
  arrange(., 매출변화_퍼센트) %>%
  head(10)

#총매출변화 증가 상위 10개 업종
sales_sum %>%
  filter(매출변화 > 0) %>%
  arrange(desc(), 매출변화_퍼센트) %>%
  head(10)



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

sales_mean$매출변화 <- (sales_mean$점당평균매출_ac - sales_mean$점당평균매출_bc)
sales_mean$매출변화_퍼센트 <- round(sales_mean$매출변화 / sales_mean$점당평균매출_bc, 4)*100

#점당평균매출 감소 상위 10개 업종
sales_mean %>%
  filter(매출변화 < 0) %>%
  arrange(., 매출변화_퍼센트) %>%
  head(10)

#점당평균매출 증가 상위 10개 업종
sales_mean %>%
  filter(매출변화 > 0) %>%
  arrange(desc(), 매출변화_퍼센트) %>%
  head(10)
