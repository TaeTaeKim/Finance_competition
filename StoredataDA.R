StoreData$거리두기 <- ifelse(StoreData$기준년월 %in% c(201903, 201909), 0,
                         ifelse(StoreData$기준년월 == 202003, 1, 3))

#업종에 따른 카드매출금액 총액
sales_type <- StoreData %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류, 기준년월) %>%
  summarise(카드총매출 = sum(카드매출금액))

sales_type <- as.data.frame(sales_type)
sales_type
View(sales_type)

#매출이 감소한 업종 : 다단계, 대중교통, 면세점, 문구용품, 미용/사우나/마사지,
#보건소, 부페, 여행사, 예술품, 웨딩, 유흥주점, 의류, 자동차정비, 전시/관람/체험,
#주점 및 주류판매, 컴퓨터소프트웨어, 한식, 항공사, 휴게소, 
#매출이 증가한 업종 : 동물병원, 상품권, 식품, 여객선, 온라인, 음반/음원/영상, 자동차,
#전기/가전, 전문용역서비스, 터널/유료도로/하이패스, 한의학, 해외사용, 화물운송업, 


#지역에 따른 업종별 카드매출금액 총액
sales_region <- StoreData %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(광역시도명, 업종중분류, 기준년월) %>%
  summarise(카드총매출 = sum(카드매출금액))

sales_region <- as.data.frame(sales_region)
sales_region


#업종별 점당 매출 평균
sales_type_mean <- StoreData %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(업종중분류, 기준년월) %>%
  summarise(점당평균매출 = mean(점당매출금액))

sales_type_mean <- as.data.frame(sales_type_mean)
sales_type_mean


#지역에 따른 업종별 점당 매출 평균
sales_region_mean <- StoreData %>% 
  filter(!is.na(업종대분류)) %>%
  group_by(광역시도명,업종중분류, 기준년월) %>%
  summarise(점당평균매출 = mean(점당매출금액))

sales_region_mean <- as.data.frame(sales_region_mean)
sales_region_mean

