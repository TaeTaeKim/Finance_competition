# 대분류로 재범주화 코드
transit = c('운수장비','운송장비·부품','운수창고업','운송')
electro = c('전기전자','반도체','IT부품','일반전기전자')
health = c('제약','의료정밀','의약품','의료·정밀기기')
mineral = c('철강금속','금속','비금속광물','비금속','광업')
finance = c('금융','보험','기타금융','증권','은행')
service = c('서비스업','기타서비스','컴퓨터서비스')
distribution = c("유통",'유통업')
culture = c('디지털컨텐츠','방송서비스','오락·문화')
food = c('음식료품','음식료·담배','숙박·음식')
communi = c("통신장비",'정보기기','통신업','통신서비스','인터넷')
mechanic = c('기계','기계·장비')
paper = c('출판·매체복제','종이·목재','종이목재')
socialService = c("전기가스업",'전기·가스·수도')
clothes =c('섬유의복','섬유·의류')
structure = c("건설업",'건설')
manufacture = c("기타제조업",'기타제조')


bigcate = c("운수",'전기전자','의료','금속·광물',
            '금융','서비스','유통','오락·문화',
            '숙박·음식','통신','기계','종이·출판',
            '사회서비스','섬유·의류','건설','제조업')
recategorylist = list(transit,electro,health,mineral,finance,service,distribution,
                      culture,food,communi,mechanic,paper,socialService,clothes,
                      structure,manufacture)
InvestData['대분류'] = 'temp'

for(i in 1:length(recategorylist)){
  for(j in 1:length(recategorylist[[i]])){
    InvestData[InvestData$업종명==recategorylist[[i]][j],'대분류'] = bigcate[i]
  }
}
InvestData[InvestData$업종명=="화학",'대분류'] = '화학'
InvestData[InvestData$업종명=="소프트웨어",'대분류'] = '소프트웨어'
InvestData[InvestData$업종명=="농업, 임업 및 어업",'대분류'] = '1차산업'
str(InvestData)


InvestData$대분류 <- as.factor(InvestData$대분류)