#######################각 데이터 불러오기
library(dplyr)
library(readxl)


######### working directory설정

#setwd(r'(C:\Users\hyeon\Desktop\Hyeon\Finance_competition\data)')
setwd("C:/Users/TaeyunKim/Documents/R_script/FInance_data")


# ===========1. 신한은행 데이터 로드=============

#거리두기 단계를 19년 3월, 9월은 0 / 20년 3월은 1 / 20년 9월과 21년 3월은 3으로 줌
shinhan1903 <- read_excel("신한은행201903.xlsx")
shinhan1903$거리두기 <- 0
shinhan1909 <- read_excel("신한은행201909.xlsx")
shinhan1909$거리두기 <- 0
shinhan2003 <- read_excel("신한은행202003.xlsx")
shinhan2003$거리두기 <- 1
shinhan2009 <- read_excel("신한은행202009.xlsx")
shinhan2009$거리두기 <- 3
shinhan2103 <- read_excel("신한은행202103.xlsx")
shinhan2103$거리두기 <- 3
shinhan <- rbind(shinhan1903, shinhan1909, shinhan2003, shinhan2009, shinhan2103)
shinhan <- as.data.frame(shinhan)
# 메모리 아끼기 위해서 나머지 변수 다 지움.
rm(shinhan1903); rm(shinhan1909); rm(shinhan2003); rm(shinhan2009);rm(shinhan2103)



# ===========2. 비씨카드 온라인 매출 데이터 로드=============
online <- read.csv("온라인 품목별 소비데이터.csv")

# ===========3. 한국 투자증권 데이터 로드=============
Investment = read_excel("동학개미운동 신규 고객 투자 정보.xlsx",sheet=1)
Investment = as.data.frame(Investment)

# ===========4. 서울시 가맹점 데이터 로드=============
StoreData = read_excel("광역시도별 업종별 가맹점 데이터.xlsx",sheet=1)
StoreData = as.data.frame(StoreData)
