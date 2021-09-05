### 분석전 필요 library 불러오기 (여기다 모든 라이브러리 불러와줘)
library(dplyr)
library(readxl)

## working directory설정

#setwd(r'(C:\Users\hyeon\Desktop\Hyeon\Finance_competition\data)')
#setwd("C:/Users/hc07c/Documents/R/공모전데이터/")


# =================================== 1. 데이터 로드 ===================================

#신한은행 데이터 로드
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
#메모리 위해서 나머지 변수 제거
rm(shinhan1903); rm(shinhan1909); rm(shinhan2003); rm(shinhan2009);rm(shinhan2103)


#비씨카드 온라인 매출 데이터 로드
online <- read.csv("온라인 품목별 소비데이터.csv")



# =================================== 2. 신한은행 데이터 분석 ===================================


# =================================== 3. 비씨카드 데이터 분석 ===================================


# =================================== 4. 예측 모델링 ===================================


# =================================== 5. 시각화? ===================================