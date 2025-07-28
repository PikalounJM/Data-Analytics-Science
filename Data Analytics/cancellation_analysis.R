library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)
library(reshape2)
library(readxl)
library(openxlsx)
library(lubridate)

#취소데이터
cancel_23 <- read_excel('C:/Users/user/Desktop/cancel_23.xlsx')
cancel_24 <- read_excel('C:/Users/user/Desktop/cancel_24.xlsx')
cancel_23 <- cancel_23[!(names(cancel_23) %in% c("...29"))] 

all <- rbind(cancel_23, cancel_24)
View(all)

View(table(all$`예약유형 중분류명`))
nolive <- all[is.na(all$고객번호),]
length(unique(all$고객번호)) 

View(all)

#단체, 임직원 예약유형은 제외
all <- subset(all, !(`예약유형 중분류명` %in% c('성인단체','연회', '외국단체','직원이용','학생단체')))
View(sort(table(all$`고객 유형`)))

#고객유형 중 크루, 기타 제외
all <- subset(all, !(`고객 유형` %in% c('(주)티몬','한국지역난방공사', '크루(AQ)','크루(H&R)','크루(TTB)')))
table(all$`고객 유형`)

View(all[is.na(all$`가입 일자`),])

#결측치, 이상치 파악
all <- all[!is.na(all$`객실 수`),] #오른쪽으로 밀린 데이터 행(14) 제거

#중복데이터 제거 및 4박 이하의 취소데이터만 고려
#1~4박 데이터가 전체 데이터의 약 98% 이상 차지
all <- unique(all)
all <- all[all$`박 수`<=4,]

#평균적으로 취소는 언제 일어나는가?
View(all)
ggplot(all, aes(`취소일-예약일`)) + geom_histogram()
sum(all$`취소일-예약일`==0)

all <- read_excel('C:/Users/user/Desktop/all_data.xlsx')
all <- all[!all$`취소일-예약일`<0,]

#write.xlsx(all, "C:/Users/user/Desktop/all_data.xlsx")

#최종 전처리 데이터 불러오기
all <- read_excel('C:/Users/user/Desktop/all_data.xlsx')
all <- all[!all$`취소일-예약일`<0,]
all <- all %>% select(-`도착일-예약일(리드타임)`, -`예약 주기`)

#평균적으로 도착일자 대비 예약은 언제 하는가?
all <- all %>%
  mutate('LEADTIME'= as.numeric(as.Date(all$`도착 일자`) - as.Date(all$`예약 일자`)))

ggplot(all, aes(`LEADTIME`)) + geom_histogram(stat='count') + xlim(0,100) + theme_minimal()
ggplot(all, aes(`LEADTIME`)) + geom_histogram(stat='count') + xlim(0,90) + theme_minimal()
ggplot(all, aes(`LEADTIME`)) + geom_histogram(stat='count') + xlim(0,60) + theme_minimal()
ggplot(all, aes(`LEADTIME`)) + geom_histogram(stat='count') + xlim(0,30) + theme_minimal()

sum(all$LEADTIME==0) 
sum(all$LEADTIME<=30) 
sum(all$LEADTIME>=60)

summary(all$LEADTIME) #중앙값: 38일

#평균 예약 박수는?
table(all$`박 수`)

#평균적으로 취소는 언제 일어나는가?
#예약일 기준(최초 예약하고 언제 취소하는가?)
ggplot(all, aes(`취소일-예약일`)) + geom_freqpoly()
sum(all$`취소일-예약일`== 0) 
sum(all$`취소일-예약일`<= 3) 
sum(all$`취소일-예약일` <= 30) 
sum(all$`취소일-예약일` <= 60) 
sum(all$`취소일-예약일` > 60) 

summary(all$`취소일-예약일`)

ggplot(all, aes(`취소일-예약일`)) + geom_freqpoly() + xlim(0,20)

#도착일 기준(도착일 전 언제 취소하는가?)
summary(all$`도착일-취소일`)

sum(all$`도착일-취소일`==0) 
sum(all$`도착일-취소일`<=7) 
sum(all$`도착일-취소일`<=14) 
sum(all$`도착일-취소일`<=21) 

ggplot(all, aes(`도착일-취소일`)) + geom_freqpoly() + xlim(0,100)
ggplot(all, aes(`도착일-취소일`)) + geom_freqpoly() + xlim(0,60)
ggplot(all, aes(`도착일-취소일`)) + geom_freqpoly() + xlim(0,30)
ggplot(all, aes(`도착일-취소일`)) + geom_freqpoly() + xlim(0,14)


#지불한 객실료
summary(all$객실료)

#ADR 기준
all <- all %>%
  mutate('ADR' = `객실료` / `박 수`)

summary(all$ADR)

#전체 취소 고객 수(고객번호 중 '결측값' 제외)
all$`고객 유형`[all$`고객 유형` %in% c('골프프렌즈','콘도골프프렌즈','콘도프렌즈')] <- '프렌즈'
all$`고객 유형`[all$`고객 유형` %in% c('골프회원','콘도개인회원','콘도골프회원')] <- '개인회원'

all <- all[!is.na(all$고객번호),] #고객번호 없는 비회원(9,604명)

View(table(all$예약유형명))

#전체 취소데이터 현황
all$`취소 일자` <- as.Date(all$`취소 일자`, format = "%Y%m%d")
all$`취소 일자` <- format(all$`취소 일자`, "%Y-%m")
all$`취소 일자` <- as.Date(paste0(all$`취소 일자`, "-01"), format = "%Y-%m-%d")

#총 취소 고객 수
length(unique(all$고객번호))
sum(is.na(all$고객번호))

#월별 취소 건 수
all %>%
  group_by(`취소 일자`) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  ggplot(aes(`취소 일자`, freq)) + geom_line() + theme(axis.text = element_text(angle = 45, hjust=1)) +
  scale_x_date(date_breaks = '1 month', date_labels = "%y-%m") + theme_minimal()


#월별 취소 고객 수
all %>%
  group_by(`취소 일자`) %>%
  summarise(unique_customer = n_distinct(고객번호)) %>%
  ungroup() %>%
  ggplot(aes(`취소 일자`, unique_customer)) + 
  geom_line(size=1, color='orange') + scale_x_date(date_breaks = '1 month', date_labels = "%y-%m") + 
  theme(axis.text = element_text(angle = 45, hjust=1))


#월별 취소 객실 수
all %>%
  group_by(`취소 일자`) %>%
  summarize(sum_room = sum(`객실 수`, na.rm = TRUE)) %>%
  ungroup() %>%
  View()
  ggplot(aes(`취소 일자`, sum_room)) + geom_bar(stat='identity') +
  scale_x_date(date_breaks = '1 month', date_labels="%y-%m") + theme_minimal() 

#월별 취소매출액
all %>%
  group_by(`취소 일자`) %>%
  summarise(count = n(),
            sales_sum = sum(객실료)) %>%
  View()

#사업본부별 총 취소 건 수
all %>%
  group_by(`영업장`) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  ggplot(aes(reorder(`영업장`, -freq), freq)) + geom_col(fill='dark blue', alpha=0.7) + 
  geom_text(aes(label=freq), vjust=-0.5) + theme_minimal() + theme(axis.title.x = element_blank())

#사업본부별 월별 취소 고객 수
all %>%
  group_by(`영업장`, `취소 일자`) %>%
  summarise(count = n(),
            unique_customer = n_distinct(고객번호)) %>%
  View()

#사업본부별 월별 취소 객실 수
all %>%
  group_by(`영업장`, `취소 일자`) %>%
  summarise(count=n(),
            room_sum = sum(`객실 수`, na.rm=TRUE)) %>%
  ungroup() %>%
  ggplot(aes(`취소 일자`, room_sum)) + geom_line() + facet_wrap(~`영업장`, ncol=3) +
  scale_x_date(date_breaks = '2 month', date_labels = "%y-%m") +
  theme(axis.text = element_text(angle = 45, hjust=1))

View(all[all$`고객 유형`=='파트너',])

View(all[all$`고객 유형`=='콘도법인회원',])
