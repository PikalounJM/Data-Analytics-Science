library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)
library(reshape2)
library(readxl)
library(openxlsx)

#포인트 데이터 불러오기
point <- read_excel('C:/Users/user/Desktop/point.xlsx')
View(point)

#고객 데이터 불러오기
df <- read_csv('C:/Users/user/Desktop/customer.csv')
df$CUST_TYPE[df$CUST_TYPE %in% c('SNS회원','콘도골프프렌즈','골프프렌즈','콘도프렌즈')] <- '프렌즈'
df$CUST_TYPE[df$CUST_TYPE %in% c('골프회원','콘도개인회원','콘도골프회원')] <- '개인회원'
df$CUST_TYPE[df$CUST_TYPE %in% c('크루(AQ)','크루(H&R)','크루(TTB)')] <- '임직원'
df <- df[!is.na(df$CUST_TYPE), ]

#df의 중복된 CUST_NO 제거 및 포인트 사용자 unqiue 고객 수
df <- df %>%
  distinct(CUST_NO, .keep_all = TRUE)

df_select <- df %>%
  select(CUST_NO, CUST_TYPE, CUST_GRAD_NM, SMS)

point_2 <- left_join(point, df_select, by='CUST_NO')
point_2 <- point_2[!(point_2$BRCH_NM=='백암온천' | point_2$BRCH_NM=='더테이스터블'),]
point_2$CUST_TYPE[is.na(point_2$CUST_TYPE)] <- '고객번호 없음(탈퇴자 등)'

write.xlsx(all, "C:/Users/user/Desktop/po.xlsx")

# CUST_NO별로 양수와 음수 포인트가 상쇄되는 경우 찾기
cancelled_ponts <- point_2 %>%
  group_by(CUST_NO) %>%
  filter(PONT != 0) %>% #0이 아닌 값 찾기
  mutate(abs_pont = abs(PONT)) %>% #PONT에 절대값 취하여 양수와 음수를 같은 값으로 취급
  semi_join(
    point_2 %>%
      filter(PONT != 0) %>%
      mutate(abs_pont = abs(PONT)),
    by = c("CUST_NO", "abs_pont")
  ) %>%
  group_by(CUST_NO, abs_pont) %>%
  filter(n_distinct(PONT) > 1) %>%
  ungroup() %>%
  select(CUST_NO, PONT)

# 상쇄되는 포인트를 가진 행을 제외
point_result <- point_2 %>%
  anti_join(cancelled_ponts, by = c("CUST_NO", "PONT"))

length(unique(result$CUST_NO))
point_result <- point_result[!(point_result$PONT < 0),]

write.xlsx(point_result, "C:/Users/user/Desktop/point_result.xlsx")

point_plus <- read_excel('C:/Users/user/Desktop/point_result.xlsx')
View(point_plus)
table(point_plus$OPERDVSN_DIV_NM)

golf <- point_plus[point_plus$OPERDVSN_DIV_NM=='골프',]
View(golf)

golf %>%
  group_by(DIV, CUST_TYPE) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  View()
  

#포인트 적립/사용 현황(EDA)
ggplot(result, aes(DIV)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  scale_x_discrete(limits=c("사용","적립"))
table(result$DIV)

#포인트 적립 -> 업장별 사용 현황
point_save <- result %>% filter(result$DIV=='적립')

table(point_save$LOC_GRP_NM)
ggplot(point_save, aes(OPERDVSN_DIV_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma)
ggplot(point_save, aes(LOC_GRP_NM, fill=LOC_GRP_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma)
ggplot(point_save, aes(BRCH_NM, fill=BRCH_NM)) + geom_bar() +
  scale_y_continuous(labels=scales::comma) + theme(axis.text.x=element_text(angle=45, hjust=1))

View(table(point_save$BRCH_NM))
jeju_point <- point_save %>% filter(point_save$BRCH_NM=='제주')
table(jeju_point$OPERDVSN_DIV_NM)

#포인트 적립 -> 고객유형별 사용 현황
point_save %>% group_by(CUST_TYPE, CUST_GRAD_NM) %>%
  summarise(freq=n()) %>% ungroup()
ggplot(point_save, aes(CUST_TYPE, fill=CUST_GRAD_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) + 
  scale_x_discrete(limits=c("프렌즈", "개인회원", "파트너", "임직원", "고객번호 없음(탈퇴자 등)"))


#포인트 적립 -> 사업장별 사용 현황
sul <- point_save %>% filter(point_save$BRCH_NM=='설악')
table(sul$LOC_NM)
ggplot(geo, aes(LOC_GRP_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma)
table(geo$LOC_NM)


#사용 포인트 구간
summary(point_save$PONT)
ggplot(point_save, aes(PONT)) + geom_histogram() + scale_x_continuous(labels=scales::comma) + 
  scale_y_continuous(labels=scales::comma) + 
  scale_x_continuous(breaks = seq(0, 5000, by = 500), limits = c(0, 5000))  

point_save %>% filter(point_save$PONT >= 500 & point_save$PONT <=1500)
View(point_save %>% filter(point_save$PONT == '172546'))
View(point_save %>%
  arrange(CUST_NO))

#현재 기준 잔여포인트 구간
length(unique(point_save$CUST_NO))
total <- point_save %>%
  group_by(CUST_NO) %>%
  filter(n_distinct(TOTAL_REMNDR_PONT) ==1 | TOTAL_REMNDR_PONT == min(TOTAL_REMNDR_PONT)) %>%
  distinct(CUST_NO, TOTAL_REMNDR_PONT, .keep_all = TRUE) %>%
  ungroup()

ggplot(total, aes(TOTAL_REMNDR_PONT)) + geom_histogram() + scale_x_continuous(labels=scales::comma) +
  scale_y_continuous(labels=scales::comma) + 
  scale_x_continuous(breaks = seq(0, 20000, by = 1000), limits = c(0, 20000))

total %>% filter(total$TOTAL_REMNDR_PONT >= 500 & total$TOTAL_REMNDR_PONT <=2500)
View(point_save)

#고객별 평균 적립 수
point_save %>%
  group_by(CUST_NO) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  summarise(mean_freq = mean(freq))

#객실 적립 X & 부대시설 적립 고객 비중
percentage <- point_save %>%
  summarize(
    '객실 투숙 O' = sum(!is.na(RSRV_NO)),
    '객실 투숙 X & 부대시설 적립' = sum(is.na(RSRV_NO))
  ) %>%
  pivot_longer(cols=everything(), names_to = 'CASE', values_to ='개수')

percentage

ggplot(percentage, aes(CASE, 개수, fill=CASE)) + geom_bar(stat="identity") + 
  scale_y_continuous(labels=scales::comma)
View(point_save[is.na(point_save$RSRV_NO), ])

no_condo <-point_save[is.na(point_save$RSRV_NO), ]
length(unique(no_condo$CUST_NO))

table(no_condo$BRCH_NM)

ggplot(no_condo, aes(BRCH_NM, fill=LOC_GRP_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma)

#객실 적립 X & 부대시설 적립 고객의 도착일자 분포 확인
View(no_condo)
no_condo$date <- format(as.Date(no_condo$BSN_DATE, format = "%Y%m%d"), "%Y-%m")
sum <- no_condo %>%
  group_by(date) %>%
  summarise(freq=n()) %>%
  ungroup()
A <- no_condo %>% filter(no_condo$date == '2024-06')

no_condo %>% filter(no_condo$SMS=='Y')
table(no_condo$SMS)
ggplot(no_condo, aes(SMS)) + geom_bar()

#설악 부대시설 적립처
jeju_no <- no_condo %>% filter(no_condo$BRCH_NM=='제주')
table(jeju_no$LOC_NM)

ggplot(jeju_no, aes(LOC_NM)) + geom_bar() + 
  scale_y_continuous(labels=scales::comma) + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip() +
  scale_x_discrete(limits=c("내츄럴 테라피", "제주 사우나", "제주 CC프런트"))


#포인트 사용 현황
point_use <- result %>% filter(result$DIV=='사용')
ggplot(point_use, aes(OPERDVSN_DIV_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) + 
  scale_x_discrete(limits=c("콘도","골프","아쿠아"))

length(unique(point_use$CUST_NO))

#고객유형별 사용 현황(고객번호별 중복 포함)
ggplot(point_use, aes(CUST_TYPE, fill = LOC_GRP_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  scale_x_discrete(limits=c("프렌즈","개인회원","파트너","임직원","고객번호 없음(탈퇴자 등)"))

View(point_use %>%
       group_by(CUST_GRAD_NM, LOC_GRP_NM) %>%
       summarise(freq=n()) %>%
       ungroup())


#등급별 사용현황(고객번호별 중복 포함)
table(point_use$CUST_GRAD_NM)
ggplot(point_use, aes(CUST_GRAD_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  scale_x_discrete(limits=c("Welcome","Silver","Gold","Platinum"))

View(point_use %>%
  group_by(CUST_GRAD_NM, CUST_TYPE) %>%
  summarise(freq=n()) %>%
  ungroup())

#사업본부별 사용 현황(고객번호별 중복 포함)
point_use <- point_use[!(point_use$BRCH_NM == "아쿠아플라넷 여수" | point_use$BRCH_NM == "아쿠아플라넷 제주"), ]
ggplot(point_use, aes(BRCH_NM, fill=BRCH_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.x=element_text(angle=45, hjust=1))
View(table(point_use$BRCH_NM))

point_use %>% 
  filter(point_use$BRCH_NM=='설악') %>%
  count(LOC_NM) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = "", y=n, fill=LOC_NM)) + 
  geom_bar(stat="identity") +
  coord_polar("y") + theme_void() + geom_text(aes(label=n), position=position_stack(vjust=0.5))

#업장별 count
View(point_use %>% 
  filter(point_use$BRCH_NM=='대천') %>%
  count(LOC_NM) %>%
  arrange(desc(n)))

#업장별 부대업장 category
ggplot(point_use, aes(BRCH_NM, fill=LOC_GRP_NM)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

point_use$date <- format(as.Date(point_use$BSN_DATE, format = "%Y%m%d"), "%Y-%m")
View(point_use %>%
  group_by(BRCH_NM, date) %>%
  summarise(freq=n()) %>%
  pivot_wider(names_from = date, values_from = freq))

point_use %>% filter(point_use$date == '2024-01')


#투숙 후, 적립/미적립 여부
room_point <- read_excel('C:/Users/user/Desktop/room_point.xlsx')
View(room_point)
table(room_point$`적립 여부`)

View(room_point[duplicated(room_point$'예약번호'), ])

room_point <- room_point %>%
  distinct(예약번호, .keep_all = TRUE)

ggplot(room_point, aes(`적립 여부`)) + geom_bar()

View(room_point[room_point$`예약유형(중분류)`=='비회원일반',])

using <- room_point[room_point$`적립 여부`== 'Y',]
ggplot(using, aes(`예약유형(중분류)`)) + geom_bar() + scale_y_continuous(labels=scales::comma)
View(using[using$`예약유형(중분류)`=='개인회원',])
table(friends$`예약유형(소분류)`)

ggplot(using, aes(`적립한 고객유형`)) + geom_bar() + scale_y_continuous(labels=scales::comma) +
  scale_x_discrete(limits=c("콘도개인회원",'콘도골프프렌즈','파트너'))
table(using$`적립한 고객유형`)
View(using[is.na(using$`적립한 고객유형`), ])
View(using)
