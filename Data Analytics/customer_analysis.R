library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)
library(reshape2)
library(lubridate)

#데이터 불러오기
df <- read_csv('C:/Users/user/Desktop/customer.csv')

#컬럼명 확인
names(df)
dim(df) #행/컬럼 수
length(df) #컬럼 수
df <- df[!duplicated(df$CUST_NO), ] #고객번호 중복값 제거
sum(duplicated(df$CUST_NO))

#중복 유형 합치기
df$CUST_TYPE[df$CUST_TYPE %in% c('SNS회원','콘도골프프렌즈','골프프렌즈','콘도프렌즈')] <- '프렌즈'
df$CUST_TYPE[df$CUST_TYPE %in% c('골프회원','콘도개인회원','콘도골프회원')] <- '개인회원'
df$CUST_TYPE[df$CUST_TYPE %in% c('크루(AQ)','크루(H&R)','크루(TTB)')] <- '임직원'

View(table(df$CUST_TYPE))
df <- df[!is.na(df$CUST_TYPE), ] #결측치 행 추출

membership <- subset(df, (CUST_TYPE %in% c('골프회원','콘도개인회원','콘도골프회원')))
View(table(membership$CUST_TYPE))

View(df)
#고객 유형별 비중
ggplot(df, aes(CUST_TYPE, fill=CUST_TYPE)) + geom_bar() + 
  scale_fill_brewer(palette='Greens')+ scale_y_continuous(labels=scales::comma)+theme(axis.title.x = element_blank())

head(df)

#등급별 활동점수 분포
sum(is.na(df$CUST_GRAD_PONT))
df$CUST_GRAD_PONT <- ifelse(is.na(df$CUST_GRAD_PONT), 0, df$CUST_GRAD_PONT)
filter <- df[df$CUST_GRAD_PONT >= 0, ]
ggplot(filter, aes(CUST_GRAD_NM, CUST_GRAD_PONT)) + geom_boxplot() + ylim(0,2500)
summary(filter$CUST_GRAD_PONT)

table(filter$CUST_GRAD_NM)

pivot_table <- filter %>%
  group_by(CUST_GRAD_NM, CUST_TYPE) %>%
  summarise(freq=n()) %>%
  ungroup()

pivot_table <- filter %>%
  group_by(CUST_GRAD_NM, CUST_TYPE) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = CUST_TYPE, 
    values_from = freq,
    values_fill = list(freq = 0) # NA 값 대신 0으로 채움
  )


#각 등급별 활동점수 확인
welcome <- filter %>%
  filter(filter$CUST_GRAD_NM == 'Welcome') %>%
  select(CUST_GRAD_NM, CUST_GRAD_PONT)

welcome <- welcome %>% mutate(group = case_when(
  CUST_GRAD_PONT == 0  ~ "0점",
  CUST_GRAD_PONT >=1 & CUST_GRAD_PONT <= 199 ~ "200점 이하",
  CUST_GRAD_PONT >=200 ~ "200점 이상"
))

table(welcome$group)
summary(welcome$CUST_GRAD_PONT)

silver <- filter %>%
  filter(filter$CUST_GRAD_NM == 'Silver') %>%
  select(CUST_GRAD_NM, CUST_GRAD_PONT)

silver <- silver %>% mutate(group = case_when(
  CUST_GRAD_PONT == 0  ~ "0점",
  CUST_GRAD_PONT >=1 & CUST_GRAD_PONT <= 199 ~ "200점 미만",
  CUST_GRAD_PONT >=200 ~ "200점 이상"
))

table(silver$group)

gold <- filter %>%
  filter(filter$CUST_GRAD_NM == 'Gold') %>%
  select(CUST_GRAD_NM, CUST_GRAD_PONT) %>%
  mutate(group = case_when(
    CUST_GRAD_PONT == 0  ~ "0점",
    CUST_GRAD_PONT >=1 & CUST_GRAD_PONT <= 999 ~ "1000점 미만",
    CUST_GRAD_PONT >=1000 ~ "1000점 이상"
  ))

table(gold$group)

platinum <- filter %>%
  filter(filter$CUST_GRAD_NM == 'Platinum') %>%
  select(CUST_GRAD_NM, CUST_GRAD_PONT) %>%
  mutate(group = case_when(
    CUST_GRAD_PONT == 0  ~ "0점",
    CUST_GRAD_PONT >=1 & CUST_GRAD_PONT <= 2499 ~ "2500점 미만",
    CUST_GRAD_PONT >= 2500 ~ "2500점 이상"
  ))

table(platinum$group)

# 구간별 boxplot 그리기
welcome <- welcome[!is.na(welcome$wel_group), ]
ggplot(welcome, aes(wel_group, CUST_GRAD_PONT)) + geom_boxplot()


#연령대별 고객 비중
#결측치를 평균값(42)으로 대체하기
age_mean <- median(df$AGE, na.rm=T)
df$AGE <- ifelse(is.na(df$AGE), age_mean, df$AGE)
df$AGE <- ifelse(df$AGE > 90 | df$AGE < 1, 42, df$AGE)

#나이대 column 생성
df <- df %>% mutate(age_group = case_when(
  AGE < 30 ~ "10-20대",
  AGE >= 30 & AGE < 40 ~ "30대",
  AGE >= 40 & AGE < 50 ~ "40대",
  AGE >= 50 & AGE < 60 ~ "50대",
  AGE >= 60 ~ "60대 이상"
))

table(df$age_group)
qplot(data=df, x=age_group, fill=age_group) + scale_y_continuous(labels=scales::comma)
ggplot(df, aes(AGE)) + geom_histogram(binwidth = 3)
ggplot(df, aes(AGE)) + geom_freqpoly(binwidth=1)

#연령대에 따른 회원 유형별 비중(x축=age_group, y축=고객 유형별)
# 데이터 프레임 그룹핑 및 요약
df_count <- df %>% 
  group_by(age_group, CUST_TYPE) %>%
  summarise(Freq = n()) %>%
  ungroup()
df_count
ggplot(df, aes(x=age_group, fill=CUST_TYPE)) + geom_bar() + scale_y_continuous(labels=scales::comma)

#연령대별 등급 분류
df1 <- df %>%
  group_by(age_group, CUST_GRAD_NM) %>%
  summarise(Freq=n()) %>%
  ungroup()
df1
ggplot(df1) + geom_mosaic(aes(x=product(age_group), weight=Freq, fill=CUST_GRAD_NM))

#가입기간별 고객 비중(일반/네고왕/세일페스타/불꽃1)
df <- df %>% 
  mutate(가입기간 = case_when(
    SBSCRB_DATE >= 20230323 & SBSCRB_DATE <= 20230330 ~ "네고왕",
    SBSCRB_DATE >= 20230915 & SBSCRB_DATE <= 20231013 ~ "세일페스타",
    SBSCRB_DATE >= 20240325 & SBSCRB_DATE <= 20240414 ~ "불꽃쎄일",
    TRUE ~ "일반"
  ))

View(table(df$가입기간))
View(df)

ggplot(df, aes(가입기간, fill = 가입기간)) + geom_bar() + scale_y_continuous(labels=scales::comma)

df2 <- df %>%
  group_by(가입기간) %>%
  summarise(freq=n()) %>%
  ungroup()

df2
ggplot(df2, aes(x="", y=freq, fill = 가입기간)) + geom_bar(stat = "identity")
ggplot(df2, aes(x="", y=freq, fill = 가입기간)) + geom_bar(stat="identity", alpha=0.8) + coord_polar("y") +
  geom_text(aes(label=freq), position=position_stack(vjust=0.5))


#연도별/일별 마지막 로그인 시간 분포
filter %>% select(LGI_DS)
filter$year <- year(filter$LGI_DS)
filter$month <- month(filter$LGI_DS)
filter$day <- mday(filter$LGI_DS)

table(filter$LGI_DS)
View(filter)

#연도별 group
time <- filter %>%
  group_by(year) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  filter(year %in% c(2022, 2023, 2024))

ggplot(time, aes(year, freq)) + geom_bar(stat="identity") + scale_y_continuous(labels=scales::comma) + theme_classic()

#연도/월별 group
time2 <- filter %>%
  group_by(year, month,day)%>%
  summarise(freq=n()) %>%
  ungroup()

time2 <- time2 %>%
  mutate(date = ymd(paste(year,month,day, sep="-")))

A <- time2 %>% filter(year>=2022 & year<=2024)

#월 평균 마지막 로그인 수
A %>%
  group_by(year,month) %>%
  summarise(avg_freq=mean(freq)) %>%
  ungroup() %>%
  View()

View(A)
ggplot(A, aes(date, freq)) + geom_line(size=0.7) + theme_classic()
A
View(filter)

time2_month <- filter %>%
  group_by(CUST_GRAD_NM, year, month) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  mutate(date = ym(paste(year,month,sep="-")))

View(time2_month)

ggplot(time2_month, aes(date, freq, col=CUST_GRAD_NM)) + geom_line(size=0.8) + facet_wrap(~CUST_GRAD_NM, ncol=1)

#월단위
filter %>%
  filter(year %in% c(2022, 2023, 2024)) %>%
  ggplot(aes(LGI_DS)) +
  geom_freqpoly(binwidth=2592000, size=1.5, color='darkblue') + theme_classic() +
  scale_x_date(date_breaks = '1 month', date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#일단위
filter %>%
  ggplot(aes(LGI_DS)) +
  geom_freqpoly(binwidth=86400)

#고객 유형별 월 단위 마지막 로그인 시간 분포
filter %>% select(CUST_TYPE, LGI_DS)
filter %>%
  filter(year %in% c(2022, 2023, 2024)) %>%
  filter(CUST_TYPE %in% c('개인회원', '파트너', '프렌즈')) %>%
  ggplot(aes(LGI_DS, fill=CUST_TYPE, col=CUST_TYPE)) + geom_freqpoly(binwidth=259200, size=1) +
  facet_wrap(~CUST_TYPE, ncol=1) 

time3 <- filter %>%
  group_by(CUST_TYPE, year) %>%
  summarise(freq=n()) %>%
  ungroup()

View(time3)

#등급별 마지막 로그인 시간 분포
filter %>%
  ggplot(aes(LGI_DS, fill = CUST_GRAD_NM, col=CUST_GRAD_NM)) + geom_freqpoly(binwidth=259200, size=0.8) + facet_wrap(~CUST_GRAD_NM, ncol=1)

#현재날짜
Sys.Date()


#yyyy-mm-dd 형태 변환
as.Date(filter$LGI_DS)

#현재 기준(7.23) 기준 미로그인 일수 산정하기 + 정수만 추출
filter <- filter %>%
  mutate(nologin = as.Date("2024-07-23") - as.Date(filter$LGI_DS))

filter$nologin <- as.integer(filter$nologin)
filter$nologin_month <- as.integer((filter$nologin) / 30) #개월 수 환산

#회원유형별 미로그인 개월 수 분포 및 통계치
nologin_12 <- filter %>% filter(filter$nologin_month < 13)

ggplot(nologin_12, aes(CUST_TYPE, nologin_month)) + geom_boxplot() + scale_x_discrete(limits=c("프렌즈", "파트너", "개인회원","임직원"))

filter %>%
  group_by(CUST_TYPE) %>%
  summarize(
    count = n(),
    mean = mean(nologin_month, na.rm = TRUE),
    sd = sd(nologin_month, na.rm = TRUE),
    median = median(nologin_month, na.rm = TRUE),
    min = min(nologin_month, na.rm = TRUE),
    max = max(nologin_month, na.rm = TRUE),
    Q1 = quantile(nologin_month, 0.25, na.rm = TRUE),
    Q3 = quantile(nologin_month, 0.75, na.rm = TRUE)
  )

filter <- filter %>%
  filter(year %in% c(2022, 2023, 2024))

fre <- filter %>%
  filter(filter$CUST_TYPE=='프렌즈' & filter$nologin_month <13)

View(filter)
#최근 90일 이내 미로그인
nine <- filter %>% filter(nologin<=90) %>% select(CUST_TYPE, CUST_GRAD_NM, SEX_DIV_NM, nologin)
table(nine$CUST_GRAD_NM)
dim(nine)

#최근 180일 이내 미로그인
eight <- filter %>% filter(nologin>90 & nologin<=180) %>% select(CUST_TYPE, CUST_GRAD_NM,nologin)
table(eight$CUST_GRAD_NM)

#최근 365일 이내 미로그인
oneyear <- filter %>% filter(nologin>180 & nologin<=365) %>% select(CUST_TYPE, CUST_GRAD_NM,nologin)
table(oneyear$CUST_GRAD_NM)

#나머지 365일 이상
high <- filter %>% filter(nologin>365) %>% select(CUST_TYPE, CUST_GRAD_NM, nologin)
table(high$CUST_GRAD_NM)

#전체 대상 미로그인 일수 분포
summary(filter$nologin)
ggplot(filter, aes(x = nologin)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(breaks = seq(0, 2000, by = 50), limits = c(0, 365)) + theme_classic()

#미로그인 일수별 고객유형별 비중
filter <- filter %>% mutate(login_group = case_when(
  nologin <= 90 ~ "90일 이내",
  nologin > 90 & nologin <= 180 ~ "91일-180일",
  nologin > 180 & nologin <= 365 ~ "181일-365일",
  nologin > 365 ~ "365일 이상"
))

table(filter$login_group)
count_login <- qplot(data=filter, x=login_group) + scale_y_continuous(labels=scales::comma)
count_login + scale_x_discrete(limits=c("90일 이내", "91일-180일", "181일-365일", "365일 이상"))
type_login <- qplot(data=filter, x=login_group, fill=CUST_TYPE) + scale_y_continuous(labels=scales::comma)
type_login + scale_x_discrete(limits=c("90일 이내", "91일-180일", "181일-365일", "365일 이상"))
grad_login <- qplot(data=filter, x=login_group, fill=CUST_GRAD_NM) + scale_y_continuous(labels=scales::comma)
grad_login + scale_x_discrete(limits=c("90일 이내", "91일-180일", "181일-365일", "365일 이상"))


#가입기간 대비 미로그인 일수 관계
ggplot(filter, aes(SBSCRB_DATE, nologin)) + geom_point()

table(filter$nologin)

#고객유형별 미로그인 일수
names(filter)

#180일 이상 미로그인 일수
month_180 <- filter %>% filter(filter$nologin >= 180)

View(month_180 %>%
  group_by(CUST_TYPE, CUST_GRAD_NM) %>%
  summarise(freq=n()) %>%
  ungroup())
