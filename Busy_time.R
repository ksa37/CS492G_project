library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggmap)
library(doBy)
library(devtools)

#한글 사용을 위한 작업
font_import()
theme_set(theme_gray(base_family='NanumGothic'))

data_subway <-read.csv(file='seoul_subway.csv', header = T, fileEncoding = "euc-kr")
data_subway <- data_subway %>% filter(사용월<201804)
data_sub <-data_subway %>% filter(사용월<201804) 
tail(data_sub)

# 각 행별로 값  합해서 total col으로 추가하기, 측정시간 제외
data_pure <- data_subway %>% select(starts_with('X'))
data_subway = data_subway %>% mutate(total =rowSums(data_pure))
data_subway<-data_subway %>% select(c(1:51, 53))
str(data_subway)

# 시간대별 데이터
colSums(data_pure)
colnames(data_pure)
sort(colSums(data_pure), decreasing= T)
# 이를 통해 가장 많은 사람들이 이용하는 시간대 파악가능

# 역별, 호선별 데이터 
By_station <- data_subway %>% group_by(지하철역) %>% summarise(avg = mean(total))
By_branch <- data_subway %>% group_by(호선명) %>% summarise(avg = mean(total))

By_station_order <-By_station %>% arrange(desc(avg))
By_station_order[1:10,] %>% ggplot(aes(reorder(지하철역,avg), y = avg))+ geom_bar(stat="identity", colour="Black") + xlab("상위 10개 지하철역") +coord_flip()

By_branch_order <-By_branch %>% arrange(desc(avg))
By_branch_order %>% ggplot(aes(reorder(호선명,avg), y = avg))+ geom_bar(stat="identity", colour="Grey") + xlab("지하철 노선") +coord_flip()

#출퇴근 시간 데이터 정리
crowd_time <- data_subway %>%select(c(1:3, 12:13, 32:33))
crowd_time %>% group_by(지하철역) %>% summarise(m = mean(X08시.09시.승차인원))
crowd_time %>% group_by(지하철역) %>% summarise(m = mean(X08시.09시.하차인원))
crowd_time %>% group_by(지하철역) %>% summarise(m = mean(X18시.19시.승차인원))
crowd_time %>% group_by(지하철역) %>% summarise(m = mean(X18시.19시.하차인원))

station1 <- data_subway %>% group_by(지하철역) %>% summarise(mean_ = mean(X08시.09시.승차인원))
station1_ <- data_subway %>% group_by(지하철역) %>% summarise(mean_ = mean(X08시.09시.하차인원))
station2 <- data_subway %>% group_by(지하철역) %>% summarise(mean_ = mean(X18시.19시.승차인원))
station2_ <- data_subway %>% group_by(지하철역) %>% summarise(mean_ = mean(X18시.19시.하차인원))


df_station <-data.frame(station1,station1_,station2,station2_)
df_station <- df_station %>% select(c(1,2,4,6,8))
df_station_arrange <- df_station %>% arrange(desc(mean_,mean_.1,mean_.2,mean_.3))

commute_1 <- station1 %>% arrange(desc(mean_)) 
commute_2 <- station1_ %>% arrange(desc(mean_))
commute_3 <- station2%>% arrange(desc(mean_))
commute_4 <- station2_ %>% arrange(desc(mean_))

commute_1[1:5,] %>% ggplot(aes(reorder(지하철역,mean_), y = mean_))+ geom_bar(stat="identity", colour="Grey") + xlab("08-09시 승차") +coord_flip()
commute_2[1:5,] %>% ggplot(aes(reorder(지하철역,mean_), y = mean_))+ geom_bar(stat="identity", colour="Grey") + xlab("08-09시 하차") +coord_flip()
commute_3[1:5,] %>% ggplot(aes(reorder(지하철역,mean_), y = mean_))+ geom_bar(stat="identity", colour="Grey") + xlab("18-19시 승차") +coord_flip()
commute_4[1:5,] %>% ggplot(aes(reorder(지하철역,mean_), y = mean_))+ geom_bar(stat="identity", colour="Grey") + xlab("18-19시 하차") +coord_flip()

