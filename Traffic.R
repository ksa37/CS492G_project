library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(ggmap)
library(doBy)
library(devtools)
library(ggiraphExtra)


#구글지도 api 가져오기
devtools::install_github("dkahle/ggmap")
register_google(key = 'AIzaSyA5ZmxIO6YKu_aYXbmSjk-xAZbq4EwDSo8')
Map_seoul <- get_map(location=c(lat=37.55, lon=126.97), zoom=11, maptype="roadmap")
MM <- ggmap(Map_seoul)

# 데이터 전처리
font_import()
theme_set(theme_gray(base_family='NanumGothic'))
traffic <-read.csv(file='seoul_traffic.csv', header = T)
traffic <- traffic %>% filter(일자>=20160101)  #2015년 데이터 제외, 지점명칭이 다름
traffic <-na.omit(traffic)
str(traffic)

# 데이터 숫자로 바꿔주기 Chr -> num
traffic$X0시<-as.numeric(traffic$X0시)
traffic$X1시<-as.numeric(traffic$X1시)
traffic$X2시<-as.numeric(traffic$X2시)
traffic$X3시<-as.numeric(traffic$X3시)
traffic$X4시<-as.numeric(traffic$X4시)
traffic$X5시<-as.numeric(traffic$X5시)
traffic$X6시<-as.numeric(traffic$X6시)
traffic$X7시<-as.numeric(traffic$X7시)
traffic$X8시<-as.numeric(traffic$X8시)
traffic$X9시<-as.numeric(traffic$X9시)
traffic$X10시<-as.numeric(traffic$X10시)
traffic$X11시<-as.numeric(traffic$X11시)
traffic$X12시<-as.numeric(traffic$X12시)
traffic$X13시<-as.numeric(traffic$X13시)
traffic$X14시<-as.numeric(traffic$X14시)
traffic$X15시<-as.numeric(traffic$X15시)
traffic$X16시<-as.numeric(traffic$X16시)
traffic$X17시<-as.numeric(traffic$X17시)
traffic$X18시<-as.numeric(traffic$X18시)
traffic$X19시<-as.numeric(traffic$X19시)
traffic$X20시<-as.numeric(traffic$X20시)
traffic$X21시<-as.numeric(traffic$X21시)
traffic$X22시<-as.numeric(traffic$X22시)
traffic$X23시<-as.numeric(traffic$X23시)
str(traffic)
traffic_num <- traffic %>% select(starts_with('X'))
str(traffic_num)

# 전체 구간에서 가장 많은 시간대
time_num = colSums(traffic_num, na.rm = TRUE)
sort(time_num, decreasing= T)


# 출퇴근 시간대
#7~9시, 17시~19시, 월~금
busy_traffic = traffic %>% select( c(1,2,3,4,5,6,15,16,17,24,25,26))
busy_traffic <- busy_traffic%>%filter(요일=='월'|요일=='화'|요일=='수'|요일=='목'|요일=='금')

onwork = subset(busy_traffic, select = c(4,8))
offwork = subset(busy_traffic, select = c(4,11))

colnames(onwork) = c('loc','avg')
colnames(offwork) = c('loc','avg')

# 위치에 따른 교통량 (모든 날짜에 대한 것을 합침)
perst_on = aggregate(.~loc, onwork, mean)
perst_off = aggregate(.~loc, offwork, mean)

# 마지막 항에 합친거 추가해주기
#perst_on_num <- perst_on %>% select(c(2:4))
#perst_on = perst_on %>% mutate(avg =rowMeans(perst_on_num))
#perst_off_num <- perst_off %>% select(c(2:4))
#perst_off = perst_off %>% mutate(avg =rowMeans(perst_off_num))

# 내림차순 정렬
loc_on_order <- perst_on %>% arrange(desc(avg))
loc_off_order <- perst_off %>% arrange(desc(avg))

#각 지점별 위도, 경도 가져오기
traffic_loc <-read.csv(file='지점별 위치.csv', header = T)
str(traffic_loc)
traffic_loc1 <- traffic_loc %>% select(c(1,3,4))

#출근시 top10 
onwork_top10 <- loc_on_order[1:10,]# %>% select(c(1, 3))
onwork_top10[,"x"] = c(1:10)
onwork_top10[,"y"] = c(1:10)
onwork_top10[,"rail"] = c(1:10)

#위도, 경도 넣어주기
for (i in 1:dim(traffic_loc1)[1]) {
  for (j in 1:dim(onwork_top10)[1]) {
    if(onwork_top10[j,"loc"] == traffic_loc1[i,"지점번호"]){
      onwork_top10[j,'x'] = traffic_loc1[i, "위도"]
      onwork_top10[j,'y'] = traffic_loc1[i, "경도"]
      onwork_top10[j, 'rail'] = traffic_loc[i,"지점명칭"]
      
    }
  }
  
}
str(onwork_top10)

#퇴근시 top10 
offwork_top10 <- loc_off_order[1:10,] #%>% select(c(1, 3))
offwork_top10[,"x"] = c(1:10)
offwork_top10[,"y"] = c(1:10)
offwork_top10[,"rail"] = c(1:10)

#위도, 경도 넣어주기
for (i in 1:dim(traffic_loc1)[1]) {
  for (j in 1:dim(offwork_top10)[1]) {
    if(offwork_top10[j,"loc"] == traffic_loc1[i,"지점번호"]){
      offwork_top10[j,'x'] = traffic_loc1[i, "위도"]
      offwork_top10[j,'y'] = traffic_loc1[i, "경도"]
      offwork_top10[j, 'rail'] = traffic_loc[i,"지점명칭"]
    }
  }
  
}
str(offwork_top10)

# 전체 구간에서 top10
 
onwork_top10 %>% ggplot(aes(reorder(rail,avg), y = avg))+ geom_bar(stat="identity", colour="Black") + xlab("Top 10 onwork") +coord_flip()
 
offwork_top10 %>% ggplot(aes(reorder(rail,avg), y = avg))+ geom_bar(stat="identity", colour="Black") + xlab("Top 10 offwork") +coord_flip()

# 구글맵에 시각화
MM_on <- MM + geom_point(aes(x=y, y=x, size=avg), alpha=0.5, data=onwork_top10)
MM_on + scale_size_area(name=c("Onwork Traffic"), max_size=15)+geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=onwork_top10)+ labs(x="longitude", y="latitude") 


MM_off <- MM + geom_point(aes(x=y, y=x, size=avg), alpha=0.5, data=offwork_top10)
MM_off + scale_size_area(name=c("Offwork Traffic"), max_size=15) +geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=offwork_top10)+ labs(x="longitude", y="latitude")


#지하철역 위치 데이터
subway_loc <-read.csv(file='지하철역위치.csv', header = T)
subway_loc = subway_loc[-c(1), ]
str(subway_loc)
subway_loc1 <- subway_loc%>% select(c(2,8,9))
names(subway_loc1) = c("station","lat", "lon")
subway_loc1$lat<-as.numeric(subway_loc1$lat)
subway_loc1$lon<-as.numeric(subway_loc1$lon)

#지하철 모든 역
MM_sub <- MM + geom_point(aes(x=lon, y=lat), data=subway_loc1, color = 'purple')
MM_sub


#지상 교통 모든 측정 지점
MM_traffic <- MM + geom_point(aes(x=경도, y=위도), data=traffic_loc1 )
MM_traffic

#지하철역/지상 교통 같은 지도에 나타내기
MM_sub + geom_point(aes(x=경도, y=위도), data=traffic_loc1, color='red')




#지하철 top10
data_subway <-read.csv(file='seoul_subway.csv', header = T, fileEncoding = "euc-kr")
data_subway <- data_subway %>% filter(사용월<201804)
data_sub <-data_subway %>% filter(사용월<201804) 
tail(data_sub)

sort(apply(data_sub[,-1], 2, sum))
sort(apply(data_sub[,-1], 1, sum))


data_pure <- data_subway %>% select(starts_with('X'))
data_subway = data_subway %>% mutate(total =rowSums(data_pure))
data_subway<-data_subway %>% select(c(1:51, 53))
str(data_subway)

# 오래된 데이터 이름 고쳐주기
for (j in 1:dim(data_subway)[1]){
  if (data_subway[j, "호선명"] == "9호선2단계"){
    data_subway[j, "호선명"] == "9호선2~3단계"
  } 
  if (data_subway[j, "지하철역"] == "교대(법원.검찰청)"){
    data_subway[j, "지하철역"] = "교대"
  }
  
  if (data_subway[j, "지하철역"] == "상봉(시외버스터미널)"){
    data_subway[j, "지하철역"] = "상봉"
  }
  if (data_subway[j, "지하철역"] == "올림픽공원(한국체대)"){
    data_subway[j, "지하철역"] = "올림픽공원"
  }
  
  if (data_subway[j, "지하철역"] == "대림(구로구청)"){
    data_subway[j, "지하철역"] = "대림"
  }
  if (data_subway[j, "지하철역"] == "성신여대입구(돈암)"){
    data_subway[j, "지하철역"] = "성신여대입구"
  }
  
  if (data_subway[j, "지하철역"] == "온수(성공회대입구)"){
    data_subway[j, "지하철역"] = "온수"
  }
  
  if (data_subway[j, "지하철역"] == "잠실(송파구청)"){
    data_subway[j, "지하철역"] = "잠실"
  }
  
  if (data_subway[j, "지하철역"] == "천호(풍납토성)"){
    data_subway[j, "지하철역"] = "천호"
  }
  if (data_subway[j, "지하철역"] == "청량리(서울시립대입구)"){
    data_subway[j, "지하철역"] = "청량리"
  }
  
  if (data_subway[j, "지하철역"] == "청량리(지하)"){
    data_subway[j, "지하철역"] = "청량리"
  }
  
  if (data_subway[j, "지하철역"] == "신정(은행정)"){
    data_subway[j, "지하철역"] = "신정"
  }
  if (data_subway[j, "지하철역"] == "군자(능동)"){
    data_subway[j, "지하철역"] = "군자"
  }
  if (data_subway[j, "지하철역"] == "미아(서울사이버대학)"){
    data_subway[j, "지하철역"] = "미아"
  }
  if (data_subway[j, "지하철역"] == "이촌(국립중앙박물관)"){
    data_subway[j, "지하철역"] = "이촌"
  }
  if (data_subway[j, "지하철역"] == "동작(현충원)"){
    data_subway[j, "지하철역"] = "동작"
  }
  if (data_subway[j, "지하철역"] == "서울대입구(관악구청)"){
    data_subway[j, "지하철역"] = "서울대입구"
  }
  if (data_subway[j, "지하철역"] == "왕십리(성동구청)"){
    data_subway[j, "지하철역"] = "왕십리"
  }
  if (data_subway[j, "지하철역"] == "서울역"){
    data_subway[j, "지하철역"] = "서울"
  }
  if (data_subway[j, "지하철역"] == "삼성(무역센터)"){
    data_subway[j, "지하철역"] = "삼성"
  }
  if (data_subway[j, "지하철역"] == "충정로(경기대입구)"){
    data_subway[j, "지하철역"] = "충정로"
  }
  
  if (data_subway[j, "지하철역"] == "용두(동대문구청)"){
    data_subway[j, "지하철역"] = "용두"
  }
  
  if (data_subway[j, "지하철역"] == "구의(광진구청)"){
    data_subway[j, "지하철역"] = "구의"
  }
  
  if (data_subway[j, "지하철역"] == "구의(광진구청)"){
    data_subway[j, "지하철역"] = "구의"
  }
  
  if (data_subway[j, "지하철역"] == "강변(동서울터미널)"){
    data_subway[j, "지하철역"] = "강변"
  }
  
  if (data_subway[j, "지하철역"] == "양재(서초구청)"){
    data_subway[j, "지하철역"] = "양재"
  }
  
  if (data_subway[j, "지하철역"] == "남부터미널(예술의전당)"){
    data_subway[j, "지하철역"] = "남부터미널"
  }
  
  if (data_subway[j, "지하철역"] == "경복궁(정부서울청사)"){
    data_subway[j, "지하철역"] = "경복궁"
  }
  
  if (data_subway[j, "지하철역"] == "수유(강북구청)"){
    data_subway[j, "지하철역"] = "수유"
  }
  if (data_subway[j, "지하철역"] == "숙대입구(갈월)"){
    data_subway[j, "지하철역"] = "숙대입구"
  }
  if (data_subway[j, "지하철역"] == "한성대입구(삼선교)"){
    data_subway[j, "지하철역"] = "한성대입구"
  }
  if (data_subway[j, "지하철역"] == "총신대입구(이수)"){
    data_subway[j, "지하철역"] = "총신대입구"
  }
  if (data_subway[j, "지하철역"] == "회현(남대문시장)"){
    data_subway[j, "지하철역"] = "회현"
  }
  
  if (data_subway[j, "지하철역"] == "화랑대(서울여대입구)"){
    data_subway[j, "지하철역"] = "화랑대"
  }
  if (data_subway[j, "지하철역"] == "오목교(목동운동장앞)"){
    data_subway[j, "지하철역"] = "오목교"
  }
  if (data_subway[j, "지하철역"] == "아차산(어린이대공원후문)"){
    data_subway[j, "지하철역"] = "아차산"
  }
  if (data_subway[j, "지하철역"] == "굽은다리(강동구민회관앞)"){
    data_subway[j, "지하철역"] = "굽은다리"
  }
  if (data_subway[j, "지하철역"] == "광화문(세종문화회관)"){
    data_subway[j, "지하철역"] = "광화문"
  }
  if (data_subway[j, "지하철역"] == "광나루(장신대)"){
    data_subway[j, "지하철역"] = "광나루"
  }
  
  if (data_subway[j, "지하철역"] == "고려대(종암)"){
    data_subway[j, "지하철역"] = "고려대"
  }
  
  if (data_subway[j, "지하철역"] == "상월곡(한국과학기술연구원)"){
    data_subway[j, "지하철역"] = "상월곡"
  }
  if (data_subway[j, "지하철역"] == "안암(고대병원앞)"){
    data_subway[j, "지하철역"] = "안암"
  }
  if (data_subway[j, "지하철역"] == "봉화산(서울의료원)"){
    data_subway[j, "지하철역"] = "봉화산"
  }
  if (data_subway[j, "지하철역"] == "대흥(서강대앞)"){
    data_subway[j, "지하철역"] = "대흥"
  }
  if (data_subway[j, "지하철역"] == "녹사평(용산구청)"){
    data_subway[j, "지하철역"] = "녹사평"
  }
  if (data_subway[j, "지하철역"] == "광흥창(서강)"){
    data_subway[j, "지하철역"] = "광흥창"
  }
  
  if (data_subway[j, "지하철역"] == "월곡(동덕여대)"){
    data_subway[j, "지하철역"] = "월곡"
  }
  
  
  if (data_subway[j, "지하철역"] == "증산(명지대앞)"){
    data_subway[j, "지하철역"] = "증산"
  }
  
  if (data_subway[j, "지하철역"] == "공릉(서울과학기술대)"){
    data_subway[j, "지하철역"] = "증산"
  }
  if (data_subway[j, "지하철역"] == "숭실대입구(살피재)"){
    data_subway[j, "지하철역"] = "증산"
  }
  if (data_subway[j, "지하철역"] == "어린이대공원(세종대)"){
    data_subway[j, "지하철역"] = "증산"
  }
  if (data_subway[j, "지하철역"] == "신창(순천향대)"){
    data_subway[j, "지하철역"] = "신창"
  }
  if (data_subway[j, "지하철역"] == "남한산성입구(성남법원.검찰청)"){
    data_subway[j, "지하철역"] = "남한산성입구"
  }
  if (data_subway[j, "지하철역"] == "몽촌토성(평화의문)"){
    data_subway[j, "지하철역"] = "몽촌토성"
  }
  if (data_subway[j, "지하철역"] == "흑석(중앙대입구)"){
    data_subway[j, "지하철역"] = "흑석"
  }
  if (data_subway[j, "지하철역"] == "청량리(지상)"){
    data_subway[j, "지하철역"] = "청량리"
  }
}

# 역별, 호선별 데이터 
By_station <- data_subway %>% group_by(지하철역) %>% summarise(avg = mean(total))
By_branch <- data_subway %>% group_by(호선명) %>% summarise(avg = mean(total))

By_station_order <-By_station %>% arrange(desc(avg))
By_station_order[1:10,] %>% ggplot(aes(reorder(지하철역,avg), y = avg))+ geom_bar(stat="identity", colour="Black") + xlab("상위 10개 지하철역") +coord_flip()

By_branch_order <-By_branch %>% arrange(desc(avg))
By_branch_order %>% ggplot(aes(reorder(호선명,avg), y = avg))+ geom_bar(stat="identity", colour="Grey") + xlab("지하철 노선") +coord_flip()


#출퇴근 시간 데이터 정리

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

commute_1_top10 <- commute_1[1:10,]
commute_2_top10 <- commute_2[1:10,]
commute_3_top10 <- commute_3[1:10,]
commute_4_top10 <- commute_4[1:10,]
#위도, 경도 넣어주기
for (i in 1:dim(subway_loc)[1]) {
  for (j in 1:dim(commute_1_top10)[1]) {
    if(commute_1_top10[j,"지하철역"] == subway_loc[i,"전철역명"]){
      commute_1_top10[j,'x'] = subway_loc[i, "X좌표.WGS."]
      commute_1_top10[j,'y'] = subway_loc[i, "Y좌표.WGS."]
      commute_1_top10[j, 'rail'] = subway_loc[i,"호선"]
      
    }
  }
  
}

for (i in 1:dim(subway_loc)[1]) {
  for (j in 1:dim(commute_2_top10)[1]) {
    if(commute_2_top10[j,"지하철역"] == subway_loc[i,"전철역명"]){
      commute_2_top10[j,'x'] = subway_loc[i, "X좌표.WGS."]
      commute_2_top10[j,'y'] = subway_loc[i, "Y좌표.WGS."]
      commute_2_top10[j, 'rail'] = subway_loc[i,"호선"]
      
    }
  }
  
}
for (i in 1:dim(subway_loc)[1]) {
  for (j in 1:dim(commute_3_top10)[1]) {
    if(commute_3_top10[j,"지하철역"] == subway_loc[i,"전철역명"]){
      commute_3_top10[j,'x'] = subway_loc[i, "X좌표.WGS."]
      commute_3_top10[j,'y'] = subway_loc[i, "Y좌표.WGS."]
      commute_3_top10[j, 'rail'] = subway_loc[i,"호선"]
      
    }
  }
  
}
for (i in 1:dim(subway_loc)[1]) {
  for (j in 1:dim(commute_4_top10)[1]) {
    if(commute_4_top10[j,"지하철역"] == subway_loc[i,"전철역명"]){
      commute_4_top10[j,'x'] = subway_loc[i, "X좌표.WGS."]
      commute_4_top10[j,'y'] = subway_loc[i, "Y좌표.WGS."]
      commute_4_top10[j, 'rail'] = subway_loc[i,"호선"]
      
    }
  }
  
}


# 문자열 숫자로 바꾸기
commute_1_top10$x<-as.numeric(commute_1_top10$x)
commute_1_top10$y<-as.numeric(commute_1_top10$y)

commute_2_top10$x<-as.numeric(commute_2_top10$x)
commute_2_top10$y<-as.numeric(commute_2_top10$y)

commute_3_top10$x<-as.numeric(commute_3_top10$x)
commute_3_top10$y<-as.numeric(commute_3_top10$y)

commute_4_top10$x<-as.numeric(commute_4_top10$x)
commute_4_top10$y<-as.numeric(commute_4_top10$y)


# 출근 승차 지하철 top10
str(commute_1_top10)

MM_subon_on <- MM + geom_point(aes(x=y, y=x, size=mean_), data=commute_1_top10, color= 'purple')
MM_subon_on + scale_size_area(name=c("Onwork Subway_on"), max_size=15)+geom_text(aes(x=y, y=x, label=지하철역,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=commute_1_top10)+ labs(x="longitude", y="latitude") 
MM_subon_on+ geom_point(aes(x=y, y=x, size=avg*40), data=onwork_top10,color= 'red')

# 출근 하차 지하철 top10
MM_subon_off <- MM + geom_point(aes(x=y, y=x, size=mean_), data=commute_2_top10, color= 'purple')
MM_subon_off+ scale_size_area(name=c("Onwork Subway_off"), max_size=15) + geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=commute_2_top10)+ labs(x="longitude", y="latitude") 
MM_subon_off+ geom_point(aes(x=y, y=x, size=avg*40),  data=onwork_top10,color= 'red')

# 퇴근 승차 지하철 top10
MM_suboff_on <- MM + geom_point(aes(x=y, y=x, size=mean_), data=commute_3_top10, color= 'purple')
MM_suboff_on+ scale_size_area(name=c("Offwork Subway_on"), max_size=15) + geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=commute_3_top10)+ labs(x="longitude", y="latitude") 
MM_suboff_on+ geom_point(aes(x=y, y=x, size=avg*40), data=offwork_top10,color= 'red')

# 퇴근 하차 지하철 top10
MM_suboff_off <- MM + geom_point(aes(x=y, y=x, size=mean_), data=commute_4_top10, color= 'purple')
MM_suboff_off + scale_size_area(name=c("Offwork Subway_off"), max_size=15)+ geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=commute_4_top10)+ labs(x="longitude", y="latitude") 
MM_suboff_off+ geom_point(aes(x=y, y=x, size=avg*40), data=offwork_top10,color= 'red')

MM_on <- MM + geom_point(aes(x=y, y=x, size=avg), alpha=0.5, data=onwork_top10)
MM_on + scale_size_area(name=c("Onwork Traffic"), max_size=15)+geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=onwork_top10)+ labs(x="longitude", y="latitude") 


MM_off <- MM + geom_point(aes(x=y, y=x, size=avg), alpha=0.5, data=offwork_top10)
MM_off + scale_size_area(name=c("Offwork Traffic"), max_size=15) +geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=offwork_top10)+ labs(x="longitude", y="latitude")

MM_on <- MM + geom_point(aes(x=y, y=x, size=avg), alpha=0.5, data=onwork_top10)
MM_on + scale_size_area(name=c("Onwork Traffic"), max_size=15)+geom_text(aes(x=y, y=x, label=rail,family= "NanumGothic"), colour="red", vjust=0, size=3, fontface="bold", data=onwork_top10)+ labs(x="longitude", y="latitude") 









