
###############################################################################################
########################################Data setting###########################################
###############################################################################################
## data load
setwd("D:/r/2017BC")
movie=read.csv("movie0920.csv")
str(movie)
## load packages
library(reshape)
library(reshape2)
library(ggplot2)
library(cluster)
library(plyr)

## data 확인
length(which(is.na(movie))) #0
str(movie)
dim(movie)





###############################################################################################
########################################Data processing########################################
###############################################################################################
## 변수목록 : 1.영화코드 2.영화명 3.누적관객수 4.감독누적관객수 5.배우파워 
## 6.평점(파이썬 크롤링 이용) 7.코멘트수(파이썬 크롤링 이용)
## 8.누적최대관객수 9.일차 10.일차^2 11.일차^3 12.등급군 13.국적군 14.감독평균최대관객수 
## 15.감독.group 16.배급사평균최대관객수
## 17.배급사.group 18.장르평균최대관객수 19.장르.group 20.요일구분 21.월.group  
## 22.공휴일 23.누적공휴일(엑셀작업) 24.공유일.합계 25.평점계(엑셀작업) 26.기사(파이썬 크롤링 이용)


## 영화코드번호 필드 추가(엑셀작업) #영화명이 중복되는 것이 있기 때문에, 영화별 고유값을 설정함

## 감독누적관객수 필드 추가
melt = melt(movie, "감독", "관객수")
head(melt,10)
movie_director = cast(melt, 감독 ~ variable, fun=sum)
movie_director = movie_director[c(order(-movie_director$관객수)), ]
head(movie_director)
movie$감독누적관객수=movie_director



## 배우파워 필드 추가(eda 후 엑셀 작업)

# 배우1--관객수
melt = melt(movie, "배우1", "관객수")
head(melt,10)
movie_actor_1 = cast(melt, 배우1 ~ variable, fun=sum)
movie_actor_1 = movie_actor_1[c(order(-movie_actor_1$관객수)), ]
head(movie_actor_1)
movie_actor_1_20 = movie_actor_1[1:20,] #상위 20개 추출
ggplot(movie_actor_1_20, aes(배우1, 관객수))+geom_bar(stat="identity")

# 배우2--관객수
melt = melt(movie, "배우2", "관객수")
head(melt,10)
movie_actor_2 = cast(melt, 배우2 ~ variable, fun=sum)
movie_actor_2 = movie_actor_2[c(order(-movie_actor_2$관객수)), ]
head(movie_actor_2)
movie_actor_2_20 = movie_actor_2[1:20,] #상위 20개 추출
ggplot(movie_actor_2_20, aes(배우2, 관객수))+geom_bar(stat="identity")

# 배우3--관객수
melt = melt(movie, "배우3", "관객수")
head(melt,10)
movie_actor_3 = cast(melt, 배우3 ~ variable, fun=sum)
movie_actor_3 = movie_actor_3[c(order(-movie_actor_3$관객수)), ]
head(movie_actor_3)
movie_actor_3_20 = movie_actor_3[1:20,] #상위 20개 추출
ggplot(movie_actor_3_20, aes(배우3, 관객수))+geom_bar(stat="identity")

colnames(movie_actor_1)=c("배우","관객수")
colnames(movie_actor_2)=c("배우","관객수")
colnames(movie_actor_3)=c("배우","관객수")

#동명이인 확인
length(unique(movie_actor_1$배우))==length(movie_actor_1$배우) #TRUE
length(unique(movie_actor_2$배우))==length(movie_actor_2$배우) #TRUE
length(unique(movie_actor_3$배우))==length(movie_actor_3$배우) #TRUE

movie_actor_all = rbind(movie_actor_1, movie_actor_2, movie_actor_3)
colnames(movie_actor_all)=c("배우","관객수")
head(movie_actor_all)

# movie_actor_power
movie_actor_power<-ddply(movie_actor_all, '배우', summarise, 관객수=sum(관객수))
movie_actor_power = movie_actor_power[c(order(-movie_actor_power$관객수)), ]
head(movie_actor_power)
movie_actor_power=movie_actor_power[-1,] # 결측치 제거
head(movie_actor_power)
movie_actor_power_20 = movie_actor_power[1:20,] #상위 20개 추출
ggplot(movie_actor_power_20, aes(배우, 관객수))+geom_bar(stat="identity")
# write.csv(movie_actor_power, file="배우별누적관객.csv")
# 나머지 엑셀로 처리
# #N/A=0처리


## 누적최대관객수 필드 추가

movie.List <- as.matrix(unique(movie$영화코드))
max.aud <- c()
for (i in 1:length(movie.List)){
  max.aud[i] <- max(movie[movie$영화코드==movie.List[i],"누적관객수"])
  if (i%%100==0) print(i)
}
aud <- data.frame(cbind(movie.List,max.aud))
names(aud)=c('영화명','누적최대관객수')
movie <- join(movie, aud, by='영화명')


## 일차(개봉후 경과일수) 필드 추가  (일자-개봉일)

for (i in 1:length(movie[,5])){
  movie[i,37] <- as.integer(movie[i,2] - movie[i,5])
  if (i%%100==0) print(i)
}
names(movie)[37]=c("일차")


## 일차 제곱 필드 추가

movie[38]=movie$일차^2
names(movie)[38]=c("일차2")

## 일차 세제곱 필드 추가

movie[39]=movie$일차^3
names(movie)[39]=c("일차3")

## 등급군(전체관람가->0, 12세이상->1, 15세이상->2, 청불->3) 필드 추가

grade.group <- c()
for(i in 1:length(movie[,20])){
  if('전체관람가'==movie[i,20]){
    grade.group <- c(grade.group,0)
  }
  else{
    if('12세이상관람가'==movie[i,20]){
      grade.group <- c(grade.group,1)
    }
    else{
      if('15세이상관람가'==movie[i,20]){
        grade.group <- c(grade.group,2)
      }
      else{ grade.group <- c(grade.group,3) }
    }
  }
  if(i%%1000==0) print(i)
}
movie <- cbind(movie,'등급군'=grade.group)

## 국적군(미국->0, 한국->1, 기타->2) 필드 추가
country.group <- c()
for(i in 1:length(movie[,17])){
  if('미국'==movie[i,17]){
    country.group <- c(country.group,0)
  }
  else{
    if('한국'==movie[i,17]){
      country.group <- c(country.group,1)
    }
    else{ country.group <- c(country.group,2) }
  }
  if(i%%1000==0) print(i)
}
movie <- cbind(movie,'국적군'=country.group)


## 감독평균최대관객수(평균최대관객수로 감독 클러스터링) , 감독.group 필드 추가

temp.dir <- aggregate(누적관객수~영화명+감독, data=movie, max)
director <- temp.dir$감독
temp.dir <- cbind(temp.dir, director)
temp.dir <- melt(temp.dir, measure.vars='director', variable.name='temp.dir', value.name='director')
director.mean <- ddply(temp.dir, .(director), summarize, ave=mean(누적관객수))   #감독별 누적관객수 평균
director.mean <- na.omit(director.mean)
K.dir=5  #그룹 개수 지정
km.dir <- kmeans(director.mean$ave, K.dir)    #clustering
temp.dir <- order(km.dir$centers)
temp.dir.cluster <- km.dir$cluster
for (i in 1:K.dir){
  temp.dir.cluster[km.dir$cluster==temp.dir[i]]=i
}
director.group <- cbind(director.mean, '감독.group'=temp.dir.cluster)   #감독 clustering결과 묶기
names(director.group)[1:2]=c('감독','감독평균최대관객수')   
movie <- join(movie, director.group, by='감독')   #감독 clustering 결과를 감독에 매치하여 join


## 배급사평균최대관객수(평균최대관객수로 배급사 클러스터링), 배급사.group 필드 추가

temp.mar <- aggregate(누적관객수~영화명+배급사, data=movie, max)
mar.mean <- ddply(temp.mar,.(배급사),summarize,ave=mean(누적관객수)) #배급사별 누적관객수 평균

K.mar=3 # 그룹 개수 지정
km.mar <- kmeans(mar.mean$ave,K.mar) #clustering
temp.mar <- order(km.mar$centers)
temp.mar.cluster <- km.mar$cluster
for (i in 1:K.mar){
  temp.mar.cluster[km.mar$cluster==temp.mar[i]]=i
}
mar.group <- cbind(mar.mean,"배급사.group"=temp.mar.cluster) # 배급사 cluster결과 리스트
movie <- join(movie, mar.group,by="배급사") # 배급사 clustering 결과를 match
colnames(movie)[43] <- '배급사평균최대관객수'

## 장르평균최대관객수(평균최대관객수로 장르 클러스터링), 장르.group 필드 추가

unique(movie[,20])
temp.genre <- aggregate(누적관객수~영화명+장르, data=movie, max)
genre <- temp.genre$장르
temp.genre <- cbind(temp.genre, genre)
temp.genre <- melt(temp.genre, measure.vars='장르', variable.name='temp', value.name='genre')
genre.mean <- ddply(temp.genre,.(genre),summarize,ave=mean(누적관객수))

K.genre=4
km.genre <- kmeans(genre.mean$ave, K.genre)
temp.genre <- order(km.genre$centers)
temp.genre.cluster <- km.genre$cluster
for (i in 1:K.genre){
  temp.genre.cluster[km.genre$cluster==temp.genre[i]]=i
}
genre.group <- cbind(genre.mean, '장르.group'=temp.genre.cluster)

names(genre.group)[1:2] <- c('장르','장르평균최대관객수')
movie <- join(movie, genre.group, by='장르')

## 등급평균최대관객수(평균최대관객수로 등급 클러스터링) , 등급.group 필드 추가

temp.grade <- aggregate(누적관객수~영화명+등급, data=movie, max)
grade <- temp.grade$등급
temp.grade <- cbind(temp.grade, grade)
temp.grade <- melt(temp.grade, measure.vars='grade', variable.name='temp.grade', value.name='grade')
grade.mean <- ddply(temp.grade, .(grade), summarize, ave=mean(누적관객수))   #등급별 누적관객수 평균
grade.mean <- na.omit(grade.mean)
names(grade.mean)[1:2]=c('등급','등급평균최대관객수')   
movie <- join(movie, grade.mean, by='등급')   
## 요일구분 필드 추가

movie$요일 = weekdays(as.Date(movie$일자))
movie$요일 = factor(movie$요일)
head(movie$요일)
movie$요일구분 = movie$요일
movie$요일구분 = ifelse(movie$요일구분=="금요일",1,ifelse(movie$요일구분=="토요일"|movie$요일구분=="일요일",2,0))

## 월구분 필드 추가

movie$월구분 = months(as.Date(movie$일자))

## 월 클러스터링(7,8월 ->1 ,   1,2,5,6,9,12월 -> 2, 나머지 월 ->3) ,월.group 필드 추가

month.group <- c()
for(i in 1:length(movie[,48])){
  if(movie[i,48]=='1월'|movie[i,48]=='2월'|movie[i,48]=='5월'|movie[i,48]=='6월'|movie[i,48]=='9월'|movie[i,48]=='12월'){
    month.group <- c(month.group,2)
  }
  else{
    if(movie[i,48]=='7월'|movie[i,48]=='8월'){
      month.group <- c(month.group,1)
    }
    else{
      month.group <- c(month.group,3)
    }
  }
  if(i%%1000==0) print(i)
}
movie <- cbind(movie,'월.group'=month.group)





str(movie)
dim(movie)




## 공휴일 필드 추가 (일자구분별 평균최대관객수)
melt = melt(movie, "일자구분", "관객수")
head(melt,10)
movie_day_category = cast(melt, 일자구분 ~ variable, fun=mean)
movie_day_category = movie_day_category[c(order(-movie_day_category$관객수)), ]
movie_day_category
names(movie_day_category)=c('일자구분','공휴일평균관객수')
movie=merge(movie,movie_day_category,by='일자구분')

## 공휴일.합계 필드 추가(공휴일 변수의 총합)

movie.List <- unique(movie$영화코드)
movie.List[order(movie.List)]
movie.TotalSeen <- c()
for (i in 1:length(movie.List)){
  movie.TotalSeen[i]=sum(movie[movie$영화코드==movie.List[i],"공휴일평균관객수"])
  if (i%%100==0) print(i)
}

movie.seen <- data.frame(as.matrix(movie.List),movie.TotalSeen)
movie.seen

names(movie.seen)=c('영화코드','공휴일총합')

movie <- join(movie, movie.seen, by='영화코드')


## 누적최대관객수 100000명 이하 데이터 삭제 
movie.List <- unique(movie$영화코드)
movie.List[order(movie.List)]
movie.TotalSeen <- c()
for (i in 1:length(movie.List)){
  movie.TotalSeen[i]=max(movie[movie$영화코드==movie.List[i],"누적최대관객수"])
  if (i%%100==0) print(i)
}

movie.seen <- data.frame(as.matrix(movie.List),movie.TotalSeen)
movie.seen

sum(movie.seen[,2]>10^5)  
var <- NULL
for (i in 1:length(movie.seen[,2])){
  if(movie.seen[i,2] > 10^5)  var <- c(var,i)
}
movie.seen[var,]
movie.Clean <- as.matrix(movie.seen[var,1]) #추출한 영화코드번호와 매칭되는 데이터세트 구성
movie.Name <- c()
for(i in 1:length(movie.Clean)){
  movie.Name <- c(movie.Name, which(movie[,5]==movie.Clean[i]))
}
movie <- movie[movie.Name,]

###############################################################################################
############################################ EDA ##############################################
###############################################################################################

## 요일별 관객수
movie$요일구분 = factor(movie$요일구분)
melt = melt(movie, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 월별 관객수
movie$요일구분 = factor(movie$월구분)
melt = melt(movie, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")
# 8월, 1월에 많이 본다. 상대적으로 9월 10월 에은 적게 보는 것으로 보임
# 1,2월, 7,8월 방학 시즌에 많이 본다.

## 상영 차수(전체)
movie$일차 = factor(movie$일차)
melt = melt(movie, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

#2~3주차에 평균 관객수가 제일 많이 보이는 것으로 확인

## 배급사별 관객수
movie$배급사 = factor(movie$배급사)
melt = melt(movie, "배급사", "관객수")
head(melt,10)
movie_distribute = cast(melt, 배급사 ~ variable, fun=sum)
movie_distribute = movie_distribute[c(order(-movie_distribute$관객수)), ] # 내림차순으로 정리
head(movie_distribute, 10) 
movie_distribute_10 = movie_distribute[1:10,] # 상위 10개 추출
(sum(movie_distribute_10$관객수))/(sum(movie_distribute$관객수)) 
# 상위 10개의 배급사가 전체 관객수의 87.4%를 차지하고 있음
# 즉 배급사가 많은 영향을 끼치는 것으로 보임
ggplot(movie_distribute_10, aes(배급사, 관객수))+geom_bar(stat="identity")

## 등급별 관객수 
# 여러 등급이 있을때 상위 등급으로 처리(12세, 15세 → 15세로 처리)
melt = melt(movie, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")
# 기타, 미정 처리 필요 → 삭제

## 장르별 관객수
melt = melt(movie, "장르", "관객수")
head(melt,10)
movie_genre = cast(melt, 장르 ~ variable, fun=mean)
movie_genre = movie_genre[c(order(-movie_genre$관객수)), ]
movie_genre
ggplot(movie_genre, aes(장르, 관객수))+geom_bar(stat="identity")

## 장르(합계)별 관객수
melt = melt(movie, "장르", "관객수")
head(melt,10)
movie_genre = cast(melt, 장르 ~ variable, fun=sum)
movie_genre = movie_genre[c(order(-movie_genre$관객수)), ]
movie_genre
ggplot(movie_genre, aes(장르, 관객수))+geom_bar(stat="identity")

## 일자구분별 관객수
melt = melt(movie, "일자구분", "관객수")
head(melt,10)
movie_day_category = cast(melt, 일자구분 ~ variable, fun=mean)
movie_day_category = movie_day_category[c(order(-movie_day_category$관객수)), ]
movie_day_category
ggplot(movie_day_category, aes(일자구분, 관객수))+geom_bar(stat="identity")

## 스크린수별 관객수
cor(movie$스크린수, movie$관객수)
# 0.779 → 약 80% 상관관계
plot(movie$스크린수, movie$관객수)

## 배급사별 스크린수
melt = melt(movie, "배급사", "스크린수")
head(melt,10)
movie_distribute_screen = cast(melt, 배급사 ~ variable, fun=sum)
movie_distribute_screen = movie_distribute_screen[c(order(-movie_distribute_screen$스크린수)), ]
movie_distribute_screen
movie_distribute_screen_10 = movie_distribute_screen[1:10,] #상위 10개 추출
ggplot(movie_distribute_screen_10, aes(배급사, 스크린수))+geom_bar(stat="identity")
# 이상치 제거 요망

## 감독별 관객수
# 감독이 여러명인 영화는 대표감독 하나로 표시
melt = melt(movie, "감독", "관객수")
head(melt,10)
movie_director = cast(melt, 감독 ~ variable, fun=sum)
movie_director = movie_director[c(order(-movie_director$관객수)), ]
head(movie_director)
movie_director_20 = movie_director[1:20,] #상위 20개 추출
ggplot(movie_director_20, aes(감독, 관객수))+geom_bar(stat="identity")

## 월별 관객수   
JAN <- subset(movie$관객수, movie$월구분=='1월')
FEB <- subset(movie$관객수, movie$월구분=='2월')
MAR <- subset(movie$관객수, movie$월구분=='3월')
APR <- subset(movie$관객수, movie$월구분=='4월')
MAY <- subset(movie$관객수, movie$월구분=='5월')
JUN <- subset(movie$관객수, movie$월구분=='6월')
JUL <- subset(movie$관객수, movie$월구분=='7월')
AUG <- subset(movie$관객수, movie$월구분=='8월')
SEP <- subset(movie$관객수, movie$월구분=='9월')
OCT <- subset(movie$관객수, movie$월구분=='10월')
NOV <- subset(movie$관객수, movie$월구분=='11월')
DEC <- subset(movie$관객수, movie$월구분=='12월')
month <- c(sum(JAN)/length(JAN), sum(FEB)/length(FEB), sum(MAR)/length(MAR), sum(APR)/length(APR),
           sum(MAY)/length(MAY), sum(JUN)/length(JUN), sum(JUL)/length(JUL), sum(AUG)/length(AUG),
           sum(SEP)/length(SEP), sum(OCT)/length(OCT), sum(NOV)/length(NOV), sum(DEC)/length(DEC))
month
barplot(month, names.arg=c('1','2','3','4','5','6','7','8','9','10','11','12'))

#### 애니메이션, 사극, 액션별

action=movie[movie$장르 =="액션",]
sa=movie[movie$장르 =="사극",]
ani=movie[movie$장르=="애니메이션",]


## 액션 요일별 관객수
melt = melt(action, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 액션 월별 관객수
melt = melt(action, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 액션 상영 차수(전체)
melt = melt(action, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 액션 배급사별 관객수
melt = melt(action, "배급사", "관객수")
head(melt,10)
movie_distribute = cast(melt, 배급사 ~ variable, fun=sum)
movie_distribute = movie_distribute[c(order(-movie_distribute$관객수)), ] # 내림차순으로 정리
head(movie_distribute, 10) 
movie_distribute_10 = movie_distribute[1:10,] # 상위 10개 추출
(sum(movie_distribute_10$관객수))/(sum(movie_distribute$관객수)) 

ggplot(movie_distribute_10, aes(배급사, 관객수))+geom_bar(stat="identity")

## 액션 등급별 관객수 
melt = melt(action, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")



## 사극 요일별 관객수
melt = melt(sa, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 사극 월별 관객수
melt = melt(sa, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")


## 사극 상영 차수(전체)
melt = melt(sa, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 사극 배급사별 관객수
melt = melt(sa, "배급사", "관객수")
head(melt,10)
movie_distribute = cast(melt, 배급사 ~ variable, fun=sum)
movie_distribute = movie_distribute[c(order(-movie_distribute$관객수)), ] # 내림차순으로 정리
head(movie_distribute, 10) 
movie_distribute_10 = movie_distribute[1:10,] # 상위 10개 추출
(sum(movie_distribute_10$관객수))/(sum(movie_distribute$관객수)) 
ggplot(movie_distribute_10, aes(배급사, 관객수))+geom_bar(stat="identity")

## 사극 등급별 관객수 
melt = melt(sa, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")

## 애니메이션 요일별 관객수
melt = melt(ani, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 애니메이션 월별 관객수
melt = melt(ani, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 애니메이션 상영 차수(전체)
melt = melt(ani, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 애니메이션 배급사별 관객수
melt = melt(ani, "배급사", "관객수")
head(melt,10)
movie_distribute = cast(melt, 배급사 ~ variable, fun=sum)
movie_distribute = movie_distribute[c(order(-movie_distribute$관객수)), ] # 내림차순으로 정리
head(movie_distribute, 10) 
movie_distribute_10 = movie_distribute[1:10,] # 상위 10개 추출
(sum(movie_distribute_10$관객수))/(sum(movie_distribute$관객수)) 
ggplot(movie_distribute_10, aes(배급사, 관객수))+geom_bar(stat="identity")

## 등급별 관객수 
melt = melt(ani, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")


#### 남한산성, 킹스맨 배급사별

cj=movie[movie$배급사=="씨제이이앤엠(주)",] #남한
fox=movie[movie$배급사=="이십세기폭스코리아(주)",] #킹스맨
rot=movie[movie$배급사=="롯데쇼핑㈜롯데엔터테인먼트",] #넛잡

## 씨제이 요일별 관객수
melt = melt(cj, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 씨제이 월별 관객수
melt = melt(cj, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 씨제이 상영 차수(전체)
melt = melt(cj, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")


## 씨제이 상영 차수별 스크린수 (전체)
melt = melt(cj, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")


## 이십세기폭스 상영 차수별 스크린수 (전체)
melt = melt(fox, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")

## 롯데 상영 차수별 스크린수 (전체)
melt = melt(rot, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")

## 롯데 장르별 스크린수 (전체)
melt = melt(rot, "장르", "스크린수")
head(melt,10)
movie_days = cast(melt, 장르 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(장르, 스크린수))+geom_bar(stat="identity")

## 씨제이 장르별 스크린수 (전체)
melt = melt(cj, "장르", "스크린수")
head(melt,10)
movie_days = cast(melt, 장르 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(장르, 스크린수))+geom_bar(stat="identity")

## 이십세기폭스 장르별 스크린수 
melt = melt(fox, "장르", "스크린수")
head(melt,10)
movie_days = cast(melt, 장르 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(장르, 스크린수))+geom_bar(stat="identity")

## 씨제이 요일별 관객수
melt = melt(cj, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 롯데+애니메이션 상영 차수별 스크린수
rotani=rot[rot$장르=="애니메이션",]
melt = melt(rotani, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")

## 롯데+애니메이션 요일별 스크린수
melt = melt(rotani, "요일", "스크린수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 스크린수))+geom_bar(stat="identity")

## 롯데+애니메이션 월별 스크린수
melt = melt(rotani, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")




## 씨제이+사극 상영 차수별 스크린수
cjsa=cj[cj$장르=="사극",]
melt = melt(cjsa, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")
## 이십세기폭스+액션 상영 차수별 스크린ㅅ
foxact=fox[fox$장르=="액션",]
melt = melt(foxact, "일차", "스크린수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 스크린수))+geom_bar(stat="identity")

## 등급별 관객수 
# 여러 등급이 있을때 상위 등급으로 처리(12세, 15세 → 15세로 처리)
melt = melt(cj, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")
# 기타, 미정 처리 필요 → 삭제

## 씨제이 장르별 관객수 
melt = melt(rot, "장르", "관객수")
head(melt,10)
movie_genre = cast(melt, 장르 ~ variable, fun=mean)
movie_genre = movie_genre[c(order(-movie_genre$관객수)), ]
movie_genre
ggplot(movie_genre, aes(장르, 관객수))+geom_bar(stat="identity")


## 이십세기폭스 요일별 관객수
melt = melt(fox, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 이십세기폭스 월별 관객수
melt = melt(fox, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 이십세기폭스 상영 차수(전체)
melt = melt(fox, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 등급별 관객수 
melt = melt(fox, "등급", "관객수")
head(melt,10)
movie_rating = cast(melt, 등급 ~ variable, fun=sum)
movie_rating = movie_rating[c(order(-movie_rating$관객수)), ]
ggplot(movie_rating, aes(등급, 관객수))+geom_bar(stat="identity")


### 넛잡: 땅콩 도둑들, 킹스맨1 eda
nut1=movie[movie$영화명=="넛잡: 땅콩 도둑들",]
king1=movie[movie$영화명=="킹스맨 : 시크릿 에이전트",]

## 넛잡1 요일별 관객수
melt = melt(nut1, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 넛잡1 월별 관객수
melt = melt(nut1, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 넛잡1 상영 차수(전체)
melt = melt(nut1, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 넛잡1 일자구분별 스크린수
melt = melt(nut1, "일자구분", "스크린수")
head(melt,10)
movie_days = cast(melt, 일자구분 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일자구분, 스크린수))+geom_bar(stat="identity")
## 넛잡1 일자구분별 관객수 
melt = melt(nut1, "일자구분", "관객수")
head(melt,10)
movie_days = cast(melt, 일자구분 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일자구분, 관객수))+geom_bar(stat="identity")


## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

## 킹스맨1 요일별 관객수
melt = melt(king1, "요일", "관객수")
head(melt,10)
movie_weekday = cast(melt, 요일 ~ variable, fun=mean)
movie_weekday
ggplot(movie_weekday, aes(요일, 관객수))+geom_bar(stat="identity")

## 킹스맨1 월별 관객수
melt = melt(king1, "월구분", "관객수")
head(melt,10)
movie_month = cast(melt, 월구분 ~ variable, fun=mean)
movie_month = movie_month[c(order(-movie_month$관객수)), ]
movie_month
ggplot(movie_month, aes(월구분 , 관객수))+geom_bar(stat="identity")

## 킹스맨1 상영 차수(전체)
melt = melt(king1, "일차", "관객수")
head(melt,10)
movie_days = cast(melt, 일차 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일차, 관객수))+geom_bar(stat="identity")

## 킹스맨1 일자구분별 스크린수
melt = melt(king1, "일자구분", "스크린수")
head(melt,10)
movie_days = cast(melt, 일자구분 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일자구분, 스크린수))+geom_bar(stat="identity")

## 킹스맨1 일자구분별 관객수 
melt = melt(king1, "일자구분", "관객수")
head(melt,10)
movie_days = cast(melt, 일자구분 ~ variable, fun=mean)
movie_days
ggplot(movie_days, aes(일자구분, 관객수))+geom_bar(stat="identity")


## 상영 차수(30일까지)
movie_days_30 = movie_days[1:30,]
ggplot(movie_days_30, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(60일까지)
movie_days_60 = movie_days[1:60,]
ggplot(movie_days_60, aes(일차, 관객수))+geom_bar(stat="identity")

## 상영 차수(90일까지)
movie_days_90 = movie_days[1:90,]
ggplot(movie_days_90, aes(일차, 관객수))+geom_bar(stat="identity")

###############################################################################################
######################################## Modeling #############################################
###############################################################################################



# load packages
install.packages("glmnet")
library("glmnet")

## data load
setwd("D:/r/2017BC")
movie=read.csv("movie0920.csv")
head(movie)
str(movie)


#log(data set)
movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
movie$등급편균최대관객수 = log(movie$등급평균최대관객수)
movie$배우파워 = sqrt(movie$배우파워)
movie$배급사평균최대관객수 = log(movie$배급사평균최대관객수)
movie$공휴일총합=log(movie$공휴일총합)
movie$공휴일누적평균관객수=log(movie$공휴일누적평균관객수)
movie$공휴일평균관객수=log(movie$공휴일평균관객수)

#movie=scale(movie)
movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")])

#variable selection
var=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")

## 배열 만들기 (2^14) (이후 최종 엑셀처리) # 14개의 변수로 만들 수 있는 모든 경우의 수 Array 생성
array <- c()
Array <- c()
cnt.ar <- 0
for (a in 0:1){ for (b in 0:1){ for (c in 0:1){ for (d in 0:1){ for (e in 0:1){ 
  for (f in 0:1){ for (g in 0:1){ for(h in 0:1){ for(i in 0:1){ for(j in 0:1){
    for (k in 0:1){for (l in 0:1){for (m in 0:1){for (n in 0:1){
      array <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      Array <- rbind(Array, array)
      cnt.ar <- cnt.ar+1
      rownames(Array)[cnt.ar] <- paste('array',cnt.ar)
      if(cnt.ar%%100==0) print(cnt.ar)
    } } } } } } } } } } } } } } 
head(Array)
tail(Array)

Array = as.data.frame(Array)

length.ar <- length(Array[,1])
colnames(Array)=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")
form <- matrix(0,length.ar,14)
colnames(form)=c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","등급평균최대관객수","공휴일평균관객수" ,"공휴일누적평균관객수", "공휴일총합", "기사수")



for(z in 1:length.ar){
  if(Array[z,1]==1) form[z,1] = colnames(Array[1])
  if(Array[z,2]==1) form[z,2] = colnames(Array[2])
  if(Array[z,3]==1) form[z,3] = colnames(Array[3])
  if(Array[z,4]==1) form[z,4] = colnames(Array[4])
  if(Array[z,5]==1) form[z,5] = colnames(Array[5])
  if(Array[z,6]==1) form[z,6] = colnames(Array[6])
  if(Array[z,7]==1) form[z,7] = colnames(Array[7])
  if(Array[z,8]==1) form[z,8] = colnames(Array[8])
  if(Array[z,9]==1) form[z,9] = colnames(Array[9])
  if(Array[z,10]==1) form[z,10] = colnames(Array[10])
  if(Array[z,11]==1) form[z,11] = colnames(Array[11])
  if(Array[z,12]==1) form[z,12] = colnames(Array[12])
  if(Array[z,13]==1) form[z,13] = colnames(Array[13])
  if(Array[z,14]==1) form[z,14] = colnames(Array[14])
}        
form2=matrix(0,length.ar,1)

for(z in 1:length.ar){
  if(form[z,1]!=0) form2[z] = paste(form[z,1],sep=",") 
  if(form[z,2]!=0) form2[z] = paste(form2[z],form[z,2],sep=",")
  if(form[z,3]!=0) form2[z] = paste(form2[z],form[z,3],sep=",")
  if(form[z,4]!=0) form2[z] = paste(form2[z],form[z,4],sep=",")
  if(form[z,5]!=0) form2[z] = paste(form2[z],form[z,5],sep=",")
  if(form[z,6]!=0) form2[z] = paste(form2[z],form[z,6],sep=",")
  if(form[z,7]!=0) form2[z] = paste(form2[z],form[z,7],sep=",")
  if(form[z,8]!=0) form2[z] = paste(form2[z],form[z,8],sep=",")
  if(form[z,9]!=0) form2[z] = paste(form2[z],form[z,9],sep=",")
  if(form[z,10]!=0) form2[z] = paste(form2[z],form[z,10],sep=",")
  if(form[z,11]!=0) form2[z] = paste(form2[z],form[z,11],sep=",")
  if(form[z,12]!=0) form2[z] = paste(form2[z],form[z,12],sep=",")
  if(form[z,13]!=0) form2[z] = paste(form2[z],form[z,13],sep=",")
  if(form[z,14]!=0) form2[z] = paste(form2[z],form[z,14],sep=",")
  
}


head(form2)
write.csv(form2,file="Array(12).csv")


ibrary(glmnet)
rm(list=ls())
movie=read.csv("movie0920.csv")
movie=subset(movie, movie$일차<=7) #킹스맨 일차 0~7까지, 남한산성,넛잡 일차 0~14까지로 subset 
Array <-read.csv("Array(12).csv")  # 엑셀작업으로 유의미하지 않은 변수 조합은 제거(12개 이상만 선택)

#"몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2", "퍼시 잭슨과 괴물의 바다", "컨저링", 
#"남자가 사랑할 때", "수상한 그녀", "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014",
#"피끓는 청춘", "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", "조선명탐정 : 사라진 놉의 딸", 
#"킹스맨 : 시크릿 에이전트", "도라에몽 : 스탠 바이 미", "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", 
#"오즈의 마법사: 돌아온 도로시", "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", 
#"인턴", "탐정 : 더 비기닝", "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", 
#"최강전사 미니특공대: 영웅의 탄생", "캐롤", "장난감이 살아있다", "거울나라의 앨리스", 
#"고산자, 대동여지도", "달빛궁궐", "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허",
#"카페 소사이어티", "공조", "더 킹", "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", 
#"짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격"

target_list=list("몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2", 
                 "퍼시 잭슨과 괴물의 바다", "컨저링", "남자가 사랑할 때", "수상한 그녀", 
                 "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014", "피끓는 청춘", 
                 "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", 
                 "조선명탐정 : 사라진 놉의 딸", "도라에몽 : 스탠 바이 미", 
                 "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", "오즈의 마법사: 돌아온 도로시", 
                 "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", "인턴", "탐정 : 더 비기닝", 
                 "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", "최강전사 미니특공대: 영웅의 탄생", 
                 "캐롤", "장난감이 살아있다", "거울나라의 앨리스", "고산자, 대동여지도", "달빛궁궐", 
                 "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허", "카페 소사이어티", "공조", "더 킹", 
                 "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격","밀정","관상"
) 

# 4년 9개월간 명절을 포함하고 있는 영화 데이터만을 추출한 리스트

a=data.frame() 
b=seq(0,1,0.1) #알파값
len.ar =length(Array$변수) ;len.ar
#rm(list=ls())변수1당 5분 /12당 1시간/ 24당 2시간
#system.time(    #총 변수 조합 106개를 나눠서 돌림 # 알파값은 0.1단위로 0~1까지, 12개 이상의 변수의 조합의 모든 경우의 수를 loop문으로 작성 
for (k in 3:17){            
  for (i in target_list){   
    for (j in b){
      movie=read.csv("movie0920.csv")
      movie=subset(movie, movie$일차<=7)
      Array=read.csv("Array(12).csv")
      
      movie$등급군 = as.factor(movie$등급군)
      movie$국적군 = as.factor(movie$국적군)
      movie$감독.group = as.factor(movie$감독.group)
      movie$배급사.group = as.factor(movie$배급사.group)
      movie$장르.group = as.factor(movie$장르.group)
      movie$월.group = as.factor(movie$월.group)
      movie$X9월 = as.factor(movie$X9월)
      movie$X10월 = as.factor(movie$X10월)
      movie$요일구분 = as.factor(movie$요일구분)
      
      #log(data set)
      #관객수로 만든 변수 log / 수치가 큰 데이터(코멘트 수, 평점계) 변수 log
      movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
      movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
      movie$감독누적관객수 = log(movie$감독누적관객수)
      movie$배우파워 = sqrt(movie$배우파워)
      
      #scale Data
      movie[ ,c("등급평균최대관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","공휴일" ,"누적공휴일", "공휴일.합계", "기사수")] = scale(movie[ ,c("등급평균최대관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수","공휴일" ,"누적공휴일", "공휴일.합계", "기사수")])
      #movie=scale(movie)
      
      un=unlist(strsplit(as.character(Array[k,2]),fixed=TRUE,split=","))
      x_train<-movie[,c('영화명',un)]
      y_train = movie[,c("영화명","일차","누적관객수")]
      
      
      x_test = x_train[x_train$영화명==i,]
      y_test = y_train[y_train$영화명==i,]
      x_train = x_train[x_train$영화명!=i,]
      y_train = y_train[y_train$영화명!=i,]
      x_test = x_test[,-1]
      y_test = y_test[,3]
      x_train = x_train[,-1]
      y_train = y_train[,3]
      y_test = log(y_test)
      y_train = log(y_train)
      
      ###elastic(alpha=0.1)
      #elastic.fit <- glmnet(data.matrix(x_train), y_train, alpha=j, family="gaussian")
      
      #elastic-cv
      set.seed(1)
      cv.elastic = cv.glmnet(data.matrix(x_train), y_train, type.measure="mse", alpha =j, family="gaussian")
      bestlam = cv.elastic$lambda.min
      elastic.fit = glmnet(data.matrix(x_train), y_train, alpha =j, lambda=bestlam, family="gaussian")
      result_elastic=predict(elastic.fit, data.matrix(x_test))
      
      #exp
      result_elastic = round(exp(result_elastic),2)
      y_test = exp(y_test)
      
      rmse = sqrt(mean((result_elastic - y_test)^2))
      error = round(abs(result_elastic - y_test),2)
      accuracy = round((result_elastic/y_test)*100, 2)
      comparison3 = cbind(result_elastic, y_test,error,rmse, accuracy)
      colnames(comparison3) = c("predict","real","error","rmse","accuracy")
      last_result=cbind(k, j, i,tail(comparison3,1))
      a=rbind(a,last_result)
    }
  }
}

