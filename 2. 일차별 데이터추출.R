setwd("D:/r/공모전")
library(glmnet)
rm(list=ls())
movie=read.csv("movie0924.csv") #새로운데이터
movie=subset(movie, movie$일차<=7) #새로운데이터 
Array <-read.csv("Array(13).csv")
#"몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2", "퍼시 잭슨과 괴물의 바다", "컨저링", "남자가 사랑할 때", "수상한 그녀", "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014", "피끓는 청춘", "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", "조선명탐정 : 사라진 놉의 딸", "킹스맨 : 시크릿 에이전트", "도라에몽 : 스탠 바이 미", "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", "오즈의 마법사: 돌아온 도로시", "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", "인턴", "탐정 : 더 비기닝", "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", "최강전사 미니특공대: 영웅의 탄생", "캐롤", "장난감이 살아있다", "거울나라의 앨리스", "고산자, 대동여지도", "달빛궁궐", "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허", "카페 소사이어티", "공조", "더 킹", "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격"

target_list=list("몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2","킹스맨 : 시크릿 에이전트"
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

a=data.frame()
b=seq(0,1,0.1)
len.ar =length(Array$변수) ;len.ar
#rm(list=ls())변수1당 5분 /12당 1시간/ 24당 2시간
#system.time(
for (k in 1:60){
  for (i in target_list){
    for (j in b){
      movie=read.csv("movie0924.csv")
      movie=subset(movie, movie$일차<=7)
      Array=read.csv("Array(13).csv")
      
      #log(data set)
      #관객수로 만든 변수 log / 수치가 큰 데이터(코멘트 수, 평점계) 변수 log
      movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
      movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
      movie$감독누적관객수 = log(movie$감독누적관객수)
      movie$배우파워 = sqrt(movie$배우파워)
    
      
      #scale Data
      movie[ ,c("감독누적관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "공휴일" ,"누적공휴일", "공휴일.합계", "기사수")] = scale(movie[ ,c("감독누적관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "공휴일" ,"누적공휴일", "공휴일.합계", "기사수")])
      # +등급별 데이터 추가 
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
#)
write.csv(a,file="7일차.csv")

###################################
##14일차#################

setwd("D:/r/공모전")
library(glmnet)
rm(list=ls())
movie=read.csv("movie0924.csv")  #새로운데이터
#movie=subset(movie, movie$일차<=7) 
Array <-read.csv("Array0924.csv") #새로운데이터 
#"몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2", "퍼시 잭슨과 괴물의 바다", "컨저링", "남자가 사랑할 때", "수상한 그녀", "넛잡: 땅콩 도둑들", "조선미녀삼총사", "폴리스 스토리 2014", "피끓는 청춘", "미라클 벨리에", "아메리칸 울트라", "치외법권", "앤트맨", "오피스", "조선명탐정 : 사라진 놉의 딸", "킹스맨 : 시크릿 에이전트", "도라에몽 : 스탠 바이 미", "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀", "오즈의 마법사: 돌아온 도로시", "이미테이션 게임", "뮨: 달의 요정", "서부전선", "에베레스트", "인턴", "탐정 : 더 비기닝", "쿵푸팬더3", "검사외전", "앨빈과 슈퍼밴드: 악동 어드벤처", "최강전사 미니특공대: 영웅의 탄생", "캐롤", "장난감이 살아있다", "거울나라의 앨리스", "고산자, 대동여지도", "달빛궁궐", "로빈슨 크루소", "드림 쏭", "매그니피센트 7", "벤허", "카페 소사이어티", "공조", "더 킹", "터닝메카드W: 블랙미러의 부활", "레지던트 이블: 파멸의 날", "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격"

target_list=list("몬스터 대학교", "섀도우 헌터스: 뼈의 도시", "슈퍼배드 2","킹스맨 : 시크릿 에이전트",
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

a=data.frame()
b=seq(0,1,0.1)
len.ar =length(Array$변수) ;len.ar
#rm(list=ls())변수1당 5분 /12당 1시간/ 24당 2시간
#system.time(
for (k in 1:53){
  for (i in target_list){
    for (j in b){
      movie=read.csv("movie0924.csv")
      #movie=subset(movie, movie$일차<=7)
      Array=read.csv("Array0924.csv")
      
      #log(data set)
      #관객수로 만든 변수 log / 수치가 큰 데이터(코멘트 수, 평점계) 변수 log
      
      movie$감독평균최대관객수 = log(movie$감독평균최대관객수)
      movie$장르평균최대관객수 = log(movie$장르평균최대관객수)
      movie$배우파워 = sqrt(movie$배우파워)
      movie$배급사평균최대관객수 = log(movie$배급사평균최대관객수)
      movie$등급평균최대관객수 = log(movie$등급평균최대관객수)
      movie$공휴일평균관객수 = log(movie$공휴일평균관객수)
      movie$공휴일누적평균관객수 = sqrt(movie$공휴일누적평균관객수)
      movie$공휴일총합 = log(movie$공휴일총합)
      
      #scale Data
      #movie[ ,c("감독누적관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "공휴일" ,"누적공휴일", "공휴일.합계", "기사수")] = scale(movie[ ,c("감독누적관객수","배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "공휴일" ,"누적공휴일", "공휴일.합계", "기사수")])
      movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수",
                "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합",
                "평점계", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", 
                "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수",
                "공휴일평균관객수", 
                                                                                                                                                                                   "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")])
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
#)
write.csv(a,file="combi(0924민걸).csv")
