
setwd("D:/r/공모전")
library(glmnet)

rm(list=ls())
movie=read.csv("movie0925(2)킹스맨2.csv")
#movie=read.csv("movie0925(4).csv")
#movie=read.csv("movie0925(3).csv")

Array=read.csv("Array0924.csv")
#
a=data.frame()


#7일차 
#movie=subset(movie, movie$일차<=7)
#k= 7#변수
#i= "넛잡2"
#j= 0.9#알파 


#14일차
k=83
i="킹스맨2"
j=0.1


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
movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", "감독평균최대관객수", "장르평균최대관객수",
          "배급사평균최대관객수", "등급평균최대관객수", "공휴일평균관객수", "공휴일누적평균관객수", "공휴일총합",
          "평점계", "기사수")] = scale(movie[ ,c("배우파워", "평점", "코멘트수", "일차", "일차2", "일차3", 
                                           "감독평균최대관객수", "장르평균최대관객수", "배급사평균최대관객수", "등급평균최대관객수",
                                           "공휴일평균관객수", 
                                           "공휴일누적평균관객수", "공휴일총합", "평점계", "기사수")])
# +등급별 데이터 추가 
#movie=scale(movie)

un=unlist(strsplit(as.character(Array[k,2]),fixed=TRUE,split=","))
x_train<-movie[,c('영화명',un)]
y_train = movie[,c("영화명","일차","누적관객수")]

tail(x_train)
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
result_elastic = predict(elastic.fit, data.matrix(x_test))

#exp
result_elastic = round(exp(result_elastic),2)
result_elastic

#elastic.fit$beta
bestlam

y_test = exp(y_test)

rmse = sqrt(mean((result_elastic - y_test)^2))
error = round(abs(result_elastic - y_test),2)
accuracy = round((result_elastic/y_test)*100, 2)
comparison3 = cbind(result_elastic, y_test,error,rmse, accuracy)
colnames(comparison3) = c("predict","real","error","rmse","accuracy")
last_result=cbind(k, j, i,tail(comparison3,1))
a=rbind(a,last_result)      

