library(reshape)
library(reshape2)
library(ggplot2)
library(cluster)
library(plyr)

#####각 조합 코드별로 최소 rmse값을 구한다.
setwd("C:/Users/Hong/Desktop/빅콘/데이터/170924")

##data load
data1 = read.csv("7일차조합.csv")
str(data1)

data2 = read.csv("13일차조합.csv")
str(data2)

##7일차
# setting data
data1=data1[,c(3,4,7)]
data1$error2 = (data1$error)^2
data1$count = 1

# sum of error2
melt1 = melt(data1, "combi.id", "error2")
head(melt1,10)
error2_sum = cast(melt1, combi.id ~ variable, fun=sum)
head(error2_sum)

melt2 = melt(data1, "combi.id", "count")
head(melt2,10)
count_sum = cast(melt2, combi.id ~ variable, fun=sum)
head(count_sum)

RMSE_day7=merge(error2_sum, count_sum, by="combi.id")
RMSE_day7$MSE = RMSE_day7$error2/RMSE_day7$count
RMSE_day7$MSE = sqrt(RMSE_day7$MSE)
head(RMSE_day7)

##13일차
# setting data
data2=data2[,c(3,4,7)]
data2$error2 = (data2$error)^2
data2$count = 1

# sum of error2
melt1 = melt(data2, "combi.id", "error2")
head(melt1,10)
error2_sum = cast(melt1, combi.id ~ variable, fun=sum)
head(error2_sum)

melt2 = melt(data2, "combi.id", "count")
head(melt2,10)
count_sum = cast(melt2, combi.id ~ variable, fun=sum)
head(count_sum)

RMSE_day13=merge(error2_sum, count_sum, by="combi.id")
RMSE_day13$MSE = RMSE_day13$error2/RMSE_day13$count
RMSE_day13$MSE = sqrt(RMSE_day13$MSE)
head(RMSE_day13)


write.csv(RMSE_day7, "RMSE_day7.csv")
write.csv(RMSE_day13, "RMSE_day13.csv")
