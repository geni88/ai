gisan_location = read.csv("/Users/jin/Documents/academy/gisanAnaly/gisanData_mun.csv")
summary(gisan_location)
str(gisan_location)
as.numeric(gisan_location$floor)
gisan_location <- gisan_location %>% mutate(floor = as.numeric(floor))

################################결측치 처리 ####################################3
any(is.na(gisan_location))
na.omit(gisan_location)
head(gisan_location)
summary(gisan_location)
hist(gisan_location$floor)
hist(gisan_location$Elev)
hist(gisan_location$price)

if(!require('pastecs')) {
  install.packages('pastecs')
  library('pastecs')
}
round(stat.desc(gisan_location,  basic=TRUE, desc=TRUE, norm=TRUE, p=0.90), 2)
cor(gisan_location)
cor(gisan_location, method = "pearson")
round(stat.desc(gisan_location,  basic=TRUE, desc=TRUE, norm=TRUE, p=0.90), 2)

library(ggplot2)
ggplot(gisan_location, aes(x = floor)) + geom_bar()
ggplot(gisan_location, aes(x = price)) + geom_bar()
ggplot(gisan_location, aes(x = reorder(floor, Elev, parking))) + geom_bar()
#install.packages("tidyverse")
#library(tidyverse)
#library(dplyr)

gisan_location <- gisan_location %>% mutate(unitPrice = price/useSquare)

#########################################################    회귀분석    ###################################

#### ->>>>>>>>>>>>>>>>>>>>>>>>단위면적당 가격분석은 결정계수값이 낮음#############
result=lm(unitPrice~ floor + year + highestFloor + landSquare + totalSquare+ useSquare + Elev + parking + distance ,data=gisan_location)

summary(result)

#############전체 거래금액으로 분석하는 경우 #######################
resultTotal=lm(price~ floor + year + highestFloor + landSquare + totalSquare + useSquare+ Elev  + parking + distance ,data=gisan_location)

summary(resultTotal)
str(gisan_location)
#########################################bptest, whitetest#############################
library(lmtest)
bptest(result)
library(skedastic)
white_lm(result)

install.packages('moonBook')
library(moonBook)
mytable(gisan_location)

library("PerformanceAnalytics")
chart.Correlation(gisan_location, historgram=TRUE, pch=19)


################################################   코로나, 이자율 포함 회귀분석   ########################################33


gisan_location = read.csv("/Users/jin/Documents/academy/gisanAnaly/gisanData_mun.csv")
summary(gisan_location)
str(gisan_location)
as.numeric(gisan_location$floor)
gisan_location <- gisan_location %>% mutate(floor = as.numeric(floor))

################################결측치 처리 ####################################3
any(is.na(gisan_location))
na.omit(gisan_location)
head(gisan_location)
summary(gisan_location)
hist(gisan_location$floor)
hist(gisan_location$Elev)
hist(gisan_location$interrate)
hist(gisan_location$price)
hist(gisan_location$userSquare)
install.packages("psych")
library(psych)
pairs.panels(gisan_location)
pca = prcomp(gisan_location)
typeof(pca)
gisan_location$date<-NULL
summary(pca)
pca

library(ggplot2)
ggplot(gisan_location, aes(x = floor)) + geom_bar()
ggplot(gisan_location, aes(x = price)) + geom_bar()
ggplot(gisan_location, aes(x = reorder(floor, Elev, parking))) + geom_bar()
#install.packages("tidyverse")
#library(tidyverse)
#library(dplyr)

gisan_location <- gisan_location %>% mutate(unitPrice = price/useSquare)

#########################################################회귀분석###################################

#### ->>>>>>>>>>>>>>>>>>>>>>>>단위면적당 가격분석은 결정계수값이 낮음#############
result=lm(unitPrice~ floor + year + highestFloor + landSquare + totalSquare+ useSquare + Elev + parking + distance + interrate + coronaBool ,data=gisan_location)

summary(result)

#############전체 거래금액으로 분석하는 경우 ####################### totalSquare 제외함.
resultTotal=lm(price~ floor + year + highestFloor + landSquare + useSquare+ Elev  + parking + distance + interrate + coronaBool ,data=gisan_location)

summary(resultTotal)
str(gisan_location)

#############전체 거래금액으로 분석하는 경우 ####################### interrate 제외함.
resultTotal_corona=lm(price~ floor + year + highestFloor + landSquare + useSquare+ Elev  + parking + distance  + coronaBool ,data=gisan_location)

summary(resultTotal_corona)

########################                  딥러닝  #######################################################

install.packages("neuralnet")
library(neuralnet)
install.packages("Metrics")
library(Metrics)

data=gisan_location
data=scale(data)
f = price~ floor + year + highestFloor + landSquare + useSquare+ Elev  + parking + distance + interrate + coronaBool
apply(data,2, function(x) sum(is.na(x)))
any(is.na(data))
data  = na.omit(data)
any(is.na(data))
set.seed(2016)
n=nrow(data)
n
train=sample(1:n, 400, FALSE)
fit=neuralnet(f,data=data[train,], hidden=c(10,12,20), algorithm="rprop+",
              err.fct ="sse", act.fct="logistic", threshold=0.1, linear.output=TRUE)
plot(fit)

pred=compute(fit, data[-train, !colnames(data)%in%c("date","tatalsquare")])
round(cor(pred$net.result, data[-train, 10])**2,6)
mse(data[-train,10], pred$net.result)
rmse(data[-train,10], pred$net.result) 
Response=data[-train,10]
Predicted_Value=pred$net.result
plot(Response~Predicted_Value)

1차 수정입니다.