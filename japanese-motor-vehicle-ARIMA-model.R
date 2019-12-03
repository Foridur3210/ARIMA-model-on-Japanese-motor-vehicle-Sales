getwd()
setwd("C:/Users/admin/Desktop/sem 3/seema maam")
jap_am<-read.csv("japanese-annual-motor-vehicle-pr.csv")
library(fpp2)
plot(jap_am)
head(jap_am)
str(jap_am)
class(jap_am)
timeseries<-ts(jap_am$annual.motor.vehicle.prod,start=1947,end = 1989,frequency = 1)
timeseries
View(jap_am)
class(timeseries)
plot(timeseries,main="jap annual motor production",xlab="year",ylab="no. of motor sales")
library(urca)
test_jap<-ur.kpss(timeseries)
summary(test_jap)

test_dif1<-diff(timeseries,differences = 1)
plot(test_dif1,xlab="year",ylab="no of vehicles")
test_jap1<-ur.kpss(test_dif1)
summary(test_jap1)

test_dif2<-diff(test_dif1,differences = 1)
plot(test_dif2,xlab="year",ylab="no of vehicles")
test_jap2<-ur.kpss(test_dif2)
summary(test_jap2)

par(mfrow=c(1,2))
pacf(test_dif2,main="PACF",xlab="lag",ylab=" PARTIAL ACF")
Acf(test_dif2,main="ACF",xlab="lag")

#fit ARIMA model
#try p=1,2 d=2, q=1,2
fit1<-Arima(timeseries,order = c(1,2,1))
summary(fit1)

#best model here is p=1,d=2,q=2 which has least AICC

#IDENTIFICATION OF arima using auto.arima
ARIMAFIT=auto.arima(timeseries,approximation = FALSE,stepwise = FALSE)
plot(ARIMAFIT)
summary(ARIMAFIT)

#residual test
checkresiduals(ARIMAFIT)
