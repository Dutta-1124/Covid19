rm(list = ls())
getwd()
library(covid19.analytics)
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)
#Data
tsc<- covid19.data(case = 'ts-confirmed')
tsc<- tsc %>% filter(Country.Region=="US")
tsc<- data.frame(t(tsc))
tsc<-cbind(rownames(tsc),data.frame(tsc,row.names = NULL))
colnames(tsc)<- c("Dates","Confirmed")
tsc$Dates<-ymd(tsc$Dates)
tsc<- tsc[-c(1:4),]
str(tsc)
tsc$Confirmed<- as.numeric(tsc$Confirmed)

#Plot
qplot(Dates,Confirmed,data = tsc,main="Covid19 Confirmed cases in US
      ")

ds<- tsc$Dates
y<- log(tsc$Confirmed)


df<- data.frame(ds,y)

#Forecasting
library(prophet)
m<- prophet(df)

future<-make_future_dataframe(m,periods = 28)

forecast<- predict(m,future)
forecast

#plot Forecast
plot(m,forecast)
dyplot.prophet(m,forecast)
#Forecast Components
prophet_plot_components(m,forecast)


#Model performance
pred<- forecast$yhat[1:410]
Actual<- m$history$y

plot(Actual,pred)
abline(lm(pred~Actual), col="red")
summary(lm(pred~Actual))

#Data for INDIA
#just have to change the country name in line number 10
#And rest all same code
