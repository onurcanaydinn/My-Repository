library(knitr)
library(formatR)


opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

rmarkdown::render("/Users/onurcanaydin/Desktop/hw2/markdown.Rmd",output_format="pdf_document")
rmarkdown::render("/Users/onurcanaydin/Desktop/hw2/markdown.Rmd",output_format="html_document")

el_cons<-read.csv("/Users/onurcanaydin/Desktop/360-hw3/GercekZamanliTuketim-01012016-20052021.csv",header=F)

require(lubridate)
library(forecast)
require(data.table)
el_cons<-data.table(el_cons)
el_cons<-el_cons[-c(1),]
colnames(el_cons)=c("Date","Hour","Consumption")

el_cons[,Consumption:=as.numeric(gsub(",","",Consumption,fixed=TRUE))]
el_cons[,Hour:=as.numeric(gsub("[:]","",Hour,perl =TRUE))]
el_cons[,Date:=as.Date(Date,"%d.%m.%Y")]
el_cons[,datetime:=ymd(Date)+dhours(Hour)]
el_cons=el_cons[order(Date)]
acf(el_cons$Consumption)

hour_cons<-el_cons$Consumption
acf(hour_cons)
ts_hour<-ts(hour_cons,frequency = 24)
ts.plot(ts_hour)

hour_cons<-el_cons$Consumption
acf(hour_cons)
ts_hour<-ts(hour_cons,frequency = 24)
ts.plot(ts_hour)
hour_add<-decompose(ts_hour,type="additive")
#print(hour_add)
plot(hour_add)

deseason_hour=ts_hour-hour_add$seasonal
detrend_hour=deseason_hour-hour_add$trend
#plot(detrend_hour)
acf(detrend_hour,na.action = na.pass)

library(dplyr)  
library(zoo)
daily_series=el_cons[,list(avg_consumption=mean(Consumption)),by=list(Date)]
#is.ts(daily_series)

acf(daily_series$avg_consumption)
ts_daily<-ts(daily_series$avg_consumption,frequency = 30,)
ts.plot(ts_daily)

daily_add<-decompose(ts_daily,type="additive")
#print(daily_add)
plot(daily_add)

deseason_daily=ts_daily-daily_add$seasonal
detrend_daily=deseason_daily-daily_add$trend
#plot(detrend_daily)
acf(detrend_daily,na.action = na.pass)

el_cons[,week:=strftime(Date, format = "%Y-W%V")]

weekly_series=el_cons[,list(avg_consumption=mean(Consumption)),by=list(week)]
ts_weekly<-ts(weekly_series$avg_consumption,frequency = 52,start = c(2016,1))
ts.plot(ts_weekly)
weekly_add<-decompose(ts_weekly,type="additive")
#print(weekly_add)
plot(weekly_add)

deseason_weekly=ts_weekly-weekly_add$seasonal
detrend_weekly=deseason_weekly-weekly_add$trend
#plot(detrend_weekly)
acf(detrend_weekly,na.action = na.pass)

el_cons[,month:=strftime(Date,format="%Y-%m")]
monthly_series=el_cons[,list(avg_consumption=mean(Consumption)),by=list(month)]
#is.ts(monthly_series)
#acf(monthly_series$avg_consumption)
ts_monthly<-ts(monthly_series$avg_consumption,frequency=12)
ts.plot(ts_monthly)

monthly_add<-decompose(ts_monthly,type="additive")
#print(monthly_add)
plot(monthly_add)
deseason_mon<-ts_monthly-monthly_add$seasonal
detrend_mon<-deseason_mon-monthly_add$trend
#plot(detrend_mon)
acf(detrend_mon,na.action=na.pass)

model_ar<-arima(detrend_daily,c(1,0,0))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(2,0,0))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(3,0,0))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(7,0,0))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(14,0,0))
print(model_ar)
AIC(model_ar)
model_ar<-arima(detrend_daily,c(21,0,0))
print(model_ar)
AIC(model_ar)
model_ar<-arima(detrend_daily,c(28,0,0))
print(model_ar)
AIC(model_ar)

model_ar<-arima(detrend_daily,c(0,0,1))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,2))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,3))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,7))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,14))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,21))
print(model_ar)
AIC(model_ar)
BIC(model_ar)
model_ar<-arima(detrend_daily,c(0,0,28))
print(model_ar)
AIC(model_ar)
BIC(model_ar)

model_ar<-arima(detrend_daily,c(28,0,28))
print(model_ar)

model_ar<-arima(detrend_daily,c(2,0,35))
print(model_ar)

model_ar<-arima(detrend_daily,c(2,0,21))
print(model_ar)

model_ar<-arima(detrend_daily,c(2,0,42))
print(model_ar)

model_ar<-arima(detrend_daily,c(2,0,14))
print(model_ar)

model_ar<-arima(detrend_daily,c(28,0,28))

model_fitted <- detrend_daily- residuals(model_ar)
#We add because that is additive type of decomposition.
model_fitted_transformed <- model_fitted+daily_add$trend+daily_add$seasonal

plot(daily_add$random, xlab = "Days", ylab = "Electricity Consumption",main="Daily Electricity Consumption Random Part")
points(model_fitted, type = "l", col = 2, lty = 2)

plot(daily_add$x, xlab = "Days", ylab = "Electricity Consumption",main="Daily Electricity Consumption Random Part")
points(model_fitted_transformed, type = "l", col = 2, lty = 2)

model_forecast <- predict(model_ar, n.ahead = 15)$pred
model_forecast=ts(model_forecast,frequency = 30,start=c(66,3))

last_trend_value <-tail(daily_add$trend[!is.na(daily_add$trend)],1)
seasonality=daily_add$seasonal[6:20]
model_forecast=model_forecast+last_trend_value+seasonality

plot(ts_daily,,xlab = "Days", ylab = "Electricity Consumption",main="Daily Electricity Consumption ")
points(model_forecast, type = "l", col = 2, lty = 2)

diff<-abs(tail(ts_daily,15)-model_forecast)
wmpastat<-sum(diff/tail(ts_daily,15))*100*tail(ts_daily,15)/sum(tail(ts_daily,15))
wmpastat

