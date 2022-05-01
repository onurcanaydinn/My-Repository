library(knitr)
library(formatR)


opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
#
rmarkdown::render("/Users/onurcanaydin/Desktop/hw2/markdown.Rmd",output_format="pdf_document")
rmarkdown::render("/Users/onurcanaydin/Desktop/hw2/markdown.Rmd",output_format="html_document")
#
library(data.table)
library(lubridate)
library(forecast)
require(ggcorrplot)
require(ggplot2)

require(openxlsx )
library(openxlsx)
library(zoo)
#

setwd("~/Desktop/hw2")
izmirmortgage<-fread('hw2.csv')
izmirmortgage[,date:=as.yearmon(Date)]
#str(izmirmortgage)
izmirmortgage[,V6:=NULL]
izmirmortgage[,Date:=NULL]
plot(izmirmortgage)
#
mortgage<-data.table(izmirmortgage$MortgageIzmır)

mortgage[,trend:=1:.N]
mortgage[,date:=izmirmortgage$date]
names(mortgage)[1]<-"mortgages"
ggplot(mortgage,aes(x=date,y=mortgages))+geom_line()
#
earthquake<-read.xlsx('deprem.xlsx')
earthquake<-data.table(earthquake)
earthquake[,earthqizmir:=as.numeric(izmirearthquake)]
earthquake[is.na(earthqizmir)==T,earthqizmir:=0]
earthquake[,date:=as.yearmon(Ay)]
ggplot(earthquake,aes(x=date,y=earthqizmir))+geom_line()
earthquake[,date:=NULL]
earthquake[,Ay:=NULL]
#
mortgage<-cbind(mortgage,earthquake)
fit<-lm(mortgages~earthqizmir,mortgage)
summary(fit)
#
mortgage[trend==54,is_earth:=1]
mortgage[trend==95,is_earth:=1]

mortgage[is.na(is_earth)==T,is_earth:=0]
fit<-lm(mortgages~as.factor(is_earth),mortgage)
summary(fit)
#
mortgage[,intrateonhouse:=izmirmortgage$InterestrateonHouse]
fit<-lm(mortgages~intrateonhouse,mortgage)
summary(fit)
checkresiduals(fit)
#
mortgage[,buyconfordurable:=izmirmortgage$BuyingforDurableGoods]
fit<-lm(mortgages~intrateonhouse+buyconfordurable,mortgage)
summary(fit)
checkresiduals(fit)
#
mortgage[,exchangerate:=izmirmortgage$ExchangeRate]
fit<-lm(mortgages~intrateonhouse+buyconfordurable+exchangerate,mortgage)
summary(fit)
checkresiduals(fit)
#
mortgage[, lag.value:=c(NA, mortgages[-.N])]

mortgage$lag.value[1]=2153


fit<-lm(mortgages~intrateonhouse+buyconfordurable+lag.value,mortgage)
summary(fit)
checkresiduals(fit)
#
mortgage[,fitted:=fitted(fit)]

mortgage[,residual:=residuals(fit)]

mortgage %>%  
  ggplot(aes(x=fitted, y=residual)) + 
  geom_point()
#
mortgage %>%
  ggplot(aes(x=fitted, y=mortgages)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0)
#
month_info<-c(4)

mortgage=rbind(mortgage,data.table(month=as.factor(month_info)),fill=T)

predict(mortgage$intrateonhouse)
mortgage$intrateonhouse[100]=17.74003
mortgage[,trend:=1:.N]
predict(mortgage$buyconfordurable)
mortgage$buyconfordurable[100]=43.34977
predict(mortgage$lag.value)
mortgage$lag.value[100]=1005.977
mortgage$fitted[100]=predict(fit,mortgage)
#
mortgage$fitted[100]
cols <- c("predicted" = "orange", "actual" = "blue")
ggplot() + 
  geom_line(data = mortgage[trend<101], aes(x = trend, y = fitted,color = "predicted")) +
  geom_line(data = mortgage[trend<101], aes(x = trend, y = mortgages,color = "actual")) +
  xlab('time') +
  ylab('sales') +
  scale_color_manual(values = cols)
#
