library(knitr)
library(readxl)
library(zoo)
library(ggplot2)
library(data.table)
library(forecast)
library(lubridate)
library(stats)
library(urca)
library(xts)
library(plyr)
library(dplyr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE, warning=FALSE, message=FALSE)

data_path= '/Users/onurcanaydin/Desktop/360\ proje/ProjectRawData.csv'
daily_data= fread(data_path)
str(daily_data)
daily_data$event_date <- as.Date(daily_data$event_date, "%d.%m.%Y")

product1_data <- daily_data[product_content_id==48740784]
product2_data <- daily_data[product_content_id==73318567]
product3_data <- daily_data[product_content_id==32737302]
product4_data <- daily_data[product_content_id==31515569]
product5_data <- daily_data[product_content_id==6676673]
product6_data <- daily_data[product_content_id==7061886]
product7_data <- daily_data[product_content_id==85004]
product8_data <- daily_data[product_content_id==4066298]
product9_data <- daily_data[product_content_id==32939029]

ggplot(product1_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 1 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product1_data$sold_count,lag.max = 365, main="The ACF of the Product1")

ggplot(product2_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 2 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product2_data$sold_count,lag.max = 365, main="The ACF of the Product2")

ggplot(product3_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 3 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product3_data$sold_count,lag.max = 365, main="The ACF of the Product3")

ggplot(product4_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 4 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product4_data$sold_count,lag.max = 365, main="The ACF of the Product4")

ggplot(product5_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 5 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product5_data$sold_count,lag.max = 365, main="The ACF of the Product5")

ggplot(product6_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 6 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product6_data$sold_count,lag.max = 365, main="The ACF of the Product6")

ggplot(product7_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 7 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product7_data$sold_count,lag.max = 365, main="The ACF of the Product7")

ggplot(product8_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 8 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product8_data$sold_count,lag.max = 365, main="The ACF of the Product8")

ggplot(product9_data,aes(x=event_date,y=sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Product 9 Graph",x="Time",y="Sales quantity") + theme_dark()

acf(product9_data$sold_count,lag.max = 365, main="The ACF of the Product9")
product1_ts <- ts(product1_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product2_ts <- ts(product2_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product3_ts <- ts(product3_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product4_ts <- ts(product4_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product5_ts <- ts(product5_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product6_ts <- ts(product6_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product7_ts <- ts(product7_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product8_ts <- ts(product8_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product9_ts <- ts(product9_data$sold_count, start = as.Date("2020-05-25"), end = as.Date("2021-05-31"), frequency = 7)
product1_dec_daily <- decompose(x = product1_ts,type = "additive")
product2_dec_daily <- decompose(x = product2_ts,type = "additive")
product3_dec_daily <- decompose(x = product3_ts,type = "additive")
product4_dec_daily <- decompose(x = product4_ts,type = "additive")
product5_dec_daily <- decompose(x = product5_ts,type = "additive")
product6_dec_daily <- decompose(x = product6_ts,type = "additive")
product7_dec_daily <- decompose(x = product7_ts,type = "additive")
product8_dec_daily <- decompose(x = product8_ts,type = "additive")
product9_dec_daily <- decompose(x = product9_ts,type = "additive")
acf(product1_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product1")
acf(product2_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product2")
acf(product3_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product3")
acf(product4_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product4")
acf(product5_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product5")
acf(product6_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product6")
acf(product7_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product7")
acf(product8_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product8")
acf(product9_dec_daily$random,na.action = na.pass,lag.max = 365, main="The ACF of the Random component for Product9")
summary(ur.kpss(product1_dec_daily$random))
summary(ur.kpss(product2_dec_daily$random))
summary(ur.kpss(product3_dec_daily$random))
summary(ur.kpss(product4_dec_daily$random))
summary(ur.kpss(product5_dec_daily$random))
summary(ur.kpss(product6_dec_daily$random))
summary(ur.kpss(product7_dec_daily$random))
summary(ur.kpss(product8_dec_daily$random))
summary(ur.kpss(product9_dec_daily$random))
weekly_product1_data=product1_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product2_data=product2_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product3_data=product3_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product4_data=product4_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product5_data=product5_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product6_data=product6_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product7_data=product7_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product8_data=product8_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product9_data=product9_data[,list(weekly_mean_sold_count=mean(sold_count, na.rm = T)),by=list(week(event_date),year(event_date))]

weekly_product9_data
ggplot(weekly_product1_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 1 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product2_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 2 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product3_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 3 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product4_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 4 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product5_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 5 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product6_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 6 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product7_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 7 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product8_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 8 Graph",x="Time",y="Sales quantity") + theme_dark()

ggplot(weekly_product9_data,aes(x=week,y=weekly_mean_sold_count))+ geom_line(colour = "firebrick2",size = 1.5)+labs(title="Weekly Mean of Product 9 Graph",x="Time",y="Sales quantity") + theme_dark()

weekly_product1_ts <- ts(weekly_product1_data$weekly_mean_sold_count, frequency = 4)
weekly_product2_ts <- ts(weekly_product2_data$weekly_mean_sold_count, frequency = 4)
weekly_product3_ts <- ts(weekly_product3_data$weekly_mean_sold_count, frequency = 4)
weekly_product4_ts <- ts(weekly_product4_data$weekly_mean_sold_count, frequency = 4)
weekly_product5_ts <- ts(weekly_product5_data$weekly_mean_sold_count, frequency = 4)
weekly_product6_ts <- ts(weekly_product6_data$weekly_mean_sold_count, frequency = 4)
weekly_product7_ts <- ts(weekly_product7_data$weekly_mean_sold_count, frequency = 4)
weekly_product8_ts <- ts(weekly_product8_data$weekly_mean_sold_count, frequency = 4)
weekly_product9_ts <- ts(weekly_product9_data$weekly_mean_sold_count, frequency = 4)

product1_dec_weekly <- decompose(x =weekly_product1_ts,type = "additive")
product2_dec_weekly <- decompose(x =weekly_product2_ts,type = "additive")
product3_dec_weekly <- decompose(x =weekly_product3_ts,type = "additive")
product4_dec_weekly <- decompose(x =weekly_product4_ts,type = "additive")
product5_dec_weekly <- decompose(x =weekly_product5_ts,type = "additive")
product6_dec_weekly <- decompose(x =weekly_product6_ts,type = "additive")
product7_dec_weekly <- decompose(x =weekly_product7_ts,type = "additive")
product8_dec_weekly <- decompose(x =weekly_product8_ts,type = "additive")
product9_dec_weekly <- decompose(x =weekly_product9_ts,type = "additive")
acf(product1_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product1")
acf(product2_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product2")
acf(product3_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product3")
acf(product4_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product4")
acf(product5_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product5")
acf(product6_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product6")
acf(product7_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product7")
acf(product8_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product8")
acf(product9_dec_weekly$random,na.action = na.pass,lag.max = 52, main="The ACF of the Random component for Product9")
summary(ur.kpss(product1_dec_weekly$random))
summary(ur.kpss(product2_dec_weekly$random))
summary(ur.kpss(product3_dec_weekly$random))
summary(ur.kpss(product4_dec_weekly$random))
summary(ur.kpss(product5_dec_weekly$random))
summary(ur.kpss(product6_dec_weekly$random))
summary(ur.kpss(product7_dec_weekly$random))
summary(ur.kpss(product8_dec_weekly$random))
summary(ur.kpss(product9_dec_weekly$random))
product1_arima_model= auto.arima(product1_dec_daily$random,max.p = 2,
                                 max.q = 2)
product1_arima_model

product2_arima_model= auto.arima(product2_dec_daily$random,max.p = 2,
                                 max.q = 2)
product2_arima_model

product3_arima_model= auto.arima(product3_dec_daily$random,max.p = 2,
                                 max.q = 2)
product3_arima_model

product4_arima_model= auto.arima(product4_dec_daily$random,max.p = 2,
                                 max.q = 2)
product4_arima_model

product5_arima_model= auto.arima(product5_dec_daily$random,max.p = 2,
                                 max.q = 2)
product5_arima_model

product6_arima_model= auto.arima(product6_dec_daily$random,max.p = 2,
                                 max.q = 2)
product6_arima_model

product7_arima_model= auto.arima(product7_dec_daily$random,max.p = 2,
                                 max.q = 2)
product7_arima_model

product8_arima_model= auto.arima(product8_dec_daily$random,max.p = 2,
                                 max.q = 2)
product8_arima_model

product9_arima_model= auto.arima(product9_dec_daily$random,max.p = 2,
                                 max.q = 2)
product9_arima_model
product1_data[,price_diff:=price-shift(price,1)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(price_diff,res)],col=day(product1_data$event_date))
product1_data[,visit_count_diff:=visit_count-shift(visit_count,7)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(visit_count_diff,res)],col=day(product1_data$event_date))
product1_data[,basket_count_diff:=basket_count-shift(basket_count,7)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(visit_count_diff,res)],col=day(product1_data$event_date))
product1_data[,favored_count_diff:=favored_count-shift(favored_count,7)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(favored_count_diff,res)],col=day(product1_data$event_date))
product1_data[,category_sold_diff:=category_sold-shift(category_sold,7)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(category_sold_diff,res)],col=day(product1_data$event_date))
product1_data[,category_basket_diff:=category_basket-shift(category_basket,7)]

product1_data[,res:=c(rep(NA,7),as.numeric(product1_arima_model$residuals[1:365]))]

plot(product1_data[,list(category_basket_diff,res)],col=day(product1_data$event_date))
product1_data[is.na(price_diff)==T,price_diff:=0]
price_diff_mat_p1<-matrix(product1_data$price_diff)
arimax_product1=auto.arima(product1_data$sold_count,xreg=price_diff_mat_p1,seasonal=T,trace=T,stepwise=F,approximation=F)

product2_data[,category_sold_diff:=category_sold-shift(category_sold,7)]

product2_data[,res:=c(rep(NA,7),as.numeric(product2_arima_model$residuals[1:365]))]

plot(product2_data[,list(category_sold_diff,res)],col=day(product2_data$event_date))

product2_data[,visit_count_diff:=visit_count-shift(visit_count,7)]


product2_data[,res:=c(rep(NA,7),as.numeric(product2_arima_model$residuals[1:365]))]

plot(product2_data[,list(visit_count_diff,res)],col=day(product2_data$event_date))
product2_data[,favored_count_diff:=favored_count-shift(favored_count,7)]


product2_data[,res:=c(rep(NA,7),as.numeric(product2_arima_model$residuals[1:365]))]

plot(product2_data[,list(favored_count_diff,res)],col=day(product2_data$event_date))
product2_data[is.na(visit_count_diff)==T,visit_count_diff:=0]
visit_count_diff_mat_p2<-matrix(product1_data$visit_count_diff)
arimax_product2=auto.arima(product2_data$sold_count,xreg=visit_count_diff_mat_p2,seasonal=T,trace=T,stepwise=F,approximation=F)

product3_data[,price_diff:=price-shift(price,7)]

product3_data[,res:=c(rep(NA,7),as.numeric(product3_arima_model$residuals[1:365]))]

plot(product3_data[,list(price_diff,res)],col=day(product3_data$event_date))
product3_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product3_data[,list(visit_count_diff,res)],col=day(product3_data$event_date))
product3_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product3_data[,list(basket_count_diff,res)],col=day(product3_data$event_date))
product3_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product3_data[,list(favored_count_diff,res)],col=day(product3_data$event_date))
product3_data[is.na(basket_count)==T,basket_count:=0]
basket_count_mat_p3<-matrix(product3_data$basket_count)
arimax_product3=auto.arima(product3_data$sold_count,xreg=basket_count_mat_p3,seasonal=T,trace=T,stepwise=F,approximation=F)

product4_data[,price_diff:=price-shift(price,7)]

product4_data[,res:=c(rep(NA,7),as.numeric(product4_arima_model$residuals[1:365]))]

plot(product4_data[,list(price_diff,res)],col=day(product4_data$event_date))
product4_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product4_data[,list(visit_count_diff,res)],col=day(product4_data$event_date))
product4_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product4_data[,list(basket_count_diff,res)],col=day(product4_data$event_date))
product4_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product4_data[,list(favored_count_diff,res)],col=day(product4_data$event_date))
product4_data[,category_sold_diff:=category_sold-shift(category_sold,4)]
plot(product4_data[,list(category_sold_diff,res)],col=day(product4_data$event_date))
product4_data[is.na(category_sold_diff)==T,category_sold_diff:=0]
category_sold_diff_mat_p4<-matrix(product4_data$category_sold_diff)
arimax_product4=auto.arima(product4_data$sold_count,xreg=category_sold_diff_mat_p4,seasonal=T,trace=T,stepwise=F,approximation=F)
product5_data[,price_diff:=price-shift(price,4)]

product5_data[,res:=c(rep(NA,7),as.numeric(product5_arima_model$residuals[1:365]))]

plot(product5_data[,list(price_diff,res)],col=day(product5_data$event_date))
product5_data[,basket_count_diff:=basket_count-shift(basket_count,4)]
plot(product5_data[,list(basket_count_diff,res)],col=day(product5_data$event_date))
product5_data[,favored_count_diff:=favored_count-shift(favored_count,4)]
plot(product5_data[,list(favored_count_diff,res)],col=day(product5_data$event_date))
product5_data[,category_sold_diff:=category_sold-shift(category_sold,4)]
plot(product5_data[,list(category_sold_diff,res)],col=day(product5_data$event_date))
product5_data[,category_brand_sold_diff:=category_brand_sold-shift(category_brand_sold,4)]
plot(product5_data[,list(category_brand_sold_diff,res)],col=day(product5_data$event_date))
arima_p5<-arima(product5_data$sold_count ,order=c(1,0,1),seasonal = c(1,0,1))
product5_data[,residuals:=residuals(arima_p5)]
residuals_mat_p5<-matrix(product5_data$residuals)
arimax_product5=auto.arima(product5_data$sold_count,xreg=residuals_mat_p5,seasonal=T,trace=T,stepwise=F,approximation=F)
product6_data[,price_diff:=price-shift(price,7)]
product6_data[,res:=c(rep(NA,7),as.numeric(product6_arima_model$residuals[1:365]))]

plot(product6_data[,list(price_diff,res)],col=day(product6_data$event_date))
product6_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product6_data[,list(visit_count_diff,res)],col=day(product6_data$event_date))
product6_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product6_data[,list(basket_count_diff,res)],col=day(product6_data$event_date))
product6_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product6_data[,list(favored_count_diff,res)],col=day(product6_data$event_date)) 
product6_data[,category_sold_diff:=category_sold-shift(category_sold,7)]
plot(product6_data[,list(category_sold_diff,res)],col=day(product6_data$event_date))
product6_data[,category_brand_sold_diff:=category_brand_sold-shift(category_brand_sold,7)]
plot(product6_data[,list(category_brand_sold_diff,res)],col=day(product6_data$event_date))
product6_data[is.na(basket_count_diff)==T,basket_count_diff:=0]
basket_count_diff_mat_p6<-matrix(product6_data$basket_count_diff)
arimax_product6=auto.arima(product6_data$sold_count,xreg=basket_count_diff_mat_p4,seasonal=T,trace=T,stepwise=F,approximation=F)
product7_data[,price_diff:=price-shift(price,7)]
product7_data[,res:=c(rep(NA,7),as.numeric(product7_arima_model$residuals[1:365]))]

plot(product7_data[,list(price_diff,res)],col=day(product7_data$event_date))
product7_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product7_data[,list(visit_count_diff,res)],col=day(product7_data$event_date))
product7_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product7_data[,list(basket_count_diff,res)],col=day(product7_data$event_date))
product7_data$favored_count
product7_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product7_data[,list(favored_count_diff,res)],col=day(product7_data$event_date)) 
product7_data[is.na(visit_count)==T,visit_count:=0]
visit_count_mat_p7<-matrix(product7_data$visit_count)
arimax_product7=auto.arima(product7_data$sold_count,xreg=visit_count_mat_p7,seasonal=T,trace=T,stepwise=F,approximation=F)
product8_data[,price_diff:=price-shift(price,7)]
product8_data[,res:=c(rep(NA,7),as.numeric(product8_arima_model$residuals[1:365]))]

plot(product8_data[,list(price_diff,res)],col=day(product8_data$event_date))
product8_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product8_data[,list(visit_count_diff,res)],col=day(product8_data$event_date))
product8_data$basket_count
product8_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product8_data[,list(basket_count_diff,res)],col=day(product8_data$event_date))
product8_data$favored_count
product8_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product8_data[,list(favored_count_diff,res)],col=day(product8_data$event_date)) 
product8_data$category_sold
product8_data[,category_sold_diff:=category_sold-shift(category_sold,7)]
plot(product8_data[,list(category_sold_diff,res)],col=day(product8_data$event_date))
product8_data[is.na(category_sold_diff)==T,category_sold_diff:=0]
category_sold_mat_p8<-matrix(product8_data$category_sold_diff)
arimax_product8=auto.arima(product7_data$sold_count,xreg=category_sold_mat_p8,seasonal=T,trace=T,stepwise=F,approximation=F)
product9_data[,price_diff:=price-shift(price,7)]
product9_data[,res:=c(rep(NA,7),as.numeric(product9_arima_model$residuals[1:365]))]

plot(product9_data[,list(price_diff,res)],col=day(product9_data$event_date))
product9_data[,visit_count_diff:=visit_count-shift(visit_count,7)]
plot(product9_data[,list(visit_count_diff,res)],col=day(product9_data$event_date))
product9_data[,basket_count_diff:=basket_count-shift(basket_count,7)]
plot(product9_data[,list(basket_count_diff,res)],col=day(product9_data$event_date))
product9_data$favored_count
product9_data[,favored_count_diff:=favored_count-shift(favored_count,7)]
plot(product9_data[,list(favored_count_diff,res)],col=day(product9_data$event_date)
product9_data$category_sold
product9_data[,category_sold_diff:=category_sold-shift(category_sold,7)]
plot(product9_data[,list(category_sold_diff,res)],col=day(product9_data$event_date))
product9_data$category_brand_sold
product9_data[,category_brand_sold_diff:=category_brand_sold-shift(category_brand_sold,7)]
plot(product9_data[,list(category_brand_sold_diff,res)],col=day(product9_data$event_date))
product9_data[is.na(price_diff)==T,price_diff:=0]
price_diff_mat_p9<-matrix(product9_data$price_diff)
arimax_product9=auto.arima(product9_data$sold_count,xreg=price_diff_mat_p9,seasonal=T,trace=T,stepwise=F,approximation=F)
train_start=as.Date('2020-05-25')
test_start=as.Date('2021-05-11')
test_end=as.Date('2021-05-29')
train_date=seq(train_start,test_start,by='day')
str(train_date)
test_dates=seq(test_start,test_end,by='day')
test_dates

# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(0,0,1),seasonal=c(0,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product1
  
  #forecasted=forecast(fitted,xreg=price_diff_mat_p1[239],  h=nahead)
  forecasted=forecast(fitted,xreg=tail(price_diff_mat_p1[!is.na(price_diff_mat_p1)], 1),  h=nahead)
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product1_data[event_date<=current_date]
  forecast_data=product1_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}

performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(2,0,0),seasonal = c(2,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product2
  
  forecasted=forecast(fitted,xreg=tail(visit_count_diff_mat_p2[!is.na(visit_count_diff_mat_p2)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product2_data[event_date<=current_date]
  forecast_data=product2_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))

melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(2,0,0),seasonal = c(2,0,2))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product3
  
  forecasted=forecast(fitted,xreg=tail(basket_count_mat_p3[!is.na(basket_count_mat_p3)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product3_data[event_date<=current_date]
  forecast_data=product3_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(1,0,2),seasonal = c(0,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product4
  
  forecasted=forecast(fitted,xreg=tail(category_sold_diff_mat_p4[!is.na(category_sold_diff_mat_p4)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product4_data[event_date<=current_date]
  forecast_data=product4_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance

# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(1,0,1),seasonal = c(1,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product5
  
  forecasted=forecast(fitted,xreg=tail(residuals_mat_p5[!is.na(residuals_mat_p5)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product5_data[event_date<=current_date]
  forecast_data=product5_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(1,0,2),seasonal = c(1,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product6
  
  forecasted=forecast(fitted,xreg=tail(basket_count_diff_mat_p6[!is.na(basket_count_diff_mat_p6)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product7_data[event_date<=current_date]
  forecast_data=product7_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(1,0,2),seasonal = c(2,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product7
  
  forecasted=forecast(fitted,xreg=tail(visit_count_mat_p7[!is.na(visit_count_mat_p7)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product7_data[event_date<=current_date]
  forecast_data=product7_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(0,0,1),seasonal = c(0,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product8
  
  forecasted=forecast(fitted,xreg=tail(category_sold_mat_p8[!is.na(category_sold_mat_p8)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product8_data[event_date<=current_date]
  forecast_data=product8_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance
# forecast with ARIMA models
forecast_with_arima=function(data,forecast_ahead,target_name='sold_count',
                             is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  fitted=arima(input_series,order=c(1,0,1),seasonal = c(1,0,1))
  
  forecasted=forecast(fitted,h=forecast_ahead)
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
# forecast with ARIMAX models
forecast_with_arimax=function(data,forecast_ahead,target_name='sold_count',
                              is_seasonal=F,is_stepwise=F,is_trace=T,is_approx=F){
  command_string=sprintf('input_series=data$%s',target_name)
  print(command_string)
  eval(parse(text=command_string))
  
  
  fitted=arimax_product9
  
  forecasted=forecast(fitted,xreg=tail(price_diff_mat_p9[!is.na(price_diff_mat_p9)], 1),  h=nahead)
  
  
  return(list(forecast=as.numeric(forecasted$mean),model=fitted))
}
#loop over the test dates
forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  print(test_dates[i])
  past_data=product8_data[event_date<=current_date]
  forecast_data=product8_data[event_date==test_dates[i]]
  
  
  
  arima_forecast=forecast_with_arima(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arima_prediction:=arima_forecast$forecast]
  arimax_forecast=forecast_with_arimax(past_data,forecast_ahead,'sold_count',is_trace=F)
  forecast_data[,arimax_prediction:=arimax_forecast$forecast]
  
  results[[i]]=forecast_data
}

overall_results=rbindlist(results)

melted_result=melt(overall_results,c('event_date','sold_count'),c('arima_prediction','arimax_prediction'))
melted_result 
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}
performance=melted_result[,accu(sold_count,value),by=list(variable)]

performance

