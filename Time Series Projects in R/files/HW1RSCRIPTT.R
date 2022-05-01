
require(openxlsx) 
require(ggplot2)
require(data.table)
require(skimr)
require(GGally)
require(ggcorrplot)
setwd("~/Downloads")
library("readxl")
path=("/Users/onurcanaydin/Downloads/unemployment.xlsx")
unemployment<-read.xlsx("unemployment.xlsx")

ggplot(unemployment,aes(x=UnemploymentRate))+geom_histogram()



  unemployment<-data.table(unemployment)
str(unemployment)


  
  correl_info2=cor(unemployment)
ggcorrplot(correl_info2,hc.order = TRUE, 
           type = "lower",
           lab = TRUE)
ggpairs(unemployment)

  
  lm1<-lm(UnemploymentRate~CapacityUtilizationRateofManufacturing+ DollarExchangeRate+FirmsStatisticsNewlyEstablished +InterestRate+ FirmsStatisticsNewlyClosed,data=unemployment)
summary(lm1)

  
  predicted=predict(lm1,unemployment)
unemployment$predictions=predicted
ggplot(unemployment,aes(x=UnemploymentRate,y=predictions))+geom_point()

  
  step(lm1)

  
  setwd("~/Downloads")
searchkey<-read.xlsx("searchvolume.xlsx")

  
  searchkey<-data.table(searchkey)

ggplot(searchkey, aes(x=Issızlık)) + geom_histogram()+xlab("Unemployment Salary Search Volume")

ggplot(unemployment, aes(x=CapacityUtilizationRateofManufacturing)) + geom_histogram()
ggplot(unemployment, aes(x=DollarExchangeRate)) + geom_histogram()
ggplot(unemployment, aes(x=FirmsStatisticsNewlyEstablished)) + geom_histogram()
ggplot(unemployment, aes(x=InterestRate)) + geom_histogram()
ggplot(unemployment, aes(x=FirmsStatisticsNewlyClosed)) + geom_histogram()
ggplot(unemployment, aes(x=UnemploymentRate)) + geom_histogram()
searchkey$Issızlık<-as.numeric(searchkey$Issızlık)

  
  cor_1<-cor(searchkey$Issızlık,unemployment$CapacityUtilizationRateofManufacturing,use='complete.obs')
cor_2<-cor(searchkey$Issızlık,unemployment$DollarExchangeRate,use='complete.obs')
cor_3<-cor(searchkey$Issızlık,unemployment$FirmsStatisticsNewlyEstablished,use='complete.obs')
cor_4<-cor(searchkey$Issızlık,unemployment$InterestRate,use='complete.obs')
cor_5<-cor(searchkey$Issızlık,unemployment$FirmsStatisticsNewlyClosed,use='complete.obs')
cor_1
cor_2
cor_3
cor_4
cor_5
