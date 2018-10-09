rattle()
install.packages("sparklyr")
library(sparklyr)
spark_install(version = "1.6.2")
sc <- spark_connect(master = "local")
setwd ("D:/bigdata")
train1<- read.table("2008.csv.bz2",sep = ",", header = T)
summary(train)
train2<- read.table("2007.csv.bz2",sep = ",", header = T)
library(sparklyr)
sc <- spark_connect(master = "local")
train$Date <- as.Date( paste( train$DayofMonth ,train$Month , train$Year ,sep = "-" ))
summary(train$Year)
summary(train$Data)
train$Date <- as.Date( paste(  train$Year,train$Month,train$DayofMonth,sep = "-" ), format = "%d-%m-%Y" )
newdata <- train[1:5,]
summary(newdata)
newdata$mon <- month(newdata$Month)
newdata$Date <- as.Date( paste( newdata$DayofMonth,newdata$Month,newdata$Year,sep = "-" ), format = "%d-%m-%Y" )
newdata$Date <- ISOdate(newdata$DayofMonth,newdata$Month,newdata$Year)
newdata$Month <- month.abb(newdata$Month) 
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

newdata$MonthAbb <- mymonths[ newdata$Month ]
newdata
newdata$Date <- as.Date( paste( newdata$DayofMonth,newdata$MonthAbb,newdata$Year,sep = "-" ), format = "%d-%m-%Y" )

train1$CarrierDelay[is.na(train1$CarrierDelay)]=0
train1$WeatherDelay[is.na(train1$WeatherDelay)]=0
train1$NASDelay[is.na(train1$NASDelay)]=0
train1$SecurityDelay[is.na(train1$SecurityDelay)]=0
train1$LateAircraftDelay[is.na(train1$LateAircraftDelay)]=0
train1$ArrDelay[is.na(train1$ArrDelay)]=0
train1$DepDelay[is.na(train1$DepDelay)]=0
train1$Delay <-	train1$CarrierDelay+train1$WeatherDelay+train1$NASDelay+train1$SecurityDelay+train1$LateAircraftDelay+
  train1$ArrDelay+train1$DepDelay
monthly_totals7<-aggregate(train1$Delay, list(train1$Month,train1$Year), sum)
monthly_totals7



train2$CarrierDelay[is.na(train2$CarrierDelay)]=0
train2$WeatherDelay[is.na(train2$WeatherDelay)]=0
train2$NASDelay[is.na(train2$NASDelay)]=0
train2$SecurityDelay[is.na(train2$SecurityDelay)]=0
train2$LateAircraftDelay[is.na(train2$LateAircraftDelay)]=0
train2$ArrDelay[is.na(train2$ArrDelay)]=0
train2$DepDelay[is.na(train2$DepDelay)]=0
train2$Delay <-	train2$CarrierDelay+train2$WeatherDelay+train2$NASDelay+train2$SecurityDelay+train2$LateAircraftDelay+
  train2$ArrDelay+train2$DepDelay
monthly_totals8<-aggregate(train2$Delay, list(train2$Month,train2$Year), sum)
monthly_totals8
colnames(monthly_totals7) <- c("Month", "Year","Delay")

colnames(monthly_totals8) <- c("Month", "Year","Delay")


total <- rbind(monthly_totals7, monthly_totals8)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

total$Month <- mymonths[ total$Month ]
total <- as.factor(total)
total
str(total)
total$Date <- as.Date( paste("1",total$Month,total$Year,sep = "-" ), format = "%d-%b-%Y" )
total$Date<- as.Date(total$Date,"%Y %b %d")

total
myts <- ts(total$Delay, start=c(2007, 1,1), frequency=12)


f <- decompose(myts)
plot(f)
plot(f$figure, type="b", xaxt="n", xlab="")
monthNames <- months(ISOdate(2011,1:12,5))
axis(1, at=1:12, labels=monthNames, las=2)


