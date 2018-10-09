# Databricks notebook source exported at Sun, 20 Nov 2016 12:49:08 UTC
indata7 <- read.df(sqlContext, path="/FileStore/tables/nauzlc2c1479573369118/2007_csv-6dd32.bz2", 
   source="com.databricks.spark.csv", header="true",inferSchema='true',na.strings = "0")


# COMMAND ----------

data2007 <- subset(indata7, select = c(1,2,3,15,16,25,26,27,28,29))

# COMMAND ----------

data2007 <-fillna(data2007, 0)

# COMMAND ----------

data2007$Year  <- cast(data2007$Year ,"integer")
data2007$Month  <- cast(data2007$Month ,"integer")
data2007$DayofMonth  <- cast(data2007$DayofMonth ,"integer")
data2007$ArrDelay  <- cast(data2007$ArrDelay ,"integer")
data2007$DepDelay  <- cast(data2007$DepDelay ,"integer")
data2007$CarrierDelay <- cast(data2007$CarrierDelay,"integer")
data2007$WeatherDelay  <- cast(data2007$WeatherDelay,"integer")
data2007$NASDelay  <- cast(data2007$NASDelay ,"integer")
data2007$SecurityDelay   <- cast(data2007$SecurityDelay  ,"integer")
data2007$LateAircraftDelay <- cast(data2007$LateAircraftDelay ,"integer")



# COMMAND ----------

data2007$totaldelay = data2007$CarrierDelay+data2007$WeatherDelay+data2007$NASDelay+data2007$SecurityDelay+data2007$LateAircraftDelay+data2007$ArrDelay+data2007$DepDelay

# COMMAND ----------

totaldelay_2007 <- collect(agg(groupBy(data2007, "Month"), total_delay=sum(data2007$totaldelay)))

# COMMAND ----------


totaldelay_2007$Month <- factor(totaldelay_2007$Month, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), 

  labels=c('Jan2007','Feb2007','Mar2007','Apr2007','May2007','Jun2007','Jul2007','Aug2007','Sep2007','Oct2007','Nov2007','Dec2007'))

# COMMAND ----------

indata8 <- read.df(sqlContext, path="/FileStore/tables/nauzlc2c1479573369118/2008_csv-db05f.bz2", 
   source="com.databricks.spark.csv", header="true",inferSchema='true',na.strings = "0")


# COMMAND ----------

data2008 <- subset(indata8, select = c(1,2,3,15,16,25,26,27,28,29))

# COMMAND ----------

data2008 <-fillna(data2008, 0)

# COMMAND ----------

data2008$Year  <- cast(data2008$Year ,"integer")
data2008$Month  <- cast(data2008$Month ,"integer")
data2008$DayofMonth  <- cast(data2008$DayofMonth ,"integer")
data2008$ArrDelay  <- cast(data2008$ArrDelay ,"integer")
data2008$DepDelay  <- cast(data2008$DepDelay ,"integer")
data2008$CarrierDelay <- cast(data2008$CarrierDelay,"integer")
data2008$WeatherDelay  <- cast(data2008$WeatherDelay,"integer")
data2008$NASDelay  <- cast(data2008$NASDelay ,"integer")
data2008$SecurityDelay   <- cast(data2008$SecurityDelay  ,"integer")
data2008$LateAircraftDelay <- cast(data2008$LateAircraftDelay ,"integer")

# COMMAND ----------

data2008$totaldelay = data2008$CarrierDelay+data2008$WeatherDelay+data2008$NASDelay+data2008$SecurityDelay+data2008$LateAircraftDelay+data2008$ArrDelay+data2008$DepDelay

# COMMAND ----------

totaldelay_2008 <- collect(agg(groupBy(data2008, "Month"), total_delay=sum(data2008$totaldelay)))

# COMMAND ----------


totaldelay_2008$Month <- factor(totaldelay_2008$Month, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), 

  labels=c('Jan2008','Feb2008','Mar2008','Apr2008','May2008','Jun2008','Jul2008','Aug2008','Sep2008','Oct2008','Nov2008','Dec2008'))

# COMMAND ----------


overall_delay = rbind(totaldelay_2007,totaldelay_2008)



# COMMAND ----------

overall_delay$Date = as.Date(overall_delay$Month,"%d%b%Y")



attach(overall_delay)

overall_delay = overall_delay[order(Date),]

overall_delay



myts <- ts(overall_delay$total_delay, start=c(2007,1,1), frequency=12)

myts



f <- decompose(myts)

plot(f)

plot(f$figure, type="b", xaxt="n", xlab="")

monthNames <- months(ISOdate(2007,1:12,5))

axis(1, at=1:12, labels=monthNames, las=2)
