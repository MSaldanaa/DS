# Databricks notebook source exported at Sat, 15 Oct 2016 10:34:38 UTC
indata=read.df(sqlContext,"/FileStore/tables/lq1ekjbf1476522508071/ClusterData.csv",source="com.databricks.spark.csv",header='true',inferSchema='true')

# COMMAND ----------

kmeansmodel <-spark.kmeans(indata,~ age + height + weight+DailySteps,k=3)
summary(kmeansmodel)

# COMMAND ----------

schema <-structType(structField("age","integer"),structField("height","integer"),structField("weight","integer"),structField("DailySteps","integer"),structField("DS_log","double"))

df1 <- dapply(indata, function(x) {x <-cbind (x,log(x$DailySteps))},schema)


head(df1)


# COMMAND ----------

kmeansmodel1 <-spark.kmeans(df1,~ age + height + weight+ DS_log,k=3)
summary(kmeansmodel1)

