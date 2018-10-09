# Load SparkR library into your R session
Sys.setenv(SPARK_HOME="/home/karthik/spark-2.0.0")
Sys.setenv(JAVA_HOME="/home/karthik/jdk1.7.0_79")

library(SparkR, lib.loc = "/home/karthik/spark-2.0.0/R/lib/")

# args <- commandArgs(trailing = TRUE)
# 
# if (length(args) != 1) {
#   print("Usage: data-manipulation.R <path-to-flights.csv>")
#   print("The data can be downloaded from: http://s3-us-west-2.amazonaws.com/sparkr-data/flights.csv")
#   q("no")
# }
sqlContext = sparkR.session()

indata = read.csv('~/Datasets/ClusterData.csv',header=T)

sp_indata  <- createDataFrame(indata)
sp_indata$DS_Scaled = sp_indata$DailySteps / 1000;

kmeansModel <- spark.kmeans(sp_indata, ~ age + height + weight + DS_Scaled,k = 3)

# Model summary
summary(kmeansModel)

# Get fitted result from the k-means model
showDF(fitted(kmeansModel))

# Prediction
kmeansPredictions <- predict(kmeansModel, kmeansTestDF)
showDF(kmeansPredictions)
sqlContext
flights <- read.df(sqlContext, "./nycflights13.csv", "com.databricks.spark.csv", header="true")
# sc <- sparkR.init(master = 'spark://nucluster3:7077', appName='SparkR')
# sqlContext <- sparkRSQL.init(sc)
sqlContext= sparkR.session()
postcodes <- read.df(sqlContext, 'hdfs://localhost:50070/bigdata/bigdata.csv', source = 'csv')
sc <- sparkR.init(master='local', sparkPackages="com.databricks:spark-csv_2.11:1.2.0")
  system.time(
    bigdata <- read.df(sqlContext, 
                            '/home/glimid/datasets/bigdata.csv', 
                            header='true', 
                            source = "com.databricks.spark.csv", 
                            inferSchema='true')
  )
  
  colnames(bigdata)
  head(bigdata)
  bigdata$`_c0`=NULL
  bigdata$ID=NULL
  library(rpart)
  bigdata$TARGET = as.factor(bigdata$TARGET)
  dtmod = glm(TARGET ~ ., data=bigdata,family='binomial')
  
  