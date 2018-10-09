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
sdf <- read.df(sqlContext,path = "hdfs://localhost:8020/DataExplore/SP_Flights.csv", 
               source = "com.databricks.spark.csv", inferSchema = "true",header="true")


# Print the schema of this SparkDataFrame
printSchema(sdf)

# Cache the SparkDataFrame
cache(sdf)

# Print the first 6 rows of the SparkDataFrame
showDF(sdf, numRows = 6) ## Or
head(sdf)

# Show the column names in the SparkDataFrame
columns(sdf)

# Show the number of rows in the SparkDataFrame
count(sdf)

# Select specific columns
destDF <- select(sdf, "dest", "cancelled")
head(destDF)
# Using SQL to select columns of data
# First, register the flights SparkDataFrame as a table
createOrReplaceTempView(sdf, "flightsTable")
destDF <- sql("SELECT dest, cancelled FROM flightsTable")
head(destDF)
# Use collect to create a local R data frame
local_df <- collect(destDF)

# Print the newly created local data frame
head(local_df)

# Filter flights whose destination is JFK
jfkDF <- filter(sdf, "dest = \"JFK\"") ##OR
jfkDF <- filter(sdf, sdf$dest == "JFK")

# If the magrittr library is available, we can use it to
# chain data frame operations
if("magrittr" %in% rownames(installed.packages())) {
  library(magrittr)

  # Group the flights by date and then find the average daily delay
  # Write the result into a SparkDataFrame
  groupBy(sdf, sdf$date) %>%
    summarize(avg(sdf$dep_delay), avg(sdf$arr_delay)) -> dailyDelayDF

  # Print the computed SparkDataFrame
  head(dailyDelayDF)
}

head(summarize(groupBy(sdf, sdf$date), count = n(sdf$date)))

# Stop the SparkSession now
sparkR.session.stop()
