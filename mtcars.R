#

Sys.setenv(SPARK_HOME="/home/karthik/spark-2.0.0")
Sys.setenv(JAVA_HOME="/home/karthik/jdk1.7.0_79")

library(SparkR, lib.loc = "/home/karthik/spark-2.0.0/R/lib/")

# Initialize SparkSession
sparkR.session(appName = "SparkR DataFrame HandsOn")

# Create a simple local data.frame
localDF <- data.frame(mtcars)

# Convert local data frame to a SparkDataFrame
df <- createDataFrame(localDF)

# Print its schema
printSchema(df)

# Register this DataFrame as a table.
createOrReplaceTempView(df, "cars")

# SQL statements can be run by using the sql methods
automatic <- sql("SELECT AVG(mpg) FROM cars WHERE am = 0")
print(automatic)

# Call collect to get a local data.frame
carsLocalDF <- collect(automatic)

# Print the teenagers in our dataset
print(carsLocalDF)
# Stop the SparkSession now
sparkR.session.stop()
