#

Sys.setenv(SPARK_HOME="/home/karthik/spark-2.0.0")
Sys.setenv(JAVA_HOME="/home/karthik/jdk1.7.0_79")

library(SparkR, lib.loc = "/home/karthik/spark-2.0.0/R/lib/")

# Initialize SparkSession
sparkR.session(appName = "SparkR DataFrame HandsOn")

# Create a simple local data.frame
localDF <- data.frame(name=c("John", "Smith", "Sarah"), age=c(19, 23, 18))

# Convert local data frame to a SparkDataFrame
df <- createDataFrame(localDF)

# Print its schema
printSchema(df)
# root
#  |-- name: string (nullable = true)
#  |-- age: double (nullable = true)

# Create a DataFrame from a JSON file
path <- file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json")
peopleDF <- read.json(path)
printSchema(peopleDF)
# root
#  |-- age: long (nullable = true)
#  |-- name: string (nullable = true)

# Register this DataFrame as a table.
createOrReplaceTempView(peopleDF, "people")

# SQL statements can be run by using the sql methods
teenagers <- sql("SELECT name FROM people WHERE age >= 13 AND age <= 19")

# Call collect to get a local data.frame
teenagersLocalDF <- collect(teenagers)

# Print the teenagers in our dataset
print(teenagersLocalDF)
print(teenagers)
# Stop the SparkSession now
sparkR.session.stop()
