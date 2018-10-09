# Databricks notebook source exported at Sat, 15 Oct 2016 10:35:16 UTC


# COMMAND ----------

head(mtcars)
mtcarsDF = createDataFrame(sqlContext,mtcars)




# COMMAND ----------

model <- glm(vs ~ mpg + disp + hp + wt,data=mtcarsDF,family="binomial")

# COMMAND ----------

predictions <- predict(model,newData=mtcarsDF)

# COMMAND ----------

modelpredictions <-select(predictions,"vs","prediction")

head(modelpredictions)
