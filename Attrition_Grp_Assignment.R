library(data.table)

setwd ("C:/RajeshOfficial/Technical/GreatLakes/datamining/Attrition Assignment")

getwd()
## Read the attrition CSV file
attrition_tbl=fread("1452762979_586__HR_Employee_Attrition_Data.csv",header = T,
                    sep = ',',stringsAsFactors = TRUE,
                    dec = "." )


## See the structure of the data set
str(attrition_tbl)
summary(attrition_tbl)


## Split into training  & test data set in 70/30 ratio
indexes = sample(1:nrow(attrition_tbl), 0.30*nrow(attrition_tbl))


test_attrition_data = attrition_tbl[indexes,]

training_attrition_data = attrition_tbl[-indexes,]



##Remove columns that have constant values or that does not have impact on classification
## Remove columns "EmployeeCount", "EmployeeNumber","Over18","StandardHours

colToDelete = c("EmployeeCount", "EmployeeNumber","Over18","StandardHours")
training_attrition_data[, (colToDelete) := NULL]
test_attrition_data[, (colToDelete) := NULL]


###################################################################################################
## CART

library(rpart)

rCart <- rpart(Attrition~.,method="class", data=training_attrition_data)

printcp(rCart)

plotcp(rCart)

summary(rCart)


# plot tree 
par(mfrow = c(1,2), xpd = NA)

plot(rCart, uniform=TRUE, 
     main="Classification Tree for Attrition Data")

text(rCart, use.n=TRUE, all=TRUE, cex=.8)


##Predict the model
rPredict = data.table(predict(rCart, newdata=test_attrition_data))

                         
cartPredict.predict = ifelse(round(rPredict$No) == 1,0,1)
cartPredict.result  = ifelse(as.character(test_attrition_data$Attrition) == 'No',0,1)


cartPrediction = data.table(predict=cartPredict.predict, 
                            result=cartPredict.result)


cartPrediction$actual = abs(cartPrediction$predict - cartPrediction$result)

incorrectPredict = 100 * sum(cartPrediction$actual) /length(cartPrediction$actual)

cat("CART  Prediction Accuracy% on Test Data Set  = ", 100-incorrectPredict)



## Table for capturing Model Evaluation Parameters
modelEvaluation = data.table(ModelName="",Accuracy=0, AUC=0, KS=0, Sensitivity=0, Specificity=0)


#######CART Model Evaluation###############

##Error / COnfusion MAtrix
library(caret)

paste("Confusion MAtrix for CART")
cartErrorMatrix = confusionMatrix(cartPrediction$predict,cartPrediction$actual,positive = '1')


## ROC Curve
library("ROCR")
##ROC PLot  for CART Network


 cartROC  = ROCR::prediction(as.vector(cartPrediction$predict),
                      as.vector(cartPrediction$actual))

cartPerf =  ROCR::performance(cartROC, measure = "tpr", x.measure = "fpr") 

auc <- performance(cartROC,"auc")
aucVal = paste("Area Under Curve for CART = ",auc@y.values)


plot(cartPerf,
     main="Atrition Classification using Neural Net  - ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")

abline(0,1,col="grey")

## KS Score  : max delta between cumulative bad and good rates being plotted by
#ROCR
ks = max(attr(cartPerf,'y.values')[[1]]-attr(cartPerf,'x.values')[[1]])[]

paste("KS Score for CART Based Attrition Model  :",ks)


##Lift  Chart
liftObj <- performance(cartROC, measure="lift", x.measure="rpp")
plot(liftObj,
     main="CART Attrition Network - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


library(gains)
# gains table
gains.cross <- gains(actual=rPredict$No , 
                     predicted=cartPrediction$result,
                     groups=2)
print(gains.cross)
plot.gains(gains.cross)

modelEvaluation$ModelName = "CART"
modelEvaluation$Accuracy[1] = 100-incorrectPredict
modelEvaluation$AUC[1] = auc@y.values[[1]]
modelEvaluation$KS[1] = ks
modelEvaluation$Sensitivity[1] = cartErrorMatrix$byClass[[1]] 
modelEvaluation$Specificity[1] = cartErrorMatrix$byClass[[2]]





###################################################################################################
## Random Forest


library(randomForest)

rForest <- randomForest(Attrition~., training_attrition_data,ntree=500)

summary(rForest)

plot(rForest)

forestPrediction = predict(rForest, test_attrition_data)
forestPrediction = data.table(predict=factor(forestPrediction), 
                              actual=factor(test_attrition_data$Attrition))

forestPrediction$accuracy = abs(as.numeric(forestPrediction$predict) - 
                                  as.numeric(forestPrediction$actual))


incorrectPredict = 100 * sum(forestPrediction$accuracy) /length(forestPrediction$accuracy)

cat("Forest Prediction Accuracy % = ", 100-incorrectPredict)



#######Random Forest  Model Evaluation###############

##Error / COnfusion MAtrix
library(caret)

paste("Confusion MAtrix for Random Forest ")
forestErrorMatrix= confusionMatrix(forestPrediction$predict,forestPrediction$actual,positive = "Yes")

library("ROCR")
## ROC Curve

##ROC PLot  for Random Forest

forestROC  = ROCR::prediction(as.vector(as.numeric(forestPrediction$predict)),
                      as.vector(as.numeric(cartPrediction$actual)))

forestPerf =  ROCR::performance(forestROC, measure = "tpr", x.measure = "fpr") 

auc <- ROCR::performance(forestROC,"auc")
aucVal = paste("Area Under Curve for Random Forest = ",auc@y.values)

paste(aucVal)

plot(forestPerf,
     main="Atrition Classification using Random Forest  - ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")

abline(0,1,col="grey")

## KS Score  : max delta between cumulative bad and good rates being plotted by
#ROCR
ks = max(attr(forestPerf,'y.values')[[1]]-attr(forestPerf,'x.values')[[1]])[]

paste("KS Score for Random Forest  Based Attrition Model  :",ks)


##Lift  Chart
liftObj <- performance(forestPerf, measure="lift", x.measure="rpp")
plot(liftObj,
     main="Random Forest  Attrition Network - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


library(gains)
# gains table
gains.cross <- gains(actual=as.numeric(forestPrediction$predict) , 
                     predicted=as.numeric(forestPrediction$actual),
                     groups=2)
print(gains.cross)
plot.gains(gains.cross)

data = list("Random Forest", (100-incorrectPredict),auc@y.values[[1]],
         ks,forestErrorMatrix$byClass[[1]],
         forestErrorMatrix$byClass[[2]])

modelEvaluation = rbind(modelEvaluation, data)


###################################################################################################
## Neural Net


library(neuralnet)

predictors = names(training_attrition_data)
formula <- paste(' ~ ', paste(predictors, collapse='+'))


## Normalize the model using model matrix
m = model.matrix(~Attrition+Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+
                   EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
                   JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+
                   NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+
                   RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+
                   TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
                   YearsSinceLastPromotion+YearsWithCurrManager, data =  training_attrition_data)

training_attrition_data_model = as.data.frame(m)


## Model Matrix creates non compatible R Column names, use make names to update column names
for(i in 1:length(training_attrition_data_model)) {
  
  colnames(training_attrition_data_model)[i] <- make.names(colnames(training_attrition_data_model))[i]
}


predictors = names(training_attrition_data_model)

constant_predictors = c("X.Intercept.", "AttritionYes")
idx = which(predictors %in% constant_predictors )
predictors = predictors[-idx]

training_attrition_data_model = training_attrition_data_model[-1]

formula <- as.formula(paste("AttritionYes ~ ", paste(predictors, collapse='+')))



## Normalizing  the data between 0&1
library(clusterSim)


training_attrition_data_model =  data.Normalization(training_attrition_data_model, type = "n4")


net.attrition = neuralnet(formula,data = training_attrition_data_model,act.fct = "logistic",
                          hidden = 9, threshold = 0.02,linear.output = FALSE)

plot.nn(net.attrition)

## Nrmalize the cleaning data

m = model.matrix(~Attrition+Age+BusinessTravel+DailyRate+Department+DistanceFromHome+
                   Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+
                   JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+
                   MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+
                   PerformanceRating+RelationshipSatisfaction+StockOptionLevel+
                   TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                   YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, 
                 data =  training_attrition_data)

test_attrition_data_model = as.data.frame(m)


for(i in 1:length(test_attrition_data_model)) {
  
  colnames(test_attrition_data_model)[i] <- make.names(colnames(test_attrition_data_model))[i]
}


test_attrition_data_model =  data.Normalization(test_attrition_data_model, type = "n4")

test_model = test_attrition_data_model
test_model = test_model[-1]
test_model = test_model[-1]



neuralNetresult = compute(net.attrition, test_model)


##Prediction Accuracy
neuralnetPrediction = data.table(predict=neuralNetresult$net.result, 
                                 actual=test_attrition_data_model$AttritionYes)

colnames(neuralnetPrediction)[1] = "predict"

neuralnetPrediction$accuracy = abs(neuralnetPrediction$predict - 
                                     neuralnetPrediction$actual)


incorrectPredict = 100 * sum(neuralnetPrediction$accuracy) /length(neuralnetPrediction$accuracy)

cat("NeuralNet Prediction Accuracy % = ", 100-incorrectPredict)



#######Neural Net  Model Evaluation###############

##Error / COnfusion MAtrix
library(caret)

paste("Confusion MAtrix for Neural Net ")
neuralnetErrorMatrix=confusionMatrix(round(neuralnetPrediction$predict),neuralnetPrediction$actual,
                                     positive = '1')


## ROC Curve
library("ROCR")
##ROC PLot  for Random Forest
library(ROCR)

nnetROC  = ROCR::prediction(as.vector(as.numeric(neuralnetPrediction$predict)),
                        as.vector(as.numeric(neuralnetPrediction$actual)))

nnetPerf =  ROCR::performance(nnetROC, measure = "tpr", x.measure = "fpr") 

auc <- performance(nnetROC,"auc")
aucVal = paste("Area Under Curve for Neural Net = ",auc@y.values)

paste(aucVal)

plot(nnetPerf,
     main="Atrition Classification using Neural Net  - ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")

abline(0,1,col="grey")

## KS Score  : max delta between cumulative bad and good rates being plotted by
#ROCR
ks = max(attr(nnetPerf,'y.values')[[1]]-attr(nnetPerf,'x.values')[[1]])[]

paste("KS Score for Random Forest  Based Attrition Model  :",ks)


##Lift  Chart
liftObj <- performance(nnetROC, measure="lift", x.measure="rpp")
plot(liftObj,
     main="Neural Net  Attrition Network - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


library(gains)
# gains table
gains.cross <- gains(actual=as.numeric(neuralnetPrediction$predict) , 
                     predicted=as.numeric(neuralnetPrediction$actual),
                     groups=2)
print(gains.cross)
plot.gains(gains.cross)


data = list("Neural Net", (100-incorrectPredict),auc@y.values[[1]],
            ks,neuralnetErrorMatrix$byClass[[1]],
            neuralnetErrorMatrix$byClass[[2]])

modelEvaluation = rbind(modelEvaluation, data)


