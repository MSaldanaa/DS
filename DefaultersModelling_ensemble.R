
setwd("C:/Users/vinay.jagadeesh/Desktop/Analytics/Assignments/FRA-Assignments/Assignment_1/DefaultersCaseStudy/Analysis/Datasets/")

train = read.csv("trainData_logs_SMOTE.csv")
test = read.csv("testData_logs.csv")

train$X=NULL ## ID of the data to be dropped 
test$X=NULL ## ID of the data to be dropped
train$Casenum=NULL ## ID of the data to be dropped 
test$Casenum=NULL ## ID of the data to be dropped

########## ENSEMBLE MODELLING
train$SeriousDlqin2yrs = ifelse(train$SeriousDlqin2yrs == 1, "Yes","No")
test$SeriousDlqin2yrs = ifelse(test$SeriousDlqin2yrs == 1, "Yes","No")
train$SeriousDlqin2yrs = as.factor(train$SeriousDlqin2yrs)

train$LogRUUL = as.numeric(train$LogRUUL)
train$LogDR = as.numeric(train$LogDR)
train$LogNOCLL = as.numeric(train$LogNOCLL)
train$LogND = as.numeric(train$LogND)

str(train)

library(caret)
library(mlbench)
library(pROC)
library(Metrics)

my_control <- trainControl(
  method="repeatedcv",
  number=10,
  savePredictions="final",
  index=createResample(train$SeriousDlqin2yrs, 25),
  summaryFunction=twoClassSummary,
  classProbs=TRUE
)


library(rpart)
library(randomForest)
library(nnet)
library(e1071)
library(gbm)
library(xgboost)
library(caretEnsemble)

my_tunelist = list(
#   rpart=caretModelSpec(method="rpart", tuneGrid=expand.grid(.cp=0)),
#   rf=caretModelSpec(method="rf", tuneGrid=expand.grid(.mtry=2), ntree = 50),
  svmLinear=caretModelSpec(method="svmLinear",tuneGrid=expand.grid(.C=0.3))
  # nnet=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE),
)

model_list <- caretList(
  SeriousDlqin2yrs ~ ., 
  data = train,
  trControl = my_control,
  metric = "ROC",
  # methodList = c("rpart", "glm", "rf", "nnet", "svmLinear", "gbm", "xgbTree"),
  # methodList = c("glm", "nnet", "gbm", "xgbTree"),
  methodList = c("glm", "nnet", "gbm"),
  tuneList = my_tunelist
)

#### Valuating the model list Result summary
results = resamples(model_list)
summary(results)
dotplot(results)

# Correlation between models
modelCor(resamples(model_list))
splom(resamples(model_list))

############ ENSEMBLING ALL THE MODELS
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    method="repeatedcv",
    number=10,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))

summary(greedy_ensemble)

#### Predict probability of each model and ensemble model
## For Train Data
library("caTools")
model_preds <- lapply(model_list, predict, newdata=train, type="prob")
#model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=train, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, train$SeriousDlqin2yrs)

## For Test Data
model_preds <- lapply(model_list, predict, newdata=test, type="prob")
#model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=test, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, test$SeriousDlqin2yrs)


#### vALIDATING TRAINING DATA WITH ENSEMBLE MODEL
# 1. Predicting
pred = predict(greedy_ensemble, train, type="prob")

# 2. ROC
library(ROCR)
roc = prediction(pred, train$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = pred > 0.1
trainPredRes = cbind(train$SeriousDlqin2yrs, predictDec)
table(train$SeriousDlqin2yrs, predictDec)



#### vALIDATING TEST DATA WITH ENSEMBLE MODEL
test$SeriousDlqin2yrs = as.factor(test$SeriousDlqin2yrs)
test$LogRUUL = as.numeric(test$LogRUUL)
test$LogDR = as.numeric(test$LogDR)
test$LogNOCLL = as.numeric(test$LogNOCLL)
test$LogND = as.numeric(test$LogND)

# 1. Predicting
pred = predict(greedy_ensemble, test, type="prob")

# 2. ROC
library(ROCR)
roc = prediction(pred, test$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = pred > 0.10
testPredRes = cbind(test$SeriousDlqin2yrs, predictDec)
table(test$SeriousDlqin2yrs, predictDec)




