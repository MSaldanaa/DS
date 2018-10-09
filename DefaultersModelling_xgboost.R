
setwd("C:/Users/vinay.jagadeesh/Desktop/Analytics/Assignments/FRA-Assignments/Assignment_1/DefaultersCaseStudy/Analysis/Datasets/")

## log transformed raw data
train = read.csv("trainData_logs.csv")
test = read.csv("testData_logs.csv")

## log/log1p transformed raw data
train = read.csv("PrepTrainData_logs.csv")
test = read.csv("PrepTestData_logs.csv")

## percent and log transformed
train = read.csv("trainDataPrepd_logs.csv")
test = read.csv("testDataPrepd_logs.csv")

######################### SMOTED DATAS #########################

## log transformed raw data
train = read.csv("trainData_logs_SMOTE.csv")
test = read.csv("testData_logs.csv")

## log/log1p transformed raw data
train = read.csv("PrepTrainData_logs_SMOTE.csv")
test = read.csv("PrepTestData_logs.csv")

## percent and log transformed
train = read.csv("trainDataPrepd_logs_SMOTE.csv")
test = read.csv("testDataPrepd_logs.csv")

######################## MODELLING ############################

## xgboost needs all variables in numeric
train$SeriousDlqin2yrs = as.numeric(train$SeriousDlqin2yrs)
train$LogRUUL = as.numeric(train$LogRUUL)
train$LogDR = as.numeric(train$LogDR)
train$LogNOCLL = as.numeric(train$LogNOCLL)
train$LogND = as.numeric(train$LogND)

str(train)

names(train)
names(test)

testSeriousDlqin2yrs = test$SeriousDlqin2yrs
test$SeriousDlqin2yrs=NULL ## To this Problem  Only ##(Drop Depedenet variable , as it is there is teseset)

train$X=NULL ## ID of the data to be dropped 
test$X=NULL ## ID of the data to be dropped
train$Casenum=NULL ## ID of the data to be dropped 
test$Casenum=NULL ## ID of the data to be dropped


test_vars=names(test) ## Test set variables 
train_names=names(train) ## train set variables ##
common_vars=intersect(test_vars,train_names) ## common variables set
common_vars


library(xgboost)

all_train=xgb.DMatrix(data=data.matrix(train[common_vars]),label=(train[,c("SeriousDlqin2yrs")]))

#######  Set the Model Parameters ####
param = list(objective           = "binary:logistic", 
              booster             = "gbtree",
              eta                 = 0.01, # 0.06, #0.01,0.005
              max_depth           = 20, #changed from default of 4,6,8,10,15,20
              subsample           = 0.5, #(.5,0.7,1)
              colsample_bytree    = 0.5, #(.5,0.7,1)
              min_child_weight    = 44.8833  ## 3/ Event rate - Rule of Thumb 
)


###### Use the entire training set using best parameters 
clf_best = xgboost(params        = param, 
                    data                = all_train, 
                    nrounds             = 420, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    #early.stop.round    = 200,
                    #watchlist           = watchlist,
                    maximize            = FALSE,
                    eval_metric="auc"
                    #, nfold=3
)


#### vALIDATING TRAINING DATA
# 1. Predicting
trainDataMatrix=xgb.DMatrix(data=data.matrix(train[,common_vars]))
predTrain = predict(clf_best, trainDataMatrix)
pred = matrix(predTrain, nrow=1)
pred = data.frame(t(pred))

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
predictDec = pred > 0.20
trainPredRes = cbind(train$SeriousDlqin2yrs, predictDec)
table(train$SeriousDlqin2yrs, predictDec)



#### vALIDATING TEST DATA
test$LogRUUL = as.numeric(test$LogRUUL)
test$LogDR = as.numeric(test$LogDR)
test$LogNOCLL = as.numeric(test$LogNOCLL)
test$LogND = as.numeric(test$LogND)

testSeriousDlqin2yrs = as.factor(testSeriousDlqin2yrs)

# 1. Predicting
testDataMatrix=xgb.DMatrix(data=data.matrix(test[,common_vars]))
predtest = predict(clf_best, testDataMatrix)
pred = matrix(predtest, nrow=1)
pred = data.frame(t(pred))

# 2. ROC
library(ROCR)
roc = prediction(pred, testSeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = pred > 0.20
testPredRes = cbind(testSeriousDlqin2yrs, predictDec)
table(testSeriousDlqin2yrs, predictDec)

