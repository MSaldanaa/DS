
setwd("C:/Users/vinay.jagadeesh/Desktop/Analytics/Assignments/FRA-Assignments/Assignment_1/DefaultersCaseStudy/Analysis/Datasets/")

train = read.csv("trainData_logs_SMOTE.csv")
test = read.csv("testData_logs.csv")

train$X=NULL ## ID of the data to be dropped 
test$X=NULL ## ID of the data to be dropped
train$Casenum=NULL ## ID of the data to be dropped 
test$Casenum=NULL ## ID of the data to be dropped

########## MODELLING
train$SeriousDlqin2yrs = as.factor(train$SeriousDlqin2yrs)
train$LogRUUL = as.numeric(train$LogRUUL)
train$LogDR = as.numeric(train$LogDR)
train$LogNOCLL = as.numeric(train$LogNOCLL)
train$LogND = as.numeric(train$LogND)

str(train)

library(nnet)

# Modeling using log transformed variables
nnet = nnet(SeriousDlqin2yrs ~ LogRUUL+LogDR+LogNOCLL+LogND,
             data = train,
             size = 2,
             decay = 5e-4,
             maxit = 200)


# Modeling using log and scaled variables
trainScaled = scale(train[,c("LogRUUL","LogDR","LogNOCLL","LogND")])
trainScaled = cbind(train[c("SeriousDlqin2yrs")], trainScaled)

testScaled = scale(test[,c("LogRUUL","LogDR","LogNOCLL","LogND")])
testScaled = cbind(test[c("SeriousDlqin2yrs")], testScaled)

nnet = nnet(SeriousDlqin2yrs ~ LogRUUL+LogDR+LogNOCLL+LogND,
            data = trainScaled,
            size = 2,
            decay = 5e-4,
            maxit = 200)

#### vALIDATING TRAINING DATA
# 1. Predicting
PredictScore = nnet$fitted.values

# 2. ROC
library(ROCR)
roc = prediction(PredictScore, train$SeriousDlqin2yrs)

roc = prediction(PredictScore, trainScaled$SeriousDlqin2yrs)

rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = PredictScore > 0.20

trainPredRes = cbind(train$SeriousDlqin2yrs, predictDec)

trainPredRes = cbind(trainScaled$SeriousDlqin2yrs, predictDec)

table(train$SeriousDlqin2yrs, predictDec)

table(trainScaled$SeriousDlqin2yrs, predictDec)


#### vALIDATING TEST DATA
# 1. Predicting
PredictScore = predict(nnet, test)

PredictScore = predict(nnet, testScaled)

# 2. ROC
library(ROCR)
roc = prediction(PredictScore, test$SeriousDlqin2yrs)

roc = prediction(PredictScore, testScaled$SeriousDlqin2yrs)

rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = PredictScore > 0.20

testPredRes = cbind(test$SeriousDlqin2yrs, predictDec)

testPredRes = cbind(testScaled$SeriousDlqin2yrs, predictDec)

table(test$SeriousDlqin2yrs, predictDec)

table(testScaled$SeriousDlqin2yrs, predictDec)


