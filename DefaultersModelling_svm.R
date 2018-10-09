
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

library(e1071)

svm = svm(SeriousDlqin2yrs ~ LogRUUL+LogDR+LogNOCLL+LogND , train, 
          type="C-classification", probability=TRUE,
          cost = 0.30)

summary(svm)

#### vALIDATING TRAINING DATA
# 1. Predicting
pred = predict(svm, train, probability = TRUE)
predProb = attr(pred, "probabilities")

points(train$SeriousDlqin2yrs, pred, col = "red", pch=4)

# 2. ROC
library(ROCR)
roc = prediction(predProb[,2], train$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = predProb[,2] > 0.20
trainPredRes = cbind(train$SeriousDlqin2yrs, predictDec)
table(train$SeriousDlqin2yrs, predictDec)

table(train$SeriousDlqin2yrs, pred) ## this is with threshold of 0.5


#### vALIDATING TEST DATA
# 1. Predicting
pred = predict(svm, test, probability = TRUE)
predProb = attr(pred, "probabilities")

# 2. ROC
library(ROCR)
roc = prediction(predProb[,2], test$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = predProb[,2] > 0.20
testPredRes = cbind(test$SeriousDlqin2yrs, predictDec)
table(test$SeriousDlqin2yrs, predictDec)








