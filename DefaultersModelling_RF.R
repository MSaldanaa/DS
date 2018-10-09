
setwd("C:/Users/vinay.jagadeesh/Desktop/Analytics/Assignments/FRA-Assignments/Assignment_1/DefaultersCaseStudy/Analysis/Datasets/")

train = read.csv("trainData_logs_SMOTE.csv")
test = read.csv("testData_logs.csv")

train$X=NULL ## ID of the data to be dropped 
test$X=NULL ## ID of the data to be dropped
train$Casenum=NULL ## ID of the data to be dropped 
test$Casenum=NULL ## ID of the data to be dropped

########## MODELLING
library(randomForest)

train$SeriousDlqin2yrs = as.factor(train$SeriousDlqin2yrs)
train$LogRUUL = as.numeric(train$LogRUUL)
train$LogDR = as.numeric(train$LogDR)
train$LogNOCLL = as.numeric(train$LogNOCLL)
train$LogND = as.numeric(train$LogND)

str(train)

RF = randomForest(y = train$SeriousDlqin2yrs, x = train[,-c(1)], data = train, 
                  ntree=500, mtry = 2, nodesize = 100,
                  importance=TRUE) # sqrt(4) = 2

print(RF)
plot(RF)

RF$importance
importance(RF)
varImpPlot(x = RF)


## 4.3 Tuning Random forest parameters - to get optimal mtry value
# Run tuneRF multiple times to get the most optimal mtry
tRF = tuneRF( x = train[,-c(1)], 
              y = train$SeriousDlqin2yrs,
              mtryStart = 2, 
              ntreeTry=50, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 100, 
              importance=TRUE
)

# Started with mtry = 2 i.e., Sqrt(4)
# 2 2 2 2 3 3 2
# Optimal mtry value = 2


# Important Variable plot
tRF$importance
importance(tRF)
varImpPlot(x = tRF)

#### vALIDATING TRAINING DATA
# 1. Predicting
pred = predict(tRF, train, type = "prob")

# 2. ROC
library(ROCR)
roc = prediction(pred[,2], train$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = pred[,2] > 0.10
trainPredRes = cbind(train$SeriousDlqin2yrs, predictDec)
table(train$SeriousDlqin2yrs, predictDec)



#### vALIDATING TEST DATA
# 1. Predicting
pred = predict(tRF, test, type="prob")

# 2. ROC
library(ROCR)
roc = prediction(pred[,2], test$SeriousDlqin2yrs)
rocrPerf = performance(roc, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1, 0.1), text.adj=c(-0.2,1.7))

# 3. AUC value
auc = performance(roc,"auc"); 
auc = as.numeric(auc@y.values)
auc

# 4. Confusion matrix
predictDec = pred[,2] > 0.10
testPredRes = cbind(test$SeriousDlqin2yrs, predictDec)
table(test$SeriousDlqin2yrs, predictDec)




