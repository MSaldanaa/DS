
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

logit = glm(formula = SeriousDlqin2yrs ~ LogRUUL+LogDR+LogNOCLL+LogND, data = train, family = binomial())
summary(logit)

logit = glm(formula = SeriousDlqin2yrs ~ LogRUUL+LogNOCLL+LogND, data = train, family = binomial())
summary(logit)


#### vALIDATING TRAINING DATA
# 1. Predicting
pred = predict(logit, train, type = "response")
#pred = predict(cartcv, train)
#predictClass = predict(ptree, train, type="class")

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
test$SeriousDlqin2yrs = as.factor(test$SeriousDlqin2yrs)
test$LogRUUL = as.numeric(test$LogRUUL)
test$LogDR = as.numeric(test$LogDR)
test$LogNOCLL = as.numeric(test$LogNOCLL)
test$LogND = as.numeric(test$LogND)

# 1. Predicting
pred = predict(logit, test, type="response")

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
predictDec = pred > 0.20
testPredRes = cbind(test$SeriousDlqin2yrs, predictDec)
table(test$SeriousDlqin2yrs, predictDec)




