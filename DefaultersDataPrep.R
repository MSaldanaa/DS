
setwd("C:/Users/vinay.jagadeesh/Desktop/Analytics/Assignments/FRA-Assignments/Assignment_1/DefaultersCaseStudy/Analysis/Datasets/")

trainData = read.csv("training.csv")
testData = read.csv("test.csv")

#################### DATA PREPARATION TYPE III #############################################################################
# 1. Replace NA with 0 for NumberOfDependents.
# 2. Take log transform for all variables.

## Train Data Preparation
# Before
hist(trainData$RevolvingUtilizationOfUnsecuredLines)
hist(trainData$DebtRatio)
hist(trainData$NumberOfOpenCreditLinesAndLoans)
hist(trainData$NumberOfDependents)

library(PerformanceAnalytics)
chart.Correlation(trainData[,c(3,4,5,6)], histogram=FALSE)

plot(x=trainData$RevolvingUtilizationOfUnsecuredLines, y=trainData$DebtRatio, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$RevolvingUtilizationOfUnsecuredLines, y=trainData$NumberOfOpenCreditLinesAndLoans, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$RevolvingUtilizationOfUnsecuredLines, y=trainData$NumberOfDependents, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=trainData$DebtRatio, y=trainData$NumberOfOpenCreditLinesAndLoans, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$DebtRatio, y=trainData$NumberOfDependents, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=trainData$NumberOfOpenCreditLinesAndLoans, y=trainData$NumberOfDependents, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

# Preparation
trainData$Casenum=NULL ## ID of the data to be dropped 

trainData[is.na(trainData)]=0 ## replace missing values with mode 0

# Outliers replace with median for RUUL
# trainData$RevolvingUtilizationOfUnsecuredLines[trainData$RevolvingUtilizationOfUnsecuredLines > 3]
# median(trainData$RevolvingUtilizationOfUnsecuredLines)
# 
# trainData$RevolvingUtilizationOfUnsecuredLines[trainData$RevolvingUtilizationOfUnsecuredLines > 3] = median(trainData$RevolvingUtilizationOfUnsecuredLines)

trainData$LogRUUL = log(trainData$RevolvingUtilizationOfUnsecuredLines + 1)
trainData$LogDR = log(trainData$DebtRatio + 1)
trainData$LogNOCLL = log(trainData$NumberOfOpenCreditLinesAndLoans + 1)
trainData$LogND = log(trainData$NumberOfDependents + 1)

# After
hist(trainData$LogRUUL)
hist(trainData$LogDR)
hist(trainData$LogNOCLL)
hist(trainData$LogND)

library(PerformanceAnalytics)
chart.Correlation(trainData[,c(6,7,8,9)], histogram=FALSE)

plot(x=trainData$LogRUUL, y=trainData$LogDR, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$LogRUUL, y=trainData$LogNOCLL, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$LogRUUL, y=trainData$LogND, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=trainData$LogDR, y=trainData$LogNOCLL, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=trainData$LogDR, y=trainData$LogND, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=trainData$LogNOCLL, y=trainData$LogND, col= ifelse(trainData$SeriousDlqin2yrs == 0,"green","red"))

# Write to File
write.csv(trainData[,c(1,6,7,8,9)], "trainData_logs.csv")


## Test Data Preparation
# Before
hist(testData$RevolvingUtilizationOfUnsecuredLines)
hist(testData$DebtRatio)
hist(testData$NumberOfOpenCreditLinesAndLoans)
hist(testData$NumberOfDependents)

library(PerformanceAnalytics)
chart.Correlation(testData[,c(3,4,5,6)], histogram=FALSE)

plot(x=testData$RevolvingUtilizationOfUnsecuredLines, y=testData$DebtRatio, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$RevolvingUtilizationOfUnsecuredLines, y=testData$NumberOfOpenCreditLinesAndLoans, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$RevolvingUtilizationOfUnsecuredLines, y=testData$NumberOfDependents, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=testData$DebtRatio, y=testData$NumberOfOpenCreditLinesAndLoans, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$DebtRatio, y=testData$NumberOfDependents, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=testData$NumberOfOpenCreditLinesAndLoans, y=testData$NumberOfDependents, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

# Preparation
testData$Casenum=NULL ## ID of the data to be dropped

testData[is.na(testData)]=0 ## replace missing values with 999

testData$LogRUUL = log(testData$RevolvingUtilizationOfUnsecuredLines + 1)
testData$LogDR = log(testData$DebtRatio + 1)
testData$LogNOCLL = log(testData$NumberOfOpenCreditLinesAndLoans + 1)
testData$LogND = log(testData$NumberOfDependents + 1)

# After
hist(testData$LogRUUL)
hist(testData$LogDR)
hist(testData$LogNOCLL)
hist(testData$LogND)

library(PerformanceAnalytics)
chart.Correlation(testData[,c(6,7,8,9)], histogram=FALSE)

plot(x=testData$LogRUUL, y=testData$LogDR, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$LogRUUL, y=testData$LogNOCLL, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$LogRUUL, y=testData$LogND, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=testData$LogDR, y=testData$LogNOCLL, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))
plot(x=testData$LogDR, y=testData$LogND, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

plot(x=testData$LogNOCLL, y=testData$LogND, col= ifelse(testData$SeriousDlqin2yrs == 0,"green","red"))

# Write to File
write.csv(testData[,c(1,6,7,8,9)], "testData_logs.csv")


#################### SMOTING THE DATA #############################################################################

## log transformed raw data
train = read.csv("trainData_logs.csv")
train$X = NULL

table(train$SeriousDlqin2yrs)

train$SeriousDlqin2yrs = as.factor(train$SeriousDlqin2yrs)
train$LogRUUL = as.numeric(train$LogRUUL)
train$LogDR = as.numeric(train$LogDR)
train$LogNOCLL = as.numeric(train$LogNOCLL)
train$LogND = as.numeric(train$LogND)


library(DMwR)
data = SMOTE(form = SeriousDlqin2yrs ~ ., data = train,
             perc.over = 200, perc.under=600)

table(data$SeriousDlqin2yrs)

write.csv(data, "trainData_logs_SMOTE.csv")

# After SMOTE
hist(data$LogRUUL)
hist(data$LogDR)
hist(data$LogNOCLL)
hist(data$LogND)

library(PerformanceAnalytics)
chart.Correlation(data[,c(2,3,4,5)], histogram=FALSE)

plot(x=data$LogRUUL, y=data$LogDR, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))
plot(x=data$LogRUUL, y=data$LogNOCLL, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))
plot(x=data$LogRUUL, y=data$LogND, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))

plot(x=data$LogDR, y=data$LogNOCLL, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))
plot(x=data$LogDR, y=data$LogND, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))

plot(x=data$LogNOCLL, y=data$LogND, col= ifelse(data$SeriousDlqin2yrs == 0,"green","red"))




