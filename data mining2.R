#1 import data
setwd ("D:/datamining")
getwd()
bd <- read.csv("HR_Employee_Attrition_Data.csv", header=TRUE)

## Let us quickly understand the structure of our data

## split the data in Training (70%) and Testing (30%) set
bd$random <- runif (nrow(bd), 0, 1);
bd.Training <- bd [which(bd$random <= 0.7),]
bd.Testing <- bd [which(bd$random > 0.7),]

##CART
library(rpart)
library(rpart.plot)
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
m1 <- rpart(formula = Attrition ~ ., data = bd.Training [,-1], method = "class", control = r.ctrl)
m1
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)
printcp(m1)
plotcp(m1)
ptree<- prune(m1, cp= 0.0015,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")
## scoring step
bd.Testing$predict.score <- predict(m1, bd.Testing)
## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
    ifelse(x<deciles[2], 2,
    ifelse(x<deciles[3], 3,
    ifelse(x<deciles[4], 4,
    ifelse(x<deciles[5], 5,
    ifelse(x<deciles[6], 6,
    ifelse(x<deciles[7], 7,
    ifelse(x<deciles[8], 8,
    ifelse(x<deciles[9], 9, 10
     ))))))))))
}
## deciling
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])

## Ranking code
library(data.table)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



library(ROCR)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(CTDF.dev$predict.score[,2], type="Gini")

with(CTDF.dev, table(Target, predict.class))
auc
KS
gini

## Syntax to get the node path
tree.path <- path.rpart(ptree, node = c(2, 12))



## Scoring Holdout sample
CTDF.holdout$predict.class <- predict(m1, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(m1, CTDF.holdout)

with(CTDF.holdout, table(Target, predict.class))

## Building the model using Random Forest

## importing the data
RFDF.dev <- read.table("datafile/DEV_SAMPLE.csv", sep = ",", header = T)
RFDF.holdout <- read.table("datafile/HOLDOUT_SAMPLE.csv", sep = ",", header = T)
c(nrow(RFDF.dev), nrow(RFDF.holdout))

##install.packages("randomForest")
library(randomForest)

## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(Target) ~ ., data = RFDF.dev[,-1], 
                   ntree=500, mtry = 3, nodesize = 10,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(1,2)], 
              y=as.factor(RFDF.dev$Target),
              mtryStart = 3, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
)




## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(Target, predict.class))
auc
KS
gini


## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(Target, predict.class))