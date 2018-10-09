setwd ("D:/datamining")
getwd()



nn.dev <- read.table("Training_SAMPLE.csv", sep = ",", header = T)
nn.holdout <- read.table("Testing_SAMPLE.csv", sep = ",", header = T)
nn.dev <- nn.dev[-36]
nn.holdout<- nn.holdout[-36]
nn.dev=nn.dev[,-27]
nn.dev=nn.dev[,-21]
nn.dev=nn.dev[,-9]
nn.holdout=nn.holdout[,-27]
nn.holdout=nn.holdout[,-20]
nn.holdout=nn.holdout[,-9]
str(nn.dev)
## converting factors to numeric
nn.dev$Attrition=as.numeric(nn.dev$Attrition)
nn.dev$BusinessTravel=as.numeric(nn.dev$BusinessTravel)
nn.dev$Department=as.numeric(nn.dev$Department)
nn.dev$EducationField=as.numeric(nn.dev$EducationField)
nn.dev$Gender=as.numeric(nn.dev$Gender)
nn.dev$JobRole=as.numeric(nn.dev$JobRole)
nn.dev$MaritalStatus=as.numeric(nn.dev$MaritalStatus)

nn.dev$OverTime=as.numeric(nn.dev$OverTime)

#neural net model
library(neuralnet)

nn1 <- neuralnet(formula = Attrition ~ Age +  BusinessTravel+	DailyRate+	Department+	DistanceFromHome+	Education+	EducationField+		EnvironmentSatisfaction+	Gender+	HourlyRate+	JobInvolvement+	JobLevel+	JobRole+	JobSatisfaction+	MaritalStatus+	MonthlyIncome+	MonthlyRate+	NumCompaniesWorked+	OverTime+	PercentSalaryHike+	PerformanceRating+	RelationshipSatisfaction+	StockOptionLevel+	TotalWorkingYears+	TrainingTimesLastYear+	WorkLifeBalance+	YearsAtCompany+	YearsInCurrentRole+	YearsSinceLastPromotion+	YearsWithCurrManager,
                 data = nn.dev, 
                 hidden = 10,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)
nn1
plot(nn1)
quantile(nn1$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
## The distribution of the estimated results

misClassTable = data.frame(Target = nn.dev$Attrition, Prediction = nn1$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.143,1,0)
with(misClassTable, table(Target, Classification))

## build the neural net model by scaling the variables

x=nn.dev[,-2]
nn.devscaled <- scale(x)
nn.devscaled <- cbind(nn.dev[2], nn.devscaled)
##View(nn.devscaled)
str(nn.devscaled)
nn2 <- neuralnet(formula = Attrition ~ Age +  BusinessTravel+  DailyRate+	Department+	DistanceFromHome+	Education+	EducationField+		EnvironmentSatisfaction+	Gender+	HourlyRate+	JobInvolvement+	JobLevel+	JobRole+	JobSatisfaction+	MaritalStatus+	MonthlyIncome+	MonthlyRate+	NumCompaniesWorked+	OverTime+	PercentSalaryHike+	PerformanceRating+	RelationshipSatisfaction+		StockOptionLevel+	TotalWorkingYears+	TrainingTimesLastYear+	WorkLifeBalance+	YearsAtCompany+	YearsInCurrentRole+	YearsSinceLastPromotion+	YearsWithCurrManager,
                 data = nn.devscaled, 
                 hidden = 11,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)

nn2
##plot(nn2)

quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable = data.frame(Target = nn.devscaled$Attrition, Predict.score = nn2$net.result[[1]] )
misClassTable$Predict.class = ifelse(misClassTable$Predict.score>0.21,1,0)
with(misClassTable, table(Target, Predict.class))

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
misClassTable$deciles <- decile(misClassTable$Predict.score)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(misClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

print(rank)
## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
##scoreing of dev

x=nn.dev[,-2]
x.scaled <- scale(x)
str(x.scaled)
x.scaled2 <- cbind(nn.holdout[2], x.scaled)
str(x.scaled2)

x.scaled2=x.scaled2[,-1]
compute.output = compute(nn2, x.scaled2)
nn.dev$Predict.score = compute.output$net.result
##View(nn.holdout)
quantile(nn.dev$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)



library(ROCR)
pred <- prediction(misClassTable$Predict.score, misClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
title("Performance Graph- Neural Networks")
nndev.KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
nndev.auc <- as.numeric(auc@y.values)

library(ineq)
nndev.gini = ineq(misClassTable$Predict.score, type="Gini")


nndev.auc
nndev.KS
nndev.gini
##scoring of holdor testing
nn.holdout$Attrition=as.numeric(nn.holdout$Attrition)
nn.holdout$BusinessTravel=as.numeric(nn.holdout$BusinessTravel)
nn.holdout$Department=as.numeric(nn.holdout$Department)
nn.holdout$EducationField=as.numeric(nn.holdout$EducationField)
nn.holdout$Gender=as.numeric(nn.holdout$Gender)
nn.holdout$JobRole=as.numeric(nn.holdout$JobRole)
nn.holdout$MaritalStatus=as.numeric(nn.holdout$MaritalStatus)
nn.holdout$OverTime=as.numeric(nn.holdout$OverTime)
x=nn.holdout[,-2]
x.scaled <- scale(x)
str(x.scaled)
x.scaled2 <- cbind(nn.holdout[2], x.scaled)
str(x.scaled2)

x.scaled2=x.scaled2[,-1]
compute.output = compute(nn2, x.scaled2)
nn.holdout$Predict.score = compute.output$net.result
##View(nn.holdout)
quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)



library(ROCR)
pred <- prediction(misClassTable$Predict.score, misClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
title("Performance Graph- Neural Networks")
nnhold.KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
nnhold.auc <- as.numeric(auc@y.values)

library(ineq)
nnhold.gini = ineq(misClassTable$Predict.score, type="Gini")


nnhold.auc
nnhold.KS
nnhold.gini

