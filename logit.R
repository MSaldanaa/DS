options(scipen=100)

setwd("C:/Users/manickra/OneDrive - Hewlett Packard Enterprise/GreatLakes/Predictive Modelling/offcampus assign")

data.dev=read.csv("dev.csv",header=TRUE)
data.val=read.csv("val.csv",header=TRUE)
attach(data.dev)
## Log Likelihood Ratio Test
logit=glm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins, data=data.dev,family=binomial)

library(lmtest)
lrtest(logit)

##Pseudo R-Square[MacFaden]
library(pscl)
pR2(logit)

##Individual Coefficients
summary(logit)


##ODDS-Exp(BETA)
confint(logit)
data.frame(exp(coef(logit)))
exp(confint(logit))


## Classification table(Training Dataset)
#predict(logit,type="response")
Pred=fitted(logit)
data.frame(data.dev$Churn,Pred)
gg1=floor(Pred+0.5)
head(data.frame(data.dev$Churn,Pred,gg1))
table(Actual=data.dev$Churn,Prediction=gg1)

library(Deducer)
rocplot(logit, title="ROC Curve")

#testing
test_pred=predict(logit,newdata=data.val)
gg2=floor(test_pred+0.5)
cbind(gg2,data.val$Churn)
