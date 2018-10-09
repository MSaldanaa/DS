setwd("C:/Users/manickra/OneDrive - Hewlett Packard Enterprise/GreatLakes/Predictive Modelling/offcampus assign")
mydata=read.csv("Cellphone.csv", header=TRUE)
mydata$Churn=factor(mydata$Churn, levels=c(0,1), labels=c("No Churn","Churned"))


attach(mydata)
# boxplot(AccountWeeks~Churn, ylab="AccountWeeks")
# boxplot(ContractRenewal~Churn, ylab="ContractRenewal")
# boxplot(DataPlan~Churn, ylab="DataPlan")
# boxplot(DataUsage~Churn, ylab="DataUsage")
# boxplot(CustServCalls~Churn, ylab="CustServCalls")
# boxplot(DayMins~Churn, ylab="DayMins")
# boxplot(DayCalls~Churn, ylab="DayCalls")
# boxplot(MonthlyCharge~Churn, ylab="MonthlyCharge")
# boxplot(OverageFee~Churn, ylab="OverageFee")
# boxplot(RoamMins~Churn, ylab="RoamMins")


str(mydata.dev)

library(MASS)
X=as.matrix(mydata.dev[, 2:11])
X=scale(X)
Y=as.vector(mydata.dev[, 1])
Model=lda(Y~X)
Model
LD1=predict(Model)$x[, 1]

Cor1=cor(mydata.dev[, 2:11], LD1)

StructureCorrelation=data.frame(Cor1)
names(StructureCorrelation)=c("Discriminant Function")
StructureCorrelation

Confusion.Matrix=table(Original=mydata.dev$Churn, Predicted=predict(Model)$class)
Confusion.Matrix 
Jackknife=lda(Y~X, CV=TRUE)
confusionJackknife=table(mydata.dev$Churn, Jackknife$class)
confusionJackknife


install.packages("DiscriMiner")
library(DiscriMiner)
#X1=mydata.dev[, 2:11]
X1=mydata.val[, 2:11]
X1=scale(X1)
#Y1=mydata.dev[, 1]
Y1=mydata.val[, 1]
mymodel=linDA(X1, Y1)
mymodel
mymodel$scores

#posterior probability calculation
data3=data.frame(mymodel$scores)
attach(data3)
ProbNoChurn=exp(No.Churn)/exp(No.Churn)+exp(Churned)
ProbChurn=exp(Churned)/exp(No.Churn)+exp(Churned)
PosteriorProbability=data.frame(ProbNoChurn,ProbChurn)
head(PosteriorProbability)


  X1=cbind(as.matrix(mydata.dev[, 2:11]))
Manova=manova(X1~Y1)
Manova
summary(Manova)
summary(Manova, test="Wilks")
#summary.aov(Manova) 
discPower(X1, Y1) 

