mydata=read.csv(file.choose(), header=TRUE)
mydata
Depression=mydata$Depression
State=mydata$State
boxplot(Depression~State, main="Comparative Position of Depression Index", ylab="Depression Score", col=c("Red", "Green", "Blue"))
Model=aov(Depression~State)
summary(Model)