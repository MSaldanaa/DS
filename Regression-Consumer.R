mydata=read.csv(file.choose(), header=TRUE)
Amount=mydata$Amount
Income=mydata$Income
House=mydata$House
scatter1=plot(Income, Amount,col="Red", abline(lm(Amount~Income), col="Blue"))
scatter1=plot(Income, Amount,col="Red", abline(lm(Amount~Income), col="Blue"), main="Scatter Plot of Amount and Income")
scatter2=plot(House, Amount,col="Red", abline(lm(Amount~House), col="Blue"), main="Scatter Plot of Amount and House Size")
Model1=lm(Amount~Income)
Model2=lm(Amount~house)
Model2=lm(Amount~House)
Model3=lm(Amount~Income+House)
summary(Model3)
confint(Model3, "Income", level=0.95)
confint(Model3, "House", level=0.95)
newdata=data.frame(Income=55, House=4)
predict(Model3, newdata)
predict(Model3, newdata, interval="confidence")
predict(Model3, newdata, interval="confidence", level=0.99)
anova(Model3)
data.frame(mydata, fitted.value=fitted(Model3), residual=resid(Model3))
  
