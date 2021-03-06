setwd("F:/R for SMDM/R-CSV")
data1=read.csv("Health1.csv", header=TRUE)
data1
attach(data1)
mydata=data.frame(Hospital, Work, Pay, Promotion)
attach(mydata)
mean(Work)
sd(Work)
CV1=sd(Work)/mean(Work)
mean(Pay)
sd(Pay)
mean(Promotion)
sd(Promotion)
summary(mydata)
summary.HospitalType=by(cbind(Work, Pay, Promotion), INDICES=Hospital, FUN=summary)
summary.HospitalType
breaks1=seq(60, 100, by=10)
Work.cut=cut(Work, breaks1, right=FALSE)
Work.freq=table(Work.cut)
data.frame(Work.freq)
breaks2=seq(20, 100, by=10)
Pay.cut=cut(Pay, breaks2, right=FALSE)
Pay.freq=table(Pay.cut)
data.frame(Pay.freq)
breaks3=seq(10, 100, by=10)
Promotion.cut=cut(Promotion, breaks3, right=FALSE)
Promotion.freq=table(Promotion.cut)
data.frame(Promotion.freq)
Crosstab1=table(Work.cut, Hospital)
Crosstab1
Crosstab2=table(Work.cut, Hospital)
Crosstab2
Crosstab3=table(Pay.cut, Hospital)
Crosstab3
library(ggplot2)
Histogram1=ggplot(mydata, aes(Work))
Histogram1+geom_histogram(binwidth=5, color="Black", fill="Pink")
Histogram2=ggplot(mydata, aes(Pay))
Histogram2+geom_histogram(binwidth=10, color="Black", fill="Blue")
Histogram3=ggplot(mydata, aes(Promotion))
Histogram3+geom_histogram(binwidth=10, color="Black", fill="Orange")         
Boxplot1=ggplot(mydata, aes(Hospital, Work))
Boxplot1+geom_boxplot(fill=c("Green", "Red", "Pink"))+labs(title="Comparative Picture of Work Score")
Boxplot2=ggplot(mydata, aes(Hospital, Pay))
Boxplot2+geom_boxplot(fill=c("Green", "Red", "Pink"))+labs(title="Comparative Picture of Pay Score")
Boxplot3=ggplot(mydata, aes(Hospital, Promotion))
Boxplot3+geom_boxplot(fill=c("Green", "Red", "Pink"))+labs(title="Comparative Picture of Promotion Score")
boxplot(Work, Pay, Promotion, main='Comparative Picture of Work Pay and Promotion', names=c('Work', 'Pay', 'Promotion'), col=c("Green", "Red", "Pink"))

