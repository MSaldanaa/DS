#Analysis using IRIS Data
mydata=iris
mydata
attach(mydata)
summary(mydata)
data1=by(Sepal.Length, INDICES=Species, FUN=summary) 
data1
library(ggplot2)
Boxplot1=ggplot(mydata, aes(Species, Sepal.Length, fill=Species)) 
Boxplot1+geom_boxplot()
Boxplot2=ggplot(mydata, aes(Species, Sepal.Width, fill=Species))
Boxplot2+geom_boxplot()
Boxplot3=ggplot(mydata, aes(Species, Petal.Length, fill=Species))
Boxplot3+geom_boxplot()
Boxplot4=ggplot(mydata, aes(Species, Petal.Width, fill=Species))
Boxplot4+geom_boxplot()
Histogram=ggplot(mydata, aes(Sepal.Length, fill="Orange"))
Histogram+geom_histogram(binwidth=0.8)
Histogram+geom_density()
LinePlot=ggplot(mydata, aes(Sepal.Length))
LinePlot+geom_density()
Scatter1=ggplot(mydata, aes(Sepal.Length, Petal.Length, color=Species))
Scatter1+geom_point()+geom_smooth(method="lm")
Scatter2=ggplot(mydata, aes(Sepal.Width, Petal.Width, color=Species))
Scatter2+geom_point()+geom_smooth(method="lm")


 