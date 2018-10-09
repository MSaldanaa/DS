setwd("F:/R for Data Mining/R-CSV")
data1=read.csv("PCA-nm.csv", header=TRUE)
data1
PreventCav=data1$V1
ShinyTeeth=data1$V2
StrengthGum=data1$V3
Fresh=data1$V4
Decay=data1$V5
Attractive=data1$V6
PCA=princomp(~PreventCav+ShinyTeeth+StrengthGum+Fresh+Decay+Attractive, cor=TRUE)
PCA
summary(PCA)
Factors=loadings(PCA)
Factors
Rotation=varimax(loadings(PCA))
Rotation
PCA$scores
plot(PCA, type="lines")
plot(PCA$scores[,1], PCA$scores[,2], col=c("Red", "Green"))
biplot(PCA, cex=0.7)
