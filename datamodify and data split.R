
setwd ("D:/datamining")
getwd()

## Let us import the data that we intend to use for modeling

CTDF <- read.table("HR_Employee_Attrition_Data_reoreder.csv",sep = ",", header = T)
                   
CTDF$random <- runif(nrow(CTDF), 0, 1);
CTDF <- CTDF[order(CTDF$random),]
CTDF.Training <- CTDF[which(CTDF$random <= 0.7),]
CTDF.Testing<- CTDF[which(CTDF$random > 0.7),]
c(nrow(CTDF.Training), nrow(CTDF.Testing))
c(ncol(CTDF.Training), ncol(CTDF.Testing))
cl<-as.numeric(nrow(CTDF.Training))
cl 
ct <-cl + 1
ct
CTDF.Training <- CTDF[1:cl,]
CTDF.Testing<- CTDF[ct:2940,]
write.table(CTDF.Training, file = "Training_SAMPLE.csv", sep=',', row.names = F)
write.table(CTDF.Testing, file = "Testing_SAMPLE.csv", sep=',', row.names = F)
c(nrow(CTDF.Training), nrow(CTDF.Testing))
str(CTDF.Training)
c(ncol(CTDF.Training), ncol(CTDF.Testing))



