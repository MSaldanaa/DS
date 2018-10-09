#Problem 2

#The inspection records of a hose assembly operation revealed a high level of 
#rejection. An analysis of the records showed that the "leaks" were a major 
#contributing factor to the problem. It was decided to investigate the 
#hose clamping operation. The hose clamping force (torque) was measured 
#on twenty five assemblies. (Figures in foot-pounds). 
#The data are given below: Draw the frequency histogram and comment.

# 8   13	15	10	16	
# 11	14	11	14	20	
# 15	16	12	15	13	
# 12	13	16	17	17	
# 14	14	14	18	15

#Data Entry for Torque 
Torque=c(8,13,15,10,16,11,14,11,14,20,15,16,12,15,13,12,13,16,17,17,14,14,14,18,15)
range(Torque)
Frequency=table(as.vector(Torque))
Frequency
Mode=names(Frequency)[Frequency==max(Frequency)] 
Mode
breaks=seq(8, 23, by=3)
Torque.Class=cut(Torque, breaks, right=FALSE)
plot(Torque.Class)
plot(Torque.Class, main="Histogram of Torque", xlab="Class Interval", ylab= "Frequency", space=c(0, 0, 0, 0, 0))
summary(Torque)
sd(Torque)
boxplot(Torque)
boxplot(Torque, main="Pattern in Torque Behavior Revealed by Box Plot", xlab="Torque Pressure", col="Pink",horizontal=TRUE)
hist(Torque, xlab="Torque Class", col="Pink")

 
