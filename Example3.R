Problem 3

An American Hospital Association survey found that most hospital emergency rooms are
operating at full capacity (Associated Press, April 9, 2002). The survey collected data on
the emergency room waiting times for hospitals where the emergency room is operating at
full capacity and for hospitals where the emergency room is in balance and rarely operates
at capacity. Sample data showing waiting times in minutes are as follows.

 
ERFullCapacity    ERBalance
87    59          60   39
80   110          54   32
47    83          18   56
73    79          29   26
50    50          45   37
93    66          34   38
72   115

a. Compute the mean and median emergency room waiting times for hospitals operating
at full capacity.
b. Compute the mean and median emergency room waiting times for hospitals operating
in balance.
c. What observations can you make about emergency room waiting times based on these
results? Would the American Hospital Association express concern with the statistical
results shown here?

c1=c(87, 80, 47, 73, 50, 93, 72)
c2=c(59, 110, 83, 79, 50, 66, 115)
c3=c(60, 54, 18, 29, 45, 34)
c4=c(39, 32, 56, 26, 37, 38)
WaitingERFull=c(c1, c2)
WaitingERBalance=c(c3, c4)
mean(WaitingERFull)
median(WaitingERFull)
mean(WaitingERBalance)
median(WaitingERBalance)
summary(WaitingERFull)
summary(WaitingERBalance)
boxplot(WaitingERFull, main="Waiting Time Pattern in Full Capacity Hospitals", ylab="Waiting Time(Minutes)")
boxplot(WaitingERBalance, main="Waiting Time Pattern in Low Capacity Hospitals", ylab="Waiting Time(Minutes)")
Hospital1=c("Full", "Full", "Full", "Full", "Full", "Full", "Full" )
Hospital2=c("Balance", "Balance", "Balance", "Balance", "Balance", "Balance")
Hospital=c(Hospital1, Hospital2)
ERWaitingTime=c(c1, c3, c2, c4)
mydata=data.frame(Hospital, ERWaitingTime)
mydata
summary(mydata) 
boxplot(ERWaitingTime~Hospital, main="Comparative Picture of Waiting Time In Full Capacity and Low Capacity Hospitals", names=c("Full Capacity", "Low Capacity"), col=c("Green", "PinK"), data=mydata)
 