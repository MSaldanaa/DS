mydata=read.csv(file.choose(), header=TRUE)
mydata
Current=mydata$Current
New=mydata$New
tstat1=t.test(Current, New)
tstat1
tstat2=t.test(Current, New, var.equal=TRUE)
tstat2

 
 
