#Problem1

#A student has completed 20 courses in the School of Arts and Sciences. 
#Her grades in the 20 courses are shown below.

# A  B	A	 B	C
# C	 C	B	 B	B
# B	 A	B  B	B
# C	 B	C	 B	A

#a.	Develop a frequency distribution and a bar chart for her grades.
#b.	Develop a relative frequency distribution for her grades and construct a pie chart.
#_____________________________________________________________________________________________________________________

#Entering the grades for all the courses"
Grades=c("A", "B", "A",	"B", "C", "C",	"C",	"B","B","B", "B","A",	"B", "B", "B", "C",	 "B",	"C", "B","A")
Frequency= table(as.vector(Grades))
Mode=names(Frequency)[Frequency==max(Frequency)]
Mode
data1=data.frame(Frequency)
names(data1)=c("Grade", "Frequency")
data1
barplot(Frequency, main="Frequency Distribution of Grades", ylab="Frequency", xlab="Letter Grade")
Relative=(Frequency/sum(Frequency))*100
Relative
data2=data.frame(Relative)
names(data2)=c("Grade", "Frequency")
data2
Cumulative=cumsum(Frequency)
Cumulative
data3=data.frame(Cumulative)
data3
barplot(Cumulative, col="Pink", main="Frequency Distribution of Grades", xlab="Grade Category", ylab="Frequency")
lines(Cumulative, col="Red")
Grade=c("A", "B", "C")
pie(Relative,labels=paste(Relative, "%", "", names=c("A", "B", "C")))

      
     
 
  
 
 
 
 
 
 
 
