# First convert the dataset into csv file
# Read.csv tells R to read csv file and 
# File.choose helps you to select the file you need with a dialogue prompt
# Leave the filename blank, so that you can choose the file
Ins <- read.csv(file.choose())

# See file content
str(Ins)
summary(Ins)

# Extract month from date_1 field
Ins_1<- Ins
Ins_1$Month_1 <- substr(Ins_1$date_1, 1, gregexpr(pattern="/",
                                                  Ins_1$date_1) )


length(Ins_1$Month_1)
nchar(Ins_1$Month_1)
Ins_1$Month_2 <- substr(Ins_1$Month_1, 1,  nchar(Ins_1$Month_1)-1)



# Calculate no of births in each month
# install.packages("plyr")
library("plyr")
ddply(Ins_1, "Month_2", 
      summarise,  
      sum_wt=sum(no_of_births_1)
)

str(Ins_1)
# Add additional 0 infront of number before doing summary
Ins_1$Month_3<- ifelse(Ins_1$Month_2<10, 
                       paste("0",Ins_1$Month_2,sep=""),
                       paste("",Ins_1$Month_2,sep=""))

Ins_1$Month_3<- ifelse((Ins_1$Month_2!="10" & Ins_1$Month_2!="11"
                        & Ins_1$Month_2!="12"),
                       paste("0",Ins_1$Month_2,sep=""),
                       paste("",Ins_1$Month_2,sep=""))

ddply(Ins_1, "Month_3", 
      summarise,  
      No_of_people=sum(no_of_births_1)
)


# Plot a bar chart for the same
chk<-ddply(Ins_1, "Month_3", 
           summarise,  
           No_of_people=sum(no_of_births_1)
)

barplot(height = chk$No_of_people, names.arg = chk$Month_3)
                      