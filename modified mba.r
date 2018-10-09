######### INITIALIZATION
# Install required packages
#install.packages("arulesViz")
#install.packages("arules")

# Load the packages
library(arules)
library(arulesViz)

setwd("D:/Marketing and retail analytics/MRA - Class Exercises Datasets Dec 2016")
groceries<- read.transactions("MBA-Apriori-Groceries.csv",sep = ",")
summary(groceries)

# Load the data in R


######### DATA EXPLORATION
inspect(groceries[1:3])    
inspect(groceries[9000:9005])
itemFrequency(groceries [,10])
itemFrequency(groceries [,100])
#Obtain absolute counts of occurence of an item
ifreq_df<- as.data.frame(itemFrequency(groceries, type= "relative"))
#Obtain absolute counts of occurence of an item
ifreq_count_df<-as.data.frame(itemFrequency(groceries) * 9835)
#Plot Item Frequency of top 10 items
itemFrequencyPlot(groceries,topN=10) 


##########IDENTIFY PARAMETERS
#Play around with Support & Confidence to identify the approriate parameters
#Create two rule sets to compare and identify the right support & confidence parameters 
rules01 <- apriori(groceries, parameter = list(supp = 0.01, conf = 0.01,minlen=2))
inspect (sort(rules01, by="support", decreasing=TRUE))
#Since the rules are numerous restrict the number of rules displayed
inspect (sort(rules01, by="support", decreasing=TRUE)[1:4])
inspect (sort(rules01, by="lift", decreasing=TRUE)[1:4])
plot (rules01)
plot(rules01, method = "grouped", control = list(k = 20)) 
summary(rules01) 

rules05 <- apriori(groceries, parameter = list(supp = 0.05, conf = 0.05,minlen=2))
#Since count is low remove restrictions and display all rules
inspect (sort(rules05, by="support", decreasing=TRUE))
inspect (sort(rules05, by="lift", decreasing=TRUE))
plot (rules05)
plot(rules05, method = "grouped", control = list(k = 20)) 
summary(rules05) 

