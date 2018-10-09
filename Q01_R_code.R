# Read data in R
list_A <- read.csv(file.choose())
list_B <- read.csv(file.choose())

# Create new variables - indicator variable in dataset A
list_A$A_Indicate<-1

# Convert email IDs to upper case
data.frame(list_A, toupper(list_A$Email_IDs))
List_A_1<-data.frame(list_A, toupper(list_A$Email_IDs))
str(List_A_1)

#rename the newly created field
colnames(List_A_1)[3]<- "Up_Email_ID"

# Repeat the same for another table
List_B_1<-data.frame(list_B, toupper(list_B$Email_IDs))
colnames(List_B_1)[2]<- "Up_Email_ID"
str(List_B_1)

# Merge two tables
List_A_B <- merge(List_A_1, List_B_1, by="Up_Email_ID")

# it didn't help, because it produced equi join
# Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
# Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
# Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

List_A_B <- merge(List_A_1, List_B_1, by="Up_Email_ID", all=TRUE)

# Now remove those where A is populated
New_in_B<-List_A_B[ is.na(List_A_B$A_Indicate),]
