# First convert the dataset into csv file
# Read.csv tells R to read csv file and 
# File.choose helps you to select the file you need with a dialogue prompt
# Leave the filename blank, so that you can choose the file
Hlth <- read.csv(file.choose())

# See file content
str(Hlth)
summary(Hlth)

# bringing data into correct format
Hlth_1 <- Hlth
Hlth_1$Air_Weight <- as.numeric(Hlth_1$Air_Weight)

# look at row 66

# correct data and then convert
Hlth_1<-Hlth
Hlth_1$Air_Weight <- ifelse(trimws(Hlth_1$Air_Weight)=="*","",
                            as.character.factor(Hlth_1$Air_Weight))
Hlth_1$Air_Weight <- as.numeric(Hlth_1$Air_Weight)

# Converting other variable as well
Hlth_1$Water_Weight <- ifelse(trimws(Hlth_1$Water_Weight)=="*","",
                            as.character.factor(Hlth_1$Water_Weight))
Hlth_1$Water_Weight <- as.numeric(Hlth_1$Water_Weight)

# check if conversion works
str(Hlth_1)
summary(Hlth_1)


# drop the variable not required
Hlth_2 <- Hlth_1[,c(-5)]

# remove missing data
Hlth_2 <- na.omit(Hlth_2)

# Checking linear Relationship graphically
#install.packages("lattice")
library("lattice")
with(data=Hlth_2, xyplot(Air_Weight~Water_Weight))


# Running Regression
fit <- lm(Air_Weight~Water_Weight, data=Hlth_2)
summary(fit)

# Can addition of body fat and gender can help in better
# the linear fit?

# Converting other variable as well
Hlth_1$Body_Fat <- ifelse(trimws(Hlth_1$Body_Fat)=="*","",
                              as.character.factor(Hlth_1$Body_Fat))
Hlth_1$Body_Fat <- as.numeric(Hlth_1$Body_Fat)

summary(Hlth_1)
Hlth_2 <- na.omit(Hlth_1)

# Running Regression
fit <- lm(Air_Weight~Water_Weight+Body_Fat+GENDER, data=Hlth_2)
summary(fit)

# Showing relationship for non technical people
summary(Hlth_2)
# calculate 20th, 40th .. percentile to create 5 groups
quantile(Hlth_2$Air_Weight, c(0.2, 0.4, 0.6, 0.8))

Hlth_2$Air_Weight_gp <- 
  ifelse(Hlth_2$Air_Weight <= 57.16,"01" ,
    ifelse(Hlth_2$Air_Weight <= 63.06,"02",
      ifelse(Hlth_2$Air_Weight <= 70.94,"03",
         ifelse(Hlth_2$Air_Weight <= 77.64,"04" ,"05"))))

# Calculate average of water weight for each group
# install.packages("plyr")
library("plyr")
ddply(Hlth_2, "Air_Weight_gp", 
      summarise, mean_water_wt=mean(Water_Weight)
)