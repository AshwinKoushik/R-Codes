install.packages("readr")
install.packages("mlbench")
install.packages("e1071")
library(mlbench)
library(readr)
library(e1071)
# Import  traindata
data <- read.csv("E:/Data Science/Naive Bayes/Assignments/SalaryData_Train.csv")
View(data)
attach(data)

# Structure of data
str(data)

summary(data)

# Find NA Values
sum(is.na(data)) # No NA values present.

# Boxplot
boxplot(age,horizontal=T) # There are outliers present
boxplot(educationno,horizontal=T) # There are outliers present
boxplot(capitalgain,horizontal=T) # There are outliers present
boxplot(hoursperweek,horizontal=T) # There are outliers present

# Histogram
hist(age)
hist(educationno)
hist(capitalgain)
hist(hoursperweek)

# Importing test data
test <- read.csv("E:/Data Science/Naive Bayes/Assignments/SalaryData_Test.csv")
View(test)

# Model creation
model <- naiveBayes(data$Salary~.,data=data[,-14])
pred <- predict(model,test[,-14])
mean(pred==test[14])
table(pred)
table(test[,14])
