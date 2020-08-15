# Load the data set
data <- read.csv("E:/Data Science/Decision Tree & random forest/Assignments_DT/Fraud_check.csv")
View(data)

#loading the required libraries
library(C50)
library(tree)
library(gmodels)
library(caTools)


#exploratory data analysis
summary(data)

str(data)

#boxplot
boxplot(data$Taxable.Income,horizontal = T)  # there are no outliers
boxplot(data$City.Population,horizontal = T) #No outliers
boxplot(data$Work.Experience,horizontal = T) #No outliers
boxplot(data,horizontal = T) # there are no outliers


#histogram
hist(data$Taxable.Income)
hist(data$City.Population)
hist(data$Work.Experience)

#model creation
risky_gud <- ifelse(data$Taxable.Income<=30000,'fraud','not fraud')

data <- data.frame(data,risky_gud)
View(data)

#splitting data into train and test
sample <- sample.split(data,SplitRatio = 0.75)

train <- subset(data,sample=='TRUE')
test <- subset(data,sample=='FALSE')

# model creation
model <- C5.0(risky_gud~.-Taxable.Income, data = train)
summary(model)

pred <- predict(model,test,type = 'class')
tab<- table(pred,test$risky_gud)
(sum(diag(tab))/sum(tab)) 
confusionMatrix(pred,test$risky_gud)
plot(model)
text(model,pretty = 0)
