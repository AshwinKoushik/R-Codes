# Importing data
data <- read.csv("E:/Data Science/Decision Tree & random forest/Assignments_RF/Company_Data.csv")
View(data)

# Loading required packages
library(tree)
library(gmodels)
library(caret)
library(caTools)
library(randomForest)

#Exploratory Data Analysis
summary(data)

boxplot(data,horizontal=T) # There are outliers present in the data

#histogram
hist(data$CompPrice)
hist(data$Income)
hist(data$Advertising)
hist(data$Population)
hist(data$Price)
hist(data$Age)
hist(data$Education)

#model creation
high <- ifelse(data$Sales<10,'no','yes')

data <- data.frame(data,high)
View(data)

#spliting data into train and test data
sample <- sample.split(data,SplitRatio = 0.75)

train <- subset(data,sample=="TRUE")
test <- subset(data,sample=='FALSE')


#model
model <- randomForest(high~.-Sales,data = train)
model
plot(model,lwd=3)

pred <- predict(model,test,type='class')
tab<- table(pred,test$high)
tab
confusionMatrix(pred,test$high) # Accuracy : 87.0% 


