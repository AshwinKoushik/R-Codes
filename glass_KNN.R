install.packages("readr")
install.packages("dplyr")

library(readr)
library(dplyr)

# Importing the data
mydata <- read.csv("E:/Data Science/KNN/Assignment/glass.csv")
View(mydata)

# Structure of the data
str(mydata)

sum(is.na(mydata)) #FInding NA values.

summary(mydata)

# Create function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

# Apply Normalization to the data.

mydata_n <- as.data.frame(lapply(mydata[1:10],norm))
View(mydata_n)

#Creating training data and test data

mydata_train <- mydata_n[1:150,]
mydata_test <- mydata_n[151:214,]

#Labels for training and test data

mydata_train_labels <- mydata[1:150,10]
View(mydata_train_labels)
mydata_test_labels <- mydata[151:214,10]
View(mydata_test_labels)

#Build a KNN model on taining dataset
library("class")

# Building the KNN model on training dataset and also need labels which we are including c1.

test_acc <- NULL
train_acc <- NULL
for (i in seq(3,50,2))
{
  train_glass_pred <- knn(train = mydata_train,test = mydata_train,cl = mydata_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==mydata_train_labels))
  test_glass_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==mydata_test_labels))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,50,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,50,2),test_acc,type="l",main="Test_accuracy",col="red")

#Below is the final method 
glass_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=5)
View(glass_pred)

