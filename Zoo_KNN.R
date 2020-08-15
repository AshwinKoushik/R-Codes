install.packages("readr")
install.packages("dplyr")

library(readr)
library(dplyr)

# Importing the data
mydata <- read.csv("E:/Data Science/KNN/Assignment/Zoo.csv")
View(mydata)

# Structure of the data
str(mydata)

sum(is.na(mydata)) #FInding NA values.

#mydata <- mydata[-1]
#View(mydata)

# Structure of the data
#str(mydata)

summary(mydata)

# Create function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

# Apply Normalization to the data.

mydata_n <- as.data.frame(lapply(mydata[2:18],norm))
View(mydata_n)

#Creating training data and test data

mydata_train <- mydata_n[1:70,]
mydata_test <- mydata_n[71:101,]

#Labels for training and test data

mydata_train_labels <- mydata[1:70,1]
View(mydata_train_labels)
mydata_test_labels <- mydata[71:101,1]
View(mydata_test_labels)

#Build a KNN model on taining dataset
library("class")

# Building the KNN model on training dataset and also need labels which we are including c1.

test_acc <- NULL
train_acc <- NULL
for (i in seq(3,50,2))
{
  train_zoo_pred <- knn(train = mydata_train,test = mydata_train,cl = mydata_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==mydata_train_labels))
  test_zoo_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==mydata_test_labels))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,50,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,50,2),test_acc,type="l",main="Test_accuracy",col="red")

#Below is the final method 
zoo_pred <- knn(train = mydata_train, test = mydata_test, cl = mydata_train_labels, k=7)
View(zoo_pred)

