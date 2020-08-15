#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)

#Importing data
mydata <- read.csv("E:/Data Science/Recommender systems/Assignment/book.csv")
View(mydata)
class(mydata)

#metadata about the variable
str(mydata)
table(mydata$Book.Rating)

#rating distribution
hist(mydata$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
mydata_matrix <- as(mydata, 'realRatingMatrix')

#Popularity based 
model1 <- Recommender(mydata_matrix, method="POPULAR")
summary(model1)

#Predictions for two users 
recommended_items1 <- predict(model1, mydata_matrix, n=3)
as(recommended_items1, "list")

# Collaborative Filtering
#User Based Collaborative Filtering
model2 <- Recommender(mydata_matrix, method="UBCF")
#Predictions for two users 
recommended_items2 <- predict(model2, mydata_matrix[310:311], n=1)
as(recommended_items2, "list")

