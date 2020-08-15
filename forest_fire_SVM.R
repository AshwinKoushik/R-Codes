install.packages("readr")
install.packages("kernlab")
install.packages("caret")

library(readr)
library(kernlab)
library(caret)

# Importing data

mydata <- read.csv("E:/Data Science/SVM/Assignment/forestfires.csv")
View(mydata)
table(mydata$size_category)

# structure of the data
str(mydata)

# finding NA values
sum(is.na(mydata))

summary(mydata)

# Dividing data into train & test data

train_data <- mydata[1:350,]  # created train data by selecting the important variable.
View(train_data)

test_data <- mydata[351:517,] # created test data by selecting the important variable.
View(test_data)

# Model Building
#kernel=vanilladot
model_vanilladot <- ksvm(size_category~.,data=train_data,kernel="vanilladot")
pred_model <- predict(model_vanilladot,newdata=test_data)
mean(pred_model==test_data$size_category) # 94.61%

#kernel=rbfdot
model_rbfdot <- ksvm(size_category~.,data=train_data,kernel="rbfdot")
pred_model <- predict(model_rbfdot,newdata=test_data)
mean(pred_model==test_data$size_category) # 76.64%

#kernel=polydot
model_polydot <- ksvm(size_category~.,data=train_data,kernel="polydot")
pred_model <- predict(model_polydot,newdata=test_data)
mean(pred_model==test_data$size_category) # 94.61%

#kernel=tanhdot
model_tanhdot <- ksvm(size_category~.,data=train_data,kernel="tanhdot")
pred_model <- predict(model_tanhdot,newdata=test_data)
mean(pred_model==test_data$size_category) # 67.66%

#kernel=laplacedot
model_laplacedot <- ksvm(size_category~.,data=train_data,kernel="laplacedot")
pred_model <- predict(model_laplacedot,newdata=test_data)
mean(pred_model==test_data$size_category) # 67.66%

#kernel=besseldot
model_besseldot <- ksvm(size_category~.,data=train_data,kernel="anovadot")
pred_model <- predict(model_besseldot,newdata=test_data)
mean(pred_model==test_data$size_category) # 67.66%

#kernel=anovadot
model_anovadot <- ksvm(size_category~.,data=train_data,kernel="anovadot")
pred_model <- predict(model_anovadot,newdata=test_data)
mean(pred_model==test_data$size_category) # 83.83%

#kernel=splinedot
model_splinedot <- ksvm(size_category~.,data=train_data,kernel="splinedot")
pred_model <- predict(model_splinedot,newdata=test_data)
mean(pred_model==test_data$size_category) # 94.01

# The best model is vanilladot & polydot model which has 94.61% accuracy which is highest among all.
