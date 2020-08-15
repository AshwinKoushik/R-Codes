install.packages("readr")
install.packages("kernlab")
install.packages("caret")

library(readr)
library(kernlab)
library(caret)

# Importing Train data

data_train <- read.csv("E:/Data Science/SVM/Assignment/SalaryData_Train.csv")
View(data_train)
table(data_train$Salary)

# Structure of the data
str(data_train)

# Finding NA values
sum(is.na(data_train))

summary(data_train)

# Importing test data

data_test <- read.csv("E:/Data Science/SVM/Assignment/SalaryData_Test.csv")
View(data_test)

# Model Building
# Kernel=vanilladot
model <-ksvm(Salary ~.,data=data_train,kernel = "vanilladot")
pred_model <- predict(model,newdata = data_test)
mean(pred_model==data_test$Salary) # 84.62%

#kernel=rbfdot
model_rbfdot <- ksvm(Salary ~.,data=data_train,kernel = "rbfdot")
pred_model <- predict(model_rbfdot,newdata = data_test)
mean(pred_model==data_test$Salary) # 85.43%

#kernel = polydot
model_polydot <- ksvm(Salary ~.,data=data_train,kernel = "polydot")
pred_model <- predict(model_polydot,newdata = data_test)
mean(pred_model==data_test$Salary) # 84.62%

#kernel = tanhdot
model_tanhdot <- ksvm(Salary ~.,data=data_train,kernel = "tanhdot")
pred_model <- predict(model_tanhdot,newdata = data_test)
mean(pred_model==data_test$Salary) # 66.38%

#kernel = laplacedot
model_laplacedot <- ksvm(Salary ~.,data=data_train,kernel = "laplacedot")
pred_model <- predict(model_laplacedot,newdata = data_test)
mean(pred_model==data_test$Salary) # 85.25%

#kernel=besseldot
model_besseldot <- ksvm(Salary ~.,data=data_train,kernel = "besseldot")
pred_model <- predict(model_besseldot,newdata = data_test)
mean(pred_model==data_test$Salary) # 77.03%

#kernel=anovadot
model_anovadot <- ksvm(Salary ~.,data=data_train,kernel = "anovadot")
pred_model <- predict(model_anovadot,newdata = data_test)
mean(pred_model==data_test$Salary) # 78.26%

# The best model is rbfdot model, since it has 85.43% accuracy, Highest among all.