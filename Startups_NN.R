install.packages("readr")
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(readr)

# Import data
data <- read.csv(file.choose())
View(data)
attach(data)

# Summary of data
summary(data)
str(data)

# Finding NA values
sum(is.na(data))

# Normalization
normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

data_norm <- as.data.frame(lapply(data[,c(1:3,5)],FUN = normalize))
View(data_norm)


# dividing data into train & test data
train <- data_norm[1:35,]
View(train)
test <- data_norm[36:50,]
View(test)
# Model building
model <- neuralnet(Profit~.,data = train)
str(model)
plot(model)

# Evaluating model performance
pred <- compute(model,test[,1:3])
pred_profit <- model$net.result
pred_profit
cor(pred_profit,test$Profit)
plot(pred_profit,test$Profit)
