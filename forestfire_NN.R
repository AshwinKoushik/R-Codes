install.packages("readr")
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(readr)

# Import data
data <- read.csv(file.choose())
View(data)
data <- data[,3:31]
View(data)
attach(data)

# Summary of data
summary(data)
str(data)

# Finding NA values
sum(is.na(data))

size <- ifelse(data$size_category=='small',0,1)
View(data)
data <- data.frame(data,size)
View(data)

# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

data_norm <- as.data.frame(lapply(data[,-29], normalize))
View(data_norm)
# Dividing data into train & test
train <- data_norm[1:400,]
View(train)
test <- data_norm[401:517,]

# Model building
model <- neuralnet(size~.,data=train)
str(model)
plot(model)

# Evaluating model performance
model_eval <- compute(model,test)
pre <- model_eval$net.result
pre
cor(pre,test$size)
plot(pre,test$size)

# New model
model1 <- neuralnet(size~.,data=train,hidden = c(8,5))
plot(model1)

model1_eval <- compute(model1,test)
size <- model1_eval$net.result
size
cor(size,test$size)
plot(size,test$size)
