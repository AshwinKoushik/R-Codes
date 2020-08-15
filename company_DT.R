# Load the data set
data <- read.csv(choose.files())

#loading the required libraries
library(C50)
library(tree)
library(gmodels)
library(caTools)


#exploratory data analysis
summary(data)


boxplot(data,horizontal = T) # there are outliers present in the data set


#histogram 
hist(data$Sales)
hist(data$CompPrice)
hist(data$Income)
hist(data$Advertising)
hist(data$Population)
hist(data$Price)
hist(data$Age)
hist(data$Education)


high <- ifelse(data$Sales<10,'no','yes')

data <- data.frame(data,high)

#splitting data into train and test
sample <- sample.split(data,SplitRatio = 0.75)

train <- subset(data,sample=='TRUE')
test <- subset(data,sample=='FALSE')

# model creation
model <- tree(high~.-Sales, data = train)
summary(model)

pred <- predict(model,test,type = 'class')
tab<- table(pred,test$high)
(sum(diag(tab))/sum(tab)) # 85% accuracy

plot(model)
text(model,pretty = 0)
