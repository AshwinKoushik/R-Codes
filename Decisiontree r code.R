data("iris")
View(iris)
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(iris$Species,p=.75,list=F)
training <- iris[inTraininglocal,]
View(training)
testing <- iris[-inTraininglocal,]
table(testing$Species)
#model building
model <- C5.0(training$Species~.,data = training,trails = 40)
# Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing[,-5])
table(pred)
a <- table(testing$Species,pred)
a
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list=F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]

  fittree<-C5.0(training1$Species~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)

  acc<-c(acc,sum(diag(a))/sum(a))

}

acc
summary(acc)
