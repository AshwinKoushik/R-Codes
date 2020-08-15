install.packages("readr")
library(readr)

# Importing data
mydata <- read.csv("E:/Data Science/Logistic Regression/Assignment/bank-full.csv")
View(mydata)
attach(mydata)

# Finding NA Values.
sum(is.na(mydata))

# Structure of the data.
str(mydata)

summary(mydata)

# Model Builing
fit1<-glm(y~.,data = mydata,family = "binomial")
summary(fit1)
prob <- predict(fit1,type="response")
prob

# Another model
logit<-glm(factor(y)~age+factor(job)+factor(marital)+factor(default)+factor(housing)+balance+factor(loan)+factor(contact)+day+month+duration+campaign+pdays+previous+factor(poutcome),family=binomial,data = mydata)
summary(logit)

# Final model
logit1<-glm(factor(y)~age+factor(housing)+balance+factor(loan)+factor(contact)+day+month+duration+campaign+previous+factor(poutcome),family=binomial,data = mydata)
summary(logit1)

exp(coef(logit1))
table(mydata$card)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),mydata)
prob
confusion<-table(prob>0.5,mydata$card)
probo <- prob>0.5
table(probo)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,mydata$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
