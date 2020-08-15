claimants <- read.csv("E:/Excelr Data/R Codes/Logistic Regression/claimants.csv") # Choose the claimants Data set
View(claimants)
attach(claimants)

fit1<-glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,data = claimants,family = "binomial")
summary(fit1)
# Linear regression technique can not be employed
prob1 <- predict(fit1,type="response")
# Logistic Regression 
str(claimants)
logit<-glm(factor(ATTORNEY)~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit)

logit1<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit1)

exp(coef(logit1))
table(claimants$ATTORNEY)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),claimants)
prob
confusion<-table(prob>0.5,claimants$ATTORNEY)
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
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
