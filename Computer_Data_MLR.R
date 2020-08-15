install.packages("readr")
library(readr)

# Import Data
mydata <- read.csv("E:/Data Science/Linear Regression/Assignment/MLR/Computer_Data.csv") # choose the Cars.csv data set
View(mydata)
attach(mydata)

# Exploratory Data Analysis

str(mydata)
summary(mydata)


# Find the correlation b/n Output & Input-Scatter plot
pairs(mydata)
plot(mydata)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(mydata)

# Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(mydata))

# Creating Dummy Variable
dummy <- dummy.data.frame(mydata[2:11], sep = ".")
View(dummy)
mydata <- dummy
View(mydata)

# The Linear Model of interest
model <- lm(price~.,data=mydata[,-1])
summary(model)

library(psych)
pairs.panels(mydata)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model)

## plotting Influential measures 
influenceIndexPlot(model,id.n=3) # index plots for infuence measures
influencePlot(model,id.n=3) # A user friendly representation of the above

# Regression after deleting the 1701th observation, which is influential observation
model1<-lm(price~.,data=mydata[-1701,])
summary(model1)

# Regression after deleting the 1701th & 1441st Observations
model2<-lm(price~.,data=mydata[-c(1701,1441),])
summary(model2)


## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model,id.n=2,id.cex=0.7)

## Final model
finalmodel<-lm(price~.,data=mydata[-c(1701,1441,3784,4478),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

