install.packages("readr")
library(readr)

# Import data
mydata <- read.csv("E:/Data Science/Linear Regression/Assignment/SLR/emp_data.csv")
View(mydata)

# Summary of data

summary(mydata)
str(mydata)

# Finding NA Values
sum(is.na(mydata))

names(mydata)

names(mydata) <- c("hike","churn.out")
names(mydata)
attach(mydata)

# Box Plot
boxplot(hike, horizontal=T) # No outliers
boxplot(churn.out, horizontal=T) # No outliers

# Histogram
hist(hike)
hist(churn.out)

# Scatter plot
plot(churn.out,hike) #Scatter plot

cor(hike,churn.out) # correlation coefficient

model <- lm(churn.out~hike) #model building
summary(model)

#The co-efficient values are significant.
#Multiple R-Squared values is 0.8312 is greater than 0.8

fitted.values(model) # It gives the fitted values according to the model.

residuals(model) # It gives the error.

confint(model)
