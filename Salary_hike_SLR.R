install.packages("readr")
library(readr)

# Import data
mydata <- read.csv("E:/Data Science/Linear Regression/Assignment/SLR/Salary_Data.csv")
View(mydata)
names(mydata)

# Changing variable name
names(mydata) <- c("experience","salary")
names(mydata)
attach(mydata)

# Summary of data
summary(mydata)
str(mydata)

# Finding NA values
sum(is.na(mydata))

# Box plot
boxplot(experience, horizontal=T) # No outliers
boxplot(salary,horizontal = T) # No outliers

# Histogram
hist(experience)
hist(salary)

# Scatter plot
plot(experience,salary) #strong & Positive linearity

cor(salary,experience) # correlation coefficient

model <- lm(salary ~ experience) #model building
summary(model) 

#The co-efficient values are significant.
#Multiple R-Squared values is 0.957 which is greater than 0.8.

fitted.values(model) # It gives the fitted values according to the model.

residuals(model) # It gives the error.

confint(model)
