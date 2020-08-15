install.packages("readr")
library(readr)

# Import data
mydata <- read.csv("E:/Data Science/Linear Regression/Assignment/SLR/calories_consumed.csv")
View(mydata)
names(mydata)

names(mydata) <- c("weight","calories")
names(mydata)

attach(mydata)

# Summary of data
summary(mydata)
str(mydata)

# Finding NA Values
sum(is.na(mydata))

# Boxplot
boxplot(weight, horizontal=T) # No outliers
boxplot(calories,horizontal=T) # No Outliers

# Histogram
hist(weight)
hist(calories)

plot(calories,weight) #Scatter plot

cor(weight,calories) # correlation coefficient

model <- lm(weight ~ calories) #model building
summary(model) 
#The co-efficient values are significant.
#Multiple R-Squared values is 0.8968.

# Logarithamic model
model1 <- lm(weight ~ log(calories))
summary(model1) #Multiple R-Squared values is 0.8077.

# Exponential model
model2 <- lm(log(weight) ~ calories)
summary(model2) #Multiple R-Squared values is 0.8776.

# Polynomial model with 2 degree (quadratic model)
model3 <- lm(log(weight)~calories + I(calories*calories))
summary(model3) #Multiple R-Squared values is 0.8776.

#  Polynomial model with 3 degree
model4 <- lm(log(weight) ~ calories + I(calories*calories)+I(calories*calories*calories))
summary(model4) #Multiple R-Squared values is 0.8336.

# Applying Square root
model5 <- lm(sqrt(weight) ~ calories)
summary(model5) #Multiple R-Squared values is 0.9139

### Square root model is the best model since it has the highest Multiple R-squared value.

fitted.values(model5) # It gives the fitted values according to the model.

residuals(model5) # It gives the error.

confint(model5)
