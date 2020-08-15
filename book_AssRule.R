install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

# Importing data.
mydata <- read.csv("E:/Data Science/Association Rules/Assignment/book.csv")
View(mydata)
class(mydata)

# Structure of the data
str(mydata)

summary(mydata)

# Finding NA values
sum(is.na(mydata))

# Converting data into transactions
mydata_trans <- as(as.matrix(mydata),"transactions")
inspect(mydata_trans[1:200])

# Model building
model <- apriori(mydata_trans,parameter = list(support = 0.002, confidence = 0.7, minlen = 2))
inspect(model[1:100])
plot(model)

# New support, confidence & minimum length values
model1 <- apriori(mydata_trans,parameter = list(support = 0.004, confidence = 0.5, minlen = 3))
inspect(model1[1:100])
plot(model1)

# new values
model2 <- apriori(mydata_trans,parameter = list(support = 0.001, confidence = 0.3, minlen = 4))
inspect(model2[1:100])
plot(model2)

# New rule
model3 <- apriori(mydata_trans,parameter = list(support = 0.005, confidence = 0.8, minlen = 5))
inspect(model3[1:100])
plot(model3)

# As the support, confidence & minlen value decreases, rules increses.