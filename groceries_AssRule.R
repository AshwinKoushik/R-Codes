install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

# Importing data.
mydata <- read.transactions("E:/Data Science/Association Rules/Assignment/groceries.csv", format = "basket")
inspect(mydata[1:100])
class(mydata)

# Finding NA values.
sum(is.na(mydata))
 
# Finding structure
str(mydata)

# Model building.
model <- apriori(mydata, parameter = list(support = 0.001, confidence = 0.2, minlen = 2))
inspect(model[1:100])
plot(model)

# New support, confidence & minimum length values
model1 <- apriori(mydata, parameter = list(support = 0.002, confidence = 0.4, minlen = 3))
inspect(model1)
plot(model1)

# New Values.
model2 <- apriori(mydata, parameter = list(support = 0.003, confidence = 0.6, minlen = 3))
inspect(model2)
plot(model2)

# New values.
model3 <- apriori(mydata, parameter = list(support = 0.002, confidence = 0.3, minlen = 3))
inspect(model3)
plot(model3)

# As the support, confidence & minlen value decreases, rules increses.