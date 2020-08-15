install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

# Importing data.
mydata <- read.transactions("E:/Data Science/Association Rules/Assignment/my_movies.csv", format = "basket")
inspect(mydata)
class(mydata)

# Finding NA values.
sum(is.na(mydata))

# Finding Structure.
str(mydata)

summary(mydata)

# MOdel building.
model <- apriori(mydata, parameter = list(support = 0.002, confidence = 0.5, minlen = 3))
inspect(model)
plot(model)

# New support, confidence & minimum length values
model1 <- apriori(mydata, parameter = list(support = 0.003, confidence = 0.7, minlen = 4))
inspect(model1)
plot(model1)

# New values.
model2 <- apriori(mydata, parameter = list(support = 0.004, confidence = 0.4, minlen = 5))
inspect(model2)
plot(model)

# New values.
model3 <- apriori(mydata, parameter = list(support = 0.005, confidence = 0.3, minlen = 2))
inspect(model3)
plot(model)

# As the support, confidence & minlen value decreases, rules increses.