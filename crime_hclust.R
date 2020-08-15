library(readxl)
library(readr)

input <- read.csv("E:/Data Science/Clustering/Assignment/crime_data.csv")
View(input)

mydata <- input[,2:5] # excluding the 1st column.
View(mydata)

normalized_data <- scale(mydata) #standardizing the data.
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") #distance matrix
d

# model building

model <- hclust(d,method = "complete")
model$labels

plot(model)
plot(model, hang = -1)

groups <- cutree(model, k=4) 
class(groups)

rect.hclust(model, k=4, border = "red")

membership <- (as.matrix(groups))
table(membership)

final <- data.frame(mydata,membership)
View(final)


install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))
View(final)

write.csv(final,file = "crime.csv")
getwd()
