install.packages("plyr")
library(plyr)
library(readxl)
library(readr)


input <- read.csv("E:/Data Science/Clustering/Assignment/EastWestAirlines.csv")
View(input)

mydata <- input[,2:12] # excluding the 1st column.
View(mydata)

normalized_data <- scale(mydata) #standardizing the data.
View(normalized_data)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# model building

model <- kmeans(normalized_data,6) # 6 clusters
str(model)
table(model$cluster)

final <- data.frame(mydata,model$cluster)
View(final)

setcolorder(final, neworder = c("model.cluster"))
View(final)
aggregate(mydata, by=list(model$cluster), FUN=mean)
