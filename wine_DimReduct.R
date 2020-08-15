# Loading Universities data
mydata<-read.csv("E:/Data Science/Dimension reduction/Assignment/wine.csv") ## use read.csv for csv files

View(mydata)

attach(mydata)

summary(mydata)

cor(mydata)

# Model Building#
pcaObj<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]
View(clus_data)
# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data

dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
dist1
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

rect.hclust(fit1, k=5, border="red")
membership <- as.matrix(groups) # cluster numbering 

View(membership)

final <- cbind(membership,mydata) # binding column wise with orginal data

View(final)


write.csv(final,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()

# Clustering the data using kmeans clustering.

model <- kmeans(norm_clus,5) # 
str(model)
table(model$cluster)

final1 <- data.frame(mydata,model$cluster)
View(final1)
