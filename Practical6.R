#Q6. Use Simple Kmeans, DBScan, Hierachical clustering algorithms 
#for clustering. Compare the performance of clusters by changing 
#the parameters involved in the algorithms.

#working on HTRU_2 dataset
df <- read.csv("datasets/HTRU2/HTRU_2.csv")
print(summary(df))
#View(df)
str(df)

library(ggplot2)

#normalize data for variables measured on different scale
dfScaled <- scale(df[,-9])
dfScaled
summary(dfScaled)

set.seed(234)
kmeans_model <- kmeans(x = df[,-9], centers = 2, nstart = 20)
print(kmeans_model)

print(kmeans_model$cluster)

cm <- table(df[,9], kmeans_model$cluster)
print(cm)

#library(caret)
#print(confusionMatrix(cm))

# ----------------- HIERACHICAL CLUSTERING --------------------

#distance matrix - matrix of distance b/w every point to any other
d= dist(dfScaled)
fitH = hclust(d,"ward.D2")
print(fitH)
plot(fitH)

rect.hclust(fitH,k=3,border = "blue")
cluster= cutree(fitH,3)      #we can cut off the tree at the desired number of clusters using cutree
cluster
table(df[,9],cluster)

# ---------------------- DBSCAN ---------------------------

#install.packages("dbscan",dependencies = TRUE)
library(dbscan)

kNNdistplot(dfScaled,k=3)       #to decide value of eps
abline(h=0.7,col="red",Ity = 2)
fitD<-dbscan(dfScaled,eps=0.7,minPts = 5)
print(fitD)
plot(df,col=fitD$cluster)
table(df[,9],fitD$cluster)

