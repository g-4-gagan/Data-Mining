#Q6. Use Simple Kmeans, DBScan, Hierachical clustering algorithms 
#for clustering. Compare the performance of clusters by changing 
#the parameters involved in the algorithms.

#working on HTRU_2 dataset
df <- read.csv("datasets/HTRU2/HTRU_2.csv")
print(summary(df))
#View(df)
str(df)

set.seed(234)
kmeans_model <- kmeans(x = df[,-9], centers = 2, nstart = 20)
print(kmeans_model)

print(kmeans_model$cluster)

cm <- table(df[,9], kmeans_model$cluster)
print(cm)

#library(caret)
#print(confusionMatrix(cm))

