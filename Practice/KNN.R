#data<-read.csv("datasets/Abalone/abalone_data.csv",header = TRUE)
#View(data)

data(iris)

library(e1071)
library(class)
library(caTools)
set.seed(143)
split<-sample.split(iris$Species,SplitRatio = 0.75)

training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

#feature scaling
train_scale <- scale(training_set[,1:4])
test_scale <- scale(testing_set[,1:4])

#print(train_scale)

# Fitting KNN model to traning  dataset
classifier_knn <- knn(train = train_scale, test = test_scale,
                      cl = training_set$Species,
                      k=1)
print(classifier_knn)

#confusion matrix
cm <- table(testing_set$Species, classifier_knn)
print(cm)

misClassError <- mean(classifier_knn != testing_set$Species)
print(paste("Accuracy of KNN model: ", 1 - misClassError))