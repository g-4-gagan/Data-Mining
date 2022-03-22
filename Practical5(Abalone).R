abalone <- read.csv("datasets/Abalone/abalone_data.csv", header = TRUE)
print(summary(abalone))
#View(abalone)
print(head(abalone,5))

#library imports
library(caTools)
library(rpart)
#?rpart
library(e1071)
library(class)
library(rpart.plot)
library(caret)

#part a
print("Part a start Training set = 75% Test set = 25% ")

#Hold-out Method
print("Hold out Method")

set.seed(143)
split<-sample.split(abalone$Sex,SplitRatio = 0.75)

training_set = subset(abalone,split==TRUE)
testing_set = subset(abalone,split==FALSE)

#summary(training_set)
#summary(testing_set)

#feature scaling
train_scale <- scale(training_set[,2:9])
test_scale <- scale(testing_set[,2:9])

print(dim(training_set))
print(dim(testing_set))

#Naive bayes

print("Naive bayes-Hold out-part a")

classifier_naive <- naiveBayes(Sex~., data = training_set)
print(classifier_naive)

y_pred <- predict(classifier_naive, newdata = testing_set)
print(y_pred)

#confusion matrix
cm <- table(testing_set$Sex, y_pred)
print(cm)

print(confusionMatrix(cm))

#K-Nearest

print("KNN-Hold out-part a")

# Fitting KNN model to traning  dataset
classifier_knn <- knn(train = train_scale, test = test_scale,
                      cl = training_set$Sex,
                      k=1)
print(classifier_knn)

#confusion matrix
cm <- table(testing_set$Sex, classifier_knn)
print(cm)

misClassError <- mean(classifier_knn != testing_set$Sex)
print(paste("Accuracy of KNN model: ", 1 - misClassError))

#DECISION TREE

print("Decision Tree-Hold out-part a")

dtm<-rpart(Sex~.,training_set,method = "class")

rpart.plot(dtm)
rpart.plot(dtm,type = 4, extra = 101)

p<-predict(dtm,testing_set,type = "class")

#print(testing_set[1])
#print(confusionMatrix(testing_set[,1],p))
#print(confusionMatrix(testing_set[,1],p)$table)
#print(confusionMatrix(testing_set[,1],p)$overall["Accuracy"]*100)

#Random subsampling
print("Random Subsampling")

#cross-validation
print("Cross Validation")

#Naive Bayes

print("Naive bayes-Cross Validation-part a")

nb_model<-train(abalone[,2:9],abalone[,1],'nb',
                trControl = trainControl(method = 'cv', number=4))
print(nb_model)

#KNN

print("KNN-Cross Validation-part a")

knn_model<-train(abalone[,2:9],abalone[,1],'knn',
                 trControl = trainControl(method = 'cv', number=4))
print(knn_model)

#Decision Tree

print("Decision Tree-Cross Validation-part a")

Dtree_model<-train(abalone[,2:9],abalone[,1],'rpart',
                   trControl = trainControl(method = 'cv', number=4))
print(Dtree_model)

