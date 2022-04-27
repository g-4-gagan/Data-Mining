#Use Naive bayes, K-nearest, and Decision tree classification algorithms 
#and build classifiers. Divide the data set into training and test set. 
#Compare the accuracy of the different classifiers under the following 
#situations:

#5.1 a) Training set = 75% Test set = 25% 
#b) Training set = 66.6% (2/3rd of total), Test set = 33.3%
#5.2 Training set is chosen by i) hold out method 
#ii) Random subsampling iii) Cross-Validation. 
#Compare the accuracy of the classifiers obtained.
#5.3 Data is scaled to standard format.

data(iris)
print(summary(iris))
print(head(iris,5))

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
split<-sample.split(iris$Species,SplitRatio = 0.75)

training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

#summary(training_set)
#summary(testing_set)

#feature scaling
train_scale <- scale(training_set[,1:4])
test_scale <- scale(testing_set[,1:4])

print(dim(training_set))
print(dim(testing_set))

#Naive bayes

print("Naive bayes-Hold out-part a")

classifier_naive <- naiveBayes(Species~., data = training_set)
print(classifier_naive)

y_pred <- predict(classifier_naive, newdata = testing_set)
print(y_pred)

#confusion matrix
cm <- table(testing_set$Species, y_pred)
print(cm)

print(confusionMatrix(cm))

#K-Nearest

print("KNN-Hold out-part a")

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

#DECISION TREE

print("Decision Tree-Hold out-part a")

dtm<-rpart(Species~.,training_set,method = "class")
#dtm #gives textual description
#plot(dtm)
#text(dtm) #adding text to plot

rpart.plot(dtm)
rpart.plot(dtm,type = 4, extra = 101)

p<-predict(dtm,testing_set,type = "class")

#print(confusionMatrix(testing_set[,5],p))
print(confusionMatrix(testing_set[,5],p)$table)
print(confusionMatrix(testing_set[,5],p)$overall["Accuracy"]*100)

#Random subsampling
print("Random Subsampling")

dtm_acc=list()
knn_acc=list()
naive_acc=list()

for(x in 1:5)
{
  set.seed(123)
  
  split<-sample.split(iris$Species,SplitRatio = 0.75)
  
  train_set_rndm = subset(iris,split==TRUE)
  test_set_rndm = subset(iris,split==FALSE) 
  
  dim(test_set_rndm)
  dim(train_set_rndm)
  
  train_scale_rndm <- scale(train_set_rndm[,1:4]) 
  test_scale_rndm <- scale(test_set_rndm[,1:4])
  
  dtm_rndm = rpart(Species~., train_set_rndm,method = "class")
  dtm_rndm
  #plot(dtm_rndm)
  #rpart.plot(dtm_rndm)
  #rpart.plot(dtm_rndm,type = 4 , extra = 101)
  
  p=predict(dtm_rndm , test_set_rndm,type="class")
  
  confusionMatrix(test_set_rndm[,5],p)$table
  dtm_acc[x] = (confusionMatrix(test_set_rndm[,5],p)$overall['Accuracy']*100)
  print(paste("Decision tree Accuracy: ",dtm_acc[x]))
  
  # K-NEAREST USING RANDOM SUBSAMPLING (PART A)
  classifier_knn_rndm <- knn(train = train_scale_rndm[,c(1,2,3,4)], test = test_scale_rndm[,c(1,2,3,4)],cl = train_set_rndm$Species,k=1)
  classifier_knn
  
  #confusion matrix 
  cm = table(test_set_rndm$Species, classifier_knn_rndm)
  cm
  
  misclasserror = mean(classifier_knn_rndm!=test_set_rndm$Species)
  print(paste("Accuracy of knn model is :: ", 1 - misclasserror))
  
  knn_acc[x] = confusionMatrix(test_set_rndm[,5],classifier_knn_rndm)$overall['Accuracy']*100
  knn_acc
  
  # NAIVE BAYES USING RANDOM SUBSAMPLING (PART A) 
  
  classifier_naive_rndm = naiveBayes(Species~., data= train_set_rndm)
  classifier_naive
  
  predicted_y = predict(classifier_naive_rndm,newdata = test_set_rndm)
  predicted_y
  table(predicted_y)
  
  #confusion matrix 
  cm = table(test_set_rndm$Species, predicted_y)
  print("Naive Bayes confusion matrix")
  print(cm)
  confusionMatrix(cm)
  
  naive_acc[x]=confusionMatrix(test_set_rndm$Species,predicted_y)$overall['Accuracy']*100
  naive_acc
}

#cross-validation
print("Cross Validation")

#Naive Bayes

print("Naive bayes-Cross Validation-part a")

nb_model<-train(iris[,1:4],iris[,5],'nb',
             trControl = trainControl(method = 'cv', number=4))
print(nb_model)

#KNN

print("KNN-Cross Validation-part a")

knn_model<-train(iris[,1:4],iris[,5],'knn',
             trControl = trainControl(method = 'cv', number=4))
print(knn_model)

#Decision Tree

print("Decision Tree-Cross Validation-part a")

Dtree_model<-train(iris[,1:4],iris[,5],'rpart',
             trControl = trainControl(method = 'cv', number=4))
print(Dtree_model)

#part b
print("Part b start Training set = 66.6% (2/3rd of total), Test set = 33.3% ")

#Hold-out Method
print("Hold out Method")

set.seed(143)
split<-sample.split(iris$Species,SplitRatio = 0.6666)

training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

#feature scaling
train_scale <- scale(training_set[,1:4])
test_scale <- scale(testing_set[,1:4])

print(dim(training_set))
print(dim(testing_set))

#Naive bayes

print("Naive bayes-Hold out-part b")

classifier_naive <- naiveBayes(Species~., data = training_set)
print(classifier_naive)

y_pred <- predict(classifier_naive, newdata = testing_set)
print(y_pred)

#confusion matrix
cm <- table(testing_set$Species, y_pred)
print(cm)

print(confusionMatrix(cm))

#K-Nearest

print("KNN-Hold out-part b")

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

#DECISION TREE

print("Decision Tree-Hold out-part b")

dtm<-rpart(Species~.,training_set,method = "class")

#rpart.plot(dtm)
rpart.plot(dtm,type = 4, extra = 101)

p<-predict(dtm,testing_set,type = "class")

print(confusionMatrix(testing_set[,5],p))

#Random subsampling
print("Random Subsampling")

dtm_acc=list()
knn_acc=list()
naive_acc=list()

for(x in 1:5)
{
  set.seed(123)
  
  split<-sample.split(iris$Species,SplitRatio = 0.66)
  
  train_set_rndm = subset(iris,split==TRUE)
  test_set_rndm = subset(iris,split==FALSE) 
  
  dim(test_set_rndm)
  dim(train_set_rndm)
  
  train_scale_rndm <- scale(train_set_rndm[,1:4]) 
  test_scale_rndm <- scale(test_set_rndm[,1:4])
  
  dtm_rndm = rpart(Species~., train_set_rndm,method = "class")
  dtm_rndm
  #plot(dtm_rndm)
  #rpart.plot(dtm_rndm)
  #rpart.plot(dtm_rndm,type = 4 , extra = 101)
  
  p=predict(dtm_rndm , test_set_rndm,type="class")
  
  confusionMatrix(test_set_rndm[,5],p)$table
  dtm_acc[x] = (confusionMatrix(test_set_rndm[,5],p)$overall['Accuracy']*100)
  print(paste("Decision tree Accuracy: ",dtm_acc[x]))
  
  # K-NEAREST USING RANDOM SUBSAMPLING (PART A)
  classifier_knn_rndm <- knn(train = train_scale_rndm[,c(1,2,3,4)], test = test_scale_rndm[,c(1,2,3,4)],cl = train_set_rndm$Species,k=1)
  classifier_knn
  
  #confusion matrix 
  cm = table(test_set_rndm$Species, classifier_knn_rndm)
  cm
  
  misclasserror = mean(classifier_knn_rndm!=test_set_rndm$Species)
  print(paste("Accuracy of knn model is :: ", 1 - misclasserror))
  
  knn_acc[x] = confusionMatrix(test_set_rndm[,5],classifier_knn_rndm)$overall['Accuracy']*100
  knn_acc
  
  # NAIVE BAYES USING RANDOM SUBSAMPLING (PART A) 
  
  classifier_naive_rndm = naiveBayes(Species~., data= train_set_rndm)
  classifier_naive
  
  predicted_y = predict(classifier_naive_rndm,newdata = test_set_rndm)
  predicted_y
  table(predicted_y)
  
  #confusion matrix 
  cm = table(test_set_rndm$Species, predicted_y)
  print("Naive Bayes confusion matrix")
  print(cm)
  confusionMatrix(cm)
  
  naive_acc[x]=confusionMatrix(test_set_rndm$Species,predicted_y)$overall['Accuracy']*100
  naive_acc
}

#cross-validation
print("Cross Validation")

#Naive Bayes

print("Naive bayes-Cross Validation-part b")

nb_model<-train(iris[,1:4],iris[,5],'nb',
                trControl = trainControl(method = 'cv', number=3))
print(nb_model)

#KNN

print("KNN-Cross Validation-part b")

knn_model<-train(iris[,1:4],iris[,5],'knn',
                 trControl = trainControl(method = 'cv', number=3))
print(knn_model)

#DECISION TREE

print("Decision Tree-Cross Validation-part b")

Dtree_model<-train(iris[,1:4],iris[,5],'rpart',
             trControl = trainControl(method = 'cv', number=3))
print(Dtree_model)

