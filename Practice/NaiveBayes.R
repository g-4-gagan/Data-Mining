data(iris)

library(e1071)
library(caret)
library(caTools)

set.seed(143)
split<-sample.split(iris$Species,SplitRatio = 0.75)

training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

#feature scaling
train_scale <- scale(training_set[,1:4])
test_scale <- scale(testing_set[,1:4])

classifier_naive <- naiveBayes(Species~., data = training_set)
print(classifier_naive)

y_pred <- predict(classifier_naive, newdata = testing_set)
print(y_pred)

#confusion matrix
cm <- table(testing_set$Species, y_pred)
print(cm)

print(confusionMatrix(cm))