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

#DECISION TREE
data(iris)
print(summary(iris))
print(head(iris,5))

#part a
#Hold-out Method

library(caTools)
set.seed(143)
split<-sample.split(iris$Species,SplitRatio = 0.75)

training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

#summary(training_set)
#summary(testing_set)

print(dim(training_set))
print(dim(testing_set))

library(rpart)
#?rpart

dtm<-rpart(Species~.,training_set,method = "class")
#dtm #gives textual description
#plot(dtm)
#text(dtm) #adding text to plot

#install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(dtm)
rpart.plot(dtm,type = 4, extra = 101)

p<-predict(dtm,testing_set,type = "class")

library(caret)

#print(confusionMatrix(testing_set[,5],p))
print(confusionMatrix(testing_set[,5],p)$table)
print(confusionMatrix(testing_set[,5],p)$overall["Accuracy"]*100)

#Random subsampling

#cross-validation
model<-train(iris[,1:4],iris[,5],'rpart',
             trControl = trainControl(method = 'cv', number=4))
model

