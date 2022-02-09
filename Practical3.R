#Load the data from wine dataset. 
#Check whether all attributes are standardized 
#or not (mean is 0 and standard deviation is 1). 
#If not, standardize the attributes. Do the same with Iris dataset.

#----IRIS DATASET-----
data("iris")
View(iris)
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

summary(iris)
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

#Install package caret
#install.packages("caret")

library(caret)

#for making mean = 0 and standard deviation = 1 in IRIS dataset.
iris_preprocess <- preProcess(iris[,1:4],method = c("center","scale"))
iris_transformed <- predict(iris_preprocess,iris[,1:4])
summary(iris_transformed)
sd(iris_transformed$Sepal.Length)
sd(iris_transformed$Sepal.Width)
sd(iris_transformed$Petal.Length)
sd(iris_transformed$Petal.Width)