#Load the data from wine dataset. 
#Check whether all attributes are standardized 
#or not (mean is 0 and standard deviation is 1). 
#If not, standardize the attributes. Do the same with Iris dataset.

#Install package caret
#install.packages("caret")

library(caret)

#----WINE DATASET-----

df_wine<-read.csv("wine.csv", header=FALSE)
View(df_wine)
str(df_wine)
summary(df_wine)
apply(df_wine,2,sd)


wine_pre<-preProcess(df_wine[,],method=c("center","scale"))
wine_standard<-predict(wine_pre,df_wine[,])
summary(wine_standard)
apply(wine_standard,2,sd)

#----IRIS DATASET-----
data("iris")

View(iris)
summary(iris)
apply(iris,2,sd)

#for making mean = 0 and standard deviation = 1 in IRIS dataset.
iris_pre<-preProcess(iris[,1:4], method=c("center", "scale"))
iris_standard<-predict(iris_pre, iris)
summary(iris_standard)
apply(iris_standard,2,sd)
