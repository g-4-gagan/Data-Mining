#print("hello world!")

#using sample():- splitting 70% of total rows  into test and without replacement

data(mtcars)
n = nrow(mtcars)

print(paste("Number of rows: ",n))

trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
print(trainIndex)
train = mtcars[trainIndex,]
test = mtcars[-trainIndex,]
print(test)

#using caTools library
#install.packages("caTools")
library("caTools")

data("iris")

set.seed(123) #set seed so that same sample can be reproduce in future also
split<-sample.split(iris$Species,SplitRatio = 0.75)

print(split)
training_set = subset(iris,split==TRUE)
testing_set = subset(iris,split==FALSE)

dim(training_set)
dim(testing_set)

#by genearting random numbers till number of rows
#sample(number of rows,number of data)
s<-sample(150,50)
print(s)
#split the data similarly as per the first method