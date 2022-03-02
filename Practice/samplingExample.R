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

