#Data frames in R

print("Trying Data frame in R")

data('iris')
print('Summary of Iris Dataset:')
print(summary(iris))

cat("\nStructure of Iris Dataset:\n")
str(iris)

cat("\nColumn names of dataset:- ")
print(names(iris))

cat("\nattribute() is used for row index")
