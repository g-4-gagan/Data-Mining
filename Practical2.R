#Perform the following preprocessing tasks on the dirty_iris dataset

#ii)Replace all the special values in data with NA.
#iii)Define these rules in a separate text file and read them.
#(Use editfile function in R (package editrules). 
#Print the resulting constraint object.
#-Species should be one of the following values: setosa, versicolor or virginica.
#-All measured numerical properties of an iris should be positive.
#-The petal length of an iris is at least 2 times its petal width.
#-The sepal length of an iris cannot exceed 30 cm.
#-The sepals of an iris are longer than its petals.
#iv)Determine how often each rule is broken (violatedEdits). Also summarize and plot the result.
#v)Find outliers in sepal length using boxplot and boxplot.stats

data<-read.table(file = 'dirty_iris.csv', sep=',', header = TRUE)
print(head(data,10))
print(summary(data))
str(data)

#i)Calculate the number and percentage of observations that are complete.
complete_cases <- complete.cases(data)

as.numeric(complete_cases)
n_complete<-sum(complete_cases)
print(n_complete)

percent_complete<- 100*n_complete/nrow(data)
print(paste("Cases in DIRTY_IRIS dataframe that are complete: ", percent_complete, "%"))
