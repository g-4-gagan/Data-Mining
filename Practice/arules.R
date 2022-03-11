#install.packages("arules")

library(arules)

data("Groceries")

?Groceries
str(Groceries)
summary(Groceries)
head(Groceries)
View(Groceries)

itemFrequencyPlot(Groceries,topN = 20, type = "absolute")

rules <- apriori(Groceries, parameter = list(supp=0.001, conf=0.8))

inspect(rules)
inspect(head(rules))
inspect(rules[1:10])

rules<-sort(rules, by = "confidence", decreasing = T)

#visualization or plotting

#install.packages("arulesViz", dependencies = TRUE)

library(arulesViz)

plot(rules[1:5], method = "graph", engine = "interactive")
