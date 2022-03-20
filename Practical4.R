# Q4. Run Apriori algorithm to find frequent itemsets and association rules
# 1.1 Use minimum support as 50% and minimum confidence as 75%
# 1.2 Use minimum support as 60% and minimum confidence as 60 %

receipt_df <- read.csv("datasets/Dataset for Apriori/1000/1000i.csv") 
View(receipt_df)

#Applying column names 
names(receipt_df) <- c("Receipt_Number", "Food", "Quantity")

#Applying Data Preprocessing
id<- c(1:5)
food<- c("milk", "sugar", "chocolate", "apple", "curd")
df <- data.frame(id, food)
print(df)

#map id to its text representation
receipt_df$Food <- df$food[match(receipt_df$Food,df$id)]

#after Preprocessing
head(receipt_df)
typeof(receipt_df)

library(arules)
library(arulesViz)
#convert into basket format to run  in apriori
test_df <- receipt_df[,c("Receipt_Number", "Food", "Quantity")]
df_trans <- as(split(test_df$Food, test_df$Receipt_Number),"transactions")

#support and confidence
rules <- apriori(df_trans,parameter = list(supp = .002, conf = .1))
plot(rules)

rules<- sort(rules, by="support", decreasing = T)
inspect(head(rules))



