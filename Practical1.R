#Reading file people.txt
people = read.table(file = 'people.txt', sep = '\t',header = TRUE)

print(people)

#install package edit rules
#install.packages("editrules")
library(editrules)

#Create a ruleset E that contain rules to check for the following conditions:
#1. The age should be in the range 0-150.
#2. The age should be greater than yearsmarried.
#3. The status should be married or single or widowed.
#4. If age is less than 18 the agegroup should be child, if age is between 18 and 65 
#   the agegroup should be adult, if age is more than 65 the agegroup should be elderly.

E<-editset(expression(
  Age>=0,
  Age<=150,
  Age>yearsmarried,
  status %in% c('married','single','widowed'),
  if(Age<18) agegroup=='child',
  if(Age>=18 & Age<65) agegroup=='adult',
  if(Age>=65) agegroup=='elderly'
))

print(E)

ve<-violatedEdits(E,people)

summary(ve)

print(ve)

summary(ve,E)

#plot(E)
#plot(ve)
#plot(E,layout=layout.circle)