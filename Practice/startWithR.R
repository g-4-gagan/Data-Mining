a = 4
b = 5
#print(a+b)
print(paste('a:',a))
print(paste('b:',b))
print(paste0("Sum of 2 numbers: ",(a+b)))

# paste inside print will add space between terms
# where as print0 will not include any space by self

x = "Hello World!"  # string
x1 = 255            # integer
x2 = 23.14          # float 

# string print
print(sprintf("%s is a string", x))

# integer print
print(sprintf("%d is integer", x1))

# float print
print(sprintf("%f is float", x2))

#readline or take input
a = readline()
# convert the inputted value to integer
a = as.integer(a);
print(a)