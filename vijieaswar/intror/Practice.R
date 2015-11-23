
cat <- "cat"
alpha <1
int <-3L
#for integer, you have to have the L at the end, else it will be treated as a numeric

#vectors are a sequence of objects that all have the same class. R will convert some of them if you have a word along with other numerics

list is a special type of vector that can have objects of different classes.

matrices are vectors with a deminsion attributes

x = matrix (1:6, nrow=2, ncol=3)

dim(x)
attributes(x)

x <- 1:10
dim(x) <- c(2,5)
# we added a dimension to create a matrix
# the above does the same as the command below
y <- matrix(1:10, nrow=2, ncol=5)
y
#you can also create a matrix with cbind

x <- 1:3
y <- 10:12
#binding by coloumn
x1 <- cbind(x,y)
x1
#binding by row
x2 <- rbind(x,y)
x2

#matrices all have to be the same class- cant mix char, numeric. dataframe acn have mixed classes
#Factors is another class- can label 

unclass(x) is more descriptive- 
  
num <- c(1,1,2,2,3)  
fact <- factor(num, level=c(1,2,3), labels=c("yes","no","maybe"))
#database has intergers and you are giving it an atrribute by giving it labels and saying what it means


datafram: each column has to be the same type of data 

(x <- data.frame(foo=1:4,bar=c(T,T,F,F)))

attributes are like metadata
adding information to your data without adding more values in the ddata.class(
  
  datafames have row.names
  
  you could give specific attributes such as names etc

x <-1:3
names(x)
#there wont be any names

names(x) <- c("alpha", "beta","gamma")
#the above does not give a data fram
names(x)
str(x)
#it is not a factor but it is assigning names- it is making a label
names function unique to the person but factor labels will replace any value = to that by that character

