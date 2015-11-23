
#Viji Easwar assigment Intro to R

a <- 1:10
b <- 11:20
c <-a*b

fivec <- c[5]
fivec

fifty <- c[c>50]
fifty

alpha <- c(7,9,12,2,4,13)

dim(alpha) <- c(2,3)

alpha

beta <- c(1,7,12,19,2,8,13,20,3,9,14,21)
dim(beta) <- c(3,4)
beta

alphabeta <-  alpha %*% beta
alphabeta

library(datasets)
df <- mtcars
head(df)

means <- sapply(df, mean, na.rm=T)
means

df.mpg <- df[,1]
df.mpg

df.mpg2 <- df[,"mpg"]
df.mpg2


df.4 <- df[,4]
df.4

df.3row <- df[3,]
df.3row

df.5row <- df[5,]
df.5row

df.35row <- df[c(3,5),]
df.35row

df.hp100 <- df[df$hp >100,]
df.hp100

animals <- c("cat","dog","cow","pig")
for (i in animals) {
   if (i == "dog") {
     print (i)
   }
   else {
   }
 }

stats <-function(x, y) {
  print(mean(x)) 
  print(sd(x)) 
  print(median(y))
}

stats(df$disp, df$drat)


lapply(df, range, na.rm=T)
sapply(df, range, na.rm=T)
#the sapply displays the range min and max value in 2 rows

mapply()
?mapply (not in class)

Question8 <- tapply(df$hp, df$gear, max, na.rm=T)
Question8
