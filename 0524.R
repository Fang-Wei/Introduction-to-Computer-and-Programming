rm(list=ls(all=TRUE))

#1
F1 <- read.table("cwb_466920_2006.csv",header=TRUE,sep=",")
Tair <- F1[4801:5040,5]
X=1:240

plot(X,Tair,xlab="Time",ylab="Tair",main=("Time seris of T air"))
lines(X,Tair)

#2
a <- c(3,7,-4,12)
b <- c(-5,9,10,2)
c <- c(6,13,8,11)
d <- c(15,5,4,1)

A <- rbind(a,b,c,d)

B <- A[,2:4]
C <- A[2:4,]
D <- A[1:2,2:4]