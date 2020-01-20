rm(list=ls(all=TRUE))
a <- 3
b <- 5
c <- a+b

d <- "NTU Geography"
class(d)
class(3.1415)
f <- 100
class(f)
g <- as.integer(100)
class(g)
h <- 10+9i
class(h)
k <- TRUE
class(k)

L <- c(1, 3, 4,5,9,7,8)

m <- c("NTU","GEOG","B04")
m1 <- c("NTU","GEOG",104)
m2 <- c(104,"NTU","GEOG")
m3 <- c(TRUE, FALSE, TRUE)
m4 <- c(104,TRUE)
m5 <- c(104,TRUE,"NTU")
n1 <- L[4]
n2 <- L[c(1,3,5,7)]

p <- 1:10
p1 <- seq(1,10,2)
p2 <- seq(10,-5,-3)


X <- c(2,4,6,1,8)
Y <- c(8,7,5,6,8)
length(L)

Z1 <- X+Y
Z2 <- X*Y

Z3 <- sum(Z2)
Z4 <- cumsum(X)
Z5 <- sort(X)

A<-c(1,2)
B<-c(1,2,3,4,5,6)

W1 <- rbind(X,Y)
W2 <- rbind(X)

W3 <- cbind(X,Y)

W4 <- array(X,c(1,5))
W5 <- array(X,c(5,1))
W6 <- array(X,c(6,4))
a1=length(W6)
a2=dim(W6)
a3=ncol(W6)
a4=nrow(W6)

W7=aperm(W6) # this is a test

###################
F1 <- read.table("cwb_466920_2006.csv",header=TRUE,sep=",")
Tair <- F1[1:24,5]
Hr<-F1[1:24,3]
plot(Hr, Tair)

ts <- 1:24

plot(F1$Hour[ts],F1$T.air[ts],xlab="Time (Hour)",ylab="Tair",main="Time series of T air")
lines(F1$Hour[ts],F1$T.air[ts])


