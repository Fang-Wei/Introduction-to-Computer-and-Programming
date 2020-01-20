#地理一/B04208025/林芳偉
#Homework 10

rm(list=ls(all=TRUE))

#Question 1
A1 <- c(3,-2,1)
A2 <- c(6,8,-5)
A3 <- c(7,9,10)
B1 <- c(6,9,-4)
B2 <- c(7,5,3)
B3 <- c(-8,2,1)
C1 <- c(-7,-5,2)
C2 <- c(10,6,1)
C3 <- c(3,-9,8)

A <- rbind(A1,A2,A3)
B <- rbind(B1,B2,B3)
C <- rbind(C1,C2,C3)

#(1)
D1 <- A*B*c

#(2)
F <- B[2,]*A[,1]*C[3,]

#(3)
G <- sum((C*C))

#Question 2
I <- read.table("cwb_466920_2006.csv",header=TRUE,sep=",")

Ta1 <- I[2401:2424,5]
Td1 <- I[2401:2424,6] 
H <- (1000/8)*(Ta1-Td1) #H=水氣抬升凝結高度(m)
Hr <- 1:24

plot(Hr,H,xlab="Time(hr)",ylab="H(m)",main="Time series of High")
lines(Hr,H)