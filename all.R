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

###################

rm(list=ls(all=TRUE))

#Question 1

st <- c(1,2,3,4,5,6,7,8,9,10) #st=station
P <- c(315,450,610,370,530,420,480,350,290,505) #p=日累積雨量

for (i in 1:length(P))
  if(P[i]<400) {P[i] <- 4 #P=警戒標準等級
  } else if (P[i]>=400 & P[i]<500) {P[i] <- 3
  } else if (P[i]>=500 & P[i]<600) {P[i] <- 2
  } else if (P[i]>=600) {P[i] <- 1}

write.table(P,"DATA.txt",sep=",")

#Qusetion 2

F <- read.table("cwb_466920_2006.csv",header=TRUE,sep=",")
ts <- 4801:5040
Tair <- F$T.air[ts]
Hr <- 1:240
Td <- F$Dew.point[ts]

TA <- cbind(Tair,Td)
matplot(Hr,TA,
        type="o", #線
        pch=c(2,5), #標示
        col=c(1,2), #顏色
        xlim=c(0,240), #x軸數值
        ylim=c(20,40), #y軸數值
        xlab="Time", #x軸座標名稱
        ylab="Tair and Dew point", #y軸座標名稱
        main="Relationship between Tair and Dew point") #標題
legend(0,40,c("Tair","Dew point"),pch=c(2,5),col=c(1,2)) #圖例

###################

rm(list=ls(all=TRUE))

F1 <- read.csv("cwb_2006_0101.csv")

Td <- F1$Dew.point
Ta <- F1$T.air
RH <- F1$RH
Hr <- F1$Hour

Td[Td>Ta] <- NA
Td[Td==-9999] <- NA

RH[RH>100] <- 100
RH[RH==-9999] <- NA

layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(Hr,Ta,type="o")
plot(Hr,Td,type="o")
plot(Hr,RH,type="o")

#####################################

F2 <- read.csv("cwb_466920_2006.csv")
nday <- 1
mTa <- array(0,c(1,31))

for (i in c(1:31)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iTa <- F2$T.air[nd]
  mTa[1,i] = mean(iTa) #mTa=日平均溫度
}

Day <- 1:31

par(mfrow=c(1,1))
plot(Day,mTa,type="o")

####################

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

####################

rm(list=ls(all=TRUE))

F1 <- read.csv("cwb_466920_2006.csv") #讀取csv檔案

Ta <- F1$T.air #Ta=氣溫
Td <- F1$Dew.point #Td=露點溫度
RH <- F1$RH #RH=相對濕度

Hr <- 1:365 #Hr=1～365天
nday <- 1

mTa <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iTa <- F1$T.air[nd]
  mTa[1,i] = mean(iTa) #mTa=氣溫日平均值
}

mTd <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iTd <- F1$Dew.point[nd]
  mTd[1,i] = mean(iTd) #mTd=露點溫度日平均值
}

mRH <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iRH <- F1$RH[nd]
  mRH[1,i] = mean(iRH) #mRH=相對濕度日平均值
}

imTa <- as.numeric(mTa) #把mTa化成實數
imTd <- as.numeric(mTd) #把mTd化成實數
TA <- cbind(imTa,imTd) #TA=氣溫、露點溫度日平均值

par(mfrow=c(2,1)) #2*1方式排列

matplot(Hr,TA,
        type="o", #線
        pch=c(2,5), #標示
        col=c(1,2), #顏色
        xlim=c(1,365), #x軸數值
        ylim=c(0,40), #y軸數值
        xlab="Day", #x軸座標名稱
        ylab="Tair and Dew point", #y軸座標名稱
        main="Relationship between Day and Tair、Dew point") #標題

plot(Hr,mRH,
     type="o", #線
     xlab="Day", #x軸座標名稱
     ylab="RH", #y軸座標名稱
     main="Relationship between Day and RH") #標題

####################

rm(list=ls(all=TRUE))

st <- 1:28 #st=編號
P <- c(1182,305,255.1,97.6,336.7,328.1,48.2,239,274.4,42.5,185.5,76.1,355.6,313.3,44.1,55.1,102.7,138.6,645.8,467.7,148.9,240.5,172.4,133.5,567.5,114.3,200.2,631.4) #P=月累積雨量

PL <- array(0,c(1,28)) #PL=可用水水量分類等級

for (i in 1:length(P))
  if(P[i]<150) {PL[i] <- 5
  } else if (P[i]>=150 & P[i]<300) {PL[i] <- 4
  } else if (P[i]>=300 & P[i]<450) {PL[i] <- 3
  } else if (P[i]>=450 & P[i]<600) {PL[i] <- 2
  } else if (P[i]>=600) {PL[i] <- 1}

PL <- as.integer(PL) #把PL轉換成整數

write.table(PL,"PL.txt",sep=",")

####################

rm(list=ls(all=TRUE))

P <- c(768.5,768.6,768.6,768.5,768.2,768.2,769,769,769.5,769.5,769.1,767.9,767.8,767.3,766.3,768.1,767.3,767.3,767.2,767.7,767.8,767.8,767.9,767.1) #P=測站氣壓
T <- c(12.5,12.3,11.8,11.9,11.8,12.5,12.6,14.1,16.8,17.3,16.8,17,14.7,13.8,13.4,13.3,13.5,12.7,12.1,13,12,11.4,11.7,11.5) #T=溫度
RH <- c(84,80,78,73,78,78,75,76,61,67,70,77,91,93,95,97,95,94,95,95,92,89,89,83) #RH=相對溼度
time <- 1:24

#(1)露點溫度函數

fTd <- function(T,RH){
  es <- 0.611*exp((17.5*T)/(240.97+T))
  ea <- es*RH/100
  Td <- (240.97*log(ea/0.611))/(17.5-log(ea/0.611))
  return(Td)
}

#(2)海平面大氣壓函數

fPs <- function(P){
  Ps <- P/exp((-2415)/8200)
  return(Ps)
}

#(3)

Td <- fTd(T,RH) #Td=露點溫度
Ps <- fPs(P) #Ps=海平面大氣壓
TA <- cbind(T,Td)

par(mfrow=c(2,1))

matplot(time,TA,
        type="l",
        col=c(1,2), #溫度為黑色；露點溫度為紅色
        xlab="Time",
        ylab="Temperature")

plot(time,Ps,
     type="l",
     xlab="Time",
     ylab="Pressure")