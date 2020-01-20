#2016/06/07
#地理一/B04208025/林芳偉

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