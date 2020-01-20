#B04208025/地理一/林芳偉

#Question 1

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