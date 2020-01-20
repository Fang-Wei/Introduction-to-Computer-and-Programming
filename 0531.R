#B04208025/地理一/林芳偉
#2016/5/31

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