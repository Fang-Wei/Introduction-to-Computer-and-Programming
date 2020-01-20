#B04208025/地理一/林芳偉

rm(list=ls(all=TRUE))

#1(1)
F1 <- read.csv("Yushan20150210.csv")

fTwc <- function(Ta,V) { #fTwc=風寒指數函數
  Twc <- 13.12+0.6215*Ta-11.37*(V*3600/1000)^0.16+0.3965*Ta*(V*3600/1000)^0.16
  return(Twc)
}

#1(2)
fPs <- function(P) { #fPs=海平面大氣壓函數
  Ps <- P/(exp(-3844/8200))
}

#1(3)
hr <- 1:24 #hr=時間
P <- F1$測站氣壓.hPa. #P=測站氣壓(hPa)
Ta <- F1$溫度..C. #Ta=溫度(oC)
V <- F1$風速.m.s. #V=風速(m/s)

Twc <- fTwc(Ta,V) #Twc=風寒指數
Ps <- fPs(P) #Ps=海平面大氣壓

jpeg("Yushan.jpeg") #儲存成圖檔

layout(matrix(c(1,2,3,3),2,2,byrow=T))
plot(hr,Ta,
     type="o",
     pch=18,
     col=1,
     xlab="Hour",
     ylab="Temperature(oC)",
     main="Relationship between Hour and Temperature")

plot(hr,Twc,
     type="o",
     pch=18,
     col=1,
     xlab="Hour",
     ylab="Twc(oC)",
     main="Relationship between Hour and Twc")

plot(hr,Ps,
     type="o",
     pch=18,
     col=1,
     xlab="Hour",
     ylab="Ps(hPa)",
     main="Relationship between Hour and Ps")

dev.off()

#1(4)
All <- cbind(hr,Twc,Ps)
write.table(All,"windchill.txt",sep=",")

#2(1)
F2 <- read.csv("Gutin2009.csv")

PM10 <- F2$PM10
PM2.5 <- F2$PM2.5

PM10[PM10 <- NA] <- NA
PM2.5[PM2.5 <- NA] <- NA

nday <- 1

mPM10 <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iPM10 <- F2$PM10[nd]
  mPM10[1,i] = mean(na.omit(iPM10)) #mPM10=PM10日平均濃度
}

mPM2.5 <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iPM2.5 <- F2$PM2.5[nd]
  mPM2.5[1,i] = mean(na.omit(iPM2.5)) #mPM2.5=PM2.5日平均濃度
}

#2(2)
PM <- cbind(aperm(mPM10),aperm(mPM2.5))
day <- 1:365 #day=日期

jpeg("PM.jpeg") #儲存成圖檔

matplot(day,PM,
        type="o",
        pch=18,
        col=1,
        xlab="Day",
        ylab="濃度(微克/m^3)",
        main="Relationship between Day and PM10 and PM2.5")

dev.off()

#2(3)
C <- array(0,c(1,365)) #C=分類
mPM2.5[1,18] <- 0 #使第18天NaN=0
mPM2.5[1,143] <- 0 #使第143天NaN=0
mPM2.5[1,144] <- 0 #使第1144天NaN=0

for (i in 1:length(C))
  if (mPM2.5[i]>=0 & mPM2.5[i]<36) {C[i] <- 1
  } else if (mPM2.5[i]>=36 & mPM2.5[i]<54) {C[i] <- 2
  } else if (mPM2.5[i]>=54 & mPM2.5[i]<71) {C[i] <- 3
  } else if (mPM2.5[i]>=71) {C[i] <- 4
  }

C[1,18] <- NaN #使第18天0=NaN
C[1,143] <- NaN #使第143天0=NaN
C[1,144] <- NaN #使第144天0=NaN

#2(4)
All2 <- cbind(day,aperm(C))
write.table(All2,"classification.txt",sep=",")
