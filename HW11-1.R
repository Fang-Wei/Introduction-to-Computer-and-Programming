#B04208025/�a�z�@/�L�ڰ�

#Question 1

rm(list=ls(all=TRUE))

F1 <- read.csv("cwb_466920_2006.csv") #Ū��csv�ɮ�

Ta <- F1$T.air #Ta=���
Td <- F1$Dew.point #Td=�S�I�ū�
RH <- F1$RH #RH=�۹����

Hr <- 1:365 #Hr=1��365��
nday <- 1

mTa <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iTa <- F1$T.air[nd]
  mTa[1,i] = mean(iTa) #mTa=��Ť饭����
}

mTd <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iTd <- F1$Dew.point[nd]
  mTd[1,i] = mean(iTd) #mTd=�S�I�ūפ饭����
}

mRH <- array(0,c(1,365))
for (i in c(1:365)) {
  nd <- seq((i+nday-1-1)*24+1,(i+nday-1-1)*24+24)
  iRH <- F1$RH[nd]
  mRH[1,i] = mean(iRH) #mRH=�۹���פ饭����
}

imTa <- as.numeric(mTa) #��mTa�Ʀ����
imTd <- as.numeric(mTd) #��mTd�Ʀ����
TA <- cbind(imTa,imTd) #TA=��šB�S�I�ūפ饭����

par(mfrow=c(2,1)) #2*1�覡�ƦC

matplot(Hr,TA,
        type="o", #�u
        pch=c(2,5), #�Х�
        col=c(1,2), #�C��
        xlim=c(1,365), #x�b�ƭ�
        ylim=c(0,40), #y�b�ƭ�
        xlab="Day", #x�b�y�ЦW��
        ylab="Tair and Dew point", #y�b�y�ЦW��
        main="Relationship between Day and Tair�BDew point") #���D

plot(Hr,mRH,
     type="o", #�u
     xlab="Day", #x�b�y�ЦW��
     ylab="RH", #y�b�y�ЦW��
     main="Relationship between Day and RH") #���D