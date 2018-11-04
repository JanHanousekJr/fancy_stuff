install.packages("ggplot2")
install.packages("fpp")
install.packages("reshape2")
library("ggplot2")
library("reshape2")
library("MASS")
library("dplyr")
library(fpp)
QrtNon=read.csv("Quarterly,NotAdjusted.csv")
QrtAd=read.csv("Quarterly,Adjusted.csv")
MthNon=read.csv("Monthly,NotAdjusted.csv")
MthAd=read.csv("Monthly,Adjusted.csv")



# convert date info in format '%Y-%m-%d'
MthAd$DATE=as.Date(MthAd$DATE, "%Y-%m-%d")
MthNon$DATE=as.Date(MthNon$DATE, "%Y-%m-%d")
QrtNon$DATE=as.Date(QrtNon$DATE, "%Y-%m-%d")
QrtAd$DATE=as.Date(QrtAd$DATE, "%Y-%m-%d")


#Total production quarterly graph
plot(QrtAd,type='l',col="blue",xlab='Date', ylab='Production Index',lwd=2)
lines(QrtNon, type='l', col="red", lwd=2)
legend("topleft",legend=c("Seasonally adjusted","Not adjusted"),col=c("blue","red"),pch=c(4,4))
title(main="Total Production of Intermediate Goods for Manufacturing for Denmark, Quarterly")


#Production total monthly graph
plot(MthAd,type='l',col="blue",xlab='Date', ylab='Production Index',lwd=2)
lines(MthNon, type='l', col="red", lwd=1)
legend("topleft",legend=c("Seasonally adjusted","Not adjusted"),col=c("blue","red"),pch=c(4,4))
title(main="Production: Manufacturing: Intermediate goods: Total for Denmark, Monthly")


#MEANS AND STANDART DEVIATIONS

mean(QrtNon$PRMNIG01DKQ661N)
sd(QrtNon$PRMNIG01DKQ661N)

mean(QrtAd$PRMNIG01DKQ661S)
sd(QrtAd$PRMNIG01DKQ661S)

mean(MthNon$DNKPRMNIG01IXOBM)
sd(MthNon$DNKPRMNIG01IXOBM)

mean(MthAd$DNKPRMNIG01IXOBSAM)
sd(MthAd$DNKPRMNIG01IXOBSAM)

#AUTOCORRELATION
#First is a plot with title explaining what it is
#Then there is a numerical series (if necessary)
acf(QrtNon$PRMNIG01DKQ661N,main="Autocorrelation of Quarterly non adjusted")
acf(QrtNon$PRMNIG01DKQ661N,plot="false")
acf(QrtAd$PRMNIG01DKQ661S,main="Autocorrelation of Quarterly adjusted")
acf(QrtAd$PRMNIG01DKQ661S,plot="false")
acf(MthNon$DNKPRMNIG01IXOBM,main="Autocorrelation of Monthly non adjusted")
acf(MthNon$DNKPRMNIG01IXOBM,plot="false")
acf(MthAd$DNKPRMNIG01IXOBSAM,main="Autocorrelation of Monthly adjusted")
acf(MthAd$DNKPRMNIG01IXOBSAM,plot="false")

#ARTIFICIAL QUARTERLY NON ADJUSTED
#first compute the lags for regression
lagqr1=lag(QrtNon$PRMNIG01DKQ661N,1)
lagqr2=lag(QrtNon$PRMNIG01DKQ661N,2)
lagqr3=lag(QrtNon$PRMNIG01DKQ661N,3)
lagqr4=lag(QrtNon$PRMNIG01DKQ661N,4)
lagqr5=lag(QrtNon$PRMNIG01DKQ661N,5)
lagqr6=lag(QrtNon$PRMNIG01DKQ661N,6)
lagqr7=lag(QrtNon$PRMNIG01DKQ661N,7)
#fit the linear model with our lags
lmqrtnn=lm(QrtNon$PRMNIG01DKQ661N~lagqr1+lagqr2+lagqr3+lagqr4+lagqr5+lagqr6+lagqr7)

#using regression predict our quarterly non adjusted data
predict_qrt=predict(lmqrtnn,QrtNon)
#add the prediction column to our dataset of quarterly non adjusted
QrtNon <- QrtNon %>% 
  mutate(predict_qrt)
#plot both predicted(arificial) and original data, red line is predicted

plot(x=QrtNon$DATE,y=QrtNon$PRMNIG01DKQ661N,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=QrtNon$DATE,y=QrtNon$predict_qrt,col='red',lwd=2)
legend("topleft",legend=c("Original index","Artificial index"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial and original quarterly not adjusted index")

#ARTIFICIAL MONTHLY NON ADJUSTED
lagmth1=lag(MthNon$DNKPRMNIG01IXOBM,1)
lagmth2=lag(MthNon$DNKPRMNIG01IXOBM,2)
lagmth3=lag(MthNon$DNKPRMNIG01IXOBM,3)
lagmth4=lag(MthNon$DNKPRMNIG01IXOBM,4)
lagmth5=lag(MthNon$DNKPRMNIG01IXOBM,5)
lagmth6=lag(MthNon$DNKPRMNIG01IXOBM,6)
lagmth7=lag(MthNon$DNKPRMNIG01IXOBM,7)
lagmth8=lag(MthNon$DNKPRMNIG01IXOBM,8)
lagmth9=lag(MthNon$DNKPRMNIG01IXOBM,9)
lagmth10=lag(MthNon$DNKPRMNIG01IXOBM,10)
lagmth11=lag(MthNon$DNKPRMNIG01IXOBM,11)
lagmth12=lag(MthNon$DNKPRMNIG01IXOBM,12)
lmmthnn=lm(MthNon$DNKPRMNIG01IXOBM~lagmth1+lagmth2+lagmth3+lagmth4+lagmth5+lagmth6+lagmth7+lagmth8+lagmth9+lagmth10+lagmth11+lagmth12)

predict_mth=predict(lmmthnn,MthNon)

MthNon <- MthNon %>% 
  mutate(predict_mth)


plot(x=MthNon$DATE,y=MthNon$DNKPRMNIG01IXOBM,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=MthNon$DATE,y=MthNon$predict_mth,col='red',lwd=2)
legend("topleft",legend=c("Original index","Artificial index"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial and original monthly not adjusted index")

#ARTIFICIAL QUARTERLY ADJUSTED
#first compute the lags for regression
lagqr11=lag(QrtAd$PRMNIG01DKQ661S,1)
lagqr22=lag(QrtAd$PRMNIG01DKQ661S,2)
lagqr33=lag(QrtAd$PRMNIG01DKQ661S,3)
lagqr44=lag(QrtAd$PRMNIG01DKQ661S,4)
lagqr55=lag(QrtAd$PRMNIG01DKQ661S,5)
lagqr66=lag(QrtAd$PRMNIG01DKQ661S,6)
lagqr77=lag(QrtAd$PRMNIG01DKQ661S,7)
#fit the linear model with our lags
lmqrtad=lm(QrtAd$PRMNIG01DKQ661S~lagqr11+lagqr22+lagqr33+lagqr44+lagqr55+lagqr66+lagqr77)

#using regression predict our quarterly non adjusted data
predict_qrt_ad=predict(lmqrtad,QrtNon)
#add the prediction column to our dataset of quarterly non adjusted
QrtAd <- QrtAd %>% 
  mutate(predict_qrt_ad)
#plot both predicted(arificial) and original data, red line is predicted

plot(x=QrtAd$DATE,y=QrtAd$PRMNIG01DKQ661S,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=QrtAd$DATE,y=QrtAd$predict_qrt_ad ,col='red',lwd=2)
legend("topleft",legend=c("Original index","Artificial index"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial and original quarterly seasonally adjusted index")



##ARTIFICIAL MONTHLY ADJUSTED
lagmth11=lag(MthAd$DNKPRMNIG01IXOBSAM,1)
lagmth22=lag(MthAd$DNKPRMNIG01IXOBSAM,2)
lagmth33=lag(MthAd$DNKPRMNIG01IXOBSAM,3)
lagmth44=lag(MthAd$DNKPRMNIG01IXOBSAM,4)
lagmth55=lag(MthAd$DNKPRMNIG01IXOBSAM,5)
lagmth66=lag(MthAd$DNKPRMNIG01IXOBSAM,6)
lagmth77=lag(MthAd$DNKPRMNIG01IXOBSAM,7)
lagmth88=lag(MthAd$DNKPRMNIG01IXOBSAM,8)
lagmth99=lag(MthAd$DNKPRMNIG01IXOBSAM,9)
lagmth100=lag(MthAd$DNKPRMNIG01IXOBSAM,10)
lagmth111=lag(MthAd$DNKPRMNIG01IXOBSAM,11)
lagmth122=lag(MthAd$DNKPRMNIG01IXOBSAM,12)
lmmthad=lm(MthAd$DNKPRMNIG01IXOBSAM~lagmth11+lagmth22+lagmth33+lagmth44+lagmth55+lagmth66+lagmth77+lagmth88+lagmth99+lagmth100+lagmth111+lagmth122)

predict_mth_ad=predict(lmmthad,MthNon)

MthAd <- MthAd %>% 
  mutate(predict_mth_ad)


plot(x=MthAd$DATE,y=MthAd$DNKPRMNIG01IXOBSAM,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=MthAd$DATE,y=MthAd$predict_mth_ad,col='red',lwd=2)
legend("topleft",legend=c("Original index","Artificial index"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial and original monthly seasonally adjusted index")



#COMPARISON OF our ARTIFICAL MONTHLY INDEXES BETWEEN EACH OTHER IN REGARDS TO SEASONALITY
plot(x=MthAd$DATE,y=MthAd$predict_mth_ad,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=MthNon$DATE,y=MthNon$predict_mth,col='red',lwd=1)
legend("topleft",legend=c("Seasonally Adjusted","Not Adjusted"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial monthly indexes")


#COMPARISON OF our ARTIFICAL QUARTERLY INDEXES BETWEEN EACH OTHER IN REGARDS TO SEASONALITY
plot(x=QrtAd$DATE,y=QrtAd$predict_qrt_ad,col='blue',type='l',xlab='Date', ylab='Production Index',lwd=2)
lines(x=QrtNon$DATE,y=QrtNon$predict_qrt,col='red',lwd=2)
legend("topleft",legend=c("Seasonally Adjusted","Not Adjusted"),col=c("blue","red"),pch=c(4,4))
title(main="Comparison of artificial quarterly indexes")





#SEASONAL ADJUSTMENT

#Monthly data
#first we have to create time series with our data
Mthts<- ts(MthNon$DNKPRMNIG01IXOBM, start=c(1985,1), end=c(2018, 5), frequency=12)
#Graphical representation of the seasonality
plot(stl(Mthts,s.window="period"))
#now we decompose te monthly data
decompose_Mth= decompose(Mthts, "additive")
#calculating the adjusted monthly data using additive method
adjust_Mth=Mthts-decompose_Mth$seasonal
#creating of dummy time series of the original adjusted data
Dummy_ts=ts(MthAd$DNKPRMNIG01IXOBSAM,start=c(1985,1), end=c(2018, 5), frequency=12)
#ploting our adjustment in comperison with theirs, our is blue
plot(adjust_Mth, col="blue",ylab='Production Index',lwd=1)
lines(Dummy_ts, type='l',col="red", lwd=1)
legend("topleft",legend=c("adjustment from database","our adjusment"),col=c("red","blue"),pch=c(4,4))
title(main="Comparison of adjustment on monthly data")

#Quarterly data
#first we have to create time series with our data
Qrtts <- ts(QrtNon$PRMNIG01DKQ661N, start=c(1985, 1), end=c(2018, 1), frequency=4)
#now we use stl function to determine the seasonality of the data and the trend
plot(stl(Qrtts,s.window="period"))
decompose_Qrt=decompose(Qrtts,"additive")
adjust_Qrt=Qrtts-decompose_Qrt$seasonal
Dummy_ts2=ts(QrtAd$PRMNIG01DKQ661S, start=c(1985, 1), end=c(2018, 1), frequency=4)
plot(adjust_Qrt,col="blue",ylab='Production Index',lwd=1)
lines(Dummy_ts2, type='l',col="red", lwd=1)
legend("topleft",legend=c("adjustment from database","our adjusment"),col=c("red","blue"),pch=c(4,4))
title(main="Comparison of adjustment on quarterly data")


