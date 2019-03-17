
install.packages("colorspace")
install.packages("wavemulcor")
install.packages("waveslim") 
install.packages("W2CWM2C")
install.packages("TSA")
install.packages("tseries")
install.packages("lmtest")
install.packages("vars")
install.packages("aod")
install.packages("zoo")
install.packages("foreign")
install.packages("fGarch")
install.packages("quantmod")
install.packages("wavelets")
install.packages("xlsx")




library("colorspace") 
library("wavemulcor")
library("waveslim") 
library("W2CWM2C")
library("TSA")
library("tseries")
library("lmtest")
library("vars")
library("aod")
library("zoo")
library("foreign")
library("fGarch")
library("quantmod")
library("wavelets")
library("xlsx")


################################################################################
################################################################################
################################################################################

##### Observed crypto data
VOL <- read.table("C:/Users/Jabu/Desktop/rets.csv", header=T, sep=',');VOL





CRYPvol   <- read.table("C:/Users/user/Desktop/Multiscale coherency/cryptovol.csv", header=T, sep=',');CRYPvol
BTC    <- read.table("C:/Users/user/Desktop/Multiscale coherency/BTCvol.csv", header=T, sep=',');BTC
BTS    <- read.table("C:/Users/user/Desktop/Multiscale coherency/BTSvol.csv", header=T, sep=',');BTS
DASH   <- read.table("C:/Users/user/Desktop/Multiscale coherency/DASHvol.csv", header=T, sep=',');DASH
LTC    <- read.table("C:/Users/user/Desktop/Multiscale coherency/LTCvol.csv", header=T, sep=',');LTC
XLM    <- read.table("C:/Users/user/Desktop/Multiscale coherency/XLMvol.csv", header=T, sep=',');XLM
XMR    <- read.table("C:/Users/user/Desktop/Multiscale coherency/XMRvol.csv", header=T, sep=',');XMR
XRP    <- read.table("C:/Users/user/Desktop/Multiscale coherency/XRPvol.csv", header=T, sep=',');XRP



######### Wavelet multiple correlation of Cryptocurrency 
######### markets(, XRP, LTC, DASH, XRM, XLM, BTS)
######### returns
CRYP
CRYP1  <- CRYP[-1]
CRYP2  <- CRYP1[,1:7]
CRYP3  <- apply (log(CRYP2), 2, diff)*100
CRYP4  <- ts(CRYP3, start=1, frequency=1)

Wname  <- "la8"
J      <- 7

CRYP5  <- WMC(CRYP4, Wname, J, device="screen", NULL,
             NULL, NULL, NULL, NULL)



######### Unconditional returns
######### Absolute returns
CRYPA1  <- apply (log(CRYP2), 2, diff)*100
CRYPA2  <- abs(CRYPA1)
CRYPA3  <- ts(CRYPA2, start=1, frequency=1)


Wname  <- "la8"
J      <- 7


CRYPA4  <- WMC(CRYPA3, Wname, J, device="screen", NULL,
              NULL, NULL, NULL, NULL)




######### Conditional volatility returns
######### A constant mean GARCH (1,1)

############## Bitcoin
## garchFit 
btc = ts(diff(log(CRYP$BTC)))

BTC = garchFit(~ garch(1, 1), data = btc, trace = FALSE);BTC

## volatility 
## Standard Deviation: 
BTCvol1 = volatility(BTC, type = "sigma");BTCvol1

## Variance: 
BTCvol2 = volatility(BTC, type = "h");BTCvol2


## importing volatility data from r to excel         
write.xlsx(BTCvol2, "C:/Users/user/Desktop/BTCvol.xlsx")




############## Ripple
## garchFit 
xrp = ts(diff(log(CRYP$XRP)))

XRP = garchFit(~ garch(1, 1), data = xrp, trace = FALSE);XRP

## volatility 
## Standard Deviation: 
XRPvol1 = volatility(XRP, type = "sigma");XRPvol1

## Variance: 
XRPvol2 = volatility(XRP, type = "h");XRPvol2

## importing volatility data from r to excel         
write.xlsx(XRPvol2, "C:/Users/user/Desktop/XRPvol.xlsx")




############## Litcoin
## garchFit

ltc = ts(diff(log(CRYP$LTC)))
 
LTC = garchFit(~ garch(1, 1), data = ltc, trace = FALSE);LTC

## volatility 
## Standard Deviation: 
LTCvol1 = volatility(LTC, type = "sigma");LTCvol1

## Variance: 
LTCvol2 = volatility(LTC, type = "h");LTCvol2

## importing volatility data from r to excel         
write.xlsx(LTCvol2, "C:/Users/user/Desktop/LTCvol.xlsx")




############## Stellar
## garchFit 

xlm = ts(diff(log(CRYP$XLM)))
 
XLM = garchFit(~ garch(1, 1), data = xlm, trace = FALSE);XLM

## volatility 
## Standard Deviation: 
XLMvol1 = volatility(XLM, type = "sigma");XLMvol1

## Variance: 
XLMvol2 = volatility(XLM, type = "h");XLMvol2

## importing volatility data from r to excel         
write.xlsx(XLMvol2, "C:/Users/user/Desktop/XLMvol.xlsx")




############## DASH
## garchFit 

dash = ts(diff(log(CRYP$DASH)))

DASH = garchFit(~ garch(1, 1), data = dash, trace = FALSE);DASH

## volatility 
## Standard Deviation: 
DASHvol1 = volatility(DASH, type = "sigma");DASHvol1

## Variance: 
DASHvol2 = volatility(DASH, type = "h");DASHvol2

## importing volatility data from r to excel         
write.xlsx(DASHvol2, "C:/Users/user/Desktop/DASHvol.xlsx")



############## Monero
## garchFit 

xmr = ts(diff(log(CRYP$XMR)))

XMR = garchFit(~ garch(1, 1), data = xmr, trace = FALSE);XMR

## volatility 
## Standard Deviation: 
XMRvol1 = volatility(XMR, type = "sigma");XMRvol1

## Variance: 
XMRvol2 = volatility(XMR, type = "h");XMRvol2

## importing volatility data from r to excel         
write.xlsx(XMRvol2, "C:/Users/user/Desktop/XMRvol.xlsx")




############## BitShares
## garchFit 

bts = ts(diff(log(CRYP$BTS)))

BTS = garchFit(~ garch(1, 1), data = bts, trace = FALSE);BTS

## volatility 
## Standard Deviation: 
BTSvol1 = volatility(BTS, type = "sigma");BTSvol1

## Variance: 
BTSvol2 = volatility(BTS, type = "h");BTSvol2

## importing volatility data from r to excel         
write.xlsx(BTSvol2, "C:/Users/user/Desktop/BTSvol.xlsx")



######### conditional volatility returns
CRYPvol
wmc.CRYPvol1  <- CRYPvol[-1]
wmc.CRYPvol2  <- wmc.CRYPvol1[,1:7]
wmc.CRYPvol3  <- ts(wmc.CRYPvol2, start=1, frequency=1)

Wname  <- "la8"
J      <- 7

wmc.CRYPvol4  <- WMC(wmc.CRYPvol3, Wname, J, device="screen", NULL,
              NULL, NULL, NULL, NULL)




########## Wavelet multiple cross-correlation of Cryptocurrency
########## markets(BTC, XRP, LTC, DASH, XRM, XLM, BTS)

######### returns
CRYP
CRYP11  <- CRYP[-1]
CRYP22  <- CRYP1[,1:7]
CRYP33  <- apply (log (CRYP2), 2, diff)*100
CRYP44  <- ts(CRYP3, start=1, frequency=1)

Wname  <- "la8"
J      <- 7
lmax   <- 30

CRYP55  <- WMCC(CRYP44, Wname, J, lmax, device="screen", filename="Coherence")



######### Unconditional returns
######### Absolute returns
CRYPA11  <- apply (log(CRYP2), 2, diff)*100
CRYPA22  <- abs(CRYPA11)
CRYPA33  <- ts(CRYPA22, start=1, frequency=1)


Wname  <- "la8"
J      <- 7
lmax   <- 30

CRYPA44  <- WMCC(CRYPA33, Wname, J, lmax, device="screen", filename="Coherence")



######### Conditional volatility returns
######### A constant mean GARCH (1,1)
CRYPvol
wmcc.CRYPvol1  <- CRYPvol[-1]
wmcc.CRYPvol2  <- wmcc.CRYPvol1[,1:7]
wmcc.CRYPvol3  <- ts(wmcc.CRYPvol2, start=1, frequency=1)

Wname  <- "la8"
J      <- 7
lmax   <- 30

wmcc.CRYPA44  <- WMCC(wmcc.CRYPvol3, Wname, J, lmax, device="screen", filename="Coherence")




########## Time properties of Cryptocurrency markets
########## (BTC, XRP, LTC, DASH, XRM, XLM, BTS)
CRYPT   <- read.table("C:/Users/Omane-Adjepong/Desktop/crypto.csv", header=T, sep=',');CRYPT

####### returns
BTC1   <- ts(diff(log(CRYPT$BTC))*100)
XRP1   <- ts(diff(log(CRYPT$XRP))*100)
LTC1   <- ts(diff(log(CRYPT$LTC))*100)
XLM1   <- ts(diff(log(CRYPT$XLM))*100)
DASH1  <- ts(diff(log(CRYPT$DASH))*100)
XMR1   <- ts(diff(log(CRYPT$XMR))*100)
BTS1   <- ts(diff(log(CRYPT$BTS))*100)


####### unconditional returns - absolute returns
BTC11   <- ts(abs(BTC1))
XRP11   <- ts(abs(XRP1))
LTC11   <- ts(abs(LTC1))
XLM11   <- ts(abs(XLM1))
DASH11  <- ts(abs(DASH1))
XMR11   <- ts(abs(XMR1))
BTS11   <- ts(abs(BTS1))


####### conditional volatility - from mean Garch (1,1)
BTC = ts(BTC)  
XRP = ts(XRP)  
LTC = ts(LTC)
XLM = ts(XLM)
DASH = ts(DASH)
XMR  = ts(XMR)
BTS  = ts(BTS)


############ summary measures ############
##########################################
###### mean
mean(BTC1)
mean(BTC11)
mean(BTC)

mean(XRP1)
mean(XRP11)
mean(XRP)

mean(LTC1)
mean(LTC11)
mean(LTC)

mean(XLM1)
mean(XLM11)
mean(XLM)

mean(DASH1)
mean(DASH11)
mean(DASH)

mean(XMR1)
mean(XMR11)
mean(XMR)

mean(BTS1)
mean(BTS11)
mean(BTS)



###### standard deviation
sd(BTC1)
sd(BTC11)
sd(BTC)

sd(XRP1)
sd(XRP11)
sd(XRP)

sd(LTC1)
sd(LTC11)
sd(LTC)

sd(XLM1)
sd(XLM11)
sd(XLM)

sd(DASH1)
sd(DASH11)
sd(DASH)

sd(XMR1)
sd(XMR11)
sd(XMR)

sd(BTS1)
sd(BTS11)
sd(BTS)


###### sharpe ratio
srBTC1   <- (mean(BTC1))/sd(BTC1);srBTC1
srBTC11  <- (mean(BTC11))/sd(BTC11);srBTC11
srBTC    <- (mean(BTC))/sd(BTC);srBTC

srXRP1   <- (mean(XRP1))/sd(XRP1);srXRP1
srXRP11  <- (mean(XRP11))/sd(XRP11);srXRP11
srXRP    <- (mean(XRP))/sd(XRP);srXRP

srLTC1   <- (mean(LTC1))/sd(LTC1);srLTC1
srLTC11  <- (mean(LTC11))/sd(LTC11);srLTC11
srLTC    <- (mean(LTC))/sd(LTC);srLTC

srXLM1   <- (mean(XLM1))/sd(XLM1);srXLM1
srXLM11  <- (mean(XLM11))/sd(XLM11);srXLM11
srXLM    <- (mean(XLM))/sd(XLM);srXLM

srDASH1   <- (mean(DASH1))/sd(DASH1);srDASH1
srDASH11  <- (mean(DASH11))/sd(DASH11);srDASH11
srDASH    <- (mean(DASH))/sd(DASH);srDASH

srXMR1    <- (mean(XMR1))/sd(XMR1);srXMR1
srXMR11   <- (mean(XMR11))/sd(XMR11);srXMR11
srXMR     <- (mean(XMR))/sd(XMR);srXMR

srBTS1    <- (mean(BTS1))/sd(BTS1);srBTS1
srBTS11   <- (mean(BTS11))/sd(BTS11);srBTS11
srBTS     <- (mean(BTS))/sd(BTS);srBTS



###### Skewness
skewness(BTC1)
skewness(BTC11)
skewness(BTC)

skewness(XRP1)
skewness(XRP11)
skewness(XRP)

skewness(LTC1)
skewness(LTC11)
skewness(LTC)

skewness(XLM1)
skewness(XLM11)
skewness(XLM)

skewness(DASH1)
skewness(DASH11)
skewness(DASH)

skewness(XMR1)
skewness(XMR11)
skewness(XMR)

skewness(BTS1)
skewness(BTS11)
skewness(BTS)



###### Kurtosis
kurtosis(BTC1)
kurtosis(BTC11)
kurtosis(BTC)

kurtosis(XRP1)
kurtosis(XRP11)
kurtosis(XRP)

kurtosis(LTC1)
kurtosis(LTC11)
kurtosis(LTC)

kurtosis(XLM1)
kurtosis(XLM11)
kurtosis(XLM)

kurtosis(DASH1)
kurtosis(DASH11)
kurtosis(DASH)

kurtosis(XMR1)
kurtosis(XMR11)
kurtosis(XMR)

kurtosis(BTS1)
kurtosis(BTS11)
kurtosis(BTS)



######### JB normality test
jarque.bera.test(BTC1)
jarque.bera.test(BTC11)
jarque.bera.test(BTC)

jarque.bera.test(XRP1)
jarque.bera.test(XRP11)
jarque.bera.test(XRP)

jarque.bera.test(LTC1)
jarque.bera.test(LTC11)
jarque.bera.test(LTC)

jarque.bera.test(XLM1)
jarque.bera.test(XLM11)
jarque.bera.test(XLM)

jarque.bera.test(DASH1)
jarque.bera.test(DASH11)
jarque.bera.test(DASH)

jarque.bera.test(XMR1)
jarque.bera.test(XMR11)
jarque.bera.test(XMR)

jarque.bera.test(BTS1)
jarque.bera.test(BTS11)
jarque.bera.test(BTS)



######### Serial correlation test
Box.test(BTC1, type="Ljung-Box", lag=20)
Box.test(BTC11, type="Ljung-Box", lag=20)
Box.test(BTC, type="Ljung-Box", lag=20)

Box.test(XRP1, type="Ljung-Box", lag=20)
Box.test(XRP11, type="Ljung-Box", lag=20)
Box.test(XRP, type="Ljung-Box", lag=20)

Box.test(LTC1, type="Ljung-Box", lag=20)
Box.test(LTC11, type="Ljung-Box", lag=20)
Box.test(LTC, type="Ljung-Box", lag=20)

Box.test(XLM1, type="Ljung-Box", lag=20)
Box.test(XLM11, type="Ljung-Box", lag=20)
Box.test(XLM, type="Ljung-Box", lag=20)

Box.test(DASH1, type="Ljung-Box", lag=20)
Box.test(DASH11, type="Ljung-Box", lag=20)
Box.test(DASH, type="Ljung-Box", lag=20)

Box.test(XMR1, type="Ljung-Box", lag=20)
Box.test(XMR11, type="Ljung-Box", lag=20)
Box.test(XMR, type="Ljung-Box", lag=20)

Box.test(BTS1, type="Ljung-Box", lag=20)
Box.test(BTS11, type="Ljung-Box", lag=20)
Box.test(BTS, type="Ljung-Box", lag=20)



######### Zivot-Andrews unit root test
zaBTC1   <-  ur.za(BTC1, model="both");summary(zaBTC1)
zaBTC11  <-  ur.za(BTC11, model="both");summary(zaBTC11)
zaBTC111 <-  ur.za(BTC, model="both");summary(zaBTC111)

zaXRP1   <-  ur.za(XRP1, model="both");summary(zaXRP1)
zaXRP11  <-  ur.za(XRP11, model="both");summary(zaXRP11)
zaXRP111 <-  ur.za(XRP, model="both");summary(zaXRP111)

zaLTC1   <-  ur.za(LTC1, model="both");summary(zaLTC1)
zaLTC11  <-  ur.za(LTC11, model="both");summary(zaLTC11)
zaLTC111 <-  ur.za(LTC, model="both");summary(zaLTC111)

zaXLM1   <-  ur.za(XLM1, model="both");summary(zaXLM1)
zaXLM11  <-  ur.za(XLM11, model="both");summary(zaXLM11)
zaXLM111 <-  ur.za(XLM, model="both");summary(zaXLM111)

zaDASH1   <-  ur.za(DASH1, model="both");summary(zaDASH1)
zaDASH11  <-  ur.za(DASH11, model="both");summary(zaDASH11)
zaDASH111 <-  ur.za(DASH, model="both");summary(zaDASH111)

zaXMR1   <-  ur.za(XMR1, model="both");summary(zaXMR1)
zaXMR11  <-  ur.za(XMR11, model="both");summary(zaXMR11)
zaXMR111 <-  ur.za(XMR, model="both");summary(zaXMR111)

zaBTS1   <-  ur.za(BTS1, model="both");summary(zaBTS1)
zaBTS11  <-  ur.za(BTS11, model="both");summary(zaBTS11)
zaBTS111 <-  ur.za(BTS, model="both");summary(zaBTS111)




################### Todda-Yamamoto causality (lead-lag lingages) 
################### test for volatility spillovers
## Full sample volatility linkages - conditional volatility

BTCBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCBTS.csv", header=T, sep=',');BTCBTS
BTCDASH   <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCDASH.csv", header=T, sep=',');BTCDASH
BTCLTC    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCLTC.csv", header=T, sep=',');BTCLTC 
BTCXLM    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCXLM.csv", header=T, sep=',');BTCXLM  
BTCXMR    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCXMR.csv", header=T, sep=',');BTCXMR
BTCXRP    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/BTCXRP.csv", header=T, sep=',');BTCXRP

DASHBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/DASHBTS.csv", header=T, sep=',');DASHBTS 
DASHXLM    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/DASHXLM.csv", header=T, sep=',');DASHXLM 
DASHXMR    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/DASHXMR.csv", header=T, sep=',');DASHXMR  

LTCBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/LTCBTS.csv", header=T, sep=',');LTCBTS 
LTCDASH   <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/LTCDASH.csv", header=T, sep=',');LTCDASH 
LTCXLM    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/LTCXLM.csv", header=T, sep=',');LTCXLM  
LTCXMR    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/LTCXMR.csv", header=T, sep=',');LTCXMR  

XLMBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XLMBTS.csv", header=T, sep=',');XLMBTS  

XMRBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XMRBTS.csv", header=T, sep=',');XMRBTS 
XMRXLM    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XMRXLM.csv", header=T, sep=',');XMRXLM 

XRPBTS    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XRPBTS.csv", header=T, sep=',');XRPBTS 
XRPDASH   <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XRPDASH.csv", header=T, sep=',');XRPDASH
XRPLTC    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XRPLTC.csv", header=T, sep=',');XRPLTC 
XRPXLM    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XRPXLM.csv", header=T, sep=',');XRPXLM  
XRPXMR    <- read.table("C:/Users/Omane-Adjepong/Desktop/Multiscale coherency/bivariate1/XRPXMR.csv", header=T, sep=',');XRPXMR 



####################
########## BTC & XRP
VARselect(BTCXRP,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3
V.1<-VAR(BTCXRP,p=3,type="both")
serial.test(V.1, type=c("BG"))

plot(serial.test(V.1))

#Stability analysis
1/roots(V.1)[[1]] # ">1"
1/roots(V.1)[[2]] # ">1"
plot(stability(V.1, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.1<-VAR(BTCXRP,p=4,type="both");V.1$varresult    
summary(V.1)                                     

library(aod)
#Wald-test (H0: XRP does not Granger-cause BTC)
wald.test(b=coef(V.1$varresult[[1]]), Sigma=vcov(V.1$varresult[[1]]), Terms=c(2,4,6,8))
# Could NOT be rejected at 1%, 5% & 10% X2=2.6; p=0.62)

#Wald.test (H0: BTC does not Granger-cause XRP)
wald.test(b=coef(V.1$varresult[[2]]), Sigma=vcov(V.1$varresult[[2]]), Terms= c(1,3,5,7))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.9; p=0.75)




####################
########## BTC & LTC
VARselect(BTCLTC,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.2<-VAR(BTCLTC,p=7,type="both")
serial.test(V.2, type=c("BG"))

plot(serial.test(V.2))

#Stability analysis
1/roots(V.2)[[1]] # ">1"
1/roots(V.2)[[2]] # ">1"
plot(stability(V.2, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.2<-VAR(BTCLTC,p=8,type="both");V.2$varresult   
summary(V.2)                                     

library(aod)
#Wald-test (H0: LTC does not Granger-cause BTC)
wald.test(b=coef(V.2$varresult[[1]]), Sigma=vcov(V.2$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could be rejected at 1%, 5% & 10% (X2=24.0; p=0.0023)

#Wald.test (H0: BTC does not Granger-cause LTC)
wald.test(b=coef(V.2$varresult[[2]]), Sigma=vcov(V.2$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=11.1; p=0.20)




####################
########## BTC & DASH
VARselect(BTCDASH,lag=20,type="both")     ## AIC=2

#VAR Model, lag=2
V.3<-VAR(BTCDASH,p=2,type="both")
serial.test(V.3, type=c("BG"))

plot(serial.test(V.3))

#Stability analysis
1/roots(V.3)[[1]] # ">1"
1/roots(V.3)[[2]] # ">1"
plot(stability(V.3, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.3<-VAR(BTCDASH,p=3,type="both");V.3$varresult   
summary(V.3)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause BTC)
wald.test(b=coef(V.3$varresult[[1]]), Sigma=vcov(V.3$varresult[[1]]), Terms=c(2,4,6))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.4; p=0.71)

#Wald.test (H0: BTC does not Granger-cause DASH)
wald.test(b=coef(V.3$varresult[[2]]), Sigma=vcov(V.3$varresult[[2]]), Terms= c(1,3,5))
# Could NOT be rejected at 1%, 5% & 10% (X2=2.9; p=0.4)




####################
########## BTC & XMR
VARselect(BTCXMR,lag=20,type="both")     ## AIC=7,8,9

#VAR Model, lag=7,8,9
V.4<-VAR(BTCXMR,p=9,type="both")
serial.test(V.4, type=c("BG"))

plot(serial.test(V.4))

#Stability analysis
1/roots(V.4)[[1]] # ">1"
1/roots(V.4)[[2]] # ">1"
plot(stability(V.4, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.4<-VAR(BTCXMR,p=10,type="both");V.4$varresult    
summary(V.4)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause BTC)
wald.test(b=coef(V.4$varresult[[1]]), Sigma=vcov(V.4$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20))
# Could NOT be rejected at 1%, 5% & 10% (X2=3.5; p=0.97)

#Wald.test (H0: BTC does not Granger-cause XMR)
wald.test(b=coef(V.4$varresult[[2]]), Sigma=vcov(V.4$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.8; p=0.46)




####################
########## BTC & XLM
VARselect(BTCXLM,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3
V.5<-VAR(BTCXLM,p=3,type="both")
serial.test(V.5, type=c("BG"))

plot(serial.test(V.5))

#Stability analysis
1/roots(V.5)[[1]] # ">1"
1/roots(V.5)[[2]] # ">1"
plot(stability(V.5, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.5<-VAR(BTCXLM,p=4,type="both");V.5$varresult    
summary(V.5)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause BTC)
wald.test(b=coef(V.5$varresult[[1]]), Sigma=vcov(V.5$varresult[[1]]), Terms=c(2,4,6,8))
# Could NOT be rejected at 1%, 5% & 10% (X2=0.99; p=0.91)

#Wald.test (H0: BTC does not Granger-cause XLM)
wald.test(b=coef(V.5$varresult[[2]]), Sigma=vcov(V.5$varresult[[2]]), Terms= c(1,3,5,7))
# Could NOT be rejected at 1%, 5% & 10% (X2=2.9; p=0.58)




####################
########## BTC & BTS
VARselect(BTCBTS,lag=20,type="both")     ## AIC=2

#VAR Model, lag=2;
V.6<-VAR(BTCBTS,p=2,type="both")
serial.test(V.6, type=c("BG"))

plot(serial.test(V.6))

#Stability analysis
1/roots(V.6)[[1]] # ">1"
1/roots(V.6)[[2]] # ">1"
plot(stability(V.6, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.6<-VAR(BTCBTS,p=3,type="both");V.6$varresult   
summary(V.6)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause BTC)
wald.test(b=coef(V.6$varresult[[1]]), Sigma=vcov(V.6$varresult[[1]]), Terms=c(2,4,6))
# Could be rejected at 5% & 10% (X2=10.5; p=0.015)

#Wald.test (H0: BTC does not Granger-cause BTS)
wald.test(b=coef(V.6$varresult[[2]]), Sigma=vcov(V.6$varresult[[2]]), Terms= c(1,3,5))
# Could be rejected at 1%, 5% & 10% (X2=27.2; p=0.0000055)





####################
########## XRP & LTC
VARselect(XRPLTC,lag=20,type="both")     ## AIC=7,5

#VAR Model, lag=7,5
V.7<-VAR(XRPLTC,p=5,type="both")
serial.test(V.7, type=c("BG"))

plot(serial.test(V.7))

#Stability analysis
1/roots(V.7)[[1]] # ">1"
1/roots(V.7)[[2]] # ">1"
plot(stability(V.7, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.7<-VAR(XRPLTC,p=6,type="both");V.7$varresult   
summary(V.7)                                   

library(aod)
#Wald-test (H0: LTC does not Granger-cause XRP)
wald.test(b=coef(V.7$varresult[[1]]), Sigma=vcov(V.7$varresult[[1]]), Terms=c(2,4,6,8,10,12))
# Could be rejected at 1%, 5%, 10% (X2=19.4; p=0.0035)

#Wald.test (H0: XRP does not Granger-cause LTC)
wald.test(b=coef(V.7$varresult[[2]]), Sigma=vcov(V.7$varresult[[2]]), Terms= c(1,3,5,7,9,11))
# Could be rejected at 1%, 5% & 10% (X2=354.5; p=0.0000)




####################
########## XRP & DASH
VARselect(XRPDASH,lag=20,type="both")     ## AIC=2

#VAR Model, lag=2
V.8<-VAR(XRPDASH,p=2,type="both")
serial.test(V.8, type=c("BG"))

plot(serial.test(V.8))

#Stability analysis
1/roots(V.8)[[1]] # ">1"
1/roots(V.8)[[2]] # ">1"
plot(stability(V.8, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.8<-VAR(XRPDASH,p=3,type="both");V.8$varresult   
summary(V.8)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause XRP)
wald.test(b=coef(V.8$varresult[[1]]), Sigma=vcov(V.8$varresult[[1]]), Terms=c(2,4,6))
# Could NOT be rejected at 1%, 5% & 10% (X2=0.16; p=0.98)

#Wald.test (H0: XRP does not Granger-cause DASH)
wald.test(b=coef(V.8$varresult[[2]]), Sigma=vcov(V.8$varresult[[2]]), Terms= c(1,3,5))
# Could NOT be rejected at 1%, 5% & 10% (X2=0.58; p=0.90)




####################
########## XRP & XMR
VARselect(XRPXMR,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.9<-VAR(XRPXMR,p=7,type="both")
serial.test(V.9, type=c("BG"))

plot(serial.test(V.9))

#Stability analysis
1/roots(V.9)[[1]] # ">1"
1/roots(V.9)[[2]] # ">1"
plot(stability(V.9, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.9<-VAR(XRPXMR,p=8,type="both");V.9$varresult   
summary(V.9)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause XRP)
wald.test(b=coef(V.9$varresult[[1]]), Sigma=vcov(V.9$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.5; p=0.99)

#Wald.test (H0: XRP does not Granger-cause XMR)
wald.test(b=coef(V.9$varresult[[2]]), Sigma=vcov(V.9$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=2.9; p=0.94)



####################
########## XRP & XLM
VARselect(XRPXLM,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3;
V.10<-VAR(XRPXLM,p=3,type="both")
serial.test(V.10, type=c("BG"))

plot(serial.test(V.10))

#Stability analysis
1/roots(V.10)[[1]] # ">1"
1/roots(V.10)[[2]] # ">1"
plot(stability(V.10, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.10<-VAR(XRPXLM,p=4,type="both");V.10$varresult   
summary(V.10)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause XRP)
wald.test(b=coef(V.10$varresult[[1]]), Sigma=vcov(V.10$varresult[[1]]), Terms=c(2,4,6,8))
# Could NOT be rejected at 1%, 5% & 10% (X2=0.96; p=0.92)

#Wald.test (H0: XRP does not Granger-cause XLM)
wald.test(b=coef(V.10$varresult[[2]]), Sigma=vcov(V.10$varresult[[2]]), Terms= c(1,3,5,7))
# Could NOT be rejected at 1%, 5% & 10% (X2=4.6; p=0.33)




####################
########## XRP & BTS
VARselect(XRPBTS,lag=20,type="both")     ## AIC=2

#VAR Model, lag=2
V.11<-VAR(XRPBTS,p=2,type="both")
serial.test(V.11, type=c("BG"))

plot(serial.test(V.11))

#Stability analysis
1/roots(V.11)[[1]] # ">1"
1/roots(V.11)[[2]] # ">1"
plot(stability(V.11, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.11<-VAR(XRPBTS,p=3,type="both");V.11$varresult    
summary(V.11)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XRP)
wald.test(b=coef(V.11$varresult[[1]]), Sigma=vcov(V.11$varresult[[1]]), Terms=c(2,4,6))
# Could NOT be rejected at 1%, 5%, 10% (X2=5.0; p=0.17)

#Wald.test (H0: XRP does not Granger-cause BTS)
wald.test(b=coef(V.11$varresult[[2]]), Sigma=vcov(V.11$varresult[[2]]), Terms= c(1,3,5))
# Could be rejected at 10% (X2=6.9; p=0.076)




####################
########## LTC & DASH
VARselect(LTCDASH,lag=20,type="both")     ## AIC=2

#VAR Model, lag=2;
V.12<-VAR(LTCDASH,p=2,type="both")
serial.test(V.12, type=c("BG"))

plot(serial.test(V.12))

#Stability analysis
1/roots(V.12)[[1]] # ">1"
1/roots(V.12)[[2]] # ">1"
plot(stability(V.1, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.12<-VAR(LTCDASH,p=3,type="both");V.12$varresult   
summary(V.12)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause LTC)
wald.test(b=coef(V.12$varresult[[1]]), Sigma=vcov(V.12$varresult[[1]]), Terms=c(2,4,6))
# Could be rejected at 5% (X2=3.2; p=0.37)

#Wald.test (H0: LTC does not Granger-cause DASH)
wald.test(b=coef(V.12$varresult[[2]]), Sigma=vcov(V.12$varresult[[2]]), Terms= c(1,3,5))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.1; p=0.78)




####################
########## LTC & XMR
VARselect(LTCXMR,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.13<-VAR(LTCXMR,p=7,type="both")
serial.test(V.13, type=c("BG"))

plot(serial.test(V.13))

#Stability analysis
1/roots(V.13)[[1]] # ">1"
1/roots(V.13)[[2]] # ">1"
plot(stability(V.13, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.13<-VAR(LTCXMR,p=8,type="both");V.13$varresult  
summary(V.13)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause LTC)
wald.test(b=coef(V.13$varresult[[1]]), Sigma=vcov(V.13$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.0; p=1.0)

#Wald.test (H0: LTC does not Granger-cause XMR)
wald.test(b=coef(V.13$varresult[[2]]), Sigma=vcov(V.13$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=2.8; p=0.94)




####################
########## LTC & XLM
VARselect(LTCXLM,lag=20,type="both")     ## AIC=6

#VAR Model, lag=6
V.14<-VAR(LTCXLM,p=6,type="both")
serial.test(V.14, type=c("BG"))

plot(serial.test(V.14))

#Stability analysis
1/roots(V.14)[[1]] # ">1"
1/roots(V.14)[[2]] # ">1"
plot(stability(V.1, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.14<-VAR(LTCXLM,p=7,type="both");V.14$varresult  
summary(V.14)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause LTC)
wald.test(b=coef(V.14$varresult[[1]]), Sigma=vcov(V.14$varresult[[1]]), Terms=c(2,4,6,8,10,12,14))
# Could be rejected at 1%, 5% & 10% (X2=44.0; p=0.000000021)

#Wald.test (H0: LTC does not Granger-cause XLM)
wald.test(b=coef(V.14$varresult[[2]]), Sigma=vcov(V.14$varresult[[2]]), Terms= c(1,3,5,7,9,11,13))
# Could NOT be rejected at 1%, 5% & 10% (X2=4.0; p=0.78)




####################
########## LTC & BTS
VARselect(LTCBTS,lag=20,type="both")     ## AIC=6, 5

#VAR Model, lag=6,5
V.15<-VAR(LTCBTS,p=5,type="both")
serial.test(V.15, type=c("BG"))

plot(serial.test(V.15))

#Stability analysis
1/roots(V.15)[[1]] # ">1"
1/roots(V.15)[[2]] # ">1"
plot(stability(V.15, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.15<-VAR(LTCBTS,p=6,type="both");V.15$varresult   
summary(V.15)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause LTC)
wald.test(b=coef(V.15$varresult[[1]]), Sigma=vcov(V.15$varresult[[1]]), Terms=c(2,4,6,8,10,12))
# Could be rejected at 1%, 5% & 10% (X2=64.4; p=0.0000000000057)

#Wald.test (H0: LTC does not Granger-cause BTS)
wald.test(b=coef(V.15$varresult[[2]]), Sigma=vcov(V.15$varresult[[2]]), Terms= c(1,3,5,7,9,11))
# Could be rejected at 10% (X2=10.9; p=0.092)




####################
########## DASH & XMR
VARselect(DASHXMR,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.16<-VAR(DASHXMR,p=7,type="both")
serial.test(V.16, type=c("BG"))

plot(serial.test(V.16))

#Stability analysis
1/roots(V.16)[[1]] # ">1"
1/roots(V.16)[[2]] # ">1"
plot(stability(V.16, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.16<-VAR(DASHXMR,p=8,type="both");V.16$varresult  
summary(V.16)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause DASH)
wald.test(b=coef(V.16$varresult[[1]]), Sigma=vcov(V.16$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.6; p=0.99)

#Wald.test (H0: DASH does not Granger-cause XMR)
wald.test(b=coef(V.16$varresult[[2]]), Sigma=vcov(V.16$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=3.4; p=0.91)




####################
########## DASH & XLM
VARselect(DASHXLM,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3
V.17<-VAR(DASHXLM,p=3,type="both")
serial.test(V.17, type=c("BG"))

plot(serial.test(V.17))

#Stability analysis
1/roots(V.17)[[1]] # ">1"
1/roots(V.17)[[2]] # ">1"
plot(stability(V.17, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.17<-VAR(DASHXLM,p=4,type="both");V.17$varresult   
summary(V.17)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause DASH)
wald.test(b=coef(V.17$varresult[[1]]), Sigma=vcov(V.17$varresult[[1]]), Terms=c(2,4,6,8))
# Could be rejected at 5% (X2=0.76; p=0.94)

#Wald.test (H0: DASH does not Granger-cause XLM)
wald.test(b=coef(V.17$varresult[[2]]), Sigma=vcov(V.17$varresult[[2]]), Terms= c(1,3,5,7))
# Could NOT be rejected at 1%, 5% & 10% (X2=0.90; p=0.92)




####################
########## DASH & BTS
VARselect(DASHBTS,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.18<-VAR(DASHBTS,p=7,type="both")
serial.test(V.18, type=c("BG"))

plot(serial.test(V.18))

#Stability analysis
1/roots(V.18)[[1]] # ">1"
1/roots(V.18)[[2]] # ">1"
plot(stability(V.18, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.18<-VAR(DASHBTS,p=8,type="both");V.18$varresult   
summary(V.18)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause DASH)
wald.test(b=coef(V.18$varresult[[1]]), Sigma=vcov(V.18$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could be rejected at 1%, 5% & 10% (X2=103.7; p=0.000)

#Wald.test (H0: DASH does not Granger-cause BTS)
wald.test(b=coef(V.18$varresult[[2]]), Sigma=vcov(V.18$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.2; p=0.32)




####################
########## XMR & XLM
VARselect(XMRXLM,lag=20,type="both")     ## AIC=7

#VAR Model, lag=7
V.19<-VAR(XMRXLM,p=7,type="both")
serial.test(V.19, type=c("BG"))

plot(serial.test(V.19))

#Stability analysis
1/roots(V.19)[[1]] # ">1"
1/roots(V.19)[[2]] # ">1"
plot(stability(V.19, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.19<-VAR(XMRXLM,p=8,type="both");V.19$varresult   
summary(V.19)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause XMR)
wald.test(b=coef(V.19$varresult[[1]]), Sigma=vcov(V.19$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16))
# Could NOT be rejected at 1%, 5% & 10% (X2=4.2; p=0.84)

#Wald.test (H0: XMR does not Granger-cause XLM)
wald.test(b=coef(V.19$varresult[[2]]), Sigma=vcov(V.19$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15))
# Could NOT be rejected at 1%, 5% & 10% (X2=2.1; p=0.98)




####################
########## XMR & BTS
VARselect(XMRBTS,lag=20,type="both")     ## AIC=17

#VAR Model, lag=17
V.20<-VAR(XMRBTS,p=17,type="both")
serial.test(V.20, type=c("BG"))

plot(serial.test(V.20))

#Stability analysis
1/roots(V.20)[[1]] # ">1"
1/roots(V.20)[[2]] # ">1"
plot(stability(V.20, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.20<-VAR(XMRBTS,p=18,type="both");V.20$varresult   
summary(V.20)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XMR)
wald.test(b=coef(V.20$varresult[[1]]), Sigma=vcov(V.20$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34))
# Could be rejected at 1%, 5% & 10% (X2=48.0; p=0.000085)

#Wald.test (H0: XMR does not Granger-cause BTS)
wald.test(b=coef(V.20$varresult[[2]]), Sigma=vcov(V.20$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35))
# Could NOT be rejected at 1%, 5% & 10% (X2=25.7; p=0.11)




####################
########## XLM & BTS
VARselect(XLMBTS,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3
V.21<-VAR(XLMBTS,p=3,type="both")
serial.test(V.21, type=c("BG"))

plot(serial.test(V.21))

#Stability analysis
1/roots(V.21)[[1]] # ">1"
1/roots(V.21)[[2]] # ">1"
plot(stability(V.21, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
V.21<-VAR(XLMBTS,p=4,type="both");V.21$varresult    
summary(V.21)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XLM)
wald.test(b=coef(V.21$varresult[[1]]), Sigma=vcov(V.21$varresult[[1]]), Terms=c(2,4,6,8))
# Could be rejected at 1%, 5% & 10% (X2=20.6; p=0.00039)

#Wald.test (H0: XLM does not Granger-cause BTS)
wald.test(b=coef(V.21$varresult[[2]]), Sigma=vcov(V.21$varresult[[2]]), Terms= c(1,3,5,7))
# Could be rejected at 5% & 10% (X2=13.0; p=0.011)





################### Todda-Yamamoto causality (lead-lag lingages) 
################### test for volatility spillovers - Absolute vol. returns
## ################ Full sample volatility linkages - unconditional volatility
r.BTC   <- (abs(BTC1))
r.XRP   <- (abs(XRP1))
r.LTC   <- (abs(LTC1))
r.XLM   <- (abs(XLM1))
r.DASH  <- (abs(DASH1))
r.XMR   <- (abs(XMR1))
r.BTS   <- (abs(BTS1))

r.BTCXRP   <- cbind(r.BTC,r.XRP)
r.BTCLTC   <- cbind(r.BTC,r.LTC)
r.BTCDASH  <- cbind(r.BTC,r.DASH)
r.BTCXMR   <- cbind(r.BTC,r.XMR)
r.BTCXLM   <- cbind(r.BTC,r.XLM)
r.BTCBTS   <- cbind(r.BTC,r.BTS)

r.XRPLTC   <- cbind(r.XRP,r.LTC)
r.XRPDASH  <- cbind(r.XRP,r.DASH)
r.XRPXMR   <- cbind(r.XRP,r.XMR)
r.XRPXLM   <- cbind(r.XRP,r.XLM)
r.XRPBTS   <- cbind(r.XRP,r.BTS)

r.LTCDASH  <- cbind(r.LTC,r.DASH)
r.LTCXMR   <- cbind(r.LTC,r.XMR)
r.LTCXLM   <- cbind(r.LTC,r.XLM)
r.LTCBTS   <- cbind(r.LTC,r.BTS)

r.DASHXMR   <- cbind(r.DASH,r.XMR)
r.DASHXLM   <- cbind(r.DASH,r.XLM)
r.DASHBTS   <- cbind(r.DASH,r.BTS)

r.XMRXLM   <- cbind(r.XMR,r.XLM)
r.XMRBTS   <- cbind(r.XMR,r.BTS)

r.XLMBTS   <- cbind(r.XLM,r.BTS)


########## BTC & XRP
VARselect(r.BTCXRP,lag=20,type="both")     ## AIC=10

#VAR Model, lag=10
r.1<-VAR(r.BTCXRP,p=10,type="both")
serial.test(r.1, type=c("BG"))

plot(serial.test(r.1))

#Stability analysis
1/roots(r.1)[[1]] # ">1"
1/roots(r.1)[[2]] # ">1"
plot(stability(r.1, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.1<-VAR(r.BTCXRP,p=11,type="both");r.1$varresult    
summary(r.1)                                     

library(aod)
#Wald-test (H0: XRP does not Granger-cause BTC)
wald.test(b=coef(r.1$varresult[[1]]), Sigma=vcov(r.1$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22))
# Could be rejected at 5% & 10% (X2=19.1; p=0.059)

#Wald.test (H0: BTC does not Granger-cause XRP)
wald.test(b=coef(r.1$varresult[[2]]), Sigma=vcov(r.1$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.6; p=0.57)




####################
########## BTC & LTC
VARselect(r.BTCLTC,lag=20,type="both")     ## AIC=15

#VAR Model, lag=15
r.2<-VAR(r.BTCLTC,p=15,type="both")
serial.test(r.2, type=c("BG"))

plot(serial.test(r.2))

#Stability analysis
1/roots(r.2)[[1]] # ">1"
1/roots(r.2)[[2]] # ">1"
plot(stability(r.2, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.2<-VAR(r.BTCLTC,p=16,type="both");r.2$varresult   
summary(r.2)                                     

library(aod)
#Wald-test (H0: LTC does not Granger-cause BTC)
wald.test(b=coef(r.2$varresult[[1]]), Sigma=vcov(r.2$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))
# Could be rejected at 10% (X2=24.8; p=0.073)

#Wald.test (H0: BTC does not Granger-cause LTC)
wald.test(b=coef(r.2$varresult[[2]]), Sigma=vcov(r.2$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31))
# Could be rejected at 5%, 10% (X2=26.6; p=0.046)




####################
########## BTC & DASH
VARselect(r.BTCDASH,lag=20,type="both")     ## AIC=10

#VAR Model, lag=10
r.3<-VAR(r.BTCDASH,p=10,type="both")
serial.test(r.3, type=c("BG"))

plot(serial.test(r.3))

#Stability analysis
1/roots(r.3)[[1]] # ">1"
1/roots(r.3)[[2]] # ">1"
plot(stability(r.3, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.3<-VAR(r.BTCDASH,p=11,type="both");r.3$varresult   
summary(r.3)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause BTC)
wald.test(b=coef(r.3$varresult[[1]]), Sigma=vcov(r.3$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.8; p=0.55)

#Wald.test (H0: BTC does not Granger-cause DASH)
wald.test(b=coef(r.3$varresult[[2]]), Sigma=vcov(r.3$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21))
# Could NOT be rejected at 1%, 5% & 10% (X2=12.3; p=0.34)




####################
########## BTC & XMR
VARselect(r.BTCXMR,lag=20,type="both")     ## AIC=6,7,8,9

#VAR Model, lag=6,7,8,9
r.4<-VAR(r.BTCXMR,p=6,type="both")
serial.test(r.4, type=c("BG"))

plot(serial.test(r.4))

#Stability analysis
1/roots(r.4)[[1]] # ">1"
1/roots(r.4)[[2]] # ">1"
plot(stability(r.4, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.4<-VAR(r.BTCXMR,p=10,type="both");r.4$varresult    
summary(r.4)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause BTC)
wald.test(b=coef(r.4$varresult[[1]]), Sigma=vcov(r.4$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.5; p=0.49)

#Wald.test (H0: BTC does not Granger-cause XMR)
wald.test(b=coef(r.4$varresult[[2]]), Sigma=vcov(r.4$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19))
# Could NOT be rejected at 1%, 5% & 10% (X2=7.3; p=0.70)




####################
########## BTC & XLM
VARselect(r.BTCXLM,lag=20,type="both")     ## AIC=18,10,19

#VAR Model, lag=18,10,19
r.5<-VAR(r.BTCXLM,p=19,type="both")
serial.test(r.5, type=c("BG"))

plot(serial.test(r.5))

#Stability analysis
1/roots(r.5)[[1]] # ">1"
1/roots(r.5)[[2]] # ">1"
plot(stability(r.5, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.5<-VAR(r.BTCXLM,p=20,type="both");r.5$varresult    
summary(r.5)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause BTC)
wald.test(b=coef(r.5$varresult[[1]]), Sigma=vcov(r.5$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40))
# Could be rejected at 5% & 10% (X2=35.7; p=0.017)

#Wald.test (H0: BTC does not Granger-cause XLM)
wald.test(b=coef(r.5$varresult[[2]]), Sigma=vcov(r.5$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39))
# Could be rejected at 10% (X2=31.2; p=0.053)




####################
########## BTC & BTS
VARselect(r.BTCBTS,lag=20,type="both")     ## AIC=6,7,8,10,9,5,12,11

#VAR Model, lag=11
r.6<-VAR(r.BTCBTS,p=11,type="both")
serial.test(r.6, type=c("BG"))

plot(serial.test(r.6))

#Stability analysis
1/roots(r.6)[[1]] # ">1"
1/roots(r.6)[[2]] # ">1"
plot(stability(r.6, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.6<-VAR(r.BTCBTS,p=12,type="both");r.6$varresult   
summary(r.6)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause BTC)
wald.test(b=coef(r.6$varresult[[1]]), Sigma=vcov(r.6$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24))
# Could NOT be rejected at 1% 5% & 10% (X2=8.8; p=0.72)

#Wald.test (H0: BTC does not Granger-cause BTS)
wald.test(b=coef(r.6$varresult[[2]]), Sigma=vcov(r.6$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23))
# Could be rejected at 5% & 10% (X2=21.6; p=0.043)





####################
########## XRP & LTC
VARselect(r.XRPLTC,lag=20,type="both")     ## AIC=15

#VAR Model, lag=15
r.7<-VAR(r.XRPLTC,p=15,type="both")
serial.test(r.7, type=c("BG"))

plot(serial.test(r.7))

#Stability analysis
1/roots(r.7)[[1]] # ">1"
1/roots(r.7)[[2]] # ">1"
plot(stability(r.7, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.7<-VAR(r.XRPLTC,p=16,type="both");r.7$varresult   
summary(r.7)                                   

library(aod)
#Wald-test (H0: LTC does not Granger-cause XRP)
wald.test(b=coef(r.7$varresult[[1]]), Sigma=vcov(r.7$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))
# Could be rejected at 5%, 10% (X2=27.6; p=0.035)

#Wald.test (H0: XRP does not Granger-cause LTC)
wald.test(b=coef(r.7$varresult[[2]]), Sigma=vcov(r.7$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31))
# Could be rejected at 1%, 5% & 10% (X2=106.5; p=0.000000)




####################
########## XRP & DASH
VARselect(r.XRPDASH,lag=20,type="both")     ## AIC=10

#VAR Model, lag=10
r.8<-VAR(r.XRPDASH,p=10,type="both")
serial.test(r.8, type=c("BG"))

plot(serial.test(r.8))

#Stability analysis
1/roots(r.8)[[1]] # ">1"
1/roots(r.8)[[2]] # ">1"
plot(stability(r.8, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.8<-VAR(r.XRPDASH,p=11,type="both");r.8$varresult   
summary(r.8)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause XRP)
wald.test(b=coef(r.8$varresult[[1]]), Sigma=vcov(r.8$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22))
# Could NOT be rejected at 1%, 5% & 10% (X2=17.1; p=0.11)

#Wald.test (H0: XRP does not Granger-cause DASH)
wald.test(b=coef(r.8$varresult[[2]]), Sigma=vcov(r.8$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21))
# Could NOT be rejected at 1%, 5% & 10% (X2=13.9; p=0.24)




####################
########## XRP & XMR
VARselect(r.XRPXMR,lag=20,type="both")     ## AIC=9

#VAR Model, lag=9
r.9<-VAR(r.XRPXMR,p=9,type="both")
serial.test(r.9, type=c("BG"))

plot(serial.test(r.9))

#Stability analysis
1/roots(r.9)[[1]] # ">1"
1/roots(r.9)[[2]] # ">1"
plot(stability(r.9, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.9<-VAR(r.XRPXMR,p=10,type="both");r.9$varresult   
summary(r.9)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause XRP)
wald.test(b=coef(r.9$varresult[[1]]), Sigma=vcov(r.9$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20))
# Could NOT be rejected at 1%, 5% & 10% (X2=8.4; p=0.59)

#Wald.test (H0: XRP does not Granger-cause XMR)
wald.test(b=coef(r.9$varresult[[2]]), Sigma=vcov(r.9$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19))
# Could NOT be rejected at 5% & 10% (X2=18.8; p=0.043)



####################
########## XRP & XLM
VARselect(r.XRPXLM,lag=20,type="both")     ## AIC=10

#VAR Model, lag=10
r.10<-VAR(r.XRPXLM,p=10,type="both")
serial.test(r.10, type=c("BG"))

plot(serial.test(r.10))

#Stability analysis
1/roots(r.10)[[1]] # ">1"
1/roots(r.10)[[2]] # ">1"
plot(stability(r.10, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.10<-VAR(r.XRPXLM,p=11,type="both");r.10$varresult   
summary(r.10)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause XRP)
wald.test(b=coef(r.10$varresult[[1]]), Sigma=vcov(r.10$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22))
# Could be rejected at 5% & 10% (X2=21.3; p=0.03)

#Wald.test (H0: XRP does not Granger-cause XLM)
wald.test(b=coef(r.10$varresult[[2]]), Sigma=vcov(r.10$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21))
# Could be rejected at 1%, 5% & 10% (X2=32.3; p=0.00069)




####################
########## XRP & BTS
VARselect(r.XRPBTS,lag=20,type="both")     ## AIC=13

#VAR Model, lag=13
r.11<-VAR(r.XRPBTS,p=13,type="both")
serial.test(r.11, type=c("BG"))

plot(serial.test(r.11))

#Stability analysis
1/roots(r.11)[[1]] # ">1"
1/roots(r.11)[[2]] # ">1"
plot(stability(r.11, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.11<-VAR(r.XRPBTS,p=14,type="both");r.11$varresult    
summary(r.11)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XRP)
wald.test(b=coef(r.11$varresult[[1]]), Sigma=vcov(r.11$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28))
# Could be rejected at 5%, 10% (X2=25.1; p=0.034)

#Wald.test (H0: XRP does not Granger-cause BTS)
wald.test(b=coef(r.11$varresult[[2]]), Sigma=vcov(r.11$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
# Could be rejected at 1%, 5%, 10% (X2=41.5; p=0.00015)




####################
########## LTC & DASH
VARselect(r.LTCDASH,lag=20,type="both")     ## AIC=10,11

#VAR Model, lag=11
r.12<-VAR(r.LTCDASH,p=11,type="both")
serial.test(r.12, type=c("BG"))

plot(serial.test(r.12))

#Stability analysis
1/roots(r.12)[[1]] # ">1"
1/roots(r.12)[[2]] # ">1"
plot(stability(r.12, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.12<-VAR(r.LTCDASH,p=12,type="both");r.12$varresult   
summary(r.12)                                   

library(aod)
#Wald-test (H0: DASH does not Granger-cause LTC)
wald.test(b=coef(r.12$varresult[[1]]), Sigma=vcov(r.12$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24))
# Could NOT be rejected at 1%, 5% & 10% (X2=17.1; p=0.15)

#Wald.test (H0: LTC does not Granger-cause DASH)
wald.test(b=coef(r.12$varresult[[2]]), Sigma=vcov(r.12$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23))
# Could NOT be rejected at 1%, 5% & 10% (X2=15.3; p=0.22)




####################
########## LTC & XMR
VARselect(r.LTCXMR,lag=20,type="both")     ## AIC=10

#VAR Model, lag=10
r.13<-VAR(r.LTCXMR,p=10,type="both")
serial.test(r.13, type=c("BG"))

plot(serial.test(r.13))

#Stability analysis
1/roots(r.13)[[1]] # ">1"
1/roots(r.13)[[2]] # ">1"
plot(stability(r.13, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.13<-VAR(r.LTCXMR,p=11,type="both");r.13$varresult  
summary(r.13)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause LTC)
wald.test(b=coef(r.13$varresult[[1]]), Sigma=vcov(r.13$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22))
# Could NOT be rejected at 1%, 5% & 10% (X2=7.4; p=0.77)

#Wald.test (H0: LTC does not Granger-cause XMR)
wald.test(b=coef(r.13$varresult[[2]]), Sigma=vcov(r.13$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21))
# Could NOT be rejected at 1%, 5% & 10% (X2=10.4; p=0.5)




####################
########## LTC & XLM
VARselect(r.LTCXLM,lag=20,type="both")     ## AIC=6,11

#VAR Model, lag=11
r.14<-VAR(r.LTCXLM,p=11,type="both")
serial.test(r.14, type=c("BG"))

plot(serial.test(r.14))

#Stability analysis
1/roots(r.14)[[1]] # ">1"
1/roots(r.14)[[2]] # ">1"
plot(stability(r.14, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.14<-VAR(r.LTCXLM,p=12,type="both");r.14$varresult  
summary(r.14)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause LTC)
wald.test(b=coef(r.14$varresult[[1]]), Sigma=vcov(r.14$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24))
# Could be rejected at 1%, 5% & 10% (X2=27.4; p=0.0068)

#Wald.test (H0: LTC does not Granger-cause XLM)
wald.test(b=coef(r.14$varresult[[2]]), Sigma=vcov(r.14$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23))
# Could be rejected at 5% & 10% (X2=22.1; p=0.036)




####################
########## LTC & BTS
VARselect(r.LTCBTS,lag=20,type="both")     ## AIC=8,7,6,11

#VAR Model, lag=11
r.15<-VAR(r.LTCBTS,p=11,type="both")
serial.test(r.15, type=c("BG"))

plot(serial.test(r.15))

#Stability analysis
1/roots(r.15)[[1]] # ">1"
1/roots(r.15)[[2]] # ">1"
plot(stability(r.15, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.15<-VAR(r.LTCBTS,p=12,type="both");r.15$varresult   
summary(r.15)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause LTC)
wald.test(b=coef(r.15$varresult[[1]]), Sigma=vcov(r.15$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24))
# Could be rejected at 1%, 5% & 10% (X2=45.6; p=0.000008)

#Wald.test (H0: LTC does not Granger-cause BTS)
wald.test(b=coef(r.15$varresult[[2]]), Sigma=vcov(r.15$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23))
# Could be rejected at 1%, 5% & 10% (X2=27.8; p=0.0059)




####################
########## DASH & XMR
VARselect(r.DASHXMR,lag=20,type="both")     ## AIC=6,10,8,7,13

#VAR Model, lag=13
r.16<-VAR(r.DASHXMR,p=13,type="both")
serial.test(r.16, type=c("BG"))

plot(serial.test(r.16))

#Stability analysis
1/roots(r.16)[[1]] # ">1"
1/roots(r.16)[[2]] # ">1"
plot(stability(r.16, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.16<-VAR(r.DASHXMR,p=14,type="both");r.16$varresult  
summary(r.16)                                   

library(aod)
#Wald-test (H0: XMR does not Granger-cause DASH)
wald.test(b=coef(r.16$varresult[[1]]), Sigma=vcov(r.16$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.5; p=0.8)

#Wald.test (H0: DASH does not Granger-cause XMR)
wald.test(b=coef(r.16$varresult[[2]]), Sigma=vcov(r.16$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
# Could NOT be rejected at 1%, 5% & 10% (X2=18.5; p=0.19)




####################
########## DASH & XLM
VARselect(r.DASHXLM,lag=20,type="both")     ## AIC=3

#VAR Model, lag=3
r.17<-VAR(r.DASHXLM,p=3,type="both")
serial.test(r.17, type=c("BG"))

plot(serial.test(r.17))

#Stability analysis
1/roots(r.17)[[1]] # ">1"
1/roots(r.17)[[2]] # ">1"
plot(stability(V.17, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.17<-VAR(r.DASHXLM,p=4,type="both");r.17$varresult   
summary(r.17)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause DASH)
wald.test(b=coef(r.17$varresult[[1]]), Sigma=vcov(r.17$varresult[[1]]), Terms=c(2,4,6,8))
# Could be rejected at 10% (X2=8.1; p=0.087)

#Wald.test (H0: DASH does not Granger-cause XLM)
wald.test(b=coef(r.17$varresult[[2]]), Sigma=vcov(r.17$varresult[[2]]), Terms= c(1,3,5,7))
# Could NOT be rejected at 1%, 5% & 10% (X2=1.1; p=0.89)




####################
########## DASH & BTS
VARselect(r.DASHBTS,lag=20,type="both")     ## AIC=6,4,7,8

#VAR Model, lag=8
r.18<-VAR(r.DASHBTS,p=8,type="both")
serial.test(r.18, type=c("BG"))

plot(serial.test(r.18))

#Stability analysis
1/roots(r.18)[[1]] # ">1"
1/roots(r.18)[[2]] # ">1"
plot(stability(r.18, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.18<-VAR(DASHBTS,p=9,type="both");r.18$varresult   
summary(r.18)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause DASH)
wald.test(b=coef(r.18$varresult[[1]]), Sigma=vcov(r.18$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18))
# Could be rejected at 1%, 5% & 10% (X2=106.7; p=0.000)

#Wald.test (H0: DASH does not Granger-cause BTS)
wald.test(b=coef(r.18$varresult[[2]]), Sigma=vcov(r.18$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17))
# Could NOT be rejected at 1%, 5% & 10% (X2=9.3; p=0.41)




####################
########## XMR & XLM
VARselect(r.XMRXLM,lag=20,type="both")     ## AIC=6

#VAR Model, lag=6
r.19<-VAR(r.XMRXLM,p=6,type="both")
serial.test(r.19, type=c("BG"))

plot(serial.test(r.19))

#Stability analysis
1/roots(r.19)[[1]] # ">1"
1/roots(r.19)[[2]] # ">1"
plot(stability(r.19, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.19<-VAR(r.XMRXLM,p=7,type="both");r.19$varresult   
summary(r.19)                                   

library(aod)
#Wald-test (H0: XLM does not Granger-cause XMR)
wald.test(b=coef(r.19$varresult[[1]]), Sigma=vcov(r.19$varresult[[1]]), Terms=c(2,4,6,8,10,12,14))
# Could NOT be rejected at 1%, 5% & 10% (X2=8.9; p=0.26)

#Wald.test (H0: XMR does not Granger-cause XLM)
wald.test(b=coef(r.19$varresult[[2]]), Sigma=vcov(r.19$varresult[[2]]), Terms= c(1,3,5,7,9,11,13))
# Could NOT be rejected at 1%, 5% & 10% (X2=3.4; p=0.85)




####################
########## XMR & BTS
VARselect(r.XMRBTS,lag=20,type="both")     ## AIC=13

#VAR Model, lag=13
r.20<-VAR(r.XMRBTS,p=13,type="both")
serial.test(r.20, type=c("BG"))

plot(serial.test(r.20))

#Stability analysis
1/roots(r.20)[[1]] # ">1"
1/roots(r.20)[[2]] # ">1"
plot(stability(r.20, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.20<-VAR(r.XMRBTS,p=14,type="both");r.20$varresult   
summary(r.20)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XMR)
wald.test(b=coef(r.20$varresult[[1]]), Sigma=vcov(r.20$varresult[[1]]), Terms=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28))
# Could NOT be rejected at 1%, 5% & 10% (X2=20.1; p=0.13)

#Wald.test (H0: XMR does not Granger-cause BTS)
wald.test(b=coef(r.20$varresult[[2]]), Sigma=vcov(r.20$varresult[[2]]), Terms= c(1,3,5,7,9,11,13,15,17,19,21,23,25,27))
# Could NOT be rejected at 1%, 5% & 10% (X2=19.8; p=0.14)




####################
########## XLM & BTS
VARselect(r.XLMBTS,lag=20,type="both")     ## AIC=18,5

#VAR Model, lag=5
r.21<-VAR(r.XLMBTS,p=5,type="both")
serial.test(r.21, type=c("BG"))

plot(serial.test(r.21))

#Stability analysis
1/roots(r.21)[[1]] # ">1"
1/roots(r.21)[[2]] # ">1"
plot(stability(r.21, type = "OLS-CUSUM"))

#VAR-Model: Add the maximum order of integration to the number of lags. This is the augmented VAR-model, VAR(p+m).  
r.21<-VAR(r.XLMBTS,p=6,type="both");r.21$varresult    
summary(r.21)                                   

library(aod)
#Wald-test (H0: BTS does not Granger-cause XLM)
wald.test(b=coef(r.21$varresult[[1]]), Sigma=vcov(r.21$varresult[[1]]), Terms=c(2,4,6,8,10,12))
# Could be rejected at 5% & 10% (X2=16.6; p=0.011)

#Wald.test (H0: XLM does not Granger-cause BTS)
wald.test(b=coef(r.21$varresult[[2]]), Sigma=vcov(r.21$varresult[[2]]), Terms= c(1,3,5,7,9,11))
# Could be rejected at 5% & 10% (X2=13.0; p=0.1)




##################################################################################################
##################################################################################################
############# wavelet time sclae decomposition
############# scale 1(intraweek|:short-run); 
############# sacle 8(month:medium-run);
############# scale 64(biannual:long-run) 


####### MODWT wavelet time sacle decomposition for unconditional
####### volatiloity returns - absolute returns LA8 wavelet filte
####### returns
CRYPT
BTC1   <- (diff(log(CRYPT$BTC))*100)
XRP1   <- (diff(log(CRYPT$XRP))*100)
LTC1   <- (diff(log(CRYPT$LTC))*100)
XLM1   <- (diff(log(CRYPT$XLM))*100)
DASH1  <- (diff(log(CRYPT$DASH))*100)
XMR1   <- (diff(log(CRYPT$XMR))*100)
BTS1   <- (diff(log(CRYPT$BTS))*100)


####### unconditional returns - absolute returns
a.BTC11   <- (abs(BTC1))
a.XRP11   <- (abs(XRP1))
a.LTC11   <- (abs(LTC1))
a.XLM11   <- (abs(XLM1))
a.DASH11  <- (abs(DASH1))
a.XMR11   <- (abs(XMR1))
a.BTS11   <- (abs(BTS1))


###### Bitcoin
######
la8.modwt   <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
bit.modwt1  <- modwt.forward(a.BTC11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
bit.modwt2  <- modwt.forward(a.BTC11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
bit.modwt3  <- modwt.forward(a.BTC11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(bit.modwt1$W, "C:/Users/user/Desktop/bit1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(bit.modwt2$W, "C:/Users/user/Desktop/bit2.xlsx")
write.xlsx(bit.modwt3$W, "C:/Users/user/Desktop/bit3.xlsx")


newX.modwt1 <- modwt.backward(bit.modwt1$W, bit.modwt1$V, la8.modwt, 1)   ## compute the original abs. vol. series with the level 1 coefficients
newX.modwt2 <- modwt.backward(bit.modwt2$W, bit.modwt1$V, la8.modwt, 8)   ## compute the original abs. vol. series with the level 8 coefficients
newX.modwt3 <- modwt.backward(bit.modwt3$W, bit.modwt1$V, la8.modwt, 64)  ## compute the original abs. vol. series with the level  coefficients





###### Ripple
######
la8.modwt   <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xrp.modwt1  <- modwt.forward(a.XRP11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xrp.modwt2  <- modwt.forward(a.XRP11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xrp.modwt3  <- modwt.forward(a.XRP11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xrp.modwt1$W, "C:/Users/user/Desktop/xrp1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xrp.modwt2$W, "C:/Users/user/Desktop/xrp2.xlsx")
write.xlsx(xrp.modwt3$W, "C:/Users/user/Desktop/xrp3.xlsx")



###### Litecoin
######
la8.modwt   <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
ltc.modwt1  <- modwt.forward(a.LTC11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
ltc.modwt2  <- modwt.forward(a.LTC11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
ltc.modwt3  <- modwt.forward(a.LTC11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(ltc.modwt1$W, "C:/Users/user/Desktop/ltc1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(ltc.modwt2$W, "C:/Users/user/Desktop/ltc2.xlsx")
write.xlsx(ltc.modwt3$W, "C:/Users/user/Desktop/ltc3.xlsx")



###### Stellar
######
la8.modwt   <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xlm.modwt1  <- modwt.forward(a.XLM11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xlm.modwt2  <- modwt.forward(a.XLM11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xlm.modwt3  <- modwt.forward(a.XLM11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xlm.modwt1$W, "C:/Users/user/Desktop/xlm1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xlm.modwt2$W, "C:/Users/user/Desktop/xlm2.xlsx")
write.xlsx(xlm.modwt3$W, "C:/Users/user/Desktop/xlm3.xlsx")



###### DASH
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
dash.modwt1  <- modwt.forward(a.DASH11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
dash.modwt2  <- modwt.forward(a.DASH11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
dash.modwt3  <- modwt.forward(a.DASH11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(dash.modwt1$W, "C:/Users/user/Desktop/dash1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(dash.modwt2$W, "C:/Users/user/Desktop/dash2.xlsx")
write.xlsx(dash.modwt3$W, "C:/Users/user/Desktop/dash3.xlsx")

  

###### Monero
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xmr.modwt1  <- modwt.forward(a.XMR11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xmr.modwt2  <- modwt.forward(a.XMR11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xmr.modwt3  <- modwt.forward(a.XMR11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xmr.modwt1$W, "C:/Users/user/Desktop/xmr1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xmr.modwt2$W, "C:/Users/user/Desktop/xmr2.xlsx")
write.xlsx(xmr.modwt3$W, "C:/Users/user/Desktop/xmr3.xlsx")
 
   

###### BitShares
######
la8.modwt   <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
bts.modwt1  <- modwt.forward(a.BTS11 , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
bts.modwt2  <- modwt.forward(a.BTS11 , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
bts.modwt3  <- modwt.forward(a.BTS11 , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(bts.modwt1$W, "C:/Users/user/Desktop/bts1.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(bts.modwt2$W, "C:/Users/user/Desktop/bts2.xlsx")
write.xlsx(bts.modwt3$W, "C:/Users/user/Desktop/btsxmr3.xlsx")
 
   



###########################################################################################################
####### MODWT wavelet time sacle decomposition for conditional
####### volatility - from mean Garch (1,1)LA8 wavelet filte

CRYPvol
v.BTC  <- abs(CRYPvol$BTC)  ## No transformation has been done. The vol. returns
v.XRP  <- abs(CRYPvol$XRP)  ## are already non-negatives. We just wanted to put the 
v.LTC  <- abs(CRYPvol$LTC)  ## in a vector form for the wavelet decomposition
v.XLM  <- abs(CRYPvol$XLM)
v.DASH <- abs(CRYPvol$DASH) 
v.XMR  <- abs(CRYPvol$XMR)
v.BTS  <- abs(CRYPvol$BTS)


###### Bitcoin
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
bit.modwt11  <- modwt.forward(v.BTC , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
bit.modwt22  <- modwt.forward(v.BTC , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
bit.modwt33  <- modwt.forward(v.BTC , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(bit.modwt11$W, "C:/Users/user/Desktop/bit11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(bit.modwt22$W, "C:/Users/user/Desktop/bit22.xlsx")
write.xlsx(bit.modwt33$W, "C:/Users/user/Desktop/bit33.xlsx")



###### Ripple
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xrp.modwt11  <- modwt.forward(v.XRP , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xrp.modwt22  <- modwt.forward(v.XRP , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xrp.modwt33  <- modwt.forward(v.XRP , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xrp.modwt11$W, "C:/Users/user/Desktop/xrp11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xrp.modwt22$W, "C:/Users/user/Desktop/xrp22.xlsx")
write.xlsx(xrp.modwt33$W, "C:/Users/user/Desktop/xrp33.xlsx")



###### Litecoin
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
ltc.modwt11  <- modwt.forward(v.LTC , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
ltc.modwt22  <- modwt.forward(v.LTC , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
ltc.modwt33  <- modwt.forward(v.LTC , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(ltc.modwt11$W, "C:/Users/user/Desktop/ltc11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(ltc.modwt22$W, "C:/Users/user/Desktop/ltc22.xlsx")
write.xlsx(ltc.modwt33$W, "C:/Users/user/Desktop/ltc33.xlsx")



###### Stella
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xlm.modwt11  <- modwt.forward(v.XLM , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xlm.modwt22  <- modwt.forward(v.XLM , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xlm.modwt33  <- modwt.forward(v.XLM , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xlm.modwt11$W, "C:/Users/user/Desktop/xlm11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xlm.modwt22$W, "C:/Users/user/Desktop/xlm22.xlsx")
write.xlsx(xlm.modwt33$W, "C:/Users/user/Desktop/xlm33.xlsx")



###### DASH
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
dash.modwt11  <- modwt.forward(v.DASH , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
dash.modwt22  <- modwt.forward(v.DASH , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
dash.modwt33  <- modwt.forward(v.DASH , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(dash.modwt11$W, "C:/Users/user/Desktop/dash11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(dash.modwt22$W, "C:/Users/user/Desktop/dash22.xlsx")
write.xlsx(dash.modwt33$W, "C:/Users/user/Desktop/dash33.xlsx")



###### Monero
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
xmr.modwt11  <- modwt.forward(v.XMR , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
xmr.modwt22  <- modwt.forward(v.XMR , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
xmr.modwt33  <- modwt.forward(v.XMR , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(xmr.modwt11$W, "C:/Users/user/Desktop/xmr11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(xmr.modwt22$W, "C:/Users/user/Desktop/xmr22.xlsx")
write.xlsx(xmr.modwt33$W, "C:/Users/user/Desktop/xmr33.xlsx")



###### BitShares
######
la8.modwt    <- wt.filter(modwt=TRUE)                    ## LA8 wavelet filter for MODWT
bts.modwt11  <- modwt.forward(v.BTS , la8.modwt, 1)    ## MODWT level 1 wavelet and scaling coefficients
bts.modwt22  <- modwt.forward(v.BTS , la8.modwt, 8)    ## MODWT level 8 wavelet and scaling coefficients
bts.modwt33  <- modwt.forward(v.BTS , la8.modwt, 64)   ## MODWT level 64 wavelet and scaling coefficients

write.xlsx(bts.modwt11$W, "C:/Users/user/Desktop/bts11.xlsx") ## Exporting the wavelet coefficients to excel
write.xlsx(bts.modwt22$W, "C:/Users/user/Desktop/bts22.xlsx")
write.xlsx(bts.modwt33$W, "C:/Users/user/Desktop/bts33.xlsx")




























