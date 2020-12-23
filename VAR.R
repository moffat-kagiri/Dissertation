library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

read_csv (file.choose())-> rawdata
head(rawdata)

NSE20 <- ts(rawdata$NSE20, start = c(2009,1,1), frequency = 4)
NASI <- ts(rawdata$NASI, start = c(2009,1,1), frequency = 4)
MC <- ts(rawdata$MC, start = c(2009,1,1), frequency = 4)
ET <- ts(rawdata$ET, start = c(2009,1,1), frequency = 4)
CBT <- ts(rawdata$CBT, start = c(2009,1,1), frequency = 4)
GBT <- ts(rawdata$GBT, start = c(2009,1,1), frequency = 4)
BT <- ts(rawdata$BT, start = c(2009,1,1), frequency = 4)
GDP <- ts(rawdata$GDP, start = c(2009,1,1), frequency = 4)

ts_plot(NSE20)
ts_plot(NASI)
ts_plot(MC)
ts_plot(ET)
ts_plot(CBT)
ts_plot(GBT)
ts_plot(BT)
ts_plot(GDP)

NSE20<-na.omit(NSE20)
NASI<-na.omit(NASI)
MC<-na.omit(MC)
ET<-na.omit(ET)
CBT<-na.omit(CBT)
GBT<-na.omit(GBT)
BT<-na.omit(BT)

pp.test(NSE20, lshort = TRUE)
pp.test(NASI, lshort = TRUE)
pp.test(MC, lshort = TRUE)
pp.test(ET, lshort = TRUE)
pp.test(CBT, lshort = TRUE)
pp.test(GBT, lshort = TRUE)
pp.test(BT, lshort = TRUE)

v1 <- cbind(NSE20, NASI, MC, ET, CBT, GBT, BT, GDP)
colnames(v1) <- cbind("NSE 20","NASI","Market Capitalization in Bn KSh", 
"Total Equity Turnover in Bn Ksh", "Corporate Bonds Turnover",
 "Government Bonds Turnover", "Total Bonds Turnover Bn KSh", "GDP in Bn. Ksh")

v1<-na.omit(v1)

lagselect <- VARselect(v1, lag.max = 4, type = "const")
lagselect$selection

Model <- VAR(v1, p = 2, type = "const", season = NULL, exog = NULL) 
summary(Model)

Serial <- serial.test(Model, lags.pt = 5, type = "PT.asymptotic")
Serial

Arch <- arch.test(Model, lags.multi = 15, multivariate.only = TRUE)
Arch

Norm <- normality.test(Model, multivariate.only = TRUE)
Norm

Stability <- stability(Model, type = "OLS-CUSUM")
plot(Stability)

forecast <- predict(Model, n.ahead = 4, ci = 0.95)
par(mfrow=c(2,3))
fanchart(forecast, names = "NSE20", main = "Fanchart for NSE 20", xlab = "Horizon", ylab = "NSE20 (points)")
fanchart(forecast, names = "NASI", main = "Fanchart for NASI", xlab = "Horizon", ylab = "NASI (points)")
fanchart(forecast, names = "MC", main = "Fanchart for Market Capitalization", xlab = "Horizon", ylab = "Market Capitalization(Billion Ksh)")
fanchart(forecast, names = "ET", main = "Fanchart for Total Equity Turnover", xlab = "Horizon", ylab = "ET")
fanchart(forecast, names = "CBT", main = "Fanchart for Corporate Bonds Turnover", xlab = "Horizon", ylab = "Corporate Bonds Turnover (Billion Ksh)")
fanchart(forecast, names = "GBT", main = "Fanchart for Government Bonds Turnover", xlab = "Horizon", ylab = "Government Bonds Turnover (Billion Ksh)")
fanchart(forecast, names = "BT", main = "Fanchart for Total Bonds Turnover", xlab = "Horizon", ylab = "BT")
fanchart(forecast, names = "GDP", main = "Fanchart for Gross Domestic Product", xlab = "Horizon", ylab = "GDP")


forecast
