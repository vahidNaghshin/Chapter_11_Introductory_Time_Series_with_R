stock <- "/Users/vnagh/Desktop/untitled_folder_4/stockmarket.dat"
stock_market <- read.table(stock, header=T)
print(stock_market[1:3,])
stock_lon <- stock_market[, 3]
stock_NY <- stock_market[, 7]

plot(stock_NY)

library(tseries)
adf.test(stock_lon)
adf.test(stock_NY)

stock_LON_NY.ar <- ar(cbind(stock_lon, stock_NY), method="ols", order.max = 2, dmean=T, intercept=T)
stock_LON_NY.ar$ar[, , ]
layout(1:2)
acf(stock_LON_NY.ar$res[-c(1:12), 1])
acf(stock_LON_NY.ar$res[-c(1:12), 2])
# London Stock market influence the NY the most.

po.test(cbind(stock_lon, stock_NY))
# So the null hypothesis, i.e., non-cointegrated, is rejeced.

LON_vs_NY.lm <- lm(stock_lon ~ stock_NY)
print(LON_vs_NY.lm)


LON_vs_NY.lm.res <- resid(LON_vs_NY.lm)
adf.test(LON_vs_NY.lm.res)
# since in part (d), it is concluded that two series are cointegrated,
# the Dickey-Fuller test demonstrate that the residual noise is stationary

library(vars)
data(Canada)
print(Canada)
var.can <- VAR(Canada, p = 2, type = "trend")
coef(var.can)

Can.pred <- predict(var.can, n.ahead = 4)
Can.pred

e.pred <- ts(Can.pred$fcst$e[, 1], st = 2001, fr = 4)
ts.plot(cbind(window(Canada[,1], start = 1980), e.pred), ylab='e', lty = 1:2)

prod.pred <- ts(Can.pred$fcst$prod[, 1], st = 2001, fr = 4)
ts.plot(cbind(window(Canada[,2], start = 1980), prod.pred), ylab='prod', lty = 1:2)

rw.pred <- ts(Can.pred$fcst$rw[, 1], st = 2001, fr = 4)
ts.plot(cbind(window(Canada[,3], start = 1980), rw.pred), ylab='rw', lty = 1:2)

U.pred <- ts(Can.pred$fcst$U[, 1], st = 2001, fr = 4)
ts.plot(cbind(window(Canada[,4], start = 1980), U.pred), ylab='U', lty = 1:2)


cbe <- "/Users/vnagh/Desktop/untitled_folder_4/cbe.dat"
cbe <- read.table(cbe, header = T)
print(cbe[1:3,])
elec.log.ts <- ts(log(cbe[,3]), start=1958, freq=12)
elec.log.arima <- arima(elec.log.ts, order=c(1, 1, 0), seas = list(order = c(1,1,1), 12))
acf(resid(elec.log.arima))

choc.log.ts <- ts(log(cbe[,1]), start=1958, freq=12)
choc.log.arima <- arima(choc.log.ts, order=c(1, 1, 0), seas = list(order = c(1,1,1), 12))
acf(resid(choc.log.arima))
ccf(resid(elec.log.arima), resid(choc.log.arima), main = "")

elec.pred <- predict(elec.log.arima, 1)
choc.pred <- predict(choc.log.arima, 1)

library(mvtnorm)
cov.mat <- matrix(c(1, 0.3, 0.3, 1), nr = 2)
w <- rmvnorm(10, sigma = cov.mat)

for (i in 1:10){
  elec.log.fcst <- elec.pred$pred+w[i,1]
  choc.log.fcst <- choc.pred$pred+w[i,2]
  print(cbind(elec.log.fcst, choc.log.fcst))
}

