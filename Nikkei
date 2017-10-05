load('~/Google Drive/University/Dissertation/R/Diessertation_Data.Rdata')

library(fGarch)
library(zoo)
library(xts)

nikkei.m1 = garchFit(~1+garch(1,1), data = nikkei.r, trace = F)
summary(nikkei.m1)

nikkei.v1 = volatility(nikkei.m1)
nikkei.resi = residuals(nikkei.m1, standardize = T)
nikkei.res = ts(nikkei.resi, frequency = 365, start = c(1996,2016))
plot(nikkei.res, xlab = 'Year', ylab = 'St. Residuals', type= 'l', col = 'blue')

par(mfcol = c(2,2))
acf(nikkei.resi, lag = 24)
pacf(nikkei.resi, lag = 24)
acf(nikkei.resi^2, lag = 24)
pacf(nikkei.resi^2, lag = 24)

par(mfcol = c(1,1))
nikkei.upp = 0.000397+2*nikkei.v1
nikkei.low = 0.000397-2*nikkei.v1
nikkei.tdx = c(1:4925)/246+1996
plot(nikkei.tdx,nikkei.r, xlab = 'Year', ylab = 'Series', col = 'blue', type = 'l', ylim = c(-0.12,0.12))
lines(nikkei.tdx, nikkei.upp, lty = 1, col = 'red')
lines(nikkei.tdx, nikkei.low, lty = 1, col = 'red')
abline(h = c(0.000397))

nikkei.m2 = garchFit(~1+garch(1,1), data = nikkei.r, cond.dist = "std", trace = F)
summary(nikkei.m2)

nikkei.v2 = volatility(nikkei.m2)

nikkei.m3 = garchFit(~1+garch(1,1), data = nikkei.r, cond.dist = "sstd", trace = F)
summary(nikkei.m3)

nikkei.v3 = volatility(nikkei.m3)

par(mfcol = c(3,1))
plot(nikkei.tdx, nikkei.v1, xlab = 'Year', ylab = 'Volatility', type = 'l', col = 'blue')
title(main = '(a) Gaussian')
plot(nikkei.tdx, nikkei.v2, xlab = 'Year', ylab = 'Volatility', type = 'l', col = 'blue')
title(main = '(b) Student-t')
plot(nikkei.tdx, nikkei.v3, xlab = 'Year', ylab = 'Volatility', type = 'l', col = 'blue')
title(main = '(c) Skew Student-t')

cor(cbind(nikkei.v1, nikkei.v2, nikkei.v3)) # correlation

library(fBasics)
basicStats(nikkei.r)

nikkei.tt = -0.305634 / sqrt(6/4925) # testing skewness of the data
nikkei.tt

nikkei.tsm = (0.9246-1)/0.0184 # testing skewness of the model
nikkei.tsm

nikkei.pv = 2*pnorm(nikkei.tsm) # compute p-value
nikkei.pv

par(mfcol = c(3,1))
plot(nikkei.m1)
13
0
plot(nikkei.m2)
13
0
plot(nikkei.m3)
13
0

# Returns to use for comparison purposes

library(quantmod)

getSymbols("^N225", from = "2011-01-12", to = "2013-02-06") # get historical price
nikkei.a = periodReturn(N225, period = "daily", subset = NULL, type = "log")
tail(nikkei.a)


# Forecasting using the rugarch package

library(rugarch)

# Model specification

nikkei.spec = ugarchspec(variance.model = list(model = "sGARCH"),
mean.model = list(armaOrder = c(0,0)), distribution = "sstd")
nikkei.fit = ugarchfit(data = nikkei.r, spec = nikkei.spec, out.sample = 925)
show(nikkei.fit)

# Simulation

nikkei.sim = ugarchsim(nikkei.fit, n.sim = 1116, n.start = 1000, m.sim = 25, rseed = 1:2)
nikkei.simsig = as.data.frame(nikkei.sim, which = 'sigma')
nikkei.ser = as.data.frame(nikkei.sim, which = 'series')
show(nikkei.sim)
plot(nikkei.sim, m.sim = 25, main = 'Nikkei Simulated Series')
2
0

nikkei.r1 = nikkei.r[3508:4925]
basicStats(nikkei.r1)
nikkei.tdx = c(1:1418)/246+2011
plot(nikkei.tdx,nikkei.r1, xlab = 'Year', ylab = 'Series', col = '#4582B4', 
type = 'l', ylim = c(-0.12,0.12))

# Forecasting with Bootstrap

nikkei.bootp = ugarchboot(nikkei.fit, method = c("Partial", "Full")[1], n.ahead = 500,
n.bootpred = 2000)
show(nikkei.bootp)
plot(nikkei.bootp)
2

# Value at Risk

source('~/Google Drive/University/Dissertation/R/RMfit.R', chdir = TRUE)

nikkei.rp = nikkei.r * 100
nikkei.rmm = RMfit(nikkei.rp)
nikkei.tdx = c(1:4925)/246+1996
plot(nikkei.r, xlab = 'Year', ylab = 'Log-rtn', col = '#4582B4',type = 'l')
