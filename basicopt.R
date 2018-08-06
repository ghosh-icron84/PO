library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(stringr)
library(memoise)
library(xts)

ret1 <- ret0
dta <- xts(ret1,order.by=as.Date(rownames(ret1)))
dim(dta)
