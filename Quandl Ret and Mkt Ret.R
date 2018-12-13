library(plyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(stats)
library(ggplot2)
library(grid)
library(gridExtra)
library(coefplot)
library(lme4)
library(yhat)
library(tictoc)
library(stringr)
library(Quandl)
library(bizdays)
library(moments)
library(lattice)


#########################################################################

## Creating data set of company return data along with market benchmarks

# Bring in Quandl time series data
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
dates = as.Date(as.Date("2018-11-30"):as.Date("2012-01-01"), origin="1970-01-01")
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
#write.csv(dates,"datevector.csv")
# Dates were exported and combined into strings in Excel to expedite the upload process 
dates = as.vector(read.csv("datevector.csv"))
tickers = unique(fullData$acting_party_ticker)
tickers = sort(tickers)
tickers = tickers[2:length(tickers)]
retData = Quandl.datatable('SHARADAR/SEP', date = dates[1,1], ticker = tickers[1])
for (i in 240:length(tickers)){
  for (j in 1:length(dates$Dates)){
    tic()
    temp = Quandl.datatable('SHARADAR/SEP', date = dates[j,1], ticker = tickers[i])
    retData = rbind(retData,temp)
    rm(temp)
    toc()
  }
}
retData = unique(retData[,c(1:10)])


# organize returns data, add in fwd returns
retData = retData[sort.list(retData$date,decreasing=TRUE),]
retData = retData[sort.list(retData$ticker),]
#lag function
lg1 = function(x)c(NA, x[1:(length(x)-1)])
lg2 = function(x)c(NA,NA, x[1:(length(x)-2)])
lg3 = function(x)c(NA,NA,NA, x[1:(length(x)-3)])
lg4 = function(x)c(NA,NA,NA,NA, x[1:(length(x)-4)])
lg5 = function(x)c(NA,NA,NA,NA,NA, x[1:(length(x)-5)])
lg21 = function(x)c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, x[1:(length(x)-21)])
retData$fwd1.price = ave(retData$close, retData$ticker, FUN = lg1)
retData$fwd1alt.price = ave(retData$open, retData$ticker, FUN = lg1)
retData$fwd2.price = ave(retData$close, retData$ticker, FUN = lg2)
retData$fwd3.price = ave(retData$close, retData$ticker, FUN = lg3)
retData$fwd4.price = ave(retData$close, retData$ticker, FUN = lg4)
retData$fwd5.price = ave(retData$close, retData$ticker, FUN = lg5)
retData$fwd21.price = ave(retData$close, retData$ticker, FUN = lg21)
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]
retData$lag1.price = ave(retData$close, retData$ticker, FUN = lg1)
retData$lag2.price = ave(retData$close, retData$ticker, FUN = lg2)
retData$lag3.price = ave(retData$close, retData$ticker, FUN = lg3)
retData$lag4.price = ave(retData$close, retData$ticker, FUN = lg4)
retData$lag5.price = ave(retData$close, retData$ticker, FUN = lg5)
retData$lag21.price = ave(retData$close, retData$ticker, FUN = lg21)


# add returns
retData$fwd0.ret = retData$close / retData$open
retData$fwd1.ret = retData$fwd1.price / retData$close
retData$fwd1alt.ret = retData$fwd1alt.price / retData$close
retData$fwd2.ret = retData$fwd2.price / retData$close
retData$fwd3.ret = retData$fwd3.price / retData$close
retData$fwd4.ret = retData$fwd4.price / retData$close
retData$fwd5.ret = retData$fwd5.price / retData$close
retData$fwd21.ret = retData$fwd21.price / retData$close
retData$lag1.ret = retData$open / retData$lag1.price
retData$lag1alt.ret = retData$close / retData$lag1.price
retData$lag2.ret = retData$close / retData$lag2.price
retData$lag3.ret = retData$close / retData$lag3.price
retData$lag4.ret = retData$close / retData$lag4.price
retData$lag5.ret = retData$close / retData$lag5.price
retData$lag21.ret = retData$close / retData$lag21.price


# grab our market data; we'll be looking at five different benchmarks to start for 'market'
# SPY - SPDR S&P 500 ETF
# XPH - SPDR S&P Pharmaceuticals ETF
# XLV - Health Care Select Sector SPDR ETF
# XBI - SPDR S7P Biotech ETF
# IHI - iShares US Medical Devices ETF

Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
sp500Data = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'SPY')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'SPY')
  sp500Data = rbind(sp500Data,temp)
  rm(temp)
  toc()
}
sp500Data = unique(sp500Data[,c(1:10)])
xphData = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'XPH')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'XPH')
  xphData = rbind(xphData,temp)
  rm(temp)
  toc()
}
xphData = unique(xphData[,c(1:10)])
xlvData = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'XLV')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'XLV')
  xlvData = rbind(xlvData,temp)
  rm(temp)
  toc()
}
xlvData = unique(xlvData[,c(1:10)])
xbiData = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'XBI')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'XBI')
  xbiData = rbind(xbiData,temp)
  rm(temp)
  toc()
}
xbiData = unique(xbiData[,c(1:10)])
ihiData = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'IHI')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'IHI')
  ihiData = rbind(ihiData,temp)
  rm(temp)
  toc()
}
ihiData = unique(ihiData[,c(1:10)])

# organize market data, combine and add in fwd1 rets
mktData = rbind(sp500Data,xphData)
mktData = rbind(mktData,xlvData)
mktData = rbind(mktData,xbiData)
mktData = rbind(mktData,ihiData)
mktData = mktData[sort.list(mktData$date,decreasing=TRUE),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$fwd1.price = ave(mktData$close, mktData$ticker, FUN = lg1)
mktData$fwd1alt.price = ave(mktData$open, mktData$ticker, FUN = lg1)
mktData$fwd2.price = ave(mktData$close, mktData$ticker, FUN = lg2)
mktData$fwd3.price = ave(mktData$close, mktData$ticker, FUN = lg3)
mktData$fwd4.price = ave(mktData$close, mktData$ticker, FUN = lg4)
mktData$fwd5.price = ave(mktData$close, mktData$ticker, FUN = lg5)
mktData$fwd21.price = ave(mktData$close, mktData$ticker, FUN = lg21)
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$lag1.price = ave(mktData$close, mktData$ticker, FUN = lg1)
mktData$lag2.price = ave(mktData$close, mktData$ticker, FUN = lg2)
mktData$lag3.price = ave(mktData$close, mktData$ticker, FUN = lg3)
mktData$lag4.price = ave(mktData$close, mktData$ticker, FUN = lg4)
mktData$lag5.price = ave(mktData$close, mktData$ticker, FUN = lg5)
mktData$lag21.price = ave(mktData$close, mktData$ticker, FUN = lg21)

# add market returns
mktData$fwd0.mkt = mktData$close / mktData$open
mktData$fwd1.mkt = mktData$fwd1.price / mktData$close
mktData$fwd1alt.mkt = mktData$fwd1alt.price / mktData$close
mktData$fwd2.mkt = mktData$fwd2.price / mktData$close
mktData$fwd3.mkt = mktData$fwd3.price / mktData$close
mktData$fwd4.mkt = mktData$fwd4.price / mktData$close
mktData$fwd5.mkt = mktData$fwd5.price / mktData$close
mktData$fwd21.mkt = mktData$fwd21.price / mktData$close
mktData$lag1.mkt = mktData$open / mktData$lag1.price
mktData$lag1alt.mkt = mktData$close / mktData$lag1.price
mktData$lag2.mkt = mktData$close / mktData$lag2.price
mktData$lag3.mkt = mktData$close / mktData$lag3.price
mktData$lag4.mkt = mktData$close / mktData$lag4.price
mktData$lag5.mkt = mktData$close / mktData$lag5.price
mktData$lag21.mkt = mktData$close / mktData$lag21.price

# Add in XLV market data with returns data
retData = merge(retData,subset(mktData,ticker=="XLV")[,c(2,24:38)], by.x=c("date"), by.y=c("date"), all.x=TRUE)




