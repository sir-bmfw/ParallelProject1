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

### Pre Future Event Date Drift

## bring in clinical trial data
clinicData = read.csv("two_sigma_events_2018_11_29_1242_clinical_trial_events_2018_11_29_1242.csv")
clinicData$future_date_date = gsub("T00:00:00+","",clinicData$future_date_date)
clinicData$future_date_date = as.Date(clinicData$future_date_date, format="%Y-%m-%d")
clinicData$date = as.POSIXct(clinicData$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
clinicData$date = format(clinicData$date,tz="America/New_York")
names(clinicData)[1] ='datetime'
clinicData$date = as.Date(clinicData$datetime)
clinicData$time = format(as.POSIXct(clinicData$datetime), format= "%H:%M:%S")
clinicData$time_until_future_date = clinicData$future_date_date-clinicData$date

# split up piped items into individual instances, recombine
secData = clinicData[1,]
for (i in c(12:13)){
  temp = fullData[grepl("\\|",fullData[,i])==TRUE,]
  secData = rbind(secData,temp)
  rm(temp)
}
secData = secData[2:length(secData$date),]
secData = unique(secData)
secData = as.data.frame(lapply(secData, function(y) gsub(".*\\|", "", y)))

clinicData = as.data.frame(lapply(clinicData, function(y) gsub("\\|.*", "", y)))
clinicData = rbind(clinicData,secData)

# split ticker and exchange info into separate columns
clinicData$acting_party_exchange = clinicData$acting_party_ticker
clinicData$acting_party_ticker = sub(".*:", "", clinicData$acting_party_ticker)
clinicData$acting_party_exchange = sub(":.*", "", clinicData$acting_party_exchange)
clinicData$phase_phase[clinicData$phase_phase == 'first'] = "1"
rm(secData)


# Bring in Quandl time series data
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
dates = as.Date(as.Date("2018-11-30"):as.Date("2012-01-01"), origin="1970-01-01")
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
#write.csv(dates,"datevector.csv")
# Dates were exported and combined into strings in Excel to expedite the upload process 
dates = as.vector(read.csv("datevector.csv"))
tickers = unique(clinicData$acting_party_ticker)
tickers = sort(tickers)
tickers = tickers[2:length(tickers)]
clinicRetData = Quandl.datatable('SHARADAR/SEP', date = dates[1,1], ticker = tickers[1])
for (i in 1:length(tickers)){
  for (j in 1:length(dates$Dates)){
    tic()
    temp = Quandl.datatable('SHARADAR/SEP', date = dates[j,1], ticker = tickers[i])
    clinicRetData = rbind(clinicRetData,temp)
    rm(temp)
    toc()
  }
}
clinicRetData = unique(clinicRetData[,c(1:10)])

# organize returns data, add in fwd returns
clinicRetData = clinicRetData[sort.list(clinicRetData$date,decreasing=TRUE),]
clinicRetData = clinicRetData[sort.list(clinicRetData$ticker),]
#lag function
lg1 = function(x)c(NA, x[1:(length(x)-1)])
lg2 = function(x)c(NA,NA, x[1:(length(x)-2)])
lg3 = function(x)c(NA,NA,NA, x[1:(length(x)-3)])
lg4 = function(x)c(NA,NA,NA,NA, x[1:(length(x)-4)])
lg5 = function(x)c(NA,NA,NA,NA,NA, x[1:(length(x)-5)])
lg21 = function(x)c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, x[1:(length(x)-21)])
clinicRetData$fwd1.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg1)
clinicRetData$fwd1alt.price = ave(clinicRetData$open, clinicRetData$ticker, FUN = lg1)
clinicRetData$fwd2.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg2)
clinicRetData$fwd3.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg3)
clinicRetData$fwd4.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg4)
clinicRetData$fwd5.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg5)
clinicRetData$fwd21.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg21)
clinicRetData = clinicRetData[sort.list(clinicRetData$date),]
clinicRetData = clinicRetData[sort.list(clinicRetData$ticker),]
clinicRetData$lag1.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg1)
clinicRetData$lag2.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg2)
clinicRetData$lag3.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg3)
clinicRetData$lag4.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg4)
clinicRetData$lag5.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg5)
clinicRetData$lag21.price = ave(clinicRetData$close, clinicRetData$ticker, FUN = lg21)


# add returns
clinicRetData$fwd0.ret = clinicRetData$close / clinicRetData$open
clinicRetData$fwd1.ret = clinicRetData$fwd1.price / clinicRetData$close
clinicRetData$fwd1alt.ret = clinicRetData$fwd1alt.price / clinicRetData$close
clinicRetData$fwd2.ret = clinicRetData$fwd2.price / clinicRetData$close
clinicRetData$fwd3.ret = clinicRetData$fwd3.price / clinicRetData$close
clinicRetData$fwd4.ret = clinicRetData$fwd4.price / clinicRetData$close
clinicRetData$fwd5.ret = clinicRetData$fwd5.price / clinicRetData$close
clinicRetData$fwd21.ret = clinicRetData$fwd21.price / clinicRetData$close
clinicRetData$lag1.ret = clinicRetData$open / clinicRetData$lag1.price
clinicRetData$lag1alt.ret = clinicRetData$close / clinicRetData$lag1.price
clinicRetData$lag2.ret = clinicRetData$close / clinicRetData$lag2.price
clinicRetData$lag3.ret = clinicRetData$close / clinicRetData$lag3.price
clinicRetData$lag4.ret = clinicRetData$close / clinicRetData$lag4.price
clinicRetData$lag5.ret = clinicRetData$close / clinicRetData$lag5.price
clinicRetData$lag21.ret = clinicRetData$close / clinicRetData$lag21.price


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

# add returns
mktData$fwd0.ret = mktData$close / mktData$open
mktData$fwd1.ret = mktData$fwd1.price / mktData$close
mktData$fwd1alt.ret = mktData$fwd1alt.price / mktData$close
mktData$fwd2.ret = mktData$fwd2.price / mktData$close
mktData$fwd3.ret = mktData$fwd3.price / mktData$close
mktData$fwd4.ret = mktData$fwd4.price / mktData$close
mktData$fwd5.ret = mktData$fwd5.price / mktData$close
mktData$fwd21.ret = mktData$fwd21.price / mktData$close
mktData$lag1.ret = mktData$open / mktData$lag1.price
mktData$lag1alt.ret = mktData$close / mktData$lag1.price
mktData$lag2.ret = mktData$close / mktData$lag2.price
mktData$lag3.ret = mktData$close / mktData$lag3.price
mktData$lag4.ret = mktData$close / mktData$lag4.price
mktData$lag5.ret = mktData$close / mktData$lag5.price
mktData$lag21.ret = mktData$close / mktData$lag21.price


# covariance and beta of companies against XLV
clinicRetData = merge(clinicRetData,subset(mktData,ticker=="XLV")[,c(2,37)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(clinicRetData)[37] = c("lag5.ret")
colnames(clinicRetData)[39] = c("lag5.xlv")

clinicRetData$xlvCov = NA
clinicRetData$xlvBeta = NA
clinicRetData$lag5.mean = NA
clinicRetData$lag5.sd = NA
for (i in 1:length(tic)){
  tic()
  temp = subset(clinicRetData,ticker == tic[i])
  clinicRetData = subset(clinicRetData,ticker != tic[i])
  if (length(completeFun(temp,c("lag5.ret"))$date)>0){
    temp$xlvCov = cov((temp$lag5.ret-1),(temp$lag5.xlv-1),use="complete.obs")
    temp$xlvBeta = temp$xlvCov/var((temp$lag5.xlv-1),na.rm=TRUE)
    temp$lag5.mean = mean((temp$lag5.ret)/(temp$xlvBeta*(temp$lag5.xlv)),na.rm=TRUE)
    temp$lag5.sd = sd((temp$lag5.ret)/(temp$xlvBeta*(temp$lag5.xlv)),na.rm=TRUE)
  }
  clinicRetData = rbind(clinicRetData,temp)
  rm(temp)
  toc()
}
#clinicRetData$lag5.mean = ave((clinicRetData$lag5.ret)/(clinicRetData$xlvBeta*(clinicRetData$lag5.xlv)),
#                               clinicRetData$ticker, FUN = function(x) mean(x,na.rm=TRUE))
#clinicRetData$lag5.sd = ave((clinicRetData$lag5.ret)/(clinicRetData$xlvBeta*(clinicRetData$lag5.xlv)),
#                               clinicRetData$ticker, FUN = function(x) sd(x,na.rm=TRUE))


## Make set for future events that announce data
clinicData = subset(clinicData, topic_topic == "data")
clinicData$date = as.Date(clinicData$date,format="%Y-%m-%d")
clinicData$future_date_date = as.Date(clinicData$future_date_date, format="%Y-%m-%d")
clinicData$time_until_future_date = clinicData$future_date_date-clinicData$date
clinicData$time_until_future_date = as.numeric(clinicData$time_until_future_date)
clinicData = subset(clinicData, is.na(future_date_date)==FALSE & future_date_date < "2018-12-01" & time_until_future_date > 7)


clinicData = merge(clinicData,clinicRetData[,c(1:2,37,39:43)],by.x=c("future_date_date","acting_party_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(clinicData)[37:44]=c("lag5.ret","xlv")
clinicData$factRet = (clinicData$lag5.ret)/(clinicData$xlvBeta*(clinicData$lag5.xlv))


write.csv(clinicData,"Parallel Output 120418.csv",row.names=FALSE)


#########################################################################

### Post Event Date Drift

## bring in clinical trial data
clinicData = read.csv("clinical_trial_events_2018_12_07_1226.csv")
clinicData$future_date_date = gsub("T00:00:00+","",clinicData$future_date_date)
clinicData$future_date_date = as.Date(clinicData$future_date_date, format="%Y-%m-%d")
clinicData$date = as.POSIXct(clinicData$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
clinicData$date = format(clinicData$date,tz="America/New_York")
names(clinicData)[1] ='datetime'
clinicData$date = as.Date(clinicData$datetime)
clinicData$time = format(as.POSIXct(clinicData$datetime), format= "%H:%M:%S")
clinicData$time_until_future_date = clinicData$future_date_date-clinicData$date

# split up piped items into individual instances, recombine
secData = clinicData[1,]
for (i in c(12:13)){
  temp = fullData[grepl("\\|",fullData[,i])==TRUE,]
  secData = rbind(secData,temp)
  rm(temp)
}
secData = secData[2:length(secData$date),]
secData = unique(secData)
secData = as.data.frame(lapply(secData, function(y) gsub(".*\\|", "", y)))

clinicData = as.data.frame(lapply(clinicData, function(y) gsub("\\|.*", "", y)))
clinicData = rbind(clinicData,secData)

# split ticker and exchange info into separate columns
clinicData$acting_party_exchange = clinicData$acting_party_ticker
clinicData$acting_party_ticker = sub(".*:", "", clinicData$acting_party_ticker)
clinicData$acting_party_exchange = sub(":.*", "", clinicData$acting_party_exchange)
clinicData$phase_phase[clinicData$phase_phase == 'first'] = "1"
rm(secData)

# 
clinicData$trade_date = as.Date(clinicData$trade_date,format="%Y-%m-%d")
clinicData = merge(clinicData,clinicRetData[,c(1:2,30,39:43)],by.x=c("trade_date","acting_party_ticker"),by.y=c("date","ticker"),all.x=TRUE)
clinicData = merge(clinicData,subset(mktData,ticker=="XLV")[,c(2,30)], by.x=c("trade_date"), by.y=c("date"), all.x=TRUE)
colnames(clinicData)[36]= "fwd5.ret"
colnames(clinicData)[42]= "fwd5.xlv"

temp = clinicRetData[,c("ticker","xlvCov","xlvBeta","lag5.mean","lag5.sd")]
temp = unique(temp)
clinicData = clinicData[,c(1:37,42)]
clinicData = merge(clinicData,temp,by.x = "acting_party_ticker", by.y="ticker", all.x=TRUE)
clinicData$factRet = (clinicData$fwd5.ret)/(clinicData$xlvBeta*(clinicData$fwd5.xlv))

write.csv(clinicData,"Parallel Output 120618.csv",row.names=FALSE)


#########################################################################

### Post Event Date Drift, Mergers & Acquisitions / Operational Partnerships

## bring in clinical trial data
maopData = read.csv("two_sigma_events_2018_11_29_1242_mergers_acquisitions_events_2018_11_29_1242.csv")
maopData = rbind.fill(maopData,read.csv("two_sigma_events_2018_11_29_1242_operational_partnerships_events_2018_11_29_1242.csv"))
maopData$date = as.POSIXct(maopData$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
maopData$date = format(maopData$date,tz="America/New_York")
names(maopData)[1] ='datetime'
maopData$date = as.Date(maopData$datetime)
maopData$time = format(as.POSIXct(maopData$datetime), format= "%H:%M:%S")

# split up piped items into individual instances, recombine
secData = maopData[1,]
for (i in c(12:13)){
  temp = fullData[grepl("\\|",fullData[,i])==TRUE,]
  secData = rbind(secData,temp)
  rm(temp)
}
secData = secData[2:length(secData$date),]
secData = unique(secData)
secData = as.data.frame(lapply(secData, function(y) gsub(".*\\|", "", y)))

maopData = as.data.frame(lapply(maopData, function(y) gsub("\\|.*", "", y)))
maopData = rbind(maopData,secData)

# split ticker and exchange info into separate columns
maopData$acting_party_exchange = maopData$acting_party_ticker
maopData$acting_party_ticker = sub(".*:", "", maopData$acting_party_ticker)
maopData$acquiror_ticker = sub(".*:", "", maopData$acquiror_ticker)
maopData$target_company_ticker = sub(".*:", "", maopData$target_company_ticker)
maopData$company_1_ticker = sub(".*:", "", maopData$company_1_ticker)
maopData$company_2_ticker = sub(".*:", "", maopData$company_2_ticker)
maopData$acting_party_exchange = sub(":.*", "", maopData$acting_party_exchange)
rm(secData)


# organize returns data, add in fwd returns
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
retData = Quandl.datatable('SHARADAR/SEP', date = dates[1,1], ticker = tickers[1])
for (i in 1:length(tickers)){
  for (j in 1:length(dates$Dates)){
    tic()
    temp = Quandl.datatable('SHARADAR/SEP', date = dates[1,1], ticker = tickers[i])
    retData = rbind(retData,temp)
    rm(temp)
    toc()
  }
}
retData = unique(retData[,c(1:10)])

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

retData = retData[,c(1:38)]
retData = merge(retData,subset(mktData,ticker=="XLV")[,c(2,30)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[30] = c("fwd5.ret")
colnames(retData)[39] = c("fwd5.xlv")

retData$xlvCov = NA
retData$xlvBeta = NA
retData$fwd5.mean = NA
retData$fwd5.sd = NA
for (i in 1:length(tickers)){
  tic()
  temp = subset(retData,ticker == tickers[i])
  retData = subset(retData,ticker != tickers[i])
  if (length(completeFun(temp,c("fwd5.ret"))$date)>0){
    temp$xlvCov = cov((temp$fwd5.ret-1),(temp$fwd5.xlv-1),use="complete.obs")
    temp$xlvBeta = temp$xlvCov/var((temp$fwd5.xlv-1),na.rm=TRUE)
    temp$fwd5.mean = mean((temp$fwd5.ret)/(temp$xlvBeta*(temp$fwd5.xlv)),na.rm=TRUE)
    temp$fwd5.sd = sd((temp$fwd5.ret)/(temp$xlvBeta*(temp$fwd5.xlv)),na.rm=TRUE)
  }
  retData = rbind(retData,temp)
  rm(temp)
  toc()
}

metrics = unique(retData[c(2,40:43)])

# 
maopData$trade_date = as.Date(maopData$trade_date,format="%Y-%m-%d")
maopData = merge(maopData,retData[,c(1:2,30,39:43)],by.x=c("trade_date","acting_party_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(maopData)[36]= "acting_fwd5.ret"
maopData = merge(maopData,retData[,c(1:2,30)],by.x=c("trade_date","acquiror_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(maopData)[42]= "acquiror_fwd5.ret"
maopData = merge(maopData,retData[,c(1:2,30)],by.x=c("trade_date","target_company_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(maopData)[43]= "target_fwd5.ret"
maopData = merge(maopData,retData[,c(1:2,30)],by.x=c("trade_date","company_1_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(maopData)[44]= "company_1_fwd5.ret"
maopData = merge(maopData,retData[,c(1:2,30)],by.x=c("trade_date","company_2_ticker"),by.y=c("date","ticker"),all.x=TRUE)
colnames(maopData)[45]= "company_2_fwd5.ret"
maopData = merge(maopData,subset(mktData,ticker=="XLV")[,c(2,30)], by.x=c("trade_date"), by.y=c("date"), all.x=TRUE)
colnames(maopData)[46]= "fwd5.xlv"

temp = retData[,c("ticker","xlvCov","xlvBeta","fwd5.mean","fwd5.sd")]
temp = unique(temp)
# acting factor return
maopData = maopData[,c(1:36,42:46)]
maopData = merge(maopData,temp,by.x = "acting_party_ticker", by.y="ticker", all.x=TRUE)
maopData$acting_factRet = (maopData$acting_fwd5.ret)/(maopData$xlvBeta*(maopData$fwd5.xlv))
# acquiror factor return
maopData = maopData[,c(1:41,46)]
maopData = merge(maopData,temp,by.x = "acquiror_ticker", by.y="ticker", all.x=TRUE)
maopData$acquiror_factRet = (maopData$acquiror_fwd5.ret)/(maopData$xlvBeta*(maopData$fwd5.xlv))
# target company factor return
maopData = maopData[,c(1:42,47)]
maopData = merge(maopData,temp,by.x = "target_company_ticker", by.y="ticker", all.x=TRUE)
maopData$target_factRet = (maopData$target_fwd5.ret)/(maopData$xlvBeta*(maopData$fwd5.xlv))
# company 1 factor return
maopData = maopData[,c(1:43,48)]
maopData = merge(maopData,temp,by.x = "company_1_ticker", by.y="ticker", all.x=TRUE)
maopData$company_1_factRet = (maopData$company_1_fwd5.ret)/(maopData$xlvBeta*(maopData$fwd5.xlv))
# company 2 factor return
maopData = maopData[,c(1:44,49)]
maopData = merge(maopData,temp,by.x = "company_2_ticker", by.y="ticker", all.x=TRUE)
maopData$company_2_factRet = (maopData$company_2_fwd5.ret)/(maopData$xlvBeta*(maopData$fwd5.xlv))

maopData = maopData[,c(1:45,50)]
maopData = merge(maopData,temp[,c(1,4:5)],by.x = "acting_party_ticker", by.y = "ticker", all.x=TRUE)
colnames(maopData)[47:48] = c("acting_fwd5.mean","acting_fwd5.sd")
maopData = merge(maopData,temp[,c(1,4:5)],by.x = "acquiror_ticker", by.y = "ticker", all.x=TRUE)
colnames(maopData)[49:50] = c("acquiror_fwd5.mean","acquiror_fwd5.sd")
maopData = merge(maopData,temp[,c(1,4:5)],by.x = "target_company_ticker", by.y = "ticker", all.x=TRUE)
colnames(maopData)[51:52] = c("target_fwd5.mean","target_fwd5.sd")
maopData = merge(maopData,temp[,c(1,4:5)],by.x = "company_1_ticker", by.y = "ticker", all.x=TRUE)
colnames(maopData)[53:54] = c("company_1_fwd5.mean","company_1_fwd5.sd")
maopData = merge(maopData,temp[,c(1,4:5)],by.x = "company_2_ticker", by.y = "ticker", all.x=TRUE)
colnames(maopData)[55:56] = c("company_2_fwd5.mean","company_2_fwd5.sd")


write.csv(maopData,"Parallel Output 120818.csv",row.names=FALSE)
