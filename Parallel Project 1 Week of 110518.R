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


#########################################################################

## add in all the other catalyst events

fullData = read.csv("_two_sigma_events_clinical_trial_events_201810081910.csv")
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_accounting_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_capital_structure_change_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_corporate_access_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_distress_bankruptcy_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_dividends_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_earnings_release_date_announced_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_earnings_results_released_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_guidance_debt_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_guidance_management_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_legal_class_action_and_investor_lawsuit_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_legal_other_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_management_change_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_mergers_acquisitions_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_natural_resources_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_operational_labor_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_operational_marketing_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_operational_partnerships_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_operational_ppe_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_product_launch_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_product_recalled_discontinued_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_product_update_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_regulatory_fda_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_regulatory_other_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_regulatory_patent_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_sales_agreement_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_sales_result_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_shareholder_meeting_events_201810091810.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_201810091810_shareholder_related_events_201810091810.csv"))
fullData$future_date_date = gsub("T00:00:00+","",fullData$future_date_date)
fullData$future_date_date = as.Date(fullData$future_date_date, format="%Y-%m-%d")
fullData$date = as.POSIXct(fullData$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
fullData$date = format(fullData$date,tz="America/New_York")
fullData$time_until_future_date = fullData$future_date_date-fullData$date
names(fullData)[1] ='datetime'
fullData$date = as.Date(fullData$datetime)
fullData$time = format(as.POSIXct(fullData$datetime), format= "%H:%M:%S")

# split up piped items into individual instances, recombine
secData = fullData[1,]
for (i in c(4:5)){
  temp = fullData[grepl("\\|",fullData[,i])==TRUE,]
  secData = rbind(secData,temp)
  rm(temp)
}
secData = secData[2:length(secData$date),]
secData = unique(secData)
secData = as.data.frame(lapply(secData, function(y) gsub(".*\\|", "", y)))

fullData = as.data.frame(lapply(fullData, function(y) gsub("\\|.*", "", y)))
fullData = rbind(fullData,secData)

# split ticker and exchange info into separate columns
fullData$acting_party_exchange = fullData$acting_party_ticker
fullData$acting_party_ticker = sub(".*:", "", fullData$acting_party_ticker)
fullData$acting_party_exchange = sub(":.*", "", fullData$acting_party_exchange)
fullData$phase_phase[fullData$phase_phase == 'first'] = "1"

# Bring in Quandl time series data
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
dates = as.Date(as.Date("2018-10-16"):as.Date("2012-01-01"), origin="1970-01-01")
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
#write.csv(dates,"datevector.csv")
# Dates were exported and combined into strings in Excel to expedite the upload process 
dates = as.vector(read.csv("datevector.csv"))
tickers = unique(fullData$acting_party_ticker)
retData = Quandl.datatable('SHARADAR/SEP', date = dates[1,1], ticker = tickers[1])
for (i in 1:length(tickers)){
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
lg63 = function(x)c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,x[1:(length(x)-63)])
lg126 = function(x)c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,x[1:(length(x)-126)])
lg252 = function(x)c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,x[1:(length(x)-252)])

retData$fwd1.price = ave(retData$close, retData$ticker, FUN = lg1)
retData$fwd1alt.price = ave(retData$open, retData$ticker, FUN = lg1)
retData$fwd2.price = ave(retData$close, retData$ticker, FUN = lg2)
retData$fwd3.price = ave(retData$close, retData$ticker, FUN = lg3)
retData$fwd4.price = ave(retData$close, retData$ticker, FUN = lg4)
retData$fwd5.price = ave(retData$close, retData$ticker, FUN = lg5)
retData$fwd21.price = ave(retData$close, retData$ticker, FUN = lg21)
retData$fwd63.price = NA
for (i in 64:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-63]){
    retData$fwd63.price[i] = retData$close[i-63]
  }
}
retData$fwd126.price = NA
for (i in 127:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-126]){
    retData$fwd126.price[i] = retData$close[i-126]
  }
}
retData$fwd252.price = NA
for (i in 253:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-252]){
    retData$fwd252.price[i] = retData$close[i-252]
  }
}
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]
retData$lag1.price = ave(retData$close, retData$ticker, FUN = lg1)
retData$lag2.price = ave(retData$close, retData$ticker, FUN = lg2)
retData$lag3.price = ave(retData$close, retData$ticker, FUN = lg3)
retData$lag4.price = ave(retData$close, retData$ticker, FUN = lg4)
retData$lag5.price = ave(retData$close, retData$ticker, FUN = lg5)
retData$lag21.price = ave(retData$close, retData$ticker, FUN = lg21)
retData$lag63.price = NA
for (i in 64:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-63]){
    retData$lag63.price[i] = retData$close[i-63]
  }
}
retData$lag126.price = NA
for (i in 127:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-126]){
    retData$lag126.price[i] = retData$close[i-126]
  }
}
retData$lag252.price = NA
for (i in 253:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-252]){
    retData$lag252.price[i] = retData$close[i-252]
  }
}



# add returns
retData$fwd0.ret = retData$close / retData$open
retData$fwd1.ret = retData$fwd1.price / retData$close
retData$fwd1alt.ret = retData$fwd1alt.price / retData$close
retData$fwd2.ret = retData$fwd2.price / retData$close
retData$fwd3.ret = retData$fwd3.price / retData$close
retData$fwd4.ret = retData$fwd4.price / retData$close
retData$fwd5.ret = retData$fwd5.price / retData$close
retData$fwd21.ret = retData$fwd21.price / retData$close
retData$fwd63.ret = retData$fwd63.price / retData$close
retData$fwd126.ret = retData$fwd126.price / retData$close
retData$fwd252.ret = retData$fwd252.price / retData$close
retData$lag1.ret = retData$open / retData$lag1.price
retData$lag1alt.ret = retData$close / retData$lag1.price
retData$lag2.ret = retData$close / retData$lag2.price
retData$lag3.ret = retData$close / retData$lag3.price
retData$lag4.ret = retData$close / retData$lag4.price
retData$lag5.ret = retData$close / retData$lag5.price
retData$lag21.ret = retData$close / retData$lag21.price
retData$lag63.ret = retData$close / retData$lag63.price
retData$lag126.ret = retData$close / retData$lag126.price
retData$lag252.ret = retData$close / retData$lag252.price
retData$pm3.ret = retData$fwd3.price / retData$lag3.price - 1
retData$pm1.ret = retData$fwd1.price / retData$lag1.price - 1
retData$pm5.ret = retData$fwd5.price / retData$lag5.price - 1

# add returns to two sigma data
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]

fullData$date = as.Date(fullData$date, format="%Y-%m-%d")
fullData = merge(fullData, retData[,c(1:2,30:50)], by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"), all.x=TRUE)


# Determine if announcement was made before, during, or after market hours
fullData$time = format(as.POSIXct(fullData$datetime), format= "%H:%M:%S")
fullData$mkt_announce = if_else(fullData$time < "09:00:00","Pre",if_else(fullData$time >= "16:00:00", "Post","During"))

# add in the four t1 effects (Original = lag1/fwd1, Alt 1 = lag1alt/fwd1, Alt 2 = lag1/fw1alt, Alt 3 = lag1alt/fwd1alt)
fullData$t1.effect = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1.ret,ifelse(fullData$mkt_announce == "During", fullData$fwd0.ret,ifelse(fullData$mkt_announce == "Post", fullData$fwd1.ret,"ERROR"))))
fullData$t1alt1.effect = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1alt.ret,ifelse(fullData$mkt_announce == "During", fullData$fwd0.ret,ifelse(fullData$mkt_announce == "Post", fullData$fwd1.ret,"ERROR"))))
fullData$t1alt2.effect = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1.ret,ifelse(fullData$mkt_announce == "During", fullData$fwd0.ret,ifelse(fullData$mkt_announce == "Post", fullData$fwd1alt.ret,"ERROR"))))
fullData$t1alt3.effect = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1alt.ret,ifelse(fullData$mkt_announce == "During", fullData$fwd0.ret,ifelse(fullData$mkt_announce == "Post", fullData$fwd1alt.ret,"ERROR"))))


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
mktData$fwd1.mkt = mktData$fwd1.price / mktData$close
mktData$fwd1alt.mkt = mktData$fwd1alt.price / mktData$close
mktData = mktData[sort.list(mktData$date,decreasing=TRUE),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$fwd3.price = ave(mktData$close, mktData$ticker, FUN = lg3)
mktData$fwd5.price = ave(mktData$close, mktData$ticker, FUN = lg5)
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$lag3.price = ave(mktData$close, mktData$ticker, FUN = lg3)
mktData$pm3.mkt = mktData$fwd3.price / mktData$lag3.price - 1
mktData$lag1.price = ave(mktData$close, mktData$ticker, FUN = lg1)
mktData$pm1.mkt = mktData$fwd1.price / mktData$lag1.price - 1
mktData$lag5.price = ave(mktData$close, mktData$ticker, FUN = lg5)
mktData$pm5.mkt = mktData$fwd5.price / mktData$lag5.price - 1

# bring in etf info to sync with full data set
etfData = read.csv("Healthcare Companies.csv")
retData = merge(retData,etfData[,c(1,4:7)], by.x="ticker", by.y="Symbol", all.x=TRUE)

retData = merge(retData,mktData[,c(1:2,13)], by.x=c("date","Sector.Level.ETF"), by.y=c("date","ticker"), all.x=TRUE)
colnames(retData)[63] = c("pm3.sec")
retData = merge(retData,mktData[,c(1:2,13)], by.x=c("date","Industry.Level.ETF"), by.y=c("date","ticker"), all.x=TRUE)
colnames(retData)[64] = c("pm3.ind")

completeFun = function(data, desiredCols) {
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
retData$sectorCov = NA
retData$industryCov = NA
retData$sectorBeta = NA
retData$industryBeta = NA
for (i in 1:length(tickers)){
  tic()
  temp = subset(retData,ticker == tickers[i])
  retData = subset(retData,ticker != tickers[i])
  if (length(completeFun(temp,c("Sector.Level.ETF","Industry.Level.ETF"))$date)>0){
  temp$sectorCov = cov(temp$pm3.ret,temp$pm3.sec,use="complete.obs")
  temp$industryCov = cov(temp$pm3.ret,temp$pm3.ind,use="complete.obs")
  temp$sectorBeta = temp$sectorCov/var(temp$pm3.sec,na.rm=TRUE)
  temp$industryBeta = temp$industryCov/var(temp$pm3.ind,na.rm=TRUE)
  }
  retData = rbind(retData,temp)
  rm(temp)
  toc()
}

retData = merge(retData,subset(mktData,ticker=="XPH")[,c(2,13)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[66] = c("pm3.xph")
retData = merge(retData,subset(mktData,ticker=="XLV")[,c(2,13)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[67] = c("pm3.xlv")
retData = merge(retData,subset(mktData,ticker=="XBI")[,c(2,13)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[68] = c("pm3.xbi")
retData = merge(retData,subset(mktData,ticker=="IHI")[,c(2,13)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[69] = c("pm3.ihi")


retData$xphCov = NA
retData$xlvCov = NA
retData$xbiCov = NA
retData$ihiCov = NA
retData$xphBeta = NA
retData$xlvBeta = NA
retData$xbiBeta = NA
retData$ihiBeta = NA
for (i in 1:length(tickers)){
  tic()
  temp = subset(retData,ticker == tickers[i])
  retData = subset(retData,ticker != tickers[i])
  if (length(completeFun(temp,c("pm3.ret"))$date)>0){
    temp$xphCov = cov(temp$pm3.ret,temp$pm3.xph,use="complete.obs")
    temp$xlvCov = cov(temp$pm3.ret,temp$pm3.xlv,use="complete.obs")
    temp$xbiCov = cov(temp$pm3.ret,temp$pm3.xbi,use="complete.obs")
    temp$ihiCov = cov(temp$pm3.ret,temp$pm3.ihi,use="complete.obs")
    temp$xphBeta = temp$xphCov/var(temp$pm3.xph,na.rm=TRUE)
    temp$xlvBeta = temp$xlvCov/var(temp$pm3.xlv,na.rm=TRUE)
    temp$xbiBeta = temp$xbiCov/var(temp$pm3.xbi,na.rm=TRUE)
    temp$ihiBeta = temp$ihiCov/var(temp$pm3.ihi,na.rm=TRUE)
  }
  retData = rbind(retData,temp)
  rm(temp)
  toc()
}


bench = as.vector(unique(mktData$ticker))
bench = bench[c(1,3:5)]
sectors = as.vector(unique(retData$Sector))
industries = as.vector(unique(retData$Industry))

covTable = matrix(data = NA, nrow=4,ncol=8)
rownames(covTable) = bench
colnames(covTable) = industries
for (i in 1:length(bench)){
  for (j in 1:length(industries)){
    temp = unique(subset(retData,Industry == industries[j])[,c("ticker","ihiCov","xbiCov","xlvCov","xphCov")])
    covTable[i,j] = (temp[,(i+1)])
    rm(temp)
  }
}


Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
mktCapData = Quandl.datatable('SHARADAR/SF1', calendardate = dates[1,1], ticker = tickers[1], qopts.columns = c("ticker","calendardate","marketcap"))
for (i in 593:length(tickers)){
  for (j in 1:length(dates$Dates)){
    tic()
    temp = Quandl.datatable('SHARADAR/SF1', calendardate = dates[j,1], ticker = tickers[i], qopts.columns = c("ticker","calendardate","marketcap"))
    mktCapData = rbind(mktCapData,temp)
    rm(temp)
    toc()
  }
}
mktCapData = unique(mktCapData[,c(1:3)])


fullData = merge(fullData,retData[,c(1,4,65:77)],by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"),all.x=TRUE)
fullData = merge(fullData,etfData[,c(1,4:7)], by.x="acting_party_ticker", by.y="Symbol", all.x=TRUE)
fullData$pm3.beta = ifelse(fullData$Industry==industries[1]|fullData$Industry==industries[2]|fullData$Industry==industries[3]|fullData$Industry==industries[4]|
                             fullData$Industry==industries[5],fullData$xbiBeta,
                           ifelse(fullData$Industry==industries[6]|fullData$Industry==industries[7],fullData$xphBeta,"NA"))
fullData$pm3.mkt = ifelse(fullData$Industry==industries[1]|fullData$Industry==industries[2]|fullData$Industry==industries[3]|fullData$Industry==industries[4]|
                             fullData$Industry==industries[5],fullData$pm3.xbi,
                           ifelse(fullData$Industry==industries[6]|fullData$Industry==industries[7],fullData$pm3.xph,"NA"))
fullData$pm3.beta = as.numeric(fullData$pm3.beta)
fullData$pm3.mkt = as.numeric(fullData$pm3.mkt)

catRets= matrix(data=NA,nrow=29,ncol=6)
rownames(catRets) = catalyst
colnames(catRets) = c("Mean","Median","Std Dev","Min","Max","Skew")
for (i in 1:length(catalyst)){
  temp = subset(fullData,catalyst_type==catalyst[i])
  catRets[i,1] = mean(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,2] = median(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,3] = sd(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,4] = min(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,5] = max(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,6] = skewness(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  rm(temp)
}

write.csv(catRets,"Deliverable 1 Page 0.csv")
colnames(fullData)


catRetSD= matrix(data=NA,nrow=29,ncol=6)
rownames(catRetSD) = catalyst
colnames(catRetSD) = c("Raw Mean","Raw Std Dev","Sector-Adj Mean","Sector-Adj Std Dev",
                      "Industry-Adj Mean","Industry-Adj Std Dev")
for (i in 1:length(catalyst)){
  temp = subset(fullData,catalyst_type==catalyst[i])
  catRetSD[i,1] = mean(temp$pm3.ret,na.rm=TRUE)
  catRetSD[i,2] = sd(temp$pm3.ret,na.rm=TRUE)
  catRetSD[i,3] = mean((temp$pm3.ret*temp$xlvBeta)-temp$pm3.xlv,na.rm=TRUE)
  catRetSD[i,4] = sd((temp$pm3.ret*temp$xlvBeta)-temp$pm3.xlv,na.rm=TRUE)
  catRetSD[i,5] = mean((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt,na.rm=TRUE)
  catRetSD[i,6] = sd((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt,na.rm=TRUE)
  rm(temp)
}


write.csv(fullData,"fullData.csv",row.names = FALSE)
write.csv(catRetSD,"Deliverable 1 Page 1.csv")


## Deliverable 2


retData = retData[,c(1:62)]

retData = merge(retData,subset(mktData,ticker=="XPH")[,c(2,13,19,22)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[63:65] = c("pm3.xph","pm1.xph","pm5.xph")
retData = merge(retData,subset(mktData,ticker=="XLV")[,c(2,13,19,22)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[66:68] = c("pm3.xlv","pm1.xlv","pm5.xlv")
retData = merge(retData,subset(mktData,ticker=="XBI")[,c(2,13,19,22)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[69:71] = c("pm3.xbi","pm1.xbi","pm5.xbi")
retData = merge(retData,subset(mktData,ticker=="IHI")[,c(2,13,19,22)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[72:74] = c("pm3.ihi","pm1.ihi","pm5.ihi")


retData$xphCov = NA
retData$xlvCov = NA
retData$xbiCov = NA
retData$ihiCov = NA
retData$xphBeta = NA
retData$xlvBeta = NA
retData$xbiBeta = NA
retData$ihiBeta = NA
for (i in 1:length(tickers)){
  tic()
  temp = subset(retData,ticker == tickers[i])
  retData = subset(retData,ticker != tickers[i])
  if (length(completeFun(temp,c("pm3.ret"))$date)>0){
    temp$xphCov = cov(temp$pm3.ret,temp$pm3.xph,use="complete.obs")
    temp$xlvCov = cov(temp$pm3.ret,temp$pm3.xlv,use="complete.obs")
    temp$xbiCov = cov(temp$pm3.ret,temp$pm3.xbi,use="complete.obs")
    temp$ihiCov = cov(temp$pm3.ret,temp$pm3.ihi,use="complete.obs")
    temp$xphBeta = temp$xphCov/var(temp$pm3.xph,na.rm=TRUE)
    temp$xlvBeta = temp$xlvCov/var(temp$pm3.xlv,na.rm=TRUE)
    temp$xbiBeta = temp$xbiCov/var(temp$pm3.xbi,na.rm=TRUE)
    temp$ihiBeta = temp$ihiCov/var(temp$pm3.ihi,na.rm=TRUE)
  }
  retData = rbind(retData,temp)
  rm(temp)
  toc()
}


fullData=fullData[,c(1:109)]

fullData = merge(fullData,retData[,c(1,4,63:85)],by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"),all.x=TRUE)
fullData = merge(fullData,etfData[,c(1,4:7)], by.x="acting_party_ticker", by.y="Symbol", all.x=TRUE)
fullData$pm3.beta = ifelse(fullData$Industry==industries[1]|fullData$Industry==industries[2]|fullData$Industry==industries[3]|fullData$Industry==industries[4]|
                             fullData$Industry==industries[5],fullData$xbiBeta,
                           ifelse(fullData$Industry==industries[6]|fullData$Industry==industries[7],fullData$xphBeta,"NA"))
fullData$pm3.mkt = ifelse(fullData$Industry==industries[1]|fullData$Industry==industries[2]|fullData$Industry==industries[3]|fullData$Industry==industries[4]|
                            fullData$Industry==industries[5],fullData$pm3.xbi,
                          ifelse(fullData$Industry==industries[6]|fullData$Industry==industries[7],fullData$pm3.xph,"NA"))
fullData$pm3.beta = as.numeric(fullData$pm3.beta)
fullData$pm3.mkt = as.numeric(fullData$pm3.mkt)

catRets= matrix(data=NA,nrow=29,ncol=6)
rownames(catRets) = catalyst
colnames(catRets) = c("Mean","Median","Std Dev","Min","Max","Skew")
for (i in 1:length(catalyst)){
  temp = subset(fullData,catalyst_type==catalyst[i])
  catRets[i,1] = mean(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,2] = median(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,3] = sd(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,4] = min(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,5] = max(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  catRets[i,6] = skewness(abs((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt),na.rm=TRUE)
  rm(temp)
}

write.csv(catRets,"Deliverable 1 Page 0.csv")
colnames(fullData)


catRetSD= matrix(data=NA,nrow=29,ncol=6)
catalyst = as.vector(unique(fullData$catalyst_type))
rownames(catRetSD) = catalyst
colnames(catRetSD) = c("Raw Mean","Raw Std Dev","Sector-Adj Mean","Sector-Adj Std Dev",
                       "Industry-Adj Mean","Industry-Adj Std Dev")
for (i in 1:length(catalyst)){
  temp = subset(fullData,catalyst_type==catalyst[i])
  catRetSD[i,1] = mean(temp$pm3.ret,na.rm=TRUE)
  catRetSD[i,2] = sd(temp$pm3.ret,na.rm=TRUE)
  catRetSD[i,3] = mean((temp$pm3.ret*temp$xlvBeta)-temp$pm3.xlv,na.rm=TRUE)
  catRetSD[i,4] = sd((temp$pm3.ret*temp$xlvBeta)-temp$pm3.xlv,na.rm=TRUE)
  catRetSD[i,5] = mean((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt,na.rm=TRUE)
  catRetSD[i,6] = sd((temp$pm3.ret*temp$pm3.beta)-temp$pm3.mkt,na.rm=TRUE)
  rm(temp)
}


write.csv(fullData,"fullData.csv",row.names = FALSE)
catRetSD = cbind(catalyst_type = rownames(catRetSD), catRetSD)
write.csv(catRetSD,"Deliverable 1 Page 1 v2.csv",row.names = FALSE)


# Find % of idiosyncratic returns that correspond to Parallel events
idioData = retData

# read in catalyst type for days where event occurs
idioData = merge(idioData,fullData[,c(1,2,5)],by.x=c("date","ticker"),by.y=c("date","acting_party_ticker"),all.x=TRUE)

Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
mktCapData = Quandl.datatable('SHARADAR/SF1', calendardate = dates[1,1], ticker = tickers[1], qopts.columns = c("ticker","calendardate","sharesbas"))
for (i in 1:length(tickers)){
  for (j in 1:length(dates$Dates)){
    tic()
    temp = Quandl.datatable('SHARADAR/SF1', calendardate = dates[j,1], ticker = tickers[i], qopts.columns = c("ticker","calendardate","sharesbas"))
    mktCapData = rbind(mktCapData,temp)
    rm(temp)
    toc()
  }
}
mktCapData = unique(mktCapData[,c(1:3)])


