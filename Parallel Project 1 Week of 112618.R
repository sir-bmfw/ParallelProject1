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

## add in all the other catalyst events

fullData = read.csv("two_sigma_events_2018_11_29_1242_clinical_trial_events_2018_11_29_1242.csv")
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_accounting_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_capital_structure_change_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_clinical_study_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_corporate_access_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_distress_bankruptcy_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_dividends_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_earnings_release_date_announced_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_earnings_results_released_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_guidance_debt_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_guidance_management_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_legal_class_action_and_investor_lawsuit_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_legal_other_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_management_change_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_medical_payer_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_mergers_acquisitions_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_natural_resources_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_not_applicable_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_operational_labor_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_operational_marketing_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_operational_partnerships_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_operational_ppe_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_product_launch_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_product_recalled_discontinued_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_product_update_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_regulatory_fda_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_regulatory_other_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_regulatory_patent_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_sales_agreement_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_sales_result_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_shareholder_meeting_events_2018_11_29_1242.csv"))
fullData = rbind.fill(fullData,read.csv("two_sigma_events_2018_11_29_1242_shareholder_related_events_2018_11_29_1242.csv"))
fullData$future_date_date = gsub("T00:00:00+","",fullData$future_date_date)
fullData$future_date_date = as.Date(fullData$future_date_date, format="%Y-%m-%d")
fullData$date = as.POSIXct(fullData$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
fullData$date = format(fullData$date,tz="America/New_York")
names(fullData)[1] ='datetime'
fullData$date = as.Date(fullData$datetime)
fullData$time = format(as.POSIXct(fullData$datetime), format= "%H:%M:%S")
fullData$time_until_future_date = fullData$future_date_date-fullData$date

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
rm(secData)

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
retData$pm3.ret = retData$fwd3.price / retData$lag3.price - 1
retData$pm1.ret = retData$fwd1.price / retData$lag1.price - 1
retData$pm5.ret = retData$fwd5.price / retData$lag5.price - 1

# add returns to two sigma data
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]

fullData$date = as.Date(fullData$date, format="%Y-%m-%d")
fullData = merge(fullData, retData[,c(1:2,24:41)], by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"), all.x=TRUE)

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
mktData$pm3.ret = mktData$fwd3.price / mktData$lag3.price - 1
mktData$pm1.ret = mktData$fwd1.price / mktData$lag1.price - 1
mktData$pm5.ret = mktData$fwd5.price / mktData$lag5.price - 1


confData = fullData[,c("acting_party_ticker","date","id","catalyst_type")]
confData$acting_party_ticker = as.character(confData$acting_party_ticker)
confData$id = as.character(confData$id)
confData$catalyst_type = as.character(confData$catalyst_type)
colnames(confData)[1] = "ticker"
confData$date = as.Date(confData$date,format="%Y-%m-%d")
confData = unique(confData)

tic = unique(retData$ticker)
eventData = retData[,c("date","ticker")]
eventData = merge(eventData,confData,all.x=TRUE)
eventData = unique(eventData)
eventData = eventData[sort.list(eventData$date),]
eventData = eventData[sort.list(eventData$ticker),]
eventData$lag1.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg1)
eventData$lag2.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg2)
eventData$lag3.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg3)
eventData = eventData[sort.list(eventData$date,decreasing=TRUE),]
eventData = eventData[sort.list(eventData$ticker),]
eventData$fwd1.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg1)
eventData$fwd2.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg2)
eventData$fwd3.date = ave(as.character(eventData$date), eventData$ticker, FUN = lg3)
eventData$lag1.date = as.Date(eventData$lag1.date,format="%Y-%m-%d")
eventData$lag2.date = as.Date(eventData$lag2.date,format="%Y-%m-%d")
eventData$lag3.date = as.Date(eventData$lag3.date,format="%Y-%m-%d")
eventData$fwd1.date = as.Date(eventData$fwd1.date,format="%Y-%m-%d")
eventData$fwd2.date = as.Date(eventData$fwd2.date,format="%Y-%m-%d")
eventData$fwd3.date = as.Date(eventData$fwd3.date,format="%Y-%m-%d")
eventData = left_join(eventData,confData, by=c("lag1.date" = "date","ticker"))
colnames(eventData)[11:12] = c("lag1.id","lag1.cat")
eventData = left_join(eventData,confData, by=c("fwd1.date" = "date","ticker"))
colnames(eventData)[13:14] = c("fwd1.id","fwd1.cat")
eventData = left_join(eventData,confData, by=c("lag2.date" = "date","ticker"))
colnames(eventData)[15:16] = c("lag2.id","lag2.cat")
eventData = left_join(eventData,confData, by=c("fwd2.date" = "date","ticker"))
colnames(eventData)[17:18] = c("fwd2.id","fwd2.cat")
eventData = left_join(eventData,confData, by=c("lag3.date" = "date","ticker"))
colnames(eventData)[19:20] = c("lag3.id","lag3.cat")
eventData = left_join(eventData,confData, by=c("fwd3.date" = "date","ticker"))
colnames(eventData)[21:22] = c("fwd3.id","fwd3.cat")
eventData = eventData[!duplicated(eventData[,c(1:4)]),]


confData = left_join(confData,eventData[,c("date","ticker","lag1.id","lag1.cat","fwd1.id","fwd1.cat",
                                           "lag2.id","lag2.cat","fwd2.id","fwd2.cat",
                                           "lag3.id","lag3.cat","fwd3.id","fwd3.cat")])
confData = unique(confData)
confData = confData[sort.list(confData$date),]
confData = confData[sort.list(confData$ticker),]


## Got lag and fwd events, but day-of is a bit trickier!
dayofconf = confData[,c(1:4)]
colnames(dayofconf)[3:4]=c("lag0.id","lag0.cat")

cd2 = left_join(confData,dayofconf)
cd2 = subset(cd2, catalyst_type!=lag0.cat & id != lag0.id)
cd2 = cd2[,c(1:4,17:18)]
cd2 = unique(cd2)

confData = left_join(confData,cd2)
rm(cd2)
confData = confData[!duplicated(confData[,c(1:4)]),]
dater = unique(idioData[,c("ticker","bucket")])
confData = left_join(confData,dater)
rm(dater)


# make sure Betas and returns are ready to go before transfer over
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
fullData = merge(fullData,subset(mktData,ticker=="XLV")[,c(2,32)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(fullData)[101:124]
colnames(fullData)[124] = c("lag1.xlv")
colnames(fullData)[109] = "lag1.ret"
fullData = fullData[sort.list(fullData$date),]
fullData = fullData[sort.list(fullData$acting_party_ticker),]

colnames(fullData)[2] = "ticker"
fullData$xlvCov = NA
fullData$xlvBeta = NA
fullData$lag1.mean = NA
fullData$lag1.sd = NA
for (i in 1:length(tic)){
  tic()
  temp = subset(fullData,ticker == tic[i])
  fullData = subset(fullData,ticker != tic[i])
  if (length(completeFun(temp,c("lag1.ret"))$date)>0){
    temp$xlvCov = cov((temp$lag1.ret-1),(temp$lag1.xlv-1),use="complete.obs")
    temp$xlvBeta = temp$xlvCov/var((temp$lag1.xlv-1),na.rm=TRUE)
    temp$lag1.mean = mean((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
    temp$lag1.sd = sd((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
  }
  fullData = rbind(fullData,temp)
  rm(temp)
  toc()
}


# add in return data
beta = unique(fullData[,c("ticker","xlvBeta","lag1.mean","lag1.sd")])
confData = left_join(confData,beta)
rm(beta)
mkt = unique(fullData[,c("date","lag1.xlv")])
confData = left_join(confData,mkt)
rm(mkt)
coRet = unique(fullData[,c("date","ticker","lag1.ret")])
confData = left_join(confData,coRet)
rm(coRet)

confData$absAbRet = abs((confData$lag1.ret-1)/(confData$xlvBeta*(confData$lag1.xlv-1)))

write.csv(confData,"Confounding Event Deliverable Part C NEW.csv", row.names = FALSE)



### Time to delve into future dates! Subset the full dataset to include only instances where a future date exists.
# returns data and market data
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
retData = merge(retData,subset(mktData,ticker=="XLV")[,c(2,32)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(retData)[1:43]
colnames(retData)[43] = c("lag1.xlv")
colnames(retData)[32] = "lag1.ret"
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]

retData$xlvCov = NA
retData$xlvBeta = NA
retData$lag1.mean = NA
retData$lag1.sd = NA
tic = unique(retData$ticker)
for (i in 1:length(tic)){
  tic()
  temp = subset(retData,ticker == tic[i])
  retData = subset(retData,ticker != tic[i])
  if (length(completeFun(temp,c("lag1.ret"))$date)>0){
    temp$xlvCov = cov((temp$lag1.ret-1),(temp$lag1.xlv-1),use="complete.obs")
    temp$xlvBeta = temp$xlvCov/var((temp$lag1.xlv-1),na.rm=TRUE)
    temp$lag1.mean = mean((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
    temp$lag1.sd = sd((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
  }
  retData = rbind(retData,temp)
  rm(temp)
  toc()
}

colnames(fullData)[12] = "ticker"
fullData$date = as.Date(fullData$date, format="%Y-%m-%d")
fullData$trade_date = as.Date(fullData$trade_date, format="%Y-%m-%d")
fullData = merge(fullData, retData[,c(1:2,24:43)], by.x=c("trade_date","ticker"),by.y=c("date","ticker"), all.x=TRUE)

# Determine if announcement was made before, during, or after market hours
fullData$time = format(as.POSIXct(fullData$datetime), format= "%H:%M:%S")


# make sure Betas and returns are ready to go before transfer over
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
fullData = merge(fullData,subset(mktData,ticker=="XLV")[,c(2,32)], by.x=c("trade_date"), by.y=c("date"), all.x=TRUE)
colnames(fullData)[101:120]
colnames(fullData)[121] = c("lag1.xlv")
colnames(fullData)[109] = "lag1.ret"
fullData = fullData[sort.list(fullData$trade_date),]
fullData = fullData[sort.list(fullData$ticker),]

fullData$future_date_date = as.Date(fullData$future_date_date, format="%Y-%m-%d")
futureData = subset(fullData, is.na(future_date_date)==FALSE & future_date_date < "2018-11-27" & future_date_date > date)
futureData = futureData[,c(1:100)]
futureData$isFuture = "Yes"

datelist = unique(retData$date)


futureData = futureData[,c(1:101)]
futureData = left_join(futureData,retData[,c(1:2,24:43)], by = c("future_date_date" = "date", "ticker"))
retData = unique(retData[,c(1:43)])
retData = left_join(retData, futureData[,c("id","catalyst_type","ticker","future_date_date","isFuture")], by = c("date" = "future_date_date", "ticker"))

## entire return/company library
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]
retData$id = as.character(retData$id)
retData$catalyst_type = as.character(retData$catalyst_type)
retData$lag1.id = ave(retData$id, retData$ticker, FUN = lg1)
retData$lag2.id = ave(retData$id, retData$ticker, FUN = lg2)
retData$lag3.id = ave(retData$id, retData$ticker, FUN = lg3)
retData$lag1.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg1)
retData$lag2.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg2)
retData$lag3.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg3)
retData = retData[sort.list(retData$date,decreasing=TRUE),]
retData = retData[sort.list(retData$ticker),]
retData$fwd1.id = ave(retData$id, retData$ticker, FUN = lg1)
retData$fwd2.id = ave(retData$id, retData$ticker, FUN = lg2)
retData$fwd3.id = ave(retData$id, retData$ticker, FUN = lg3)
retData$fwd1.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg1)
retData$fwd2.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg2)
retData$fwd3.cat = ave(retData$catalyst_type, retData$ticker, FUN = lg3)
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]

retData$cat.pm1 = ifelse(is.na(retData$catalyst_type)==FALSE,retData$catalyst_type,ifelse(is.na(retData$fwd1.cat)==FALSE,retData$fwd1.cat,
                          ifelse(is.na(retData$lag1.cat)==FALSE,retData$lag1.cat,NA)))
retData$id.pm1 = ifelse(is.na(retData$id)==FALSE,retData$id,ifelse(is.na(retData$fwd1.id)==FALSE,retData$fwd1.id,
                         ifelse(is.na(retData$lag1.id)==FALSE,retData$lag1.id,NA)))
retData$cat.pm2 = ifelse(is.na(retData$catalyst_type)==FALSE,retData$catalyst_type,ifelse(is.na(retData$fwd1.cat)==FALSE,retData$fwd1.cat,
                          ifelse(is.na(retData$lag1.cat)==FALSE,retData$lag1.cat,ifelse(is.na(retData$fwd2.cat)==FALSE,retData$fwd2.cat,
                          ifelse(is.na(retData$lag2.cat)==FALSE,retData$lag2.cat,NA)))))
retData$id.pm2 = ifelse(is.na(retData$id)==FALSE,retData$id,ifelse(is.na(retData$fwd1.id)==FALSE,retData$fwd1.id,
                         ifelse(is.na(retData$lag1.id)==FALSE,retData$lag1.id,ifelse(is.na(retData$fwd2.id)==FALSE,retData$fwd2.id,
                         ifelse(is.na(retData$lag2.id)==FALSE,retData$lag2.id,NA)))))
retData$cat.pm3 = ifelse(is.na(retData$catalyst_type)==FALSE,retData$catalyst_type,ifelse(is.na(retData$fwd1.cat)==FALSE,retData$fwd1.cat,
                          ifelse(is.na(retData$lag1.cat)==FALSE,retData$lag1.cat,ifelse(is.na(retData$fwd2.cat)==FALSE,retData$fwd2.cat,
                          ifelse(is.na(retData$lag2.cat)==FALSE,retData$lag2.cat,ifelse(is.na(retData$fwd3.cat)==FALSE,retData$fwd3.cat,
                          ifelse(is.na(retData$lag3.cat)==FALSE,retData$lag3.cat,NA)))))))
retData$id.pm3 = ifelse(is.na(retData$id)==FALSE,retData$id,ifelse(is.na(retData$fwd1.id)==FALSE,retData$fwd1.id,
                         ifelse(is.na(retData$lag1.id)==FALSE,retData$lag1.id,ifelse(is.na(retData$fwd2.id)==FALSE,retData$fwd2.id,
                         ifelse(is.na(retData$lag2.id)==FALSE,retData$lag2.id,ifelse(is.na(retData$fwd3.id)==FALSE,retData$fwd3.id,
                         ifelse(is.na(retData$lag3.id)==FALSE,retData$lag3.id,NA)))))))
futureData = left_join(futureData,retData[,c(1:2,46:63)], by = c("future_date_date" = "date", "ticker"))
futureData = unique(futureData)

# full data set for Present Events
fullData$isFuture = "No"
retData2 = unique(retData[,c(1:43)])
retData2 = left_join(retData2, fullData[,c("id","catalyst_type","ticker","trade_date","isFuture")], by = c("date" = "trade_date", "ticker"))

## entire return/company library
retData2 = retData2[sort.list(retData2$date),]
retData2 = retData2[sort.list(retData2$ticker),]
retData2$id = as.character(retData2$id)
retData2$catalyst_type = as.character(retData2$catalyst_type)
retData2$lag1.id = ave(retData2$id, retData2$ticker, FUN = lg1)
retData2$lag2.id = ave(retData2$id, retData2$ticker, FUN = lg2)
retData2$lag3.id = ave(retData2$id, retData2$ticker, FUN = lg3)
retData2$lag1.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg1)
retData2$lag2.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg2)
retData2$lag3.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg3)
retData2 = retData2[sort.list(retData2$date,decreasing=TRUE),]
retData2 = retData2[sort.list(retData2$ticker),]
retData2$fwd1.id = ave(retData2$id, retData2$ticker, FUN = lg1)
retData2$fwd2.id = ave(retData2$id, retData2$ticker, FUN = lg2)
retData2$fwd3.id = ave(retData2$id, retData2$ticker, FUN = lg3)
retData2$fwd1.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg1)
retData2$fwd2.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg2)
retData2$fwd3.cat = ave(retData2$catalyst_type, retData2$ticker, FUN = lg3)
retData2 = retData2[sort.list(retData2$date),]
retData2 = retData2[sort.list(retData2$ticker),]

retData2$cat.pm1 = ifelse(is.na(retData2$catalyst_type)==FALSE,retData2$catalyst_type,ifelse(is.na(retData2$fwd1.cat)==FALSE,retData2$fwd1.cat,
                         ifelse(is.na(retData2$lag1.cat)==FALSE,retData2$lag1.cat,NA)))
retData2$id.pm1 = ifelse(is.na(retData2$id)==FALSE,retData2$id,ifelse(is.na(retData2$fwd1.id)==FALSE,retData2$fwd1.id,
                        ifelse(is.na(retData2$lag1.id)==FALSE,retData2$lag1.id,NA)))
retData2$cat.pm2 = ifelse(is.na(retData2$catalyst_type)==FALSE,retData2$catalyst_type,ifelse(is.na(retData2$fwd1.cat)==FALSE,retData2$fwd1.cat,
                         ifelse(is.na(retData2$lag1.cat)==FALSE,retData2$lag1.cat,ifelse(is.na(retData2$fwd2.cat)==FALSE,retData2$fwd2.cat,
                         ifelse(is.na(retData2$lag2.cat)==FALSE,retData2$lag2.cat,NA)))))
retData2$id.pm2 = ifelse(is.na(retData2$id)==FALSE,retData2$id,ifelse(is.na(retData2$fwd1.id)==FALSE,retData2$fwd1.id,
                        ifelse(is.na(retData2$lag1.id)==FALSE,retData2$lag1.id,ifelse(is.na(retData2$fwd2.id)==FALSE,retData2$fwd2.id,
                        ifelse(is.na(retData2$lag2.id)==FALSE,retData2$lag2.id,NA)))))
retData2$cat.pm3 = ifelse(is.na(retData2$catalyst_type)==FALSE,retData2$catalyst_type,ifelse(is.na(retData2$fwd1.cat)==FALSE,retData2$fwd1.cat,
                         ifelse(is.na(retData2$lag1.cat)==FALSE,retData2$lag1.cat,ifelse(is.na(retData2$fwd2.cat)==FALSE,retData2$fwd2.cat,
                         ifelse(is.na(retData2$lag2.cat)==FALSE,retData2$lag2.cat,ifelse(is.na(retData2$fwd3.cat)==FALSE,retData2$fwd3.cat,
                         ifelse(is.na(retData2$lag3.cat)==FALSE,retData2$lag3.cat,NA)))))))
retData2$id.pm3 = ifelse(is.na(retData2$id)==FALSE,retData2$id,ifelse(is.na(retData2$fwd1.id)==FALSE,retData2$fwd1.id,
                        ifelse(is.na(retData2$lag1.id)==FALSE,retData2$lag1.id,ifelse(is.na(retData2$fwd2.id)==FALSE,retData2$fwd2.id,
                        ifelse(is.na(retData2$lag2.id)==FALSE,retData2$lag2.id,ifelse(is.na(retData2$fwd3.id)==FALSE,retData2$fwd3.id,
                        ifelse(is.na(retData2$lag3.id)==FALSE,retData2$lag3.id,NA)))))))


fullData = left_join(fullData,retData[,c(1:2,46:63)], by = c("trade_date" = "date", "ticker"))
fullData = fullData[sort.list(fullData$date),]
fullData = fullData[sort.list(fullData$ticker),]
fullData$isFuture = "No"
fullData = unique(fullData)
comboData = rbind(retData,retData2)
comboData = merge(comboData,fullData[,c("trade_date","ticker","id","catalyst_type","future_date_date")], by.x = c("date","ticker","id","catalyst_type"), by.y=c("trade_date","ticker","id","catalyst_type"), all.x=TRUE)
comboData = unique(comboData)
comboData = comboData[sort.list(comboData$isFuture,decreasing=TRUE),]
comboData = comboData[sort.list(comboData$date),]
comboData = comboData[sort.list(comboData$ticker),]
#presentData = subset(fullData, is.na(future_date_date)==FALSE & future_date_date < "2018-11-27" & future_date_date > date)

allTotal1 = comboData[,c("date","future_date_date","ticker","id.pm1","cat.pm1","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd","isFuture")]
allTotal1 = subset(allTotal1,((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) >= lag1.mean+lag1.sd | ((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) <= lag1.mean-lag1.sd)
allTotal1 = allTotal1[!is.na(allTotal1$lag1.ret),]
catalyst = catalyst[sort.list(catalyst)]
allTotal1$abbyRet = (allTotal1$lag1.ret-1)-allTotal1$xlvBeta*(allTotal1$lag1.xlv-1)
allTotal1$abby3sd = ifelse(allTotal1$abbyRet >= allTotal1$lag1.mean+3*allTotal1$lag1.sd | allTotal1$abbyRet <= allTotal1$lag1.mean-3*allTotal1$lag1.sd,
                           sprintf("%1.2f%%",allTotal1$abbyRet*100),"")
allTotal1$abby2sd = ifelse(allTotal1$abby3sd=="" & (allTotal1$abbyRet >= allTotal1$lag1.mean+2*allTotal1$lag1.sd | allTotal1$abbyRet <= allTotal1$lag1.mean-2*allTotal1$lag1.sd),
                           sprintf("%1.2f%%",allTotal1$abbyRet*100),"")
allTotal1$abby1sd = ifelse(allTotal1$abby3sd=="" & allTotal1$abby2sd=="" &
                             (allTotal1$abbyRet >= allTotal1$lag1.mean+allTotal1$lag1.sd | allTotal1$abbyRet <= allTotal1$lag1.mean-allTotal1$lag1.sd),
                           sprintf("%1.2f%%",allTotal1$abbyRet*100),"")
allTotal1 = allTotal1[,c("date","future_date_date","ticker","abby1sd","abby2sd","abby3sd","isFuture","id.pm1","cat.pm1")]
allTotal1 = allTotal1[!duplicated(allTotal1[,c("date","ticker","abby1sd","abby2sd","abby3sd")]),]
allTotal1 = unique(allTotal1)

allTotal2 = comboData[,c("date","future_date_date","ticker","id.pm2","cat.pm2","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd","isFuture")]
allTotal2 = subset(allTotal2,((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) >= lag1.mean+lag1.sd | ((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) <= lag1.mean-lag1.sd)
allTotal2 = allTotal2[!is.na(allTotal2$lag1.ret),]
catalyst = catalyst[sort.list(catalyst)]
allTotal2$abbyRet = (allTotal2$lag1.ret-1)-allTotal2$xlvBeta*(allTotal2$lag1.xlv-1)
allTotal2$abby3sd = ifelse(allTotal2$abbyRet >= allTotal2$lag1.mean+3*allTotal2$lag1.sd | allTotal2$abbyRet <= allTotal2$lag1.mean-3*allTotal2$lag1.sd,
                           sprintf("%1.2f%%",allTotal2$abbyRet*100),"")
allTotal2$abby2sd = ifelse(allTotal2$abby3sd=="" & (allTotal2$abbyRet >= allTotal2$lag1.mean+2*allTotal2$lag1.sd | allTotal2$abbyRet <= allTotal2$lag1.mean-2*allTotal2$lag1.sd),
                           sprintf("%1.2f%%",allTotal2$abbyRet*100),"")
allTotal2$abby1sd = ifelse(allTotal2$abby3sd=="" & allTotal2$abby2sd=="" &
                             (allTotal2$abbyRet >= allTotal2$lag1.mean+allTotal2$lag1.sd | allTotal2$abbyRet <= allTotal2$lag1.mean-allTotal2$lag1.sd),
                           sprintf("%1.2f%%",allTotal2$abbyRet*100),"")
allTotal2 = allTotal2[,c("date","future_date_date","ticker","abby1sd","abby2sd","abby3sd","isFuture","id.pm2","cat.pm2")]
allTotal2 = allTotal2[!duplicated(allTotal2[,c("date","ticker","abby1sd","abby2sd","abby3sd")]),]
allTotal2 = unique(allTotal2)

allTotal3 = comboData[,c("date","future_date_date","ticker","id.pm3","cat.pm3","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd","isFuture")]
allTotal3 = subset(allTotal3,((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) >= lag1.mean+lag1.sd | ((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) <= lag1.mean-lag1.sd)
allTotal3 = allTotal3[!is.na(allTotal3$lag1.ret),]
catalyst = catalyst[sort.list(catalyst)]
allTotal3$abbyRet = (allTotal3$lag1.ret-1)-allTotal3$xlvBeta*(allTotal3$lag1.xlv-1)
allTotal3$abby3sd = ifelse(allTotal3$abbyRet >= allTotal3$lag1.mean+3*allTotal3$lag1.sd | allTotal3$abbyRet <= allTotal3$lag1.mean-3*allTotal3$lag1.sd,
                           sprintf("%1.2f%%",allTotal3$abbyRet*100),"")
allTotal3$abby2sd = ifelse(allTotal3$abby3sd=="" & (allTotal3$abbyRet >= allTotal3$lag1.mean+2*allTotal3$lag1.sd | allTotal3$abbyRet <= allTotal3$lag1.mean-2*allTotal3$lag1.sd),
                           sprintf("%1.2f%%",allTotal3$abbyRet*100),"")
allTotal3$abby1sd = ifelse(allTotal3$abby3sd=="" & allTotal3$abby2sd=="" &
                             (allTotal3$abbyRet >= allTotal3$lag1.mean+allTotal3$lag1.sd | allTotal3$abbyRet <= allTotal3$lag1.mean-allTotal3$lag1.sd),
                           sprintf("%1.2f%%",allTotal3$abbyRet*100),"")
allTotal3 = allTotal3[,c("date","future_date_date","ticker","abby1sd","abby2sd","abby3sd","isFuture","id.pm3","cat.pm3")]
allTotal3 = allTotal3[!duplicated(allTotal3[,c("date","ticker","abby1sd","abby2sd","abby3sd")]),]
allTotal3 = unique(allTotal3)

write.csv(allTotal1,"allTotal1.csv",row.names=FALSE)
write.csv(allTotal2,"allTotal2.csv",row.names=FALSE)
write.csv(allTotal3,"allTotal3.csv",row.names=FALSE)

fullData = fullData[sort.list(fullData$date),]
start = fullData[!duplicated(fullData[,c("ticker")]),c("date","ticker")]
write.csv(start,"event data start.csv",row.names = FALSE)

# read in mkt cap buckets (approximation of buckets, non-time series)
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
mktCapData = Quandl.datatable('SHARADAR/TICKERS', ticker = tickers[1], qopts.columns = c("ticker","scalemarketcap"))
for (i in 1:length(tickers)){
  tic()
  temp = Quandl.datatable('SHARADAR/TICKERS', ticker = tickers[i], qopts.columns = c("ticker","scalemarketcap"))
  mktCapData = rbind(mktCapData,temp)
  rm(temp)
  toc()
}
mktCapData = unique(mktCapData[,c(1:2)])
write.csv(mktCapData,"mkt cap buckets.csv",row.names = FALSE)



