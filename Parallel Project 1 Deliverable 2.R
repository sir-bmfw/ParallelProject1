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


# grab our market data; we'll use the S&P 500 for the market since we're no longer just looking at Pharma companies
# Grabbing SPDR S&P 500
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


# organize market data, add in fwd returns
sp500Data = sp500Data[sort.list(sp500Data$date,decreasing=TRUE),]
sp500Data$fwd1.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg1)
sp500Data$fwd1alt.price = ave(sp500Data$open, sp500Data$ticker, FUN = lg1)
sp500Data$fwd2.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg2)
sp500Data$fwd3.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg3)
sp500Data$fwd4.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg4)
sp500Data$fwd5.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg5)
sp500Data$fwd21.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg21)
sp500Data$fwd63.price = NA
for (i in 64:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-63]){
    sp500Data$fwd63.price[i] = sp500Data$close[i-63]
  }
}
sp500Data$fwd126.price = NA
for (i in 127:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-126]){
    sp500Data$fwd126.price[i] = sp500Data$close[i-126]
  }
}
sp500Data$fwd252.price = NA
for (i in 253:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-252]){
    sp500Data$fwd252.price[i] = sp500Data$close[i-252]
  }
}
sp500Data = sp500Data[sort.list(sp500Data$date),]
sp500Data$lag1.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg1)
sp500Data$lag2.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg2)
sp500Data$lag3.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg3)
sp500Data$lag4.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg4)
sp500Data$lag5.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg5)
sp500Data$lag21.price = ave(sp500Data$close, sp500Data$ticker, FUN = lg21)
sp500Data$lag63.price = NA
for (i in 64:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-63]){
    sp500Data$lag63.price[i] = sp500Data$close[i-63]
  }
}
sp500Data$lag126.price = NA
for (i in 127:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-126]){
    sp500Data$lag126.price[i] = sp500Data$close[i-126]
  }
}
sp500Data$lag252.price = NA
for (i in 253:length(sp500Data$ticker)){
  if (sp500Data$ticker[i] == sp500Data$ticker[i-252]){
    sp500Data$lag252.price[i] = sp500Data$close[i-252]
  }
}


# add returns
sp500Data$fwd0.spy = sp500Data$close / sp500Data$open
sp500Data$fwd1.spy = sp500Data$fwd1.price / sp500Data$close
sp500Data$fwd1alt.spy = sp500Data$fwd1alt.price / sp500Data$close
sp500Data$fwd2.spy = sp500Data$fwd2.price / sp500Data$close
sp500Data$fwd3.spy = sp500Data$fwd3.price / sp500Data$close
sp500Data$fwd4.spy = sp500Data$fwd4.price / sp500Data$close
sp500Data$fwd5.spy = sp500Data$fwd5.price / sp500Data$close
sp500Data$fwd21.spy = sp500Data$fwd21.price / sp500Data$close
sp500Data$fwd63.spy = sp500Data$fwd63.price / sp500Data$close
sp500Data$fwd126.spy = sp500Data$fwd126.price / sp500Data$close
sp500Data$fwd252.spy = sp500Data$fwd252.price / sp500Data$close
sp500Data$lag1.spy = sp500Data$open / sp500Data$lag1.price
sp500Data$lag1alt.spy = sp500Data$close / sp500Data$lag1.price
sp500Data$lag2.spy = sp500Data$close / sp500Data$lag2.price
sp500Data$lag3.spy = sp500Data$close / sp500Data$lag3.price
sp500Data$lag4.spy = sp500Data$close / sp500Data$lag4.price
sp500Data$lag5.spy = sp500Data$close / sp500Data$lag5.price
sp500Data$lag21.spy = sp500Data$close / sp500Data$lag21.price
sp500Data$lag63.spy = sp500Data$close / sp500Data$lag63.price
sp500Data$lag126.spy = sp500Data$close / sp500Data$lag126.price
sp500Data$lag252.spy = sp500Data$close / sp500Data$lag252.price

fullData$date = as.Date(fullData$date, format="%Y-%m-%d")
fullData = merge(fullData, sp500Data[,c(2,30:50)], by.x=c("date"),by.y=c("date"), all.x=TRUE)


# calculate S&P500-Adjusted Returns (spar)
fullData$fwd0.spar = fullData$fwd0.ret - fullData$fwd0.spy + 1
fullData$fwd1.spar = fullData$fwd1.ret - fullData$fwd1.spy + 1
fullData$fwd1alt.spar = fullData$fwd1alt.ret - fullData$fwd1alt.spy + 1
fullData$fwd2.spar = fullData$fwd2.ret - fullData$fwd2.spy + 1
fullData$fwd3.spar = fullData$fwd3.ret - fullData$fwd3.spy + 1
fullData$fwd4.spar = fullData$fwd4.ret - fullData$fwd4.spy + 1
fullData$fwd5.spar = fullData$fwd5.ret - fullData$fwd5.spy + 1
fullData$fwd21.spar = fullData$fwd21.ret - fullData$fwd21.spy + 1
fullData$fwd63.spar = fullData$fwd63.ret - fullData$fwd63.spy + 1
fullData$fwd126.spar = fullData$fwd126.ret - fullData$fwd126.spy + 1
fullData$fwd252.spar = fullData$fwd252.ret - fullData$fwd252.spy + 1
fullData$lag1.spar = fullData$lag1.ret - fullData$lag1.spy + 1
fullData$lag1alt.spar = fullData$lag1alt.ret - fullData$lag1alt.spy + 1
fullData$lag2.spar = fullData$lag2.ret - fullData$lag2.spy + 1
fullData$lag3.spar = fullData$lag3.ret - fullData$lag3.spy + 1
fullData$lag4.spar = fullData$lag4.ret - fullData$lag4.spy + 1
fullData$lag5.spar = fullData$lag5.ret - fullData$lag5.spy + 1
fullData$lag21.spar = fullData$lag21.ret - fullData$lag21.spy + 1
fullData$lag63.spar = fullData$lag63.ret - fullData$lag63.spy + 1
fullData$lag126.spar = fullData$lag126.ret - fullData$lag126.spy + 1
fullData$lag252.spar = fullData$lag252.ret - fullData$lag252.spy + 1

# add in the four t1 effects (Original = lag1/fwd1, Alt 1 = lag1alt/fwd1, Alt 2 = lag1/fw1alt, Alt 3 = lag1alt/fwd1alt)
fullData$t1.spar = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1.spar,ifelse(fullData$mkt_announce == "During", fullData$fwd0.spar,ifelse(fullData$mkt_announce == "Post", fullData$fwd1.spar,"ERROR"))))
fullData$t1alt1.spar = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1alt.spar,ifelse(fullData$mkt_announce == "During", fullData$fwd0.spar,ifelse(fullData$mkt_announce == "Post", fullData$fwd1.spar,"ERROR"))))
fullData$t1alt2.spar = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1.spar,ifelse(fullData$mkt_announce == "During", fullData$fwd0.spar,ifelse(fullData$mkt_announce == "Post", fullData$fwd1alt.spar,"ERROR"))))
fullData$t1alt3.spar = as.numeric(ifelse(fullData$mkt_announce == "Pre", fullData$lag1alt.spar,ifelse(fullData$mkt_announce == "During", fullData$fwd0.spar,ifelse(fullData$mkt_announce == "Post", fullData$fwd1alt.spar,"ERROR"))))



# subsetting data based on the Unit Economic Companies sent over
capTickers = c("PTX","KRYS","VNDA","PCRX","MNK","HZNP","JAZZ","REGN","BMY","LLY")
dataCapExamples = subset(fullData, acting_party_ticker == capTickers[1])
for (i in 2:length(capTickers)){
  temp = subset(fullData, acting_party_ticker == capTickers[i])
  dataCapExamples = rbind(dataCapExamples,temp)
  rm(temp)
}
dataCapExamples$bucket = ifelse(dataCapExamples$acting_party_ticker == "PTX","1",ifelse(dataCapExamples$acting_party_ticker == "KRYS","1",
                                ifelse(dataCapExamples$acting_party_ticker == "VNDA","2",ifelse(dataCapExamples$acting_party_ticker == "PCRX","2",
                                ifelse(dataCapExamples$acting_party_ticker == "MNK","3",ifelse(dataCapExamples$acting_party_ticker == "HZNP","3",
                                ifelse(dataCapExamples$acting_party_ticker == "JAZZ","4",ifelse(dataCapExamples$acting_party_ticker == "REGN","4",
                                ifelse(dataCapExamples$acting_party_ticker == "BMY","5",ifelse(dataCapExamples$acting_party_ticker == "LLY","5","ERROR"))))))))))

write.csv(dataCapExamples,"dataCapExamplesP1D2.csv")
write.csv(fullData,"fullDataP1D2.csv")
dataCapExamples = read.csv("dataCapExamplesP1D2.csv")
fullDataEx = read.csv("fullDataP1D2.csv")

par(mfrow=c(4,10))
hist(subset(dataCapExamples,bucket=="1")$t1.effect, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1 return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="1")$t1.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1 adj-return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="2")$t1.effect, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1 return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="2")$t1.spar, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1 adj-return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="3")$t1.effect, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1 return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="3")$t1.spar, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1 adj-return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="4")$t1.effect, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1 return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="4")$t1.spar, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1 adj-return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="5")$t1.effect, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1 return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="5")$t1.spar, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1 adj-return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="1")$t1alt1.effect, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt1 return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="1")$t1alt1.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt1 adj-return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="2")$t1alt1.effect, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt1 return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="2")$t1alt1.spar, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt1 adj-return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="3")$t1alt1.effect, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt1 return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="3")$t1alt1.spar, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt1 adj-return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="4")$t1alt1.effect, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt1 return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="4")$t1alt1.spar, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt1 adj-return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="5")$t1alt1.effect, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt1 return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="5")$t1alt1.spar, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt1 adj-return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="1")$t1alt2.effect, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt2 return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="1")$t1alt2.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt2 adj-return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="2")$t1alt2.effect, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt2 return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="2")$t1alt2.spar, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt2 adj-return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="3")$t1alt2.effect, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt2 return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="3")$t1alt2.spar, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt2 adj-return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="4")$t1alt2.effect, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt2 return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="4")$t1alt2.spar, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt2 adj-return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="5")$t1alt2.effect, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt2 return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="5")$t1alt2.spar, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt2 adj-return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="1")$t1alt3.effect, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt3 return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="1")$t1alt3.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date), xlab="t1alt3 adj-return", main = "Bucket 1")
hist(subset(dataCapExamples,bucket=="2")$t1alt3.effect, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt3 return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="2")$t1alt3.spar, breaks = length(subset(dataCapExamples,bucket=="2")$date), xlab="t1alt3 adj-return", main = "Bucket 2")
hist(subset(dataCapExamples,bucket=="3")$t1alt3.effect, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt3 return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="3")$t1alt3.spar, breaks = length(subset(dataCapExamples,bucket=="3")$date), xlab="t1alt3 adj-return", main = "Bucket 3")
hist(subset(dataCapExamples,bucket=="4")$t1alt3.effect, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt3 return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="4")$t1alt3.spar, breaks = length(subset(dataCapExamples,bucket=="4")$date), xlab="t1alt3 adj-return", main = "Bucket 4")
hist(subset(dataCapExamples,bucket=="5")$t1alt3.effect, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt3 return", main = "Bucket 5")
hist(subset(dataCapExamples,bucket=="5")$t1alt3.spar, breaks = length(subset(dataCapExamples,bucket=="5")$date), xlab="t1alt3 adj-return", main = "Bucket 5")
par(mfrow=c(4,2))
hist(subset(dataCapExamples,bucket=="1")$fwd2.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd2.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd3.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd3.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd4.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd4.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd5.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd5.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
par(mfrow=c(4,2))
hist(subset(dataCapExamples,bucket=="1")$fwd21.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd21.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd63.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd63.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd126.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd126.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd252.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$fwd252.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
par(mfrow=c(2,2))
hist(subset(dataCapExamples,bucket=="1")$lag1.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag1.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag1alt.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag1alt.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
par(mfrow=c(4,2))
hist(subset(dataCapExamples,bucket=="1")$lag2.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag2.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag3.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag3.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag4.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag4.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag5.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag5.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
par(mfrow=c(4,2))
hist(subset(dataCapExamples,bucket=="1")$lag21.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag21.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag63.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag63.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag126.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag126.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag252.ret, breaks = length(subset(dataCapExamples,bucket=="1")$date))
hist(subset(dataCapExamples,bucket=="1")$lag252.spar, breaks = length(subset(dataCapExamples,bucket=="1")$date))

dataCapExamples$bucket = as.character(dataCapExamples$bucket)
summary(lm(t1.effect ~ bucket,data=dataCapExamples))
summary(lm(t1.spar ~ bucket,data=dataCapExamples))
summary(lm(t1alt1.effect ~ bucket,data=dataCapExamples))
summary(lm(t1alt1.spar ~ bucket,data=dataCapExamples))
summary(lm(t1alt2.effect ~ bucket,data=dataCapExamples))
summary(lm(t1alt2.spar ~ bucket,data=dataCapExamples))
summary(lm(t1alt3.effect ~ bucket,data=dataCapExamples))
summary(lm(t1alt3.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd2.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd2.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd3.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd3.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd4.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd4.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd5.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd5.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd21.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd21.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd63.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd63.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd126.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd126.spar ~ bucket,data=dataCapExamples))
summary(lm(fwd252.ret ~ bucket,data=dataCapExamples))
summary(lm(fwd252.spar ~ bucket,data=dataCapExamples))


summary(lm(lag1.ret ~ bucket,data=dataCapExamples))
summary(lm(lag1.spar ~ bucket,data=dataCapExamples))
summary(lm(lag2.ret ~ bucket,data=dataCapExamples))
summary(lm(lag2.spar ~ bucket,data=dataCapExamples))
summary(lm(lag3.ret ~ bucket,data=dataCapExamples))
summary(lm(lag3.spar ~ bucket,data=dataCapExamples))
summary(lm(lag4.ret ~ bucket,data=dataCapExamples))
summary(lm(lag4.spar ~ bucket,data=dataCapExamples))
summary(lm(lag5.ret ~ bucket,data=dataCapExamples))
summary(lm(lag5.spar ~ bucket,data=dataCapExamples))
summary(lm(lag21.ret ~ bucket,data=dataCapExamples))
summary(lm(lag21.spar ~ bucket,data=dataCapExamples))
summary(lm(lag63.ret ~ bucket,data=dataCapExamples))
summary(lm(lag63.spar ~ bucket,data=dataCapExamples))
summary(lm(lag126.ret ~ bucket,data=dataCapExamples))
summary(lm(lag126.spar ~ bucket,data=dataCapExamples))
summary(lm(lag252.ret ~ bucket,data=dataCapExamples))
summary(lm(lag252.spar ~ bucket,data=dataCapExamples))


summary(lm(t1.effect ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt1.effect ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt1.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt2.effect ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt2.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt3.effect ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(t1alt3.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd2.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd2.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd3.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd3.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd4.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd4.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd5.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd5.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd21.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd21.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd63.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd63.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd126.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd126.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd252.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(fwd252.spar ~ bucket + catalyst_type,data=dataCapExamples))

summary(lm(lag1.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag1.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag2.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag2.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag3.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag3.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag4.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag4.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag5.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag5.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag21.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag21.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag63.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag63.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag126.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag126.spar ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag252.ret ~ bucket + catalyst_type,data=dataCapExamples))
summary(lm(lag252.spar ~ bucket + catalyst_type,data=dataCapExamples))

summary(lm(t1.effect ~ catalyst_type,data=fullDataEx))
summary(lm(t1.spar ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt1.effect ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt1.spar ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt2.effect ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt2.spar ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt3.effect ~ catalyst_type,data=fullDataEx))
summary(lm(t1alt3.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd2.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd2.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd3.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd3.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd4.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd4.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd5.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd5.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd21.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd21.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd63.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd63.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd126.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd126.spar ~ catalyst_type,data=fullDataEx))
summary(lm(fwd252.ret ~ catalyst_type,data=fullDataEx))
summary(lm(fwd252.spar ~ catalyst_type,data=fullDataEx))

summary(lm(lag1.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag1.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag2.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag2.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag3.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag3.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag4.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag4.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag5.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag5.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag21.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag21.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag63.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag63.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag126.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag126.spar ~ catalyst_type,data=fullDataEx))
summary(lm(lag252.ret ~ catalyst_type,data=fullDataEx))
summary(lm(lag252.spar ~ catalyst_type,data=fullDataEx))



datelist = as.Date(as.Date("2018-10-16"):as.Date("2012-01-01"), origin="1970-01-01")
datelist = datelist[!weekdays(datelist) %in% c('Saturday','Sunday')]

cal =  create.calendar(name="mycal", weekdays=c("saturday", "sunday"))
with(dataCapExamples, ave(as.integer(date),acting_party_ticker, FUN=function(x) { 
  x = as.Date(x, origin="1970-01-01") 
  c(0, bizdays(head(x, -1), tail(x, -1), cal)) 
})) = dataCapExamples$prevEvent

dataCapExamples$nextEvent = 0
for (i in 1:(length(dataCapExamples$date)-1)){
  if(dataCapExamples$acting_party_ticker[i] == dataCapExamples$acting_party_ticker[i+1]){
    dataCapExamples$nextEvent[i] = dataCapExamples$prevEvent[i+1]
  }
}

temp = dataCapExamples[,c(1:5,100:159)]

dataCapExamples$Confounding = ifelse((dataCapExamples$prevEvent <= 4),"Test",
                                     ifelse((dataCapExamples$prevEvent > 4),"Control","ERROR"))

mean(subset(dataCapExamples, Confounding == "Control")$fwd5.spar, na.rm=TRUE)
sd(subset(dataCapExamples, Confounding == "Control")$fwd5.spar, na.rm=TRUE)
mean(subset(dataCapExamples, Confounding == "Test")$fwd5.spar, na.rm=TRUE)
sd(subset(dataCapExamples, Confounding == "Test")$fwd5.spar, na.rm=TRUE)
mean(subset(dataCapExamples, Confounding == "Control")$lag5.spar, na.rm=TRUE)
sd(subset(dataCapExamples, Confounding == "Control")$lag5.spar, na.rm=TRUE)
mean(subset(dataCapExamples, Confounding == "Test")$lag5.spar, na.rm=TRUE)
sd(subset(dataCapExamples, Confounding == "Test")$lag5.spar, na.rm=TRUE)

summary(lm(fwd0.spar ~ Confounding,data=dataCapExamples))
summary(lm(fwd1.spar ~ Confounding,data=dataCapExamples))
summary(lm(fwd2.spar ~ Confounding,data=dataCapExamples))
summary(lm(fwd3.spar ~ Confounding,data=dataCapExamples))
summary(lm(fwd4.spar ~ Confounding,data=dataCapExamples))
summary(lm(fwd5.spar ~ Confounding,data=dataCapExamples))
summary(lm(lag1.spar ~ Confounding,data=dataCapExamples))
summary(lm(lag2.spar ~ Confounding,data=dataCapExamples))
summary(lm(lag3.spar ~ Confounding,data=dataCapExamples))
summary(lm(lag4.spar ~ Confounding,data=dataCapExamples))
summary(lm(lag5.spar ~ Confounding,data=dataCapExamples))

coef(summary(lm(fwd0.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1"))))[8]
summary(lm(fwd1.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(fwd2.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(fwd3.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(fwd4.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(fwd5.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(lag1.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(lag2.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(lag3.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(lag4.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))
summary(lm(lag5.spar ~ Confounding,data=subset(dataCapExamples,bucket=="1")))

rets = c("fwd0.spar","fwd1.spar","fwd2.spar","fwd3.spar","fwd4.spar","fwd5.spar","lag1.spar","lag2.spar","lag3.spar","lag4.spar","lag5.spar")
catalyst = unique(dataCapExamples$catalyst_type)
sigOutputs = as.data.frame(matrix(data=NA,ncol=8))
colnames(sigOutputs) = c("Estimate","Std.Error","t value","p value","ret","confound","days","bucket")
for (i in 1:2){
  for (j in 5:1){
    if(i == 1){
      dataCapExamples$Confounding = ifelse((dataCapExamples$prevEvent <= j),"Test",
                                           ifelse((dataCapExamples$prevEvent > j),"Control","ERROR"))
      conf = "Confounder"
    } else {
      dataCapExamples$Confounding = ifelse((dataCapExamples$nextEvent <= j),"Test",
                                           ifelse((dataCapExamples$nextEvent > j),"Control","ERROR"))
      conf = "Confoundee"
    }
    for (k in 1:length(rets)){
      for (m in 1:5){
          if(coef(summary(lm(as.formula(paste0(rets[k], "~", "Confounding")),data=subset(dataCapExamples,bucket==m))))[8] <= 0.1){
          summ = summary(lm(as.formula(paste0(rets[k], "~", "Confounding")),data=subset(dataCapExamples,bucket==m)))
          output = as.data.frame(matrix(c(coef(summ)[2],coef(summ)[4],coef(summ)[6],coef(summ)[8],rets[k],conf,j,m),ncol=8))
          colnames(output) = c("Estimate","Std.Error","t value","p value","ret","confound","days","bucket")
          sigOutputs = rbind(sigOutputs,output)
          rm(output)
        }
      }
    }
  }
}
sigOutputs = sigOutputs[c(2:length(sigOutputs$Estimate)),]


fullData = fullData[sort.list(fullData$date),]
fullData = fullData[sort.list(fullData$acting_party_ticker),]

cal =  create.calendar(name="mycal", weekdays=c("saturday", "sunday"))
with(fullData, ave(as.integer(date), acting_party_ticker, FUN=function(x) { 
  x <- as.Date(x, origin="1970-01-01") 
  c(0, bizdays(head(x, -1), tail(x, -1), cal)) 
})) -> fullData$prevEvent

fullData$nextEvent = 0
for (i in 1:(length(fullData$date)-1)){
  if(fullData$acting_party_ticker[i] == fullData$acting_party_ticker[i+1]){
    fullData$nextEvent[i] = fullData$prevEvent[i+1]
  }
}

temp = fullData[,c(1:10,100:158)]

fullData$Confounding = ifelse((fullData$prevEvent <= 5),"Test",
                                     ifelse((fullData$prevEvent > 5),"Control","ERROR"))

rets = c("fwd0.spar","fwd1.spar","fwd2.spar","fwd3.spar","fwd4.spar","fwd5.spar","lag1.spar","lag2.spar","lag3.spar","lag4.spar","lag5.spar")
catalyst = unique(fullData$catalyst_type)
sigOutputsFull = as.data.frame(matrix(data=NA,ncol=8))
colnames(sigOutputsFull) = c("Estimate","Std.Error","t value","p value","ret","confound","days","catalyst")
for (i in 1:2){
  for (j in 5:1){
    if(i == 1){
      fullData$Confounding = ifelse((fullData$prevEvent <= j),"Test",
                                           ifelse((fullData$prevEvent > j),"Control","ERROR"))
      conf = "Confounder"
    } else {
      fullData$Confounding = ifelse((fullData$nextEvent <= j),"Test",
                                           ifelse((fullData$nextEvent > j),"Control","ERROR"))
      conf = "Confoundee"
    }
    for (k in 1:length(rets)){
      for (n in 1:length(catalyst)){
          if (length(subset(fullData,catalyst_type==catalyst[n])$date) > 0){
            a = subset(fullData,catalyst_type==catalyst[n])
            if (length(which(a$Confounding=="Test")) >1 & length(which(a$Confounding=="Control")) >1 ){
                if(coef(summary(lm(as.formula(paste0(rets[k], "~", "Confounding")),data=subset(fullData,catalyst_type==catalyst[n]))))[8] <= 0.1){
                  summ = summary(lm(as.formula(paste0(rets[k], "~", "Confounding")),data=subset(fullData,catalyst_type==catalyst[n])))
                  output = as.data.frame(matrix(c(coef(summ)[2],coef(summ)[4],coef(summ)[6],coef(summ)[8],rets[k],conf,j,catalyst[n]),ncol=8))
                  colnames(output) = c("Estimate","Std.Error","t value","p value","ret","confound","days","catalyst")
                  sigOutputsFull = rbind(sigOutputsFull,output)
                  rm(output)
            }
          }
        }
      }
    }
  }
}
sigOutputsFull = sigOutputsFull[c(2:length(sigOutputsFull$Estimate)),]

write.xlsx2(sigOutputs,file = "ParallelProject1Deliverable3.xlsx", sheetName = "Confounding Events Market Cap", row.names=TRUE)
write.xlsx2(sigOutputsFull,file = "ParallelProject1Deliverable3.xlsx", sheetName = "Confounding Events Catalysts", row.names=TRUE, append = TRUE)
write.xlsx2(catalyst,file = "ParallelProject1Deliverable3.xlsx", sheetName = "catalysts", row.names=TRUE, append = TRUE)




