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


# Two Sigma data read-in, datetime adjustments
data = read.csv("Data Pulls/_two_sigma_events_clinical_trial_events_201810081910.csv")
data$future_date_date = gsub("T00:00:00+","",data$future_date_date)
data$future_date_date = as.Date(data$future_date_date, format="%Y-%m-%d")
data$date = as.POSIXct(data$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")
data$date = format(data$date,tz="America/New_York")
data$time_until_future_date = data$future_date_date-data$date
names(data)[1] ='datetime'
data$date = as.Date(data$datetime)
data$time = format(as.POSIXct(data$datetime), format= "%H:%M:%S")


# split up piped items into individual instances, recombine
secData = data[1,]
for (i in c(4:5,12,18:19)){
  temp = data[grepl("\\|",data[,i])==TRUE,]
  secData = rbind(secData,temp)
  rm(temp)
}
secData = secData[2:length(secData$date),]
secData = unique(secData)
secData = as.data.frame(lapply(secData, function(y) gsub(".*\\|", "", y)))

data = as.data.frame(lapply(data, function(y) gsub("\\|.*", "", y)))
data = rbind(data,secData)

# split ticker and exchange info into separate columns
data$acting_party_exchange = data$acting_party_ticker
data$acting_party_ticker = sub(".*:", "", data$acting_party_ticker)
data$acting_party_exchange = sub(":.*", "", data$acting_party_exchange)
data$phase_phase[data$phase_phase == 'first'] = "1"

# Bring in Quandl time series data
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
dates = as.Date(as.Date("2018-10-16"):as.Date("2012-01-01"), origin="1970-01-01")
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
write.csv(dates,"datevector.csv")
# Dates were exported and combined into strings in Excel to expedite the upload process 
dates = as.vector(read.csv("Data Pulls/datevector.csv"))
tickers = unique(data$acting_party_ticker)
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
retData$fwdm1.price = ave(retData$close, retData$ticker, FUN = lg1)

# add forward returns
retData$fwdm1.ret = retData$open / retData$fwdm1.price
retData$fwdm1alt.ret = retData$close / retData$fwdm1.price
retData$fwd0.ret = retData$close / retData$open
retData$fwd1.ret = retData$fwd1.price / retData$close
retData$fwd5.ret = retData$fwd5.price / retData$close
retData$fwd21.ret = retData$fwd21.price / retData$close
retData$fwd63.ret = retData$fwd63.price / retData$close
retData$fwd126.ret = retData$fwd126.price / retData$close
retData$fwd252.ret = retData$fwd252.price / retData$close


# add returns to two sigma data
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]

data$date = as.Date(data$date, format="%Y-%m-%d")
data = merge(data, retData[,c(1:2,18:26)], by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"), all.x=TRUE)


# Determine if announcement was made before, during, or after market hours
data$time = format(as.POSIXct(data$datetime), format= "%H:%M:%S")
data$mkt_announce = if_else(data$time < "09:00:00","Pre",if_else(data$time >= "16:00:00", "Post","During"))



# store data as-is for additional returns to be added in (dataTotal)
# create subset of data that contains at least 1-day returns (data)
dataTotal = data
data = data[!is.na(data$fwd1.ret),]
data$phase_phase[data$phase_phase == '3-Jan'] = '3'
data$phase_phase[data$phase_phase == '3(1'] = '3'
data$phase_phase[data$phase_phase == '2-Jan'] = '2'
data$phase_phase[data$phase_phase == 'three'] = '3'


# Look at immediate (next return) effect on return from announcement
data$t1.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1.ret,ifelse(data$mkt_announce == "During", data$fwd0.ret,ifelse(data$mkt_announce == "Post", data$fwd1.ret,"ERROR")))
t1.effect.aov = aov(t1.effect ~ topic_topic + result_type_result_type + action_tense + phase_subphase + result_type_is_topline + mkt_announce
                    + result_type_is_new + phase_phase + action_is_negated + action_predicate,data=data)
summary(t1.effect.aov)
t1.effect.lm = lm(t1.effect ~ topic_topic + result_type_result_type + action_tense + phase_subphase + result_type_is_topline + mkt_announce
                  + result_type_is_new + phase_phase + action_is_negated + action_predicate + acting_party_is_subsidiary,data=data)
summary(t1.effect.lm)

# Look at immediate (next return) effect on return from announcement
data$t1alt.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1alt.ret,ifelse(data$mkt_announce == "During", data$fwd0.ret,ifelse(data$mkt_announce == "Post", data$fwd1.ret,"ERROR")))
t1alt.effect.aov = aov(t1alt.effect ~ topic_topic + action_tense + result_type_result_type + phase_subphase + result_type_is_topline + mkt_announce
                    + result_type_is_new + phase_phase + action_is_negated + action_predicate,data=data)
summary(t1alt.effect.aov)
t1alt.effect.lm = lm(t1alt.effect ~ topic_topic + action_tense + result_type_result_type + phase_subphase + result_type_is_topline + mkt_announce
                     + result_type_is_new + phase_phase + action_is_negated + action_predicate + acting_party_is_subsidiary,data=data)
summary(t1alt.effect.lm)

## Having the results and action components separated out was too 'muddy'. Let's use the combined result and action factors instead and see if there's an improvement.

# Create ur-action and ur-result factors, test effects on 24h (aka t1) returns
data$action_factor = with(data, interaction(action_is_negated, action_predicate, action_tense))
data$result_factor = with(data, interaction(result_type_is_new, result_type_is_topline, result_type_result_type))

t1.effect2.aov = aov(t1.effect ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t1.effect2.aov)
t1.effect2.lm = lm(t1.effect ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t1.effect2.lm)
t1alt.effect2.aov = aov(t1alt.effect ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t1alt.effect2.aov)
t1alt.effect2.lm = lm(t1alt.effect ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t1alt.effect2.lm)

t5.effect.lm = lm(fwd5.ret ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t5.effect.lm)
t21.effect.lm = lm(fwd21.ret ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t21.effect.lm)
t63.effect.lm = lm(fwd63.ret ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t63.effect.lm)
t126.effect.lm = lm(fwd126.ret ~ topic_topic + result_factor + phase_subphase + phase_phase + mkt_announce + action_factor,data=data)
summary(t126.effect.lm)

##########################################################################################


## The below are additions to the code post the First Update meeting on 10/26/18

## One of the big adjustments to make to the returns is a market adjustment for the pharma industry.
## I'm going to use the SPDR S&P Pharmaceuticals ETF since it seems to be the most comprehensive pharma ETF that I've found so far.

# grab our market data
Quandl.api_key("8QPE9HZqNkwy9uwwyDq3")
mktData = Quandl.datatable('SHARADAR/SFP', date = dates[1,1], ticker = 'XPH')
for (j in 1:length(dates$Dates)){
  tic()
  temp = Quandl.datatable('SHARADAR/SFP', date = dates[j,1], ticker = 'XPH')
  mktData = rbind(mktData,temp)
  rm(temp)
  toc()
}
mktData = unique(mktData[,c(1:10)])

# organize market data, add in fwd returns
mktData = mktData[sort.list(mktData$date,decreasing=TRUE),]
mktData$fwd1.price = ave(mktData$close, mktData$ticker, FUN = lg1)
mktData$fwd1alt.price = ave(mktData$open, mktData$ticker, FUN = lg1)
mktData$fwd5.price = ave(mktData$close, mktData$ticker, FUN = lg5)
mktData$fwd21.price = ave(mktData$close, mktData$ticker, FUN = lg21)
mktData$fwd63.price = NA
for (i in 64:length(mktData$ticker)){
  if (mktData$ticker[i] == mktData$ticker[i-63]){
    mktData$fwd63.price[i] = mktData$close[i-63]
  }
}
mktData$fwd126.price = NA
for (i in 127:length(mktData$ticker)){
  if (mktData$ticker[i] == mktData$ticker[i-126]){
    mktData$fwd126.price[i] = mktData$close[i-126]
  }
}
mktData$fwd252.price = NA
for (i in 253:length(mktData$ticker)){
  if (mktData$ticker[i] == mktData$ticker[i-252]){
    mktData$fwd252.price[i] = mktData$close[i-252]
  }
}
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$fwdm1.price = ave(mktData$close, mktData$ticker, FUN = lg1)

# add forward returns
mktData$fwdm1.mkt = mktData$open / mktData$fwdm1.price
mktData$fwdm1alt.mkt = mktData$close / mktData$fwdm1.price
mktData$fwd0.mkt = mktData$close / mktData$open
mktData$fwd1.mkt = mktData$fwd1.price / mktData$close
mktData$fwd1alt.mkt = mktData$fwd1alt.price / mktData$close
mktData$fwd5.mkt = mktData$fwd5.price / mktData$close
mktData$fwd21.mkt = mktData$fwd21.price / mktData$close
mktData$fwd63.mkt = mktData$fwd63.price / mktData$close
mktData$fwd126.mkt = mktData$fwd126.price / mktData$close
mktData$fwd252.mkt = mktData$fwd252.price / mktData$close


# add in the alternate fwd1 return (close to open)
retData = retData[sort.list(retData$date,decreasing=TRUE),]
retData = retData[sort.list(retData$ticker),]
retData$fwd1alt.price = ave(retData$open, retData$ticker, FUN = lg1)
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]
retData$fwd1alt.ret = retData$fwd1alt.price / retData$close

# add market data to returns data,, calculate market-adjusted returns for retData
mktData = mktData[sort.list(mktData$date),]
retData = merge(retData, mktData[,c(2,19:28)], by.x ="date", by.y="date", all.x=TRUE)

# calculate market-adjusted returns (mar)
retData$fwdm1.mar = retData$fwdm1.ret - retData$fwdm1.mkt + 1
retData$fwdm1alt.mar = retData$fwdm1alt.ret - retData$fwdm1alt.mkt + 1
retData$fwd0.mar = retData$fwd0.ret - retData$fwd0.mkt + 1
retData$fwd1.mar = retData$fwd1.ret - retData$fwd1.mkt + 1
retData$fwd1alt.mar = retData$fwd1alt.ret - retData$fwd1alt.mkt + 1
retData$fwd5.mar = retData$fwd5.ret - retData$fwd5.mkt + 1
retData$fwd21.mar = retData$fwd21.ret - retData$fwd21.mkt + 1
retData$fwd63.mar = retData$fwd63.ret - retData$fwd63.mkt + 1
retData$fwd126.mar = retData$fwd126.ret - retData$fwd126.mkt + 1
retData$fwd252.mar = retData$fwd252.ret - retData$fwd252.mkt + 1


# Avg Daily Volume for past month (placeholder for Market Cap)
retData = retData[sort.list(retData$date),]
retData = retData[sort.list(retData$ticker),]
retData$adv21 = NA
for (i in 1:length(retData$ticker)){
  if (retData$ticker[i] == retData$ticker[i-20]){
    retData$adv21[i] = mean(retData$volume[i]:retData$volume[i-20])
  }
}
## read in mar data into Two Sigma dataset
data = merge(data, retData[,c(1:2,39:49)], by.x=c("date","acting_party_ticker"),by.y=c("date","ticker"), all.x=TRUE)

# add in the four t1 effects (Original = fwdm1/fwd1, Alt 1 = fwdm1alt/fwd1, Alt 2 = fwdm1/fw1alt, Alt 3 = fwdm1alt/fwd1alt)
data$t1.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1.mar,ifelse(data$mkt_announce == "During", data$fwd0.mar,ifelse(data$mkt_announce == "Post", data$fwd1.mar,"ERROR")))
data$t1alt1.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1alt.mar,ifelse(data$mkt_announce == "During", data$fwd0.mar,ifelse(data$mkt_announce == "Post", data$fwd1.mar,"ERROR")))
data$t1alt2.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1.mar,ifelse(data$mkt_announce == "During", data$fwd0.mar,ifelse(data$mkt_announce == "Post", data$fwd1alt.mar,"ERROR")))
data$t1alt3.effect = ifelse(data$mkt_announce == "Pre", data$fwdm1alt.mar,ifelse(data$mkt_announce == "During", data$fwd0.mar,ifelse(data$mkt_announce == "Post", data$fwd1alt.mar,"ERROR")))

# add in phase factor, which combines phase and subphase
data$phase_factor = with(data, interaction(phase_phase, phase_subphase))


# returns on whole data set
t1.effect.aov = aov(t1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1.effect.aov)
t1.effect.lm = lm(t1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1.effect.lm)
t1alt1.effect.aov = aov(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt1.effect.aov)
t1alt1.effect.lm = lm(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt1.effect.lm)
t1alt2.effect.aov = aov(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt2.effect.aov)
t1alt2.effect.lm = lm(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt2.effect.lm)
t1alt3.effect.aov = aov(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt3.effect.aov)
t1alt3.effect.lm = lm(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t1alt3.effect.lm)


t5.effect.aov = aov(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t5.effect.aov)
t5.effect.lm = lm(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t5.effect.lm)
t21.effect.aov = aov(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t21.effect.aov)
t21.effect.lm = lm(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t21.effect.lm)
t63.effect.aov = aov(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t63.effect.aov)
t63.effect.lm = lm(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t63.effect.lm)
t126.effect.aov = aov(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t126.effect.aov)
t126.effect.lm = lm(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=data)
summary(t126.effect.lm)


# subsetting data based on the Unit Economic Companies sent over
capTickers = c("PTX","KRYS","VNDA","PCRX","MNK","HZNP","JAZZ","REGN","BMY","LLY")
dataCapExamples = subset(data, acting_party_ticker == capTickers[1])
for (i in 2:length(capTickers)){
  temp = subset(data, acting_party_ticker == capTickers[i])
  dataCapExamples = rbind(dataCapExamples,temp)
  rm(temp)
}
dataCapExamples$bucket = ifelse(dataCapExamples$acting_party_ticker == "PTX","1",ifelse(dataCapExamples$acting_party_ticker == "KRYS","1",
                                ifelse(dataCapExamples$acting_party_ticker == "VNDA","2",ifelse(dataCapExamples$acting_party_ticker == "PCRX","2",
                                ifelse(dataCapExamples$acting_party_ticker == "MNK","3",ifelse(dataCapExamples$acting_party_ticker == "HZNP","3",
                                ifelse(dataCapExamples$acting_party_ticker == "JAZZ","4",ifelse(dataCapExamples$acting_party_ticker == "REGN","4",
                                ifelse(dataCapExamples$acting_party_ticker == "BMY","5",ifelse(dataCapExamples$acting_party_ticker == "LLY","5","ERROR"))))))))))


# regression on returns
t1.effect.aov = aov(t1.effect ~ adv21 + bucket + result_factor + mkt_announce + topic_topic + phase_factor + action_factor,data=dataCapExamples)
summary(t1.effect.aov)
t1.effect.lm = lm(t1.effect ~ adv21 + bucket + result_factor + mkt_announce + topic_topic + phase_factor + action_factor,data=dataCapExamples)
summary(t1.effect.lm)
t1alt1.effect.aov = aov(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt1.effect.aov)
t1alt1.effect.lm = lm(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt1.effect.lm)
t1alt2.effect.aov = aov(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt2.effect.aov)
t1alt2.effect.lm = lm(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt2.effect.lm)
t1alt3.effect.aov = aov(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt3.effect.aov)
t1alt3.effect.lm = lm(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t1alt3.effect.lm)


t5.effect.aov = aov(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t5.effect.aov)
t5.effect.lm = lm(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t5.effect.lm)
t21.effect.aov = aov(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t21.effect.aov)
t21.effect.lm = lm(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t21.effect.lm)
t63.effect.aov = aov(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t63.effect.aov)
t63.effect.lm = lm(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t63.effect.lm)
t126.effect.aov = aov(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t126.effect.aov)
t126.effect.lm = lm(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21 + bucket,data=dataCapExamples)
summary(t126.effect.lm)


# 
write.csv(dataCapExamples,"Mkt Cap Examples 102918.csv")

temp = subset(dataCapExamples, bucket == "1")

t1.effect.aov = aov(t1.effect ~ adv21 + result_factor + mkt_announce,data=temp)
summary(t1.effect.aov)
t1.effect.lm = lm(t1.effect ~ adv21 + result_factor + mkt_announce + topic_topic + phase_factor + action_factor,data=temp)
summary(t1.effect.lm)
t1alt1.effect.aov = aov(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt1.effect.aov)
t1alt1.effect.lm = lm(t1alt1.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt1.effect.lm)
t1alt2.effect.aov = aov(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt2.effect.aov)
t1alt2.effect.lm = lm(t1alt2.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt2.effect.lm)
t1alt3.effect.aov = aov(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt3.effect.aov)
t1alt3.effect.lm = lm(t1alt3.effect ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t1alt3.effect.lm)


t5.effect.aov = aov(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t5.effect.aov)
t5.effect.lm = lm(fwd5.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t5.effect.lm)
t21.effect.aov = aov(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t21.effect.aov)
t21.effect.lm = lm(fwd21.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t21.effect.lm)
t63.effect.aov = aov(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t63.effect.aov)
t63.effect.lm = lm(fwd63.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t63.effect.lm)
t126.effect.aov = aov(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t126.effect.aov)
t126.effect.lm = lm(fwd126.mar ~ topic_topic + result_factor + phase_factor + mkt_announce + action_factor + adv21,data=temp)
summary(t126.effect.lm)

