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


## Reset the event data to just the core dimensions a la the full SAGE set that was sent over
# last week. This will be used going forward for the Confounding Events deliverable(s)

# read in SAGE data
sage = read.csv("SAGE-events.csv")
sage$ticker = as.character(sage$ticker)
sage$date = as.Date(sage$date, format = "%Y-%m-%d")
sage$event_id = as.character(sage$event_id)
colnames(sage)[3] = "id"
sage$catalyst_type = as.character(sage$catalyst_type)

# Ok, assume that the full event data sets that will be sent over will be akin to this set.
# Looks like only ticker, date, id, and catalyst_type are the only metrics coming in. Let's
# alter the current set to only include the same (and we'll feed in the new SAGE data into it 
# as well).

confData = fullData[,c("ticker","trade_date","id","catalyst_type")]
#confData = subset(confData, acting_party_ticker != "SAGE" & acting_party_ticker != "")
confData$ticker = as.character(confData$ticker)
confData$id = as.character(confData$id)
confData$catalyst_type = as.character(confData$catalyst_type)
colnames(confData)[2] = "date"
confData$date = as.Date(confData$date,format="%Y-%m-%d")
confData = rbind(confData,sage)
confData = unique(confData)

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


write.csv(confData,"Confounding Event Deliverable Parts A B.csv", row.names = FALSE)


# make sure Betas and returns are ready to go before transfer over
mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$lag1.ret = mktData$close / mktData$lag1.price
idioData = merge(idioData,subset(mktData,ticker=="XLV")[,c(2,23)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(idioData)[96] = c("lag1.xlv")
colnames(idioData)[43] = "lag1.ret"
idioData = idioData[sort.list(idioData$date),]
idioData = idioData[sort.list(idioData$ticker),]
idioData$lag1.ret = idioData$close / idioData$lag1.price
tic = unique(idioData$ticker)

idioData$xlvCov = NA
idioData$xlvBeta = NA
idioData$lag1.mean = NA
idioData$lag1.sd = NA
for (i in 1:length(tic)){
  tic()
  temp = subset(idioData,ticker == tic[i])
  idioData = subset(idioData,ticker != tic[i])
  if (length(completeFun(temp,c("lag1.ret"))$date)>0){
    temp$xlvCov = cov((temp$lag1.ret-1),(temp$lag1.xlv-1),use="complete.obs")
    temp$xlvBeta = temp$xlvCov/var((temp$lag1.xlv-1),na.rm=TRUE)
    temp$lag1.mean = mean((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
    temp$lag1.sd = sd((temp$lag1.ret-1)-(temp$xlvBeta*(temp$lag1.xlv-1)),na.rm=TRUE)
  }
  idioData = rbind(idioData,temp)
  rm(temp)
  toc()
}


# add in return data
beta = unique(idioData[,c("ticker","xlvBeta","lag1.mean","lag1.sd")])
confData = left_join(confData,beta)
rm(beta)
mkt = unique(idioData[,c("date","lag1.xlv")])
confData = left_join(confData,mkt)
rm(mkt)
coRet = unique(idioData[,c("date","ticker","lag1.ret")])
confData = left_join(confData,coRet)
rm(coRet)

confData$absAbRet = abs((confData$lag1.ret-1)/(confData$xlvBeta*(confData$lag1.xlv-1)))

write.csv(confData,"Confounding Event Deliverable Part C.csv", row.names = FALSE)

