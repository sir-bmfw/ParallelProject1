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


# time-series examples data: looking at SAGE and AMAG
tsExData = subset(idioData, ticker == "AMAG" | ticker == "SAGE")
colnames(tsExData)[86] = "cat.pm3"
fullData$date = as.Date(fullData$date, format="%Y-%m-%d")

# create +/- 1 day catalyst event overlap for deliverable
tsExData = left_join(tsExData, fullData[,c(1:2,4:5)], c("date","ticker" = "acting_party_ticker"))
tsExData$id = as.character(tsExData$id)
tsExData = tsExData[sort.list(tsExData$date),]
tsExData = tsExData[sort.list(tsExData$ticker),]
tsExData$lag1.id = ave(tsExData$id, tsExData$ticker, FUN = lg1)
tsExData = tsExData[sort.list(tsExData$date,decreasing=TRUE),]
tsExData = tsExData[sort.list(tsExData$ticker),]
tsExData$fwd1.id = ave(tsExData$id, tsExData$ticker, FUN = lg1)
tsExData = tsExData[sort.list(tsExData$date),]
tsExData = tsExData[sort.list(tsExData$ticker),]

tsExData$cat.pm1 = ifelse(is.na(tsExData$catalyst_type)==FALSE,tsExData$catalyst_type,ifelse(is.na(tsExData$fwd1.cat)==FALSE,tsExData$fwd1.cat,
                                ifelse(is.na(tsExData$lag1.cat)==FALSE,tsExData$lag1.cat,NA)))
tsExData$id.pm1 = ifelse(is.na(tsExData$id)==FALSE,tsExData$id,ifelse(is.na(tsExData$fwd1.id)==FALSE,tsExData$fwd1.id,
                                ifelse(is.na(tsExData$lag1.id)==FALSE,tsExData$lag1.id,NA)))

# re-calc covariance and beta for +/-1 day returns instead of +/-3
tic = unique(idioData$ticker)
idioData$pm1.mean = NA
idioData$pm1.sd = NA
idioData$pm2.mean = NA
idioData$pm2.sd = NA
idioData$pm3.mean = NA
idioData$pm3.sd = NA
for (i in 1:length(tic)){
  tic()
  temp = subset(idioData,ticker == tic[i])
  idioData = subset(idioData,ticker != tic[i])
  if (length(completeFun(temp,c("pm1.ret"))$date)>0){
    temp$pm1.mean = mean(temp$pm1.ret-(temp$xlvBeta*temp$pm1.xlv),na.rm=TRUE)
    temp$pm1.sd = sd(temp$pm1.ret-(temp$xlvBeta*temp$pm1.xlv),na.rm=TRUE)
    temp$pm2.mean = mean(temp$pm2.ret-(temp$xlvBeta*temp$pm2.xlv),na.rm=TRUE)
    temp$pm2.sd = sd(temp$pm2.ret-(temp$xlvBeta*temp$pm2.xlv),na.rm=TRUE)
    temp$pm3.mean = mean(temp$pm3.ret-(temp$xlvBeta*temp$pm3.xlv),na.rm=TRUE)
    temp$pm3.sd = sd(temp$pm3.ret-(temp$xlvBeta*temp$pm3.xlv),na.rm=TRUE)
  }
  idioData = rbind(idioData,temp)
  rm(temp)
  toc()
}

tsExRets = subset(tsExData,(pm1.ret-xlvBeta*pm1.xlv) >= mean+sd | (pm1.ret-xlvBeta*pm1.xlv) <= mean-sd)

## some stats
# % of events coverage in total subset: 21.3%
# % of events coverage within 1 sd: 18.6%
# % of events coverage greater than 1 sd (449): 35.6% (50.7% AMAG, 6.0% SAGE)
# % of events coverage greater than 2 sd (79): 70.9% (82.8% AMAG, 38.1% SAGE)
# % of events coverage greater than 3 sd (36): 94.4% (100% AMAG, 75% SAGE)

twoCoIOT = tsExRets[,c("date","ticker","id.pm1","cat.pm1","pm1.ret","xlvBeta","pm1.xlv","mean","sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT$catalyst_type = catalyst[as.numeric(twoCoIOT$cat.pm1)]
twoCoIOT$abbyRet = twoCoIOT$pm1.ret-twoCoIOT$xlvBeta*twoCoIOT$pm1.xlv
twoCoIOT$abby3sd = ifelse(twoCoIOT$abbyRet >= twoCoIOT$mean+3*twoCoIOT$sd | twoCoIOT$abbyRet <= twoCoIOT$mean-3*twoCoIOT$sd,
                          sprintf("%1.2f%%",twoCoIOT$abbyRet*100),"")
twoCoIOT$abby2sd = ifelse(twoCoIOT$abby3sd=="" & (twoCoIOT$abbyRet >= twoCoIOT$mean+2*twoCoIOT$sd | twoCoIOT$abbyRet <= twoCoIOT$mean-2*twoCoIOT$sd),
                          sprintf("%1.2f%%",twoCoIOT$abbyRet*100),"")
twoCoIOT$abby1sd = ifelse(twoCoIOT$abby3sd=="" & twoCoIOT$abby2sd=="" &
                            (twoCoIOT$abbyRet >= twoCoIOT$mean+twoCoIOT$sd | twoCoIOT$abbyRet <= twoCoIOT$mean-twoCoIOT$sd),
                          sprintf("%1.2f%%",twoCoIOT$abbyRet*100),"")
twoCoIOT = twoCoIOT[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm1","catalyst_type")]
twoCoIOT = unique(twoCoIOT)

xyplot((pm1.ret-(xlvBeta*pm1.xlv))~date,type='p',groups=ticker,data=tsExData,auto.key=T)

plot(subset(tsExData,is.na(cat.pm1)==FALSE)$date,(subset(tsExData, is.na(cat.pm1)==FALSE)$pm1.ret-
                                                    subset(tsExData, is.na(cat.pm1)==FALSE)$xlvBeta*
                                                    subset(tsExData, is.na(cat.pm1)==FALSE)$pm1.xlv))

write.csv(twoCoIOT,"twoCoIOT.csv",row.names=FALSE)


## ok, the same thing as above, but for the whole data set
colnames(idioData)[86] = "cat.pm3"

# create +/- 1 day catalyst event overlap for deliverable
idioData = left_join(idioData, fullData[,c(1:2,4:5)], c("date","ticker" = "acting_party_ticker"))
idioData$id = as.character(idioData$id)
idioData = idioData[sort.list(idioData$date),]
idioData = idioData[sort.list(idioData$ticker),]
idioData$lag1.id = ave(idioData$id, idioData$ticker, FUN = lg1)
idioData$lag2.id = ave(idioData$id, idioData$ticker, FUN = lg2)
idioData$lag3.id = ave(idioData$id, idioData$ticker, FUN = lg3)
idioData = idioData[sort.list(idioData$date,decreasing=TRUE),]
idioData = idioData[sort.list(idioData$ticker),]
idioData$fwd1.id = ave(idioData$id, idioData$ticker, FUN = lg1)
idioData$fwd2.id = ave(idioData$id, idioData$ticker, FUN = lg2)
idioData$fwd3.id = ave(idioData$id, idioData$ticker, FUN = lg3)
idioData = idioData[sort.list(idioData$date),]
idioData = idioData[sort.list(idioData$ticker),]

idioData = merge(idioData,subset(mktData,ticker=="XLV")[,c(2,25)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(idioData)[111] = c("pm2.xlv")

idioData$catalyst_type = as.character(idioData$catalyst_type)
catalyst = catalyst[sort.list(catalyst)]
idioData$lag1.cat = catalyst[as.numeric(idioData$lag1.cat)]
idioData$lag2.cat = catalyst[as.numeric(idioData$lag2.cat)]
idioData$lag3.cat = catalyst[as.numeric(idioData$lag3.cat)]
idioData$fwd1.cat = catalyst[as.numeric(idioData$fwd1.cat)]
idioData$fwd2.cat = catalyst[as.numeric(idioData$fwd2.cat)]
idioData$fwd3.cat = catalyst[as.numeric(idioData$fwd3.cat)]
idioData$cat.pm1 = ifelse(is.na(idioData$catalyst_type)==FALSE,idioData$catalyst_type,ifelse(is.na(idioData$fwd1.cat)==FALSE,idioData$fwd1.cat,
                                 ifelse(is.na(idioData$lag1.cat)==FALSE,idioData$lag1.cat,NA)))
idioData$id.pm1 = ifelse(is.na(idioData$id)==FALSE,idioData$id,ifelse(is.na(idioData$fwd1.id)==FALSE,idioData$fwd1.id,
                                 ifelse(is.na(idioData$lag1.id)==FALSE,idioData$lag1.id,NA)))
idioData$cat.pm2 = ifelse(is.na(idioData$catalyst_type)==FALSE,idioData$catalyst_type,ifelse(is.na(idioData$fwd1.cat)==FALSE,idioData$fwd1.cat,
                                 ifelse(is.na(idioData$lag1.cat)==FALSE,idioData$lag1.cat,ifelse(is.na(idioData$fwd2.cat)==FALSE,idioData$fwd2.cat,
                                 ifelse(is.na(idioData$lag2.cat)==FALSE,idioData$lag2.cat,NA)))))
idioData$id.pm2 = ifelse(is.na(idioData$id)==FALSE,idioData$id,ifelse(is.na(idioData$fwd1.id)==FALSE,idioData$fwd1.id,
                                 ifelse(is.na(idioData$lag1.id)==FALSE,idioData$lag1.id,ifelse(is.na(idioData$fwd2.id)==FALSE,idioData$fwd2.id,
                                 ifelse(is.na(idioData$lag2.id)==FALSE,idioData$lag2.id,NA)))))
idioData$cat.pm3 = ifelse(is.na(idioData$catalyst_type)==FALSE,idioData$catalyst_type,ifelse(is.na(idioData$fwd1.cat)==FALSE,idioData$fwd1.cat,
                                 ifelse(is.na(idioData$lag1.cat)==FALSE,idioData$lag1.cat,ifelse(is.na(idioData$fwd2.cat)==FALSE,idioData$fwd2.cat,
                                 ifelse(is.na(idioData$lag2.cat)==FALSE,idioData$lag2.cat,ifelse(is.na(idioData$fwd3.cat)==FALSE,idioData$fwd3.cat,
                                 ifelse(is.na(idioData$lag3.cat)==FALSE,idioData$lag3.cat,NA)))))))
idioData$id.pm3 = ifelse(is.na(idioData$id)==FALSE,idioData$id,ifelse(is.na(idioData$fwd1.id)==FALSE,idioData$fwd1.id,
                                 ifelse(is.na(idioData$lag1.id)==FALSE,idioData$lag1.id,ifelse(is.na(idioData$fwd2.id)==FALSE,idioData$fwd2.id,
                                 ifelse(is.na(idioData$lag2.id)==FALSE,idioData$lag2.id,ifelse(is.na(idioData$fwd3.id)==FALSE,idioData$fwd3.id,
                                 ifelse(is.na(idioData$lag3.id)==FALSE,idioData$lag3.id,NA)))))))



idioData$pm2.ret = idioData$fwd2.price / idioData$lag2.price - 1
tsExRets1 = subset(idioData,(ticker=="AMAG"|ticker=="SAGE")&((pm1.ret-xlvBeta*pm1.xlv) >= pm1.mean+pm1.sd | (pm1.ret-xlvBeta*pm1.xlv) <= pm1.mean-pm1.sd))
tsExRets2 = subset(idioData,(ticker=="AMAG"|ticker=="SAGE")&((pm2.ret-xlvBeta*pm2.xlv) >= pm2.mean+pm2.sd | (pm2.ret-xlvBeta*pm2.xlv) <= pm2.mean-pm2.sd))
tsExRets3 = subset(idioData,(ticker=="AMAG"|ticker=="SAGE")&((pm3.ret-xlvBeta*pm3.xlv) >= pm3.mean+pm3.sd | (pm3.ret-xlvBeta*pm3.xlv) <= pm3.mean-pm3.sd))

### some stats
## +/- 1-day abnormal returns
# % of events coverage in total subset: 21.3%
# % of events coverage within 1 sd: 18.6%
# % of events coverage greater than 1 sd (449): 35.6% (50.7% AMAG, 6.0% SAGE)
# % of events coverage greater than 2 sd (79): 70.9% (82.8% AMAG, 38.1% SAGE)
# % of events coverage greater than 3 sd (36): 94.4% (100% AMAG, 75% SAGE)
## +/- 2-day abnormal returns
# % of events coverage greater than 1 sd (799): 35.6% (50.7% AMAG, 6.0% SAGE)
# % of events coverage greater than 2 sd (79): 70.9% (82.8% AMAG, 38.1% SAGE)
# % of events coverage greater than 3 sd (36): 94.4% (100% AMAG, 75% SAGE)

twoCoIOT1 = tsExRets1[,c("date","ticker","id.pm1","cat.pm1","pm1.ret","xlvBeta","pm1.xlv","pm1.mean","pm1.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT1$abbyRet = twoCoIOT1$pm1.ret-twoCoIOT1$xlvBeta*twoCoIOT1$pm1.xlv
twoCoIOT1$abby3sd = ifelse(twoCoIOT1$abbyRet >= twoCoIOT1$pm1.mean+3*twoCoIOT1$pm1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$pm1.mean-3*twoCoIOT1$pm1.sd,
                          sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1$abby2sd = ifelse(twoCoIOT1$abby3sd=="" & (twoCoIOT1$abbyRet >= twoCoIOT1$pm1.mean+2*twoCoIOT1$pm1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$pm1.mean-2*twoCoIOT1$pm1.sd),
                          sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1$abby1sd = ifelse(twoCoIOT1$abby3sd=="" & twoCoIOT1$abby2sd=="" &
                            (twoCoIOT1$abbyRet >= twoCoIOT1$pm1.mean+twoCoIOT1$pm1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$pm1.mean-twoCoIOT1$pm1.sd),
                          sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1 = twoCoIOT1[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm1","cat.pm1")]
twoCoIOT1 = unique(twoCoIOT1)

twoCoIOT2 = tsExRets2[,c("date","ticker","id.pm2","cat.pm2","pm2.ret","xlvBeta","pm2.xlv","pm2.mean","pm2.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT2$abbyRet = twoCoIOT2$pm2.ret-twoCoIOT2$xlvBeta*twoCoIOT2$pm2.xlv
twoCoIOT2$abby3sd = ifelse(twoCoIOT2$abbyRet >= twoCoIOT2$pm2.mean+3*twoCoIOT2$pm2.sd | twoCoIOT2$abbyRet <= twoCoIOT2$pm2.mean-3*twoCoIOT2$pm2.sd,
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2$abby2sd = ifelse(twoCoIOT2$abby3sd=="" & (twoCoIOT2$abbyRet >= twoCoIOT2$pm2.mean+2*twoCoIOT2$pm2.sd | twoCoIOT2$abbyRet <= twoCoIOT2$pm2.mean-2*twoCoIOT2$pm2.sd),
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2$abby1sd = ifelse(twoCoIOT2$abby3sd=="" & twoCoIOT2$abby2sd=="" &
                             (twoCoIOT2$abbyRet >= twoCoIOT2$pm2.mean+twoCoIOT2$pm2.sd | twoCoIOT2$abbyRet <= twoCoIOT2$pm2.mean-twoCoIOT2$pm2.sd),
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2 = twoCoIOT2[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm2","cat.pm2")]
twoCoIOT2 = unique(twoCoIOT2)

twoCoIOT3 = tsExRets3[,c("date","ticker","id.pm3","cat.pm3","pm3.ret","xlvBeta","pm3.xlv","pm3.mean","pm3.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT3$abbyRet = twoCoIOT3$pm3.ret-twoCoIOT3$xlvBeta*twoCoIOT3$pm3.xlv
twoCoIOT3$abby3sd = ifelse(twoCoIOT3$abbyRet >= twoCoIOT3$pm3.mean+3*twoCoIOT3$pm3.sd | twoCoIOT3$abbyRet <= twoCoIOT3$pm3.mean-3*twoCoIOT3$pm3.sd,
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3$abby2sd = ifelse(twoCoIOT3$abby3sd=="" & (twoCoIOT3$abbyRet >= twoCoIOT3$pm3.mean+2*twoCoIOT3$pm3.sd | twoCoIOT3$abbyRet <= twoCoIOT3$pm3.mean-2*twoCoIOT3$pm3.sd),
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3$abby1sd = ifelse(twoCoIOT3$abby3sd=="" & twoCoIOT3$abby2sd=="" &
                             (twoCoIOT3$abbyRet >= twoCoIOT3$pm3.mean+twoCoIOT3$pm3.sd | twoCoIOT3$abbyRet <= twoCoIOT3$pm3.mean-twoCoIOT3$pm3.sd),
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3 = twoCoIOT3[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm3","cat.pm3")]
twoCoIOT3 = unique(twoCoIOT3)

write.csv(twoCoIOT1,"twoCoIOT1.csv",row.names=FALSE)
write.csv(twoCoIOT2,"twoCoIOT2.csv",row.names=FALSE)
write.csv(twoCoIOT3,"twoCoIOT3.csv",row.names=FALSE)



## entire return/company library 
tsAllRets1 = subset(idioData,(pm1.ret-xlvBeta*pm1.xlv) >= pm1.mean+pm1.sd | (pm1.ret-xlvBeta*pm1.xlv) <= pm1.mean-pm1.sd)
tsAllRets2 = subset(idioData,(pm2.ret-xlvBeta*pm2.xlv) >= pm2.mean+pm2.sd | (pm2.ret-xlvBeta*pm2.xlv) <= pm2.mean-pm2.sd)
tsAllRets3 = subset(idioData,(pm3.ret-xlvBeta*pm3.xlv) >= pm3.mean+pm3.sd | (pm3.ret-xlvBeta*pm3.xlv) <= pm3.mean-pm3.sd)

### some stats

allCoIOT1 = tsAllRets1[,c("date","ticker","id.pm1","cat.pm1","pm1.ret","xlvBeta","pm1.xlv","pm1.mean","pm1.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT1$abbyRet = allCoIOT1$pm1.ret-allCoIOT1$xlvBeta*allCoIOT1$pm1.xlv
allCoIOT1$abby3sd = ifelse(allCoIOT1$abbyRet >= allCoIOT1$pm1.mean+3*allCoIOT1$pm1.sd | allCoIOT1$abbyRet <= allCoIOT1$pm1.mean-3*allCoIOT1$pm1.sd,
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1$abby2sd = ifelse(allCoIOT1$abby3sd=="" & (allCoIOT1$abbyRet >= allCoIOT1$pm1.mean+2*allCoIOT1$pm1.sd | allCoIOT1$abbyRet <= allCoIOT1$pm1.mean-2*allCoIOT1$pm1.sd),
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1$abby1sd = ifelse(allCoIOT1$abby3sd=="" & allCoIOT1$abby2sd=="" &
                             (allCoIOT1$abbyRet >= allCoIOT1$pm1.mean+allCoIOT1$pm1.sd | allCoIOT1$abbyRet <= allCoIOT1$pm1.mean-allCoIOT1$pm1.sd),
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1 = allCoIOT1[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm1","cat.pm1")]
allCoIOT1 = unique(allCoIOT1)

allCoIOT2 = tsAllRets2[,c("date","ticker","id.pm2","cat.pm2","pm2.ret","xlvBeta","pm2.xlv","pm2.mean","pm2.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT2$abbyRet = allCoIOT2$pm2.ret-allCoIOT2$xlvBeta*allCoIOT2$pm2.xlv
allCoIOT2$abby3sd = ifelse(allCoIOT2$abbyRet >= allCoIOT2$pm2.mean+3*allCoIOT2$pm2.sd | allCoIOT2$abbyRet <= allCoIOT2$pm2.mean-3*allCoIOT2$pm2.sd,
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2$abby2sd = ifelse(allCoIOT2$abby3sd=="" & (allCoIOT2$abbyRet >= allCoIOT2$pm2.mean+2*allCoIOT2$pm2.sd | allCoIOT2$abbyRet <= allCoIOT2$pm2.mean-2*allCoIOT2$pm2.sd),
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2$abby1sd = ifelse(allCoIOT2$abby3sd=="" & allCoIOT2$abby2sd=="" &
                             (allCoIOT2$abbyRet >= allCoIOT2$pm2.mean+allCoIOT2$pm2.sd | allCoIOT2$abbyRet <= allCoIOT2$pm2.mean-allCoIOT2$pm2.sd),
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2 = allCoIOT2[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm2","cat.pm2")]
allCoIOT2 = unique(allCoIOT2)

allCoIOT3 = tsAllRets3[,c("date","ticker","id.pm3","cat.pm3","pm3.ret","xlvBeta","pm3.xlv","pm3.mean","pm3.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT3$abbyRet = allCoIOT3$pm3.ret-allCoIOT3$xlvBeta*allCoIOT3$pm3.xlv
allCoIOT3$abby3sd = ifelse(allCoIOT3$abbyRet >= allCoIOT3$pm3.mean+3*allCoIOT3$pm3.sd | allCoIOT3$abbyRet <= allCoIOT3$pm3.mean-3*allCoIOT3$pm3.sd,
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3$abby2sd = ifelse(allCoIOT3$abby3sd=="" & (allCoIOT3$abbyRet >= allCoIOT3$pm3.mean+2*allCoIOT3$pm3.sd | allCoIOT3$abbyRet <= allCoIOT3$pm3.mean-2*allCoIOT3$pm3.sd),
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3$abby1sd = ifelse(allCoIOT3$abby3sd=="" & allCoIOT3$abby2sd=="" &
                             (allCoIOT3$abbyRet >= allCoIOT3$pm3.mean+allCoIOT3$pm3.sd | allCoIOT3$abbyRet <= allCoIOT3$pm3.mean-allCoIOT3$pm3.sd),
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3 = allCoIOT3[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm3","cat.pm3")]
allCoIOT3 = unique(allCoIOT3)

write.csv(allCoIOT1,"allCoIOT1.csv",row.names=FALSE)
write.csv(allCoIOT2,"allCoIOT2.csv",row.names=FALSE)
write.csv(allCoIOT3,"allCoIOT3.csv",row.names=FALSE)


## Updating the output to only look at 1-day abnormal returns, but to see if an event occurred
# within a +/- 1,2,3 day window 

mktData = mktData[sort.list(mktData$date),]
mktData = mktData[sort.list(mktData$ticker),]
mktData$lag1.ret = mktData$open / mktData$lag1.price
idioData = merge(idioData,subset(mktData,ticker=="XLV")[,c(2,26)], by.x=c("date"), by.y=c("date"), all.x=TRUE)
colnames(idioData)[118] = c("lag1.xlv")

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

## entire return/company library 
tsAllRets = subset(idioData,((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) >= lag1.mean+lag1.sd | ((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) <= lag1.mean-lag1.sd)

allCoIOT1 = tsAllRets[,c("date","ticker","id.pm1","cat.pm1","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT1$abbyRet = (allCoIOT1$lag1.ret-1)-allCoIOT1$xlvBeta*(allCoIOT1$lag1.xlv-1)
allCoIOT1$abby3sd = ifelse(allCoIOT1$abbyRet >= allCoIOT1$lag1.mean+3*allCoIOT1$lag1.sd | allCoIOT1$abbyRet <= allCoIOT1$lag1.mean-3*allCoIOT1$lag1.sd,
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1$abby2sd = ifelse(allCoIOT1$abby3sd=="" & (allCoIOT1$abbyRet >= allCoIOT1$lag1.mean+2*allCoIOT1$lag1.sd | allCoIOT1$abbyRet <= allCoIOT1$lag1.mean-2*allCoIOT1$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1$abby1sd = ifelse(allCoIOT1$abby3sd=="" & allCoIOT1$abby2sd=="" &
                             (allCoIOT1$abbyRet >= allCoIOT1$lag1.mean+allCoIOT1$lag1.sd | allCoIOT1$abbyRet <= allCoIOT1$lag1.mean-allCoIOT1$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT1$abbyRet*100),"")
allCoIOT1 = allCoIOT1[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm1","cat.pm1")]
allCoIOT1 = unique(allCoIOT1)

allCoIOT2 = tsAllRets[,c("date","ticker","id.pm2","cat.pm2","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT2$abbyRet = (allCoIOT2$lag1.ret-1)-allCoIOT2$xlvBeta*(allCoIOT2$lag1.xlv-1)
allCoIOT2$abby3sd = ifelse(allCoIOT2$abbyRet >= allCoIOT2$lag1.mean+3*allCoIOT2$lag1.sd | allCoIOT2$abbyRet <= allCoIOT2$lag1.mean-3*allCoIOT2$lag1.sd,
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2$abby2sd = ifelse(allCoIOT2$abby3sd=="" & (allCoIOT2$abbyRet >= allCoIOT2$lag1.mean+2*allCoIOT2$lag1.sd | allCoIOT2$abbyRet <= allCoIOT2$lag1.mean-2*allCoIOT2$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2$abby1sd = ifelse(allCoIOT2$abby3sd=="" & allCoIOT2$abby2sd=="" &
                             (allCoIOT2$abbyRet >= allCoIOT2$lag1.mean+allCoIOT2$lag1.sd | allCoIOT2$abbyRet <= allCoIOT2$lag1.mean-allCoIOT2$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT2$abbyRet*100),"")
allCoIOT2 = allCoIOT2[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm2","cat.pm2")]
allCoIOT2 = unique(allCoIOT2)

allCoIOT3 = tsAllRets[,c("date","ticker","id.pm3","cat.pm3","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
allCoIOT3$abbyRet = (allCoIOT3$lag1.ret-1)-allCoIOT3$xlvBeta*(allCoIOT3$lag1.xlv-1)
allCoIOT3$abby3sd = ifelse(allCoIOT3$abbyRet >= allCoIOT3$lag1.mean+3*allCoIOT3$lag1.sd | allCoIOT3$abbyRet <= allCoIOT3$lag1.mean-3*allCoIOT3$lag1.sd,
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3$abby2sd = ifelse(allCoIOT3$abby3sd=="" & (allCoIOT3$abbyRet >= allCoIOT3$lag1.mean+2*allCoIOT3$lag1.sd | allCoIOT3$abbyRet <= allCoIOT3$lag1.mean-2*allCoIOT3$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3$abby1sd = ifelse(allCoIOT3$abby3sd=="" & allCoIOT3$abby2sd=="" &
                             (allCoIOT3$abbyRet >= allCoIOT3$lag1.mean+allCoIOT3$lag1.sd | allCoIOT3$abbyRet <= allCoIOT3$lag1.mean-allCoIOT3$lag1.sd),
                           sprintf("%1.2f%%",allCoIOT3$abbyRet*100),"")
allCoIOT3 = allCoIOT3[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm3","cat.pm3")]
allCoIOT3 = unique(allCoIOT3)

write.csv(allCoIOT1,"allCoIOT1.csv",row.names=FALSE)
write.csv(allCoIOT2,"allCoIOT2.csv",row.names=FALSE)
write.csv(allCoIOT3,"allCoIOT3.csv",row.names=FALSE)


## Redo two company set, will full SAGE set from sennt-over file

# time-series examples data: looking at SAGE and AMAG
tsExData = subset(idioData, ticker == "AMAG" | ticker == "SAGE")
sage = read.csv("SAGE-events.csv")
sage$ticker = as.character(sage$ticker)
sage$date = as.Date(sage$date, format = "%Y-%m-%d")
sage$event_id = as.character(sage$event_id)
colnames(sage)[3] = "id"
sage$catalyst_type = as.character(sage$catalyst_type)

temp = subset(tsExData,ticker == "SAGE")
tsExData = subset(tsExData,ticker != "SAGE")
for (i in 21:length(sage$ticker)){
  tic()
  for (j in 1:length(temp$date)){
    if (temp$date[j] == sage$date[i]){
      temp$id[j] = sage$id[i]
      temp$catalyst_type[j] = sage$catalyst_type[i]
      break
    }
  }
  toc()
}
tsExData = rbind(tsExData,temp)
rm(temp)

tsExData = tsExData[sort.list(tsExData$date),]
tsExData = tsExData[sort.list(tsExData$ticker),]
tsExData$lag1.id = ave(tsExData$id, tsExData$ticker, FUN = lg1)
tsExData$lag2.id = ave(tsExData$id, tsExData$ticker, FUN = lg2)
tsExData$lag3.id = ave(tsExData$id, tsExData$ticker, FUN = lg3)
tsExData$lag1.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg1)
tsExData$lag2.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg2)
tsExData$lag3.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg3)
tsExData = tsExData[sort.list(tsExData$date,decreasing=TRUE),]
tsExData = tsExData[sort.list(tsExData$ticker),]
tsExData$fwd1.id = ave(tsExData$id, tsExData$ticker, FUN = lg1)
tsExData$fwd2.id = ave(tsExData$id, tsExData$ticker, FUN = lg2)
tsExData$fwd3.id = ave(tsExData$id, tsExData$ticker, FUN = lg3)
tsExData$fwd1.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg1)
tsExData$fwd2.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg2)
tsExData$fwd3.cat = ave(tsExData$catalyst_type, tsExData$ticker, FUN = lg3)
tsExData = tsExData[sort.list(tsExData$date),]
tsExData = tsExData[sort.list(tsExData$ticker),]

tsExData$cat.pm1 = ifelse(is.na(tsExData$catalyst_type)==FALSE,tsExData$catalyst_type,ifelse(is.na(tsExData$fwd1.cat)==FALSE,tsExData$fwd1.cat,
                                                                                             ifelse(is.na(tsExData$lag1.cat)==FALSE,tsExData$lag1.cat,NA)))
tsExData$id.pm1 = ifelse(is.na(tsExData$id)==FALSE,tsExData$id,ifelse(is.na(tsExData$fwd1.id)==FALSE,tsExData$fwd1.id,
                                                                      ifelse(is.na(tsExData$lag1.id)==FALSE,tsExData$lag1.id,NA)))
tsExData$cat.pm2 = ifelse(is.na(tsExData$catalyst_type)==FALSE,tsExData$catalyst_type,ifelse(is.na(tsExData$fwd1.cat)==FALSE,tsExData$fwd1.cat,
                                                                                             ifelse(is.na(tsExData$lag1.cat)==FALSE,tsExData$lag1.cat,ifelse(is.na(tsExData$fwd2.cat)==FALSE,tsExData$fwd2.cat,
                                                                                                                                                             ifelse(is.na(tsExData$lag2.cat)==FALSE,tsExData$lag2.cat,NA)))))
tsExData$id.pm2 = ifelse(is.na(tsExData$id)==FALSE,tsExData$id,ifelse(is.na(tsExData$fwd1.id)==FALSE,tsExData$fwd1.id,
                                                                      ifelse(is.na(tsExData$lag1.id)==FALSE,tsExData$lag1.id,ifelse(is.na(tsExData$fwd2.id)==FALSE,tsExData$fwd2.id,
                                                                                                                                    ifelse(is.na(tsExData$lag2.id)==FALSE,tsExData$lag2.id,NA)))))
tsExData$cat.pm3 = ifelse(is.na(tsExData$catalyst_type)==FALSE,tsExData$catalyst_type,ifelse(is.na(tsExData$fwd1.cat)==FALSE,tsExData$fwd1.cat,
                                                                                             ifelse(is.na(tsExData$lag1.cat)==FALSE,tsExData$lag1.cat,ifelse(is.na(tsExData$fwd2.cat)==FALSE,tsExData$fwd2.cat,
                                                                                                                                                             ifelse(is.na(tsExData$lag2.cat)==FALSE,tsExData$lag2.cat,ifelse(is.na(tsExData$fwd3.cat)==FALSE,tsExData$fwd3.cat,
                                                                                                                                                                                                                             ifelse(is.na(tsExData$lag3.cat)==FALSE,tsExData$lag3.cat,NA)))))))
tsExData$id.pm3 = ifelse(is.na(tsExData$id)==FALSE,tsExData$id,ifelse(is.na(tsExData$fwd1.id)==FALSE,tsExData$fwd1.id,
                                                                      ifelse(is.na(tsExData$lag1.id)==FALSE,tsExData$lag1.id,ifelse(is.na(tsExData$fwd2.id)==FALSE,tsExData$fwd2.id,
                                                                                                                                    ifelse(is.na(tsExData$lag2.id)==FALSE,tsExData$lag2.id,ifelse(is.na(tsExData$fwd3.id)==FALSE,tsExData$fwd3.id,
                                                                                                                                                                                                  ifelse(is.na(tsExData$lag3.id)==FALSE,tsExData$lag3.id,NA)))))))


## entire return/company library 
tsExRets = subset(tsExData,((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) >= lag1.mean+lag1.sd | ((lag1.ret-1)-xlvBeta*(lag1.xlv-1)) <= lag1.mean-lag1.sd)

twoCoIOT1 = tsExRets[,c("date","ticker","id.pm1","cat.pm1","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT1$abbyRet = (twoCoIOT1$lag1.ret-1)-twoCoIOT1$xlvBeta*(twoCoIOT1$lag1.xlv-1)
twoCoIOT1$abby3sd = ifelse(twoCoIOT1$abbyRet >= twoCoIOT1$lag1.mean+3*twoCoIOT1$lag1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$lag1.mean-3*twoCoIOT1$lag1.sd,
                           sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1$abby2sd = ifelse(twoCoIOT1$abby3sd=="" & (twoCoIOT1$abbyRet >= twoCoIOT1$lag1.mean+2*twoCoIOT1$lag1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$lag1.mean-2*twoCoIOT1$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1$abby1sd = ifelse(twoCoIOT1$abby3sd=="" & twoCoIOT1$abby2sd=="" &
                             (twoCoIOT1$abbyRet >= twoCoIOT1$lag1.mean+twoCoIOT1$lag1.sd | twoCoIOT1$abbyRet <= twoCoIOT1$lag1.mean-twoCoIOT1$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT1$abbyRet*100),"")
twoCoIOT1 = twoCoIOT1[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm1","cat.pm1")]
twoCoIOT1 = unique(twoCoIOT1)

twoCoIOT2 = tsExRets[,c("date","ticker","id.pm2","cat.pm2","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT2$abbyRet = (twoCoIOT2$lag1.ret-1)-twoCoIOT2$xlvBeta*(twoCoIOT2$lag1.xlv-1)
twoCoIOT2$abby3sd = ifelse(twoCoIOT2$abbyRet >= twoCoIOT2$lag1.mean+3*twoCoIOT2$lag1.sd | twoCoIOT2$abbyRet <= twoCoIOT2$lag1.mean-3*twoCoIOT2$lag1.sd,
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2$abby2sd = ifelse(twoCoIOT2$abby3sd=="" & (twoCoIOT2$abbyRet >= twoCoIOT2$lag1.mean+2*twoCoIOT2$lag1.sd | twoCoIOT2$abbyRet <= twoCoIOT2$lag1.mean-2*twoCoIOT2$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2$abby1sd = ifelse(twoCoIOT2$abby3sd=="" & twoCoIOT2$abby2sd=="" &
                             (twoCoIOT2$abbyRet >= twoCoIOT2$lag1.mean+twoCoIOT2$lag1.sd | twoCoIOT2$abbyRet <= twoCoIOT2$lag1.mean-twoCoIOT2$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT2$abbyRet*100),"")
twoCoIOT2 = twoCoIOT2[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm2","cat.pm2")]
twoCoIOT2 = unique(twoCoIOT2)

twoCoIOT3 = tsExRets[,c("date","ticker","id.pm3","cat.pm3","lag1.ret","xlvBeta","lag1.xlv","lag1.mean","lag1.sd")]
catalyst = catalyst[sort.list(catalyst)]
twoCoIOT3$abbyRet = (twoCoIOT3$lag1.ret-1)-twoCoIOT3$xlvBeta*(twoCoIOT3$lag1.xlv-1)
twoCoIOT3$abby3sd = ifelse(twoCoIOT3$abbyRet >= twoCoIOT3$lag1.mean+3*twoCoIOT3$lag1.sd | twoCoIOT3$abbyRet <= twoCoIOT3$lag1.mean-3*twoCoIOT3$lag1.sd,
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3$abby2sd = ifelse(twoCoIOT3$abby3sd=="" & (twoCoIOT3$abbyRet >= twoCoIOT3$lag1.mean+2*twoCoIOT3$lag1.sd | twoCoIOT3$abbyRet <= twoCoIOT3$lag1.mean-2*twoCoIOT3$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3$abby1sd = ifelse(twoCoIOT3$abby3sd=="" & twoCoIOT3$abby2sd=="" &
                             (twoCoIOT3$abbyRet >= twoCoIOT3$lag1.mean+twoCoIOT3$lag1.sd | twoCoIOT3$abbyRet <= twoCoIOT3$lag1.mean-twoCoIOT3$lag1.sd),
                           sprintf("%1.2f%%",twoCoIOT3$abbyRet*100),"")
twoCoIOT3 = twoCoIOT3[,c("date","ticker","abby1sd","abby2sd","abby3sd","id.pm3","cat.pm3")]
twoCoIOT3 = unique(twoCoIOT3)

write.csv(twoCoIOT1,"twoCoIOT1.csv",row.names=FALSE)
write.csv(twoCoIOT2,"twoCoIOT2.csv",row.names=FALSE)
write.csv(twoCoIOT3,"twoCoIOT3.csv",row.names=FALSE)

