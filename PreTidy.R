library(tidyverse)
library(lubridate)
library(timeDate)


############ Input daily trading data ##############
datadir <- 'C:/Users/Hu/Documents/R/Data/'
filecd <- dir(datadir, pattern = '^TRD_Dalyr.*?.csv$') %>% paste0(datadir, .)
TRD <- data.frame()
for (i in filecd) {
  TRDS <- i %>% read_delim(delim='\t', na = '') %>%
    rename('TradingDate' = Trddt) %>%
    subset(select=c(Stkcd,TradingDate,Dretwd,Markettype,Trdsta))
  print(head(TRDS))
  TRD <- rbind(TRD,TRDS)
}
TRD$TradingDate <- as.Date(TRD$TradingDate,"%Y-%m-%d")
rm(i,filecd,TRDS);str(TRD)
# R^i minus R^f
ThrMonRf <- 0.011
Rf <- (1 + ThrMonRf)^(1/90) - 1
TRD$Dretwd <- round(TRD$Dretwd-Rf, 6)



############### Quarterly earnings report ###############
############### 03-31, the first quarter  ###############
############### 06-30, the second quarter ###############
############### 09-30, the third quarter  ###############
############### 12-31, the fourth quarter ###############
# for example, 2018-12-31 meanings that
# we concentrated on the fourth quarter of year 2018
QEAperd <- as.Date('2017-09-30') # key
TRD <- filter(TRD, TradingDate %within%
                interval(QEAperd %m+% months(-16),
                         QEAperd %m+% months(+6)))



################ Market type #################
#######  1=上海A, 2=上海B, 4=深圳A  ##########
###### 8=深圳B, 16=创业板, 32=科创板 #########
mkttype <- c(1,4,16) # key
TRD <- filter(TRD, Markettype %in% mkttype)



##### Select the stocks that have published ex-earnings report ####
## 0,选取之前发布业绩预告的企业
## 1,选取之前发布定期公告的企业
## c(0,1), 选取之前发布有信息的企业，即0、1两类企业之加和
## 2,选取之前毫无预告的企业，即从总样本中去除0、1两类企业
## 3,总样本中去除发布业绩预告的企业
## 4,总样本中去除发布定期公告的企业
## 若全部都要，输入5即可（实质上除上述数字皆可）
PreRept <- dir(datadir, pattern = 'ForecFin.csv$') %>%
  paste0(datadir, .) %>%
  read_delim(delim='\t', na = '') %>%
  select(StockCode, Source, PubliDate, AccPeriod) %>%
  filter(AccPeriod==QEAperd)

Pretype <- c(3) # key

if(Pretype %in% c(0,1)) {
  TRDSAM <- data.frame()
  Prestk <- filter(PreRept, Source %in% Pretype)
  for (i in Prestk$StockCode) {
    TRDaln <- filter(TRD, Stkcd==i)
    TRDSAM <- rbind(TRDSAM, TRDaln)
  }
  rm(TRDaln)
} else if (Pretype == 2) {
  TRDSAM <- TRD
  for (i in PreRept$StockCode) {
    TRDSAM <- filter(TRDSAM, Stkcd!=i)}
} else if (Pretype == 3) {
  TRDSAM <- TRD
  Prestk <- filter(PreRept, Source == 0 )
  for (i in Prestk$StockCode) {
    TRDSAM <- filter(TRDSAM, Stkcd!=i)
  }
} else if(Pretype == 4) {
  TRDSAM <- TRD
  Prestk <- filter(PreRept, Source == 1 )
  for (i in Prestk$StockCode) {
    TRDSAM <- filter(TRDSAM, Stkcd!=i)
  }
} else {TRDSAM <- TRD}
rm(TRD,PreRept,Prestk)



######## Fama-French three factor #########
FF3 <- dir(datadir, pattern = 'ThrfacDay.txt$') %>%
  paste0(datadir, .) %>%
  read_delim(delim='\t', na = '')
# Weighted in Total Market Value
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
FF3 <- FF3[FF3$MarkettypeID=='P9709',seq(from=2,to=8,by=2)]
colnames(FF3) <- c('TradingDate','RiskPrem','Thr_SMB','Thr_HML')
FF3 <- arrange(FF3, TradingDate) %>%
  filter(TradingDate %within%
           interval(QEAperd %m+% months(-16),
                    QEAperd %m+% months(+6)))
# Setup Working day arround QEA
WorkingDay <- FF3$TradingDate

######## Fama-French five factor #########
FF5 <- dir(datadir, pattern = 'FivefacDay.txt$') %>%
  paste0(datadir, .) %>%
  read_delim(delim='\t', na = '')
# Sorting stocks in 2*3 portfolios
FF5 <- filter(FF5, Portfolios==1) # key
# Weighted in Total Market Value
FF5 <- filter(FF5, MarkettypeID=='P9709')
FF5 <- subset(FF5, select= c(TradingDate, seq(from=5,to=13,by=2)))
FF5$TradingDate <- as.Date(FF5$TradingDate,'%Y-%m-%d')
colnames(FF5) <- c('TradingDate','RiskPremium',
                   'Five_SMB','Five_HML','Five_RMW','Five_CMA')
FF5 <- arrange(FF5, TradingDate) %>%
  filter(TradingDate %within%
           interval(QEAperd %m+% months(-16),
                    QEAperd %m+% months(+6)))

all.equal(FF3$RiskPrem, FF5$RiskPremium)



#############################################
################### Merge ###################
#############################################
TRDSAM <- merge(TRDSAM, FF3, by='TradingDate') %>%
  merge(FF5, by='TradingDate') %>%
  arrange(Stkcd) %>%
  select(c("Stkcd", "TradingDate", "Dretwd",
           "RiskPrem","Thr_SMB","Thr_HML",
           "Five_SMB","Five_HML","Five_RMW","Five_CMA",
           "Markettype","Trdsta"))
rm(FF3,FF5)



###### announcement date of quarterly financial report ######
ReptInfo <- dir(datadir, pattern = 'Rept.csv$') %>%
  paste0(datadir, .) %>%
  read_delim(delim='\t', na = '') %>%
  subset(select=c(Stkcd,Annodt,Accper,Annowk,Sctcd)) %>%
  filter(Accper %in% QEAperd) %>%
  arrange(Stkcd) %>%
  as.data.frame()

ReptInfo$Annodt <- as.Date(ReptInfo$Annodt,'%Y-%m-%d')



#############################
########## Matching #########
#############################
exwind <- -270L
bhwind <- +40L
stkwek <- data.frame()
stkwnd <- data.frame()
stkbrk <- data.frame()
for (i in 1:nrow(ReptInfo)) {
  QEAgrp <- data.frame()
  stkts <- filter(TRDSAM, Stkcd == ReptInfo[i,'Stkcd'])
  if (ReptInfo[i,'Annodt'] %in% WorkingDay) {
    n.row <- which(WorkingDay==ReptInfo[i,'Annodt'])
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (i in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate %in% QEA.date[i])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind +1,
           stkwek <- rbind(stkwek, QEAgrp),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
  } else if (wday(ReptInfo[i,'Annodt']) == 7) {
    QEADate <- ReptInfo[i,'Annodt'] + days(2)
    ifelse(QEADate %in% WorkingDay,
           n.row <- which(WorkingDay==QEADate),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (i in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate %in% QEA.date[i])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind +1,
           stkwnd <- rbind(stkwnd, QEAgrp),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
  } else if (wday(ReptInfo[i,'Annodt']) == 1) {
    QEADate <- ReptInfo[i,'Annodt'] + days(1)
    ifelse(QEADate %in% WorkingDay,
           n.row <- which(WorkingDay==QEADate),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (i in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate  %in% QEA.date[i])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind +1,
           stkwnd <- rbind(stkwnd, QEAgrp),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
  } else stkbrk <- rbind(stkbrk, ReptInfo[i,])
}
rm(ReptInfo, QEAgrp, QEAsim, stkts, QEADate, QEA.date,n.row)




#################### Trdsta [交易状态] ########################
##### 1=正常交易，2=ST，3＝*ST，                           ####
##### 4＝S（2006年10月9日及之后股改未完成），              ####
##### 5＝SST，6＝S*ST，                                    ####
##### 7=G（2006年10月9日之前已完成股改），8=GST，9=G*ST，  ####
##### 10=U（2006年10月9日之前股改未完成），                ####
##### 11=UST，12=U*ST，13=N，14=NST，15=N*ST，16=PT        ####
###############################################################
# week-day(stkwek), weekend-day(stkwnd)
# or all of them(rbind(stkwek, stkwnd))?
weekterm <- 'wkd'
if (weekterm=='wek') {
  stktrd <- stkwek
  } else if (weekterm=='wnd') {
  stktrd <- stkwnd
} else {stktrd <-rbind(stkwek, stkwnd) }

# key, Trading status
Trdstatype <- c(1)
stktrd <- filter(stktrd, Trdsta %in% Trdstatype)[,-ncol(stktrd)]
## check up the time periods not existed errors
TSN <- c(); TSA <- c()
for (i in unique(stktrd$Stkcd)) {
  TS <- nrow(filter(stktrd, Stkcd==i))
  if (TS!=bhwind-exwind +1){
    TSA <- c(TSA, i)
    stktrd <- filter(stktrd, Stkcd!=i)
  } else {TSN <- c(TSN,i)}
}



##########################################
########## Estimation Window #############
##########################################
datwind <- function(x) {
  if(x == 1L) {
    exdate <- 1L # Estimation Window
    bhdate <- 240L
  } else {
    exdate <- 251L # Event Window
    bhdate <- 311L
  }
  windlen <- bhdate-exdate +1L
  stkdat <- data.frame()
  for (i in unique(stktrd$Stkcd)) {
    stksim <- filter(stktrd, Stkcd==i) %>% .[c(exdate:bhdate),]
    stkdat <- rbind(stkdat, stksim)
  }
  return(list(stkdat,windlen))
  rm(stksim)
}
# The input number is the most important key
# for 1, we get the data within estimation window
# as for else, we get the data within event window
stkdatl <- datwind(1L)
stkdat <- stkdatl[[1]]; windlen <- stkdatl[[2]]; rm(stkdatl)



########### Markettype [股票交易市场] ############
########## 1=上海A, 4=深圳A, 16=创业板 ###########
mkttype <- c(1,4,16) # key
stkdat <- filter(stkdat, Markettype %in% mkttype)[,-ncol(stkdat)]
TSN <- unique(stkdat$Stkcd)



########################################
####### Output the Tidyr result ########
########################################
## Estimation data
paste(QEAperd, Pretype, modeltype, weekterm, 'TradStat', sep='_') %>%
  paste0(datadir, ., '.csv') %>%
  write.csv(stkdat, file=., quote=F, row.names = F)
## the sample stock code
paste(QEAperd, Pretype, modeltype, weekterm,'stkcd', sep='_') %>%
  paste0(datadir, ., '.csv') %>%
  write.csv(TSN, file=., quote=F, row.names = F)
## save the image
cdimage <- paste(QEAperd, Pretype, modeltype, weekterm, sep='_') %>%
  paste0(datadir, ., '.RData') 
save.image(cdimage)

###### stock code, day index in MATLAB ########
MATIndex <- cbind("Stkcd"=rep(1:length(TSN), each=windlen),
                  "day"=rep(seq(from=1, to=windlen, by=1), times=length(TSN))) 
paste(QEAperd, Pretype, modeltype, weekterm,'MATdex', sep='_') %>%
  paste0(datadir, ., '.csv') %>%
  write.csv(MATIndex, file=., quote=F, row.names = F)


#################################################
###### select random 300 stocks as a sample #####
#################################################
#randcd <- sample(unique(stkdat$Stkcd), size = 300)
#randsam <- data.frame()
#for (i in randcd) {
#  randstk <- filter(stkdat, Stkcd==i)
#  randsam <- rbind(randsam, randstk)
#}
#rm(randstk)

#ne300 <- paste0(datadir, QEAperd, Pretype, modeltype, 'TradStat-300',sep='_')
#necd300 <- paste0(datadir, QEAperd, Pretype, modeltype, '-stkcd-300', sep='_')
#write.csv(randsam, file=ne300, quote=F, row.names = F)
#write.csv(randcd, file=necd300, quote=F, row.names = F)
