library(tidyverse)
library(lubridate)
library(timeDate)

##############################################
############ Input trading data ##############
### Market type 1=上海A, 2=上海B, 4=深圳A #### 
###### 8=深圳B, 16=创业板, 32=科创板 #########
##############################################

file.name <- c('TRD_Dalyr1','TRD_Dalyr2','TRD_Dalyr3','TRD_Dalyr4')
TRD <- data.frame()
for (i in 1:length(file.name)) {
  path <- paste0('~/R/Data/', file.name[i],'.csv')
  TRD <- rbind(TRD, as.data.frame(read_delim(file = path, delim='\t', na = '')))
}
colnames(TRD)[2] <- 'TradingDate'
TRD$TradingDate <- as.Date(TRD$TradingDate,'%Y-%m-%d')
TRD <- as_tibble(select(TRD,c(1,2,3,8,10,12)))

mkttype.char <- c(1,4,16) # key
TRD <- filter(TRD, Markettype %in% mkttype.char)

#############################################
######### Quarterly earnings report #########
#############################################

QEAperd <- as.Date('2017-09-30') # key

TRD <- filter(TRD, TradingDate %within%
                   interval(QEAperd %m+% months(-16),
                            QEAperd %m+% months(+6)))

#### R^i minus R^f
ThrMonRf <- 0.011
Rf <- (1 + ThrMonRf)^(1/90) - 1
TRD$Dretwd <- round(TRD$Dretwd-Rf, 6)

############################################################################
####### Select the stocks that have published ex-earnings report ###########
######################## 0=业绩预告，1=定期公告 ############################
################ 此外随意数字，表示仅选取无预告的上市企业 ##################
############################################################################

PreRept <- as.data.frame(read_delim("FIN_F_ForecFin.csv", delim='\t', na = ''))
PreRept <- select(PreRept, StockCode, Source, PubliDate, AccPeriod)
PreRept <- filter(PreRept, AccPeriod==QEAperd)

Pretype <- c(2) # key

if(Pretype %in% c(0,1)) {
  TRDSAM <- data.frame()
  PreRept <- filter(PreRept, Source %in% Pretype)
  for (i in PreRept$StockCode) {
    TRDaln <- filter(TRD, Stkcd==i)
    TRDSAM <- rbind(TRDSAM, TRDaln)
  } 
  rm(TRDaln)
} else {
  TRDSAM <- TRD
  for (i in PreRept$StockCode) {
    TRDSAM <- filter(TRDSAM, Stkcd!=i)}
  }

#############################################################
############# Fama-French three & five factor ###############
#############################################################

FF3 <- as.data.frame(read_delim('STK_MKT_ThrfacDay.txt', delim='\t', na = ''))
# Weighted in Total Market Value 
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
FF3 <- FF3[FF3$MarkettypeID=='P9709',c(2,4,6,8)]
FF3 <- arrange(FF3, TradingDate)
FF3 <- filter(FF3, TradingDate %within%
                interval(QEAperd %m+% months(-16),
                         QEAperd %m+% months(+6)))
colnames(FF3) <- c('TradingDate','RiskPrem','Thr_SMB','Thr_HML')
#### Find Working day
WorkingDay <- FF3$TradingDate

FF5 <- as.data.frame(read_delim('STK_MKT_FivefacDay.txt', delim='\t', na = ''))
# Sorting stocks in 2*3 portfolios
FF5 <- FF5[FF5$Portfolios==1,]
# Weighted in Total Market Value 
FF5 <- FF5[FF5$MarkettypeID=='P9709',c(2,5,7,9,11,13)]
FF5$TradingDate <- as.Date(FF5$TradingDate,'%Y-%m-%d')
FF5 <- arrange(FF5, TradingDate)
FF5 <- filter(FF5, TradingDate %within%
                interval(QEAperd %m+% months(-16),
                         QEAperd %m+% months(+6)))
colnames(FF5) <- c('TradingDate','RiskPremium',
                   'Five_SMB','Five_HML','Five_RMW','Five_CMA')


#############################################
################### Merge ###################
#############################################

TRDSAM <- merge(TRDSAM, FF3, by='TradingDate')
TRDSAM <- merge(TRDSAM, FF5, by='TradingDate')
TRDSAM <- arrange(TRDSAM, Stkcd)
all.equal(TRDSAM$RiskPrem, TRDSAM$RiskPremium)
TRDSAM <- select(TRDSAM, c("Stkcd","TradingDate","Dretwd",
                          "RiskPrem","Thr_SMB","Thr_HML",
                          "Five_SMB","Five_HML","Five_RMW","Five_CMA",
                          "Markettype","Trdsta"))

##########################################################################
########### announcement date of quarterly financial report ##############
##########################################################################

ReptInfo <- as.data.frame(read_delim('IAR_Rept.csv', delim='\t', na = ''))
ReptInfo <- select(ReptInfo, c('Stkcd','Annodt','Accper','Annowk','Sctcd'))
ReptInfo <- arrange(arrange(ReptInfo, Accper), Stkcd)
ReptInfo <- filter(ReptInfo, Accper %in% QEAperd)
