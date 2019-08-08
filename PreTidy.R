library(tidyverse)
library(lubridate)
########### Input data###############

# daily trading data
file.name <- c('TRD_Dalyr1','TRD_Dalyr2','TRD_Dalyr3','TRD_Dalyr4')

TRD <- data.frame()
for (i in 1:length(file.name)) {
  path <- paste0(file.name[i],'.csv')
  TRD <- rbind(TRD, as.data.frame(read_delim(file = path, delim='\t', na = '')))
}

# Market type 1=上海A, 2=上海B, 4=深圳A, 8=深圳B, 16=创业板, 32=科创板
mkttype.char <- c(1,4,16)
TRDSM <- filter(TRD, Markettype %in% mkttype.char)

colnames(TRDSM)[2] <- 'TradingDate'
TRDSM[,2] <- as.Date(TRDSM$TradingDate,'%Y-%m-%d')
TRDSM <- as_tibble(select(TRDSM,c(1,2,3,8,9,4,5,6,7,10,12)))

## R^i minus R^f
Rf <- (1 + 0.0110)^(1/90) - 1
TRDSM$Dretwd <- round(TRDSM$Dretwd - Rf,6)

############### Fama-French three & five factor#############

FF3 <- as.data.frame(read_delim('STK_MKT_ThrfacDay.txt', delim='\t', na = ''))
# Weighted in Total Market Value 
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
FF3 <- FF3[FF3$MarkettypeID=='P9709',c(2,4,6,8)]
FF3 <- arrange(FF3, TradingDate)
FF3 <- FF3[-c(1:2304),]
colnames(FF3) <- c('TradingDate','RiskPremium','Thr_SMB','Thr_HML')

FF5 <- as.data.frame(read_delim('STK_MKT_FivefacDay.txt', delim='\t', na = ''))
# Sorting stocks in 2*3 portfolios
FF5 <- FF5[FF5$Portfolios==1,]
# Weighted in Total Market Value 
FF5 <- FF5[FF5$MarkettypeID=='P9709',c(2,5,7,9,11,13)]
FF5$TradingDate <- as.Date(FF5$TradingDate,'%Y-%m-%d')
FF5 <- arrange(FF5, TradingDate)
colnames(FF5) <- c('TradingDate','RiskPremium',
                   'Five_SMB','Five_HML','Five_RMW','Five_CMA')

############## Merge ########################

TRDFF <- merge(TRDSM, FF3, by='TradingDate')
TRDFF <- merge(TRDFF, FF5, by='TradingDate')
TRDFF <- arrange(TRDFF, Stkcd)

TRDFF <- select(TRDFF, c("Stkcd","TradingDate","Dretwd",
                          "RiskPremium.x","Thr_SMB","Thr_HML",
                          "Five_SMB","Five_HML","Five_RMW","Five_CMA",
                          "Markettype","Trdsta"))

########### happend date of quarterly financial announcement #############
ReptInfo <- as.data.frame(read_delim('IAR_Rept.csv', delim='\t', na = ''))
ReptInfo <- select(ReptInfo, c('Stkcd','Annodt','Accper','Annowk',
                               'Reptyp','Sctcd'))
ReptInfo <- arrange(arrange(ReptInfo, Accper), Stkcd)

############### output #########################
write.csv(TRDFF, file="TrdingStatus.csv", quote=F, row.names = F)
write.csv(ReptInfo, file="ReptInfo.csv", quote=F, row.names = F)



