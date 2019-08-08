library(timeDate)

##################
QEAper <- timeDate('2017-09-30')
ReptInfo <-  filter(ReptInfo, Accper %in% QEAper)

########### Find Working day ###############
FF3 <- as.data.frame(read_delim('STK_MKT_ThrfacDay.txt', delim='\t', na = ''))
# Weighted in Total Market Value 
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
FF3 <- FF3[FF3$MarkettypeID=='P9709',c(2,4,6,8)]
FF3 <- arrange(FF3, TradingDate)
colnames(FF3) <- c('TradingDate','RiskPremium','Thr_SMB','Thr_HML')
WorkingDay <- FF3[-c(1:4719), 'TradingDate']

########abandon the stocks that have published ex-earnings report###########
QEAfore <- as.data.frame(read_delim('fore.csv', delim=',', na = ''))
# Weighted in Total Market Value 
QEAfore$StockCode <- str_sub(as.character(QEAfore$StockCode), 
                             start = 1L, end = 6L)
TRDFFNF <- TRDFF
for (i in QEAfore$StockCode) {
  TRDFFNF <- filter(TRDFFNF, Stkcd!=i)
}




############## Tidy ###############

FF <- TRDFFNF
exwind <- -270L
bhwind <- -31L
stksam <- data.frame()
stkwnk <- data.frame()
stkbrk <- data.frame()
for (i in 1:length(ReptInfo)) {
  QEAgrp <- data.frame()
  stkts <- filter(FF, Stkcd == ReptInfo[i,'Stkcd'])
  stkts <- filter(stkts, TradingDate %within%
                    interval(ReptInfo[i,'Annodt'] %m+% months(-14),
                             ReptInfo[i,'Annodt'] %m+% months(4)))
  if (ReptInfo[i,'Annodt'] %in% WorkingDay) {
    n.row <- which(WorkingDay==ReptInfo[i,'Annodt'])
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (i in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate %in% QEA.date[i])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stksam <- rbind(stksam, QEAgrp),
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
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stkwnk <- rbind(stkwnk, QEAgrp),
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
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stkwnk <- rbind(stkwnk, QEAgrp),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
  } else stkbrk <- rbind(stkbrk, ReptInfo[i,])
}

stktol <- rbind(stksam,stkwnk)
# Trading status, one meanings normal
stkabt <- filter(stktol, Trdsta!=1)
stknor <- filter(stktol, Trdsta==1)
stknor <- stknor[,-ncol(stknor)]
## check up the time periods(no matching errors)
TSN <- c()
TSA <- c()
stkpan <- data.frame()
for (i in unique(stknor[,1])) {
  TS <- nrow(filter(stknor, Stkcd==i))
  if (TS!=bhwind - exwind +1){
  TSA <- c(TSA, i)
  stknor <- filter(stknor, Stkcd!=i)
  } else {TSN <- c(TSN,i)}
}

colnames(stknor)[4] <- 'RiskPrem'
# Shanghai
stksamSH <- filter(stknor, Markettype==1)
# Shenzheng
stksamSZ <- filter(stknor, Markettype==4)
# 创业板
stksamTC <- filter(stknor, Markettype==16)


############ Output ################
ne <- paste0('QEA-', QEAper, '.csv')
necd <- paste0('QEA-stkcd-', QEAper, '.csv')
nesa <- paste0('QEA-', QEAper, '.RData')
save.image(nesa)
write.csv(stknor, file=ne, quote=F, row.names = F)
## the sample stock code 
write.csv(TSN, file=necd, quote=F, row.names = F)


# select random 300 stocks as a sample
randcd <- sample(unique(stknor$Stkcd), size = 300)
randsam <- data.frame()
for (i in randcd) {
  randstk <- filter(stknor, Stkcd==i)
  randsam <- rbind(randsam, randstk)
}

ne300 <- paste0('QEA-300-', QEAper, '.csv')
necd300 <- paste0('QEA-300-stkcd-', QEAper, '.csv')
write.csv(randsam, file=ne300, quote=F, row.names = F)
write.csv(unique(randsam$Stkcd), file=necd300, quote=F, row.names = F)
########
