###################################################
##################### Matching ####################
##################################################
exwind <- -270L
bhwind <- +40L
stkwek <- data.frame()
stkwnk <- data.frame()
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
    ifelse(nrow(QEAgrp) == bhwind-exwind +1,
           stkwnk <- rbind(stkwnk, QEAgrp),
           stkbrk <- rbind(stkbrk, ReptInfo[i,]))
  } else stkbrk <- rbind(stkbrk, ReptInfo[i,])
}
rm(QEAgrp, QEAsim, stkts, QEADate, QEA.date)
stktol <- rbind(stkwek, stkwnk)


#################### Trdsta [交易状态] ########################
##### 1=正常交易，2=ST，3＝*ST，                           ####
##### 4＝S（2006年10月9日及之后股改未完成），              ####
##### 5＝SST，6＝S*ST，                                    ####
##### 7=G（2006年10月9日之前已完成股改），8=GST，9=G*ST，  ####
##### 10=U（2006年10月9日之前股改未完成），                ####
##### 11=UST，12=U*ST，13=N，14=NST，15=N*ST，16=PT        ####
###############################################################

######################## Important ############################
### week day(stkwek), weekend day(stkwnk)，or all of them(stktol)？
stktrd <- stktol # key

Trdstatype <- c(1) # key, Trading status
stktrd <- filter(stktrd, Trdsta %in% Trdstatype)[,-ncol(stktrd)]
## check up the time periods not existed errors
TSN <- c()
TSA <- c()
for (i in unique(stktrd$Stkcd)) {
  TS <- nrow(filter(stktrd, Stkcd==i))
  if (TS!=bhwind-exwind +1){
    TSA <- c(TSA, i)
    stktrd <- filter(stktrd, Stkcd!=i)
  } else {TSN <- c(TSN,i)}
}

##########################################
#### Estimation Window & Event Window ####
##########################################

datwind <- function(x) {
  if(x == 1) {
    exdate <- 1 # Estimation Window
    bhdate <- 240
  } else {
    exdate <- 251 # Event Window
    bhdate <- 311
  }
  stkdat <- data.frame()
  for (i in unique(stktrd$Stkcd)) {
    stksim <- filter(stktrd, Stkcd==i)
    stksim <- stksim[c(exdate:bhdate),]
    stkdat <- rbind(stkdat, stksim)
  }
  return(stkdat)
  rm(stksim)
}

stkdat <- datwind(1) # key

###############################################################
################# Markettype [股票交易市场] ###################
################ 1=上海A, 4=深圳A, 16=创业板 #### #############
###############################################################
mkttype <- c(1,4,16) # key
stkdat <- filter(stkdat, Markettype %in% mkttype)[,-ncol(stkdat)]
TSN <- unique(stkdat$Stkcd)


####################################
############ Output ################
####################################
ne <- paste0('~/R/Data/', QEAperd, '-TradStat-FF','.csv')
necd <- paste0('~/R/Data/', QEAperd, '-stkcd','.csv')
nesa <- paste0('~/R/Data/', QEAperd, '.RData')
save.image(nesa)
write.csv(stkdat, file=ne, quote=F, row.names = F)
## the sample stock code 
write.csv(TSN, file=necd, quote=F, row.names = F)


#########################################################
########## stock code, day index in MATLAB ##############
#########################################################
estwndlen <- 240L
QEAIndex <- cbind(rep(1:length(TSN), each=estwndlen),
           rep(seq(from=1, to=estwndlen, by=1), times=length(TSN)))
colnames(QEAIndex) <- c("Stkcd", "day")
neid <- paste0('~/R/Data/', QEAperd, '-Index','.csv')
write.csv(QEAIndex, file=neid, quote = F , row.names = F,)


#################################################
###### select random 300 stocks as a sample #####
#################################################
randcd <- sample(unique(stkdat$Stkcd), size = 300)
randsam <- data.frame()
for (i in randcd) {
  randstk <- filter(stkdat, Stkcd==i)
  randsam <- rbind(randsam, randstk)
}
rm(randstk)

ne300 <- paste0('~/R/Data/', QEAperd, '-TradStat-FF-300','.csv')
necd300 <- paste0('~/R/Data/', QEAperd, '-stkcd-300','.csv')
write.csv(randsam, file=ne300, quote=F, row.names = F)
write.csv(randcd, file=necd300, quote=F, row.names = F)

