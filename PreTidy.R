library(tidyverse)
library(lubridate)
library(timeDate)


############ Input daily trading data ##############
file.name <- c('TRD_Dalyr1','TRD_Dalyr2','TRD_Dalyr3','TRD_Dalyr4')
TRD <- data.frame()
for (i in 1:length(file.name)) {
  path <- paste0('~/R/Data/', file.name[i],'.csv')
  TRD <- rbind(TRD, as.data.frame(read_delim(file = path, delim='\t', na = '')))
}
colnames(TRD)[2] <- 'TradingDate'
TRD$TradingDate <- as.Date(TRD$TradingDate,'%Y-%m-%d')
TRD <- as_tibble(subset(TRD,select=c(Stkcd,TradingDate,Dretwd,Markettype,Trdsta)))

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
#######  1=上海A, 2=上海B, 4=深圳A   ######### 
###### 8=深圳B, 16=创业板, 32=科创板 #########
mkttype.char <- c(1,4,16) # key
TRD <- filter(TRD, Markettype %in% mkttype.char)

# R^i minus R^f
ThrMonRf <- 0.011
Rf <- (1 + ThrMonRf)^(1/90) - 1
TRD$Dretwd <- round(TRD$Dretwd-Rf, 6)


##### Select the stocks that have published ex-earnings report ####
## 0,选取之前发布业绩预告的企业 
## 1,选取之前发布定期公告的企业 
## c(0,1), 选取之前发布有信息的企业，即0、1两类企业之加和
## 2,选取之前毫无预告的企业，即从总样本中去除0、1两类企业
## 3，总样本中去除发布业绩预告的企业 
## 4,总样本中去除发布定期公告的企业 
## 若全部都要，输入5即可（实质上除上述数字皆可）
Pretype <- c(2) # key

PreRept <- as.data.frame(read_delim("FIN_F_ForecFin.csv", delim='\t', na = ''))
PreRept <- select(PreRept, StockCode, Source, PubliDate, AccPeriod)
PreRept <- filter(PreRept, AccPeriod==QEAperd)

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


######## Fama-French three factor #########
FF3 <- as.data.frame(read_delim('STK_MKT_ThrfacDay.txt', delim='\t', na = ''))
# Weighted in Total Market Value 
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
FF3 <- FF3[FF3$MarkettypeID=='P9709',seq(from=2,to=8,by=2)]
colnames(FF3) <- c('TradingDate','RiskPrem','Thr_SMB','Thr_HML')
FF3 <- arrange(FF3, TradingDate)
FF3 <- filter(FF3, TradingDate %within%
                interval(QEAperd %m+% months(-16),
                         QEAperd %m+% months(+6)))
# Setup Working day arround QEA
WorkingDay <- FF3$TradingDate

######## Fama-French five factor #########
FF5 <- as.data.frame(read_delim('STK_MKT_FivefacDay.txt', delim='\t', na = ''))
# Sorting stocks in 2*3 portfolios
FF5 <- filter(FF5, Portfolios==1)
# Weighted in Total Market Value 
FF5 <- filter(FF5, MarkettypeID=='P9709')
FF5 <- subset(FF5, select= c(TradingDate, seq(from=5,to=13,by=2)))
FF5$TradingDate <- as.Date(FF5$TradingDate,'%Y-%m-%d')
colnames(FF5) <- c('TradingDate','RiskPremium',
                   'Five_SMB','Five_HML','Five_RMW','Five_CMA')
FF5 <- arrange(FF5, TradingDate)
FF5 <- filter(FF5, TradingDate %within%
                interval(QEAperd %m+% months(-16),
                         QEAperd %m+% months(+6)))


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


###### announcement date of quarterly financial report ######
ReptInfo <- as.data.frame(read_delim('IAR_Rept.csv', delim='\t', na = ''))
ReptInfo <- subset(ReptInfo, select=c(Stkcd,Annodt,Accper,Annowk,Sctcd))
ReptInfo <- arrange(arrange(ReptInfo, Accper), Stkcd)
ReptInfo <- filter(ReptInfo, Accper %in% QEAperd)


#############################
########## Matching #########
#############################
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


#################### Trdsta [交易状态] ########################
##### 1=正常交易，2=ST，3＝*ST，                           ####
##### 4＝S（2006年10月9日及之后股改未完成），              ####
##### 5＝SST，6＝S*ST，                                    ####
##### 7=G（2006年10月9日之前已完成股改），8=GST，9=G*ST，  ####
##### 10=U（2006年10月9日之前股改未完成），                ####
##### 11=UST，12=U*ST，13=N，14=NST，15=N*ST，16=PT        ####
###############################################################

# week-day(stkwek), weekend-day(stkwnk)，or all of them(stktol)？
stktol <- rbind(stkwek, stkwnk)
stktrd <- stktol
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
  windlen <- bhdate-exdate +1
  stkdat <- data.frame()
  for (i in unique(stktrd$Stkcd)) {
    stksim <- filter(stktrd, Stkcd==i)
    stksim <- stksim[c(exdate:bhdate),]
    stkdat <- rbind(stkdat, stksim)
  }
  return(list(stkdat,windlen))
  rm(stksim)
}
# The input number is the most important key
# for 1, we get the data within estimation window
# as for else, we get the data within event window
stkdatt <- datwind(1)
stkdat <- stkdatt[[1]]; windlen <- stkdatt[[2]]

########### Markettype [股票交易市场] ############
########## 1=上海A, 4=深圳A, 16=创业板 ###########
mkttype <- c(1,4,16) # key
stkdat <- filter(stkdat, Markettype %in% mkttype)[,-ncol(stkdat)]
TSN <- unique(stkdat$Stkcd)



#####################################################################
##### Obtain the plots of OLS estimator used above QEA-FF data ######
#####################################################################
# CAPM, FF 3-factors or FF 5-factors?
charCAPM <- c('Stkcd', 'Dretwd', 'RiskPrem')
charFF3 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Thr_SMB', 'Thr_HML')
charFF5 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Five_SMB', 'Five_HML', 
             'Five_RMW', 'Five_CMA')
stkCAPM <- subset(stkdat, select=charCAPM)
stkFF3 <- subset(stkdat, select=charFF3)
stkFF5 <- subset(stkdat, select=charFF5)
stkff <- stkFF3

# the correlation coefficient between explanation variables
rownum <- windlen * sample(1:length(unique(stkff$Stkcd)), size = 1)
cor(subset(stkff[c(1:windlen)+rownum,], select = -c(Stkcd, Dretwd))); rm(rownum)

# personal function for getting the OLS estimator one by one
stkcoef <- function(x) {
  g <- data.frame()
  for(i in unique(x$Stkcd)) {
    q <- filter(x, Stkcd==i)[,-1]
    stkreg <- lm(Dretwd ~ ., data = q) 
    stkregcoef <- t(as.data.frame(coef(stkreg)))
    k <- cbind(i, stkregcoef)
    g <- rbind(g,k)
  }
  colnames(g)[1] <- 'Stkcd' 
  return(as.data.frame(g))
}
OLScoef <- stkcoef(stkff)

# solve the problem of data type (factor to numeric)
if (ncol(OLScoef)==5) { # 5= 2(Stkcd+alpha) + 3 factors 
   colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML")
   modeltype <- 'FF3'
} else if (ncol(OLScoef)==7) {
  colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML", "RMW", "CMA")
  modeltype <- 'FF5'
} else {
  colnames(OLScoef) <- c("Stkcd", "alpha","MKT")
  modeltype <- 'CAPM'
  }
necoef <- paste0('~/R/Data/', QEAperd, '-',modeltype,'-OLScoef', '.csv')
write.csv(OLScoef, file=necoef, quote=F, row.names = F)
OLScoef <- as.data.frame(read_delim(necoef, delim=',', na = ''))
OLScoef[, 3:ncol(OLScoef)] <- round(OLScoef[, 3:ncol(OLScoef)], 6)

library(ggplot2)
require(grid)
library(latex2exp)
grid.newpage()
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
if(ncol(OLScoef)==5) {
       {pushViewport(viewport(layout = grid.layout(2,2)))
        print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'), 
                     ylab = "Count",bins=100), vp = vplayout(1,1:2))
        print(qplot(OLScoef$SMB, xlab=TeX('$\\beta_2$ of SMB'), 
                     ylab = "Count",bins=50), vp = vplayout(2,1))       
        print(qplot(OLScoef$HML, xlab=TeX('$\\beta_3$ of HML'), 
                     ylab = "Count",bins=50), vp = vplayout(2,2))}
} else if (ncol(OLScoef)==7) {
       {pushViewport(viewport(layout = grid.layout(2,4)))
        print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'), 
                     ylab = "Count",bins=100), vp = vplayout(1,1:4))
        print(qplot(OLScoef$SMB, xlab=TeX('$\\beta_2$ of SMB'), 
                     ylab = "Count",bins=50), vp = vplayout(2,1))       
        print(qplot(OLScoef$HML, xlab=TeX('$\\beta_3$ of HML'), 
                     ylab = "Count",bins=50), vp = vplayout(2,2))
        print(qplot(OLScoef$RMW, xlab=TeX('$\\beta_4$ of RMW'), 
                     ylab = "Count",bins=50), vp = vplayout(2,3))
        print(qplot(OLScoef$CMA, xlab=TeX('$\\beta_5$ of CMA'), 
                     ylab = "Count",bins=50), vp = vplayout(2,4))}
} else {
  pushViewport(viewport(layout = grid.layout(1,1)))
  print(qplot(OLScoef$MKT, xlab=TeX('$\\beta_1$ of MKT'), 
              ylab = "Count",bins=100), vp = vplayout(1,1))
}


########################################
####### Output the Tidyr result ########
########################################
ne <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-TradStat-FF','.csv')
necd <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-stkcd','.csv')
nesa <- paste0('~/R/Data/', QEAperd, '-', modeltype, '.RData')
save.image(file=nesa)
write.csv(stkdat, file=ne, quote=F, row.names = F)
## the sample stock code 
write.csv(TSN, file=necd, quote=F, row.names = F)

###### stock code, day index in MATLAB ########
QEAIndex <- cbind(rep(1:length(TSN), each=windlen),
                  rep(seq(from=1, to=windlen, by=1), times=length(TSN)))
colnames(QEAIndex) <- c("Stkcd", "day")
neide <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-MATindex','.csv')
write.csv(QEAIndex, file=neide, quote = F , row.names = F,)


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

ne300 <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-TradStat-300','.csv')
necd300 <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-stkcd-300','.csv')
write.csv(randsam, file=ne300, quote=F, row.names = F)
write.csv(randcd, file=necd300, quote=F, row.names = F)
