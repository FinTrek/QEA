library(tidyverse)
library(timeDate)
library(lubridate)

# setting the data directory
datadir <- 'C:/Users/Hu/Documents/NutSync/MyData/QEAData/'  # Hu, PC-Carbon
datadir <- 'D:/NutSync/MyData/QEAData/' # HHY, PC-HP 


# Input the data of daily trading status of stocks =============================
# in China Mainland from 2010 to 2019 
filecd <- dir(datadir, pattern = '^TRD_Dalyr.*?[.]csv$') %>% paste0(datadir, .)
TRD <- data.frame()
for (i in filecd) {
    TRDS <- read_delim(i, delim='\t', na = '',
                       col_types = cols(Stkcd = col_character(),
                                        Trddt = col_date(format = '%Y-%m-%d'),
                                        Markettype = col_integer(),
                                        Trdsta = col_integer())) %>%
              rename('TradingDate' = Trddt) %>%
              subset(select=c(Stkcd,TradingDate,Dretwd,Markettype,Trdsta))
    TRD <- rbind(TRD,TRDS)
}

# Sorting by trading date and then by stock code
TRD <- arrange(TRD, TradingDate) %>% arrange(Stkcd)


# transform the data class from date to character,
# in order to match year and month in next process
TRD$TradingDate <- as.character(TRD$TradingDate)
# R^i minus R^f ================================================================
ThrMonRisk <- dir(datadir, pattern = '^RiskFreePremium[.]csv$') %>%
                paste0(datadir, .) %>%
                read_delim(delim = ',', na = '', col_names = T, 
                           col_types = cols(Month = col_character(), rate = col_double())) %>% 
                as.data.frame()
for (i in 1:nrow(ThrMonRisk)) {
    Mon <- ThrMonRisk[i,"Month"]
    ThrRf <- ThrMonRisk[i,"rate"] /100 
    DaiRf <- round((1 + ThrRf)^(1/90) - 1, 6)
    Daterow <- which(substring(TRD$TradingDate, 1, 7) == Mon)
    TRD[Daterow, ]$Dretwd <- TRD[Daterow, ]$Dretwd - DaiRf
}
TRD$TradingDate <- as.Date(TRD$TradingDate,"%Y-%m-%d")
rm(filecd,TRDS,Mon,ThrRf,DaiRf,Daterow)



## Fama-French three factor =====================================================

FF3 <- dir(datadir, pattern = 'ThrfacDay[.]csv$') %>%
       paste0(datadir, .) %>%
       read_delim(delim='\t', na = '',
                  col_types = cols(TradingDate = col_date('%Y-%m-%d')))
# Weighted in Total Market Value
FF3 <- subset(FF3, MarkettypeID=='P9706', seq(from=2,to=8,by=2)) %>% 
       set_names(c('TradingDate','RiskPrem','Thr_SMB','Thr_HML')) %>% 
       arrange(TradingDate)



## Fama-French five factor ======================================================

FF5 <- dir(datadir, pattern = 'FivefacDay[.]csv$') %>%
       paste0(datadir, .) %>%
       read_delim(delim='\t', na = '',
                  col_types = cols(TradingDate = col_date('%Y-%m-%d')))

# Sorting stocks in 2*3 portfolios
FF5 <- filter(FF5, Portfolios == 1) # key

# Weighted in Total Market Value
FF5 <- subset(FF5, MarkettypeID=='P9709', c(TradingDate, seq(from=5,to=13,by=2))) %>% 
       set_names(c('TradingDate','RiskPremium','Five_SMB','Five_HML',
                   'Five_RMW','Five_CMA')) %>% 
       arrange(TradingDate)




## Merge =========================================================================
TRDFF <- merge(TRD, FF3, by='TradingDate') %>%
         #merge(FF5, by='TradingDate') %>%
         arrange(Stkcd)

if (all.equal(TRDFF$RiskPrem, TRDFF$RiskPremium)) {
    TRDFF <- subset(TRDFF, select=c(Stkcd,TradingDate,Dretwd,RiskPrem,
                                     Thr_SMB,Thr_HML,Five_SMB,Five_HML,
                                     Five_RMW,Five_CMA,Markettype,Trdsta))
} else {
    TRDFF <- subset(TRDFF, select=c(Stkcd,TradingDate,Dretwd,RiskPrem,
                                    Thr_SMB,Thr_HML,RiskPremium,Five_SMB,Five_HML,
                                    Five_RMW,Five_CMA,Markettype,Trdsta))
}
rm(TRD); str(TRDFF)



    ## output the Trading data
    paste0(datadir, 'TradFF', '.csv') %>%
    write.csv(TRDFF, file=., quote=F, row.names = F)



## announcement date of quarterly financial report ================================
ReptInfo <- dir(datadir, pattern = 'Rept.csv$') %>%
            paste0(datadir, .) %>%
            read_delim(delim='\t', na = '',
                       col_types = cols(Annodt = col_date(format = "%Y-%m-%d"))) %>%
            subset(select=c(Stkcd,Annodt,Accper,Annowk,Sctcd))  %>% 
            arrange(Stkcd) %>%
            as.data.frame()




## Stocks whether had published ex-earnings report or not =========================
PreRept <- dir(datadir, pattern = 'ForecFin.csv$') %>%
           paste0(datadir, .) %>%
           read_delim(delim='\t', na = '',
                      col_types = cols(PubliDate = col_date(format = "%Y-%m-%d"),
                                       AccPeriod = col_date(format = "%Y-%m-%d"))) %>%
           select(StockCode, Source, PubliDate, AccPeriod)
        

    

# Setup Working day arround QEA ===================================================
WorkingDay <- dir(datadir, pattern = 'TRD_Cale[.]csv$') %>%
    paste0(datadir, .) %>%
    read_delim(delim='\t', na = '',
                col_types = cols(Markettype = col_integer(),
                                 Daywk = col_integer(),
                                 Clddt = col_date(format = "%Y-%m-%d"),
                                 State = col_factor(levels = c('C', 'O'))))

    
# Assure the trading date is same between shanghai-A and shenzhen-A markets.
if (all.equal(subset(WorkingDay, Markettype == 1, select = c(State)),
    subset(WorkingDay, Markettype == 4, select = c(State))) == 1) {
    WorkingDay <- subset(WorkingDay, Markettype == 1 & State == 'O', select = c(Clddt))$Clddt
} else {print('Error! The public trading date of shanghai-A and shenzhen-A markets are different.')}



    ## save the image of aggregate information
    paste0(datadir, 'TRDFF-Rept', '.RData') %>% save.image()


    
 
# The aggregate data has formed, and then
# we generate the sub-sample in every quarterly. 
# First, we set the classification parameter ====================================


    Pretype <- c(2,3,6)
## 0, select the companies that released the **performance forecast**
## 1, Select the companies that issued the **Regular Announcement**
## 2, Select the enterprises that has published information (pre-announcement or announcement), 
##    that is, the sum of the two types of enterprises, 0 and 1.
## 3, Select the enterprises that has no previous information release, 
##    that is, remove the 0 and 1 enterprises from the total sample.
## 4, Remove the companies that released the performance forecast in the total sample
## 5, Remove the companies that published regular announcements in the total sample
## If all stocks are required, enter 6 (actually, all numbers except the above numbers is OK)


    weekterm <- c('wekbind')
# Select the stocks which announcemented on week-day(stkwek) =====================
# weekend-day(stkwnd), or all of them(wekbind, rbind(stkwek, stkwnd))
    

    Accprd <- as.Date(c('2015-09-30','2016-09-30','2017-09-30'), 
                      format = '%Y-%m-%d')
## Accounting period of quarterly earnings report ==================================
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter 
## for example, 2018-12-31 meanings that
## we concentrated on the fourth quarter of year 2018



for (A in 1:length(Accprd)) { # loop in accounting period

  
    subAccprd <- Accprd[A]
    
    # subset of the aggregate data
    TRDFFSAM <- filter(TRDFF, TradingDate %within% 
                       interval(subAccprd %m+% months(-10), subAccprd %m+% months(+12)))
    
    subReptInfo <- filter(ReptInfo, Accper == subAccprd)
    
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd)
    

    
    ## Matching ====================================================================
    ## this process is the most important port in this program
    exwind <- -130L
    bhwind <- +130L # Notice: we will have length of **260+1** time series data
    stkwek <- data.frame()
    stkwnd <- data.frame()
    stkbrk <- data.frame()
    for (i in 1:nrow(subReptInfo)) {
        QEAgrp <- data.frame()
        stkts <- filter(TRDFFSAM, Stkcd == subReptInfo[i,'Stkcd'])
        if (subReptInfo[i,'Annodt'] %in% WorkingDay) {
            n.row <- which(WorkingDay==subReptInfo[i,'Annodt'])
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(1)) # one for announcement within weekday
                     stkwek <- rbind(stkwek, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 7) {
            QEADate <- subReptInfo[i,'Annodt'] + days(2)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(0)) # zero for announcement on Saturday
                     stkwnd <- rbind(stkwnd, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 1) {
            QEADate <- subReptInfo[i,'Annodt'] + days(1)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate  %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(0)) # zero for announcement on Sunday
                     stkwnd <- rbind(stkwnd, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else stkbrk <- rbind(stkbrk, subReptInfo[i,])
    }
    rm(QEAgrp, stkts, QEADate, QEA.date, n.row)
    
    
        ## save the image
        paste0(datadir, subAccprd, '.RData') %>% save.image()
    
    
    
    for (W in weekterm) { # loop in kinds of announcement day within a week
      

        if (W == 'weekday') {
            stktrd <- stkwek
        } else if (W == 'weekend') {
            stktrd <- stkwnd
        } else {stktrd <- arrange(rbind(stkwek, stkwnd), Stkcd) }
      

        
        for (P in Pretype) { # loop in kinds of stocks whether has published ex-report or not
            
            
            if(P %in% c(0,1)) {
                stktrdp <- data.frame()
                Prestk <- filter(subPreRept, Source == P)$StockCode
                for (i in Prestk) {
                    TRDaln <- filter(stktrd, Stkcd==i)
                    stktrdp <- rbind(stktrdp, TRDaln)
                }
            } else if (P==2) {
                stktrdp <- data.frame()
                Prestk <- filter(subPreRept, Source %in% c(0,1))$StockCode
                for (i in Prestk) {
                    TRDaln <- filter(stktrd, Stkcd==i)
                    stktrdp <- rbind(stktrdp, TRDaln)
                }
            } else if (P == 3) {
                stktrdp <- stktrd
                for (i in subPreRept$StockCode) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)}
            } else if (P == 4) {
                stktrdp <- stktrd
                Prestk <- filter(subPreRept, Source == 0 )$StockCode
                for (i in Prestk) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)
                }
            } else if(P == 5) {
                stktrdp <- stktrd
                Prestk <- filter(subPreRept, Source == 1 )$StockCode
                for (i in Prestk) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)
                }
            } else {stktrdp <- stktrd}
            rm(TRDaln)
          
         
              Trdstatype <- c(1)
              stktrdp <- filter(stktrdp, Trdsta %in% Trdstatype) %>% 
                subset(select= -Trdsta)
            ## Trading status ==========================================================
            # 1=正常交易, 2=ST, 3＝*ST, 4＝S(2006年10月9日及之后股改未完成)             
            # 5＝SST, 6＝S*ST, 7=G(2006年10月9日之前已完成股改)
            # 8=GST, 9=G*ST, 10=U(2006年10月9日之前股改未完成)
            # 11=UST, 12=U*ST, 13=N, 14=NST, 15=N*ST, 16=PT       

      
         
                mkttype <- c(5) 
            # Market type 股票交易市场, 1=上海A, 4=深圳A, 16=创业板 ===================
            ## 5:上海＋深圳, 其它数字则为三板总和
            
            
            for (M in mkttype) {
                
                if (M %in% c(1,4,16)) {
                    stkdat <- filter(stktrdp, Markettype == M) 
                } else if (M==5) {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4)) 
                } else {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4,16)) 
                  }
                
                
                ## Generate the symbols of the stocks in sample and
                ## assure the time periods not existed errors
                TSN <- c()
                for (i in unique(stkdat$Stkcd)) {
                    TS <- nrow(filter(stkdat, Stkcd==i))
                    if (TS!=bhwind-exwind+1){
                        stkdat <- filter(stkdat, Stkcd!=i)
                  } else {TSN <- c(TSN,i)}
                }
                
                
                    paste(subAccprd, P, M, weekterm,'est','stkcd', sep='_') %>%
                      paste0(datadir, ., '.csv') %>%
                      write.csv(TSN, file=., quote=F, row.names = F)
                
                
                ## Extract window data  ==============================================
                
                for (i in 1:2) {
                  
                    if(i == 1) {  # Output the data of estimation window: 160 days
                      
                        exdate <- 1
                        bhdate <- 90
                        exdate.bh <- 172 # 81 trading days for event window
                        bhdate.bh <- 261

                        stkest <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% 
                                        .[c(c(exdate:bhdate),c(exdate.bh:bhdate.bh)),]
                            stkest <- rbind(stkest, stksim)
                        }
                        rm(stksim)
                    
                  
                        paste(subAccprd, P, M, weekterm, 'est','TradStat', sep='_') %>%
                        paste0(datadir, ., '.csv') %>%
                        write.csv(stkest, file=., quote=F, row.names = F)

                        
                        ## Outout the stock symbols and day index to be used in MATLAB 
                        MATdex <- cbind("Stkcd"=rep(1:length(TSN), 
                                                    each=bhdate-exdate+1+bhdate.bh-exdate.bh+1),
                                        "day"=rep(seq(from=1, 
                                                      to=bhdate-exdate+1+bhdate.bh-exdate.bh+1, by=1), 
                                                  times=length(TSN)))
                        
                            paste(subAccprd, P, M,weekterm, 'est', 'MATdex', sep='_') %>%
                            paste0(datadir, ., '.csv') %>%
                            write.csv(MATdex, file=., quote=F, row.names = F)
                    
                    
                    } else {  ## Output the data of event window: 81 days
                      
                        exdate <- 91 
                        bhdate <- 171
                        
                        stkeve <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% .[c(exdate:bhdate),]
                            stkeve <- rbind(stkeve, stksim)
                        }
                        rm(stksim)
                        
                        
                            paste(subAccprd, P, M, weekterm, 'eve', 'TradStat', sep='_') %>%
                            paste0(datadir, ., '.csv') %>%
                            write.csv(stkeve, file=., quote=F, row.names = F)
                        
                    }
                }
            }
        }
    }
}
