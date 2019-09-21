library(tidyverse)
library(timeDate, lubridate)

# setting the data directory
datadir <- 'C:/Users/Hu/Documents/NutSync/MyData/QEAData/'  # Hu, PC-Carbon
datadir <- 'D:/NutSync/MyData/QEAData/' # HHY, PC-HP 


# Input daily trading data ====================================================
filecd <- dir(datadir, pattern = '^TRD_Dalyr.*?[.]csv$') %>% paste0(datadir, .)
TRD <- data.frame()
for (i in filecd) {
    TRDS <- read_delim(i, delim='\t', na = '') %>%
              rename('TradingDate' = Trddt) %>%
              subset(select=c(Stkcd,TradingDate,Dretwd,Markettype,Trdsta))
    TRD <- rbind(TRD,TRDS)
}

# Sorting by trading date and then through stock code
TRD <- arrange(TRD, TradingDate) %>% arrange(Stkcd)


# transform the data class from date to character,
# in order to match substring (year and month) in next process
TRD$TradingDate <- as.character(TRD$TradingDate)
# R^i minus R^f ================================================================
ThrMonRisk <- dir(datadir, pattern = 'FreePremium[.]csv$') %>%
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

FF3 <- dir(datadir, pattern = 'ThrfacDay.txt$') %>%
       paste0(datadir, .) %>%
       read_delim(delim='\t', na = '')
# Weighted in Total Market Value
FF3 <- subset(FF3, MarkettypeID=='P9709', seq(from=2,to=8,by=2)) %>% 
       set_names(c('TradingDate','RiskPrem','Thr_SMB','Thr_HML')) %>% 
       arrange(TradingDate)

# Setup Working day arround QEA
FF3$TradingDate <- as.Date(FF3$TradingDate,'%Y-%m-%d')
WorkingDay <- FF3$TradingDate




## Fama-French five factor ======================================================

FF5 <- dir(datadir, pattern = 'FivefacDay.txt$') %>%
       paste0(datadir, .) %>%
       read_delim(delim='\t', na = '')

# Sorting stocks in 2*3 portfolios
FF5 <- filter(FF5, Portfolios==1) # key

# Weighted in Total Market Value
FF5 <- subset(FF5, MarkettypeID=='P9709', c(TradingDate, seq(from=5,to=13,by=2))) %>% 
       set_names(c('TradingDate','RiskPremium','Five_SMB','Five_HML',
                   'Five_RMW','Five_CMA')) %>% 
       arrange(TradingDate)

FF5$TradingDate <- as.Date(FF5$TradingDate,'%Y-%m-%d')




## Merge =========================================================================
TRDFF <- merge(TRD, FF3, by='TradingDate') %>%
         merge(FF5, by='TradingDate') %>%
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
            read_delim(delim='\t', na = '') %>%
            subset(select=c(Stkcd,Annodt,Accper,Annowk,Sctcd))  %>% 
            arrange(Stkcd) %>%
            as.data.frame()

ReptInfo$Annodt <- as.Date(ReptInfo$Annodt,'%Y-%m-%d')




## Stocks whether had published ex-earnings report or not ==============
PreRept <- dir(datadir, pattern = 'ForecFin.csv$') %>%
           paste0(datadir, .) %>%
           read_delim(delim='\t', na = '') %>%
           select(StockCode, Source, PubliDate, AccPeriod)
        

    
    ## save the image of aggregate information
    cdimage <- paste0(datadir, 'TRDFF-Rept', '.RData') %>% save.image()



## 0,选取之前发布业绩预告的企业
## 1,选取之前发布定期公告的企业
## 2,选取之前发布有信息的企业，即0、1两类企业之加和
## 3,选取之前毫无预告的企业，即从总样本中去除0、1两类企业
## 4,总样本中去除发布业绩预告的企业
## 5,总样本中去除发布定期公告的企业
## 若全部都要，输入6即可（实质上除上述数字皆可）
Pretype <- c(0,1,3,6)



## Accounting period of quarterly earnings report ==================================
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter 
## for example, 2018-12-31 meanings that
## we concentrated on the fourth quarter of year 2018
Accprd <- as.Date(c('2013-09-30','2014-09-30','2015-09-30','2016-09-30','2017-09-30'), 
                  '%Y-%m-%d')



for (A in 1:length(Accprd)) {
    
    subAccprd <- Accprd[A]
    
    # subset
    TRDFFSAM <- filter(TRDFF, TradingDate %within% 
                       interval(subAccprd %m+% months(-16), subAccprd %m+% months(+6)))
    
    subReptInfo <- filter(ReptInfo, Accper == subAccprd)
    
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd)
    
        
    ## Matching ====================================================================
    
    exwind <- -270L
    bhwind <- +40L
    stkwek <- data.frame()
    stkwnd <- data.frame()
    stkbrk <- data.frame()
    for (i in 1:nrow(subReptInfo)) {
        QEAgrp <- data.frame()
        stkts <- filter(TRDFFSAM, Stkcd == subReptInfo[i,'Stkcd'])
        if (subReptInfo[i,'Annodt'] %in% WorkingDay) {
            n.row <- which(WorkingDay==subReptInfo[i,'Annodt'])
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            for (i in 1:length(QEA.date)) {
                QEAsim <- filter(stkts, TradingDate %in% QEA.date[i])
                QEAgrp <- rbind(QEAgrp, QEAsim)
              }
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                     stkwek <- rbind(stkwek, QEAgrp),
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 7) {
            QEADate <- subReptInfo[i,'Annodt'] + days(2)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            for (i in 1:length(QEA.date)) {
                QEAsim <- filter(stkts, TradingDate %in% QEA.date[i])
                QEAgrp <- rbind(QEAgrp, QEAsim)
              }
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                     stkwnd <- rbind(stkwnd, QEAgrp),
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 1) {
            QEADate <- subReptInfo[i,'Annodt'] + days(1)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            for (i in 1:length(QEA.date)) {
                QEAsim <- filter(stkts, TradingDate  %in% QEA.date[i])
                QEAgrp <- rbind(QEAgrp, QEAsim)
              }
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                     stkwnd <- rbind(stkwnd, QEAgrp),
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else stkbrk <- rbind(stkbrk, subReptInfo[i,])
    }
    rm(QEAgrp, QEAsim, stkts, QEADate, QEA.date, n.row)
    
      
      
    ## save the image
    cdimage <- paste0(datadir, subAccprd, '.RData') %>% save.image()
    
    
      
    # Select the stocks that announcemented on week-day(stkwek) =====================
    # weekend-day(stkwnd), and all of them(rbind(stkwek, stkwnd))
    
    weekterm <- c('wekbind')
    
    for (W in weekterm) {
      

        if (W == 'weekday') {
            stktrd <- stkwek
        } else if (W == 'weekend') {
            stktrd <- stkwnd
        } else {stktrd <- arrange(rbind(stkwek, stkwnd), Stkcd) }
      
    
        
        ## Select the stocks whether had published ex-earnings report or not ========
        
        for (P in Pretype) {
            
            
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
            
            
            
            ## Trading status ==========================================================
            # 1=正常交易, 2=ST, 3＝*ST, 4＝S(2006年10月9日及之后股改未完成)             
            # 5＝SST, 6＝S*ST, 7=G(2006年10月9日之前已完成股改)
            # 8=GST, 9=G*ST, 10=U(2006年10月9日之前股改未完成)
            # 11=UST, 12=U*ST, 13=N, 14=NST, 15=N*ST, 16=PT       
            Trdstatype <- c(1, 2)
            stktrdp <- filter(stktrdp, Trdsta %in% Trdstatype) %>% 
                subset(select= -Trdsta)
            
            
            
            # Market type 股票交易市场, 1=上海A, 4=深圳A, 16=创业板 ===================
            ## 5:上海＋深圳, 其它数字则为三板总和
            mkttype <- c(1, 4, 5) # key
            
            
            for (M in mkttype) {
                
                if (M %in% c(1,4,16)) {
                    stkdat <- filter(stktrdp, Markettype == M) 
                } else if (M==5) {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4)) 
                } else {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4,16)) 
                  }
                
                stkdat <- subset(stkdat, select= - Markettype)
                
                
                ## Generate the simple-cd of the stocks in sample and
                ## assure the time periods not existed errors
                TSN <- c()
                for (i in unique(stkdat$Stkcd)) {
                    TS <- nrow(filter(stkdat, Stkcd==i))
                    if (TS!=bhwind-exwind+1){
                        stkdat <- filter(stkdat, Stkcd!=i)
                  } else {TSN <- c(TSN,i)}
                }
                
                
                ## Extract window data  ==========================================
                
                for (i in 1:2) {
                  
                    if(i == 1) {
                        exdate <- 1 # Estimation Window
                        bhdate <- 240

                        stkest <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% .[c(exdate:bhdate),]
                            stkest <- rbind(stkest, stksim)
                        }
                        rm(stksim)
                    
                  
                        ## Output the estimation data =======================================
                        
                        paste(subAccprd, P, weekterm, 'est','TradStat', sep='_') %>%
                        paste0(datadir, ., '.csv') %>%
                        write.csv(stkest, file=., quote=F, row.names = F)
                        ## the sample stock code
                        paste(subAccprd, P, weekterm,'est','stkcd', sep='_') %>%
                        paste0(datadir, ., '.csv') %>%
                        write.csv(TSN, file=., quote=F, row.names = F)
                        
                        ## stock code, day index in MATLAB 
                        MATdex <- cbind("Stkcd"=rep(1:length(TSN), each=bhdate-exdate+1),
                                        "day"=rep(seq(from=1, to=bhdate-exdate+1, by=1), times=length(TSN))) 
                        paste(subAccprd, P, weekterm, 'est', 'MATdex', sep='_') %>%
                        paste0(datadir, ., '.csv') %>%
                        write.csv(MATdex, file=., quote=F, row.names = F)
                    
                    
                    } else {
                        exdate <- 251 # Event Window
                        bhdate <- 311
                        
                        stkeve <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% .[c(exdate:bhdate),]
                            stkeve <- rbind(stkeve, stksim)
                        }
                        rm(stksim)
                  
                  
                        ## Output the event window data =======================================
                        
                        paste(subAccprd, P, weekterm, 'eve', 'TradStat', sep='_') %>%
                        paste0(datadir, ., '.csv') %>%
                        write.csv(stkeve, file=., quote=F, row.names = F)
                        
                    }
                }
            }
        }
    }
}

