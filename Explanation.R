library(tidyverse)
library(lubridate)

# Specifying trem information
Accprd <- as.Date('2017-09-30')
Pretype <- '6'
weekterm <- 'wekbind'
modeltype <- 'FF3'
datadir <- '~/NutSync/MyData/QEAData/'

stkeve <- paste('^', Accprd, '.', Pretype, '.', weekterm, 
                '.*?', 'eve_TradStat[.]csv$', sep='') %>% 
    dir(datadir, pattern = .) %>% 
    paste0(datadir, .) %>% 
    read_delim(delim=',', na = '')

Stkcd_sub <- unique(stkeve$Stkcd)


TRD_Dalyr <- read_delim("~/NutSync/MyData/QEAData/TRD_Dalyr.txt", "\t", 
                        escape_double = FALSE, trim_ws = TRUE,
                        col_types = cols(
                        Adjprcnd = col_skip(), Adjprcwd = col_skip(), 
                        Capchgdt = col_skip(), Clsprc = col_number(), 
                        Dnshrtrd = col_number(), 
                        Dnvaltrd = col_number(), Dretnd = col_skip(), 
                        Dretwd = col_skip(), Dsmvosd = col_number(), 
                        Dsmvtll = col_number(), Hiprc = col_number(), 
                        Loprc = col_number(), Markettype = col_integer(), 
                        Opnprc = col_number(), Stkcd = col_character(), 
                        Trddt = col_date(format = "%Y-%m-%d"), 
                        Trdsta = col_integer())) %>% 
            rename('TradingDate' = Trddt)

TRD_Dalyr_sub <- TRD_Dalyr[-c(1,2), ] %>% 
             subset(Stkcd %in% Stkcd_sub) %>% 
             subset(TradingDate %within% interval(Accprd %m+% months(-6), Accprd %m+% months(+6)))


TRD_sam <- data.frame()    
for (i in Stkcd_sub) {
    TRD_sim <- TRD_Dalyr_sub %>% as_tibble() %>%
        subset(Stkcd == i) %>% 
        mutate(amplitude = c(NaN, (Hiprc - Loprc)[-1] / Clsprc[-length(Clsprc)]))
    TRD_sam <- rbind(TRD_sam, TRD_sim)
}
TRD_Dalyr_sub <- TRD_sam; rm(TRD_sam, TRD_sim)



STK_MKT_Dalyr <- read_delim("~/NutSync/MyData/QEAData/STK_MKT_Dalyr.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE,
                            col_types = cols(Amount = col_skip(), Symbol = col_character(),
                                             ChangeRatio = col_skip(), 
                                             CirculatedMarketValue = col_skip(), 
                                             Liquidility = col_number(), PB = col_number(), 
                                             PCF = col_skip(), PE = col_number(), 
                                             PS = col_number(), Ret = col_number(), 
                                             SecurityID = col_skip(), ShortName = col_skip(), 
                                             TradingDate = col_date(format = "%Y-%m-%d"), 
                                             Turnover = col_number())) %>% 
                rename('Stkcd' = Symbol)

STK_MKT_Dalyr_sub <- STK_MKT_Dalyr[-c(1,2), ] %>% 
    subset(Stkcd %in% Stkcd_sub) %>% 
    subset(TradingDate %within% interval(Accprd %m+% months(-6), Accprd %m+% months(+6)))



TRD_reg <- merge(stkeve, TRD_Dalyr_sub, by = c("Stkcd", "TradingDate"))
TRD_reg <- merge(TRD_reg, STK_MKT_Dalyr_sub, by = c("Stkcd", "TradingDate"))



ARcd <- paste('^', Accprd, '.', Pretype, '.', modeltype, '.', weekterm, 
                '.', 'group', '.*?', 'AR[.]csv$', sep='') %>% 
        dir(datadir, pattern = .) %>% 
        paste0(datadir, .)


for (i in 1:length(ARcd)) {
   assign(paste0('stkabr', '_g', i), read_delim(ARcd[i], delim=',', na = ''))
   stkabrgp <- get(paste0('stkabr', '_g', i))
   assign(paste0('TRD', '_reg','_g', i), subset(TRD_reg, Stkcd %in% colnames(stkabrgp)))
   TRD_reg_gp <- get(paste0('TRD', '_reg','_g', i))
   
   TRD_lm <- data.frame()
   for (z in colnames(stkabrgp)[-1]) {
       TRD_abr <- cbind(subset(TRD_reg_gp, Stkcd == z),
                        stkabrgp[, colnames(stkabrgp) == z], 
                        'group' = c(i))
       if (z == unique(TRD_abr$Stkcd)) {
          colnames(TRD_abr)[ncol(TRD_abr)-1] <- 'abnormalreturn' 
          TRD_lm <- rbind(TRD_lm, TRD_abr)
       } else cat('exsit matching error for stock', z)
   }
   assign(paste0('TRD', '_reg', '_g', i), TRD_lm)
}

TRD_reg <- mget(ls(pattern = '^TRD_reg_g'))
rm(stkabrgp, TRD_reg_gp, TRD_abr,TRD_lm)




# Regression
summary(lm(abnormalreturn ~ Liquidility, data = TRD_reg[[1]]))
summary(lm(abnormalreturn ~ Liquidility, data = TRD_reg[[2]]))
summary(lm(abnormalreturn ~ Liquidility, data = rbind()))

summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = TRD_reg[[1]]))
summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = TRD_reg[[2]]))
summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = rbind()))


# Statistical properties
summary(TRD_reg_g1$Clsprc)
summary(TRD_reg_g1$Clsprc)

plot(density(TRD_reg_g1$Clsprc))
lines(density(TRD_reg_g2$Clsprc), col='red')


# Standard deviation by everyday (event window)
TRD_clsprc_sd <- c()
for (i in 1:length(ARcd)) {
    TRD_reg_ana <- get(ls(pattern = '^TRD_reg_g')[i])
    stkabr_ana <- get(ls(pattern = '^stkabr_g')[i])
    TRD_clsprc <- c()
    for (z in 1:nrow(stkabr_ana)) {
        sd_nrow <- seq(z, nrow(stkabr_ana)*(ncol(stkabr_ana)-1), nrow(stkabr_ana))
        TRD_clssd <- sd(TRD_reg_ana$Clsprc[sd_nrow])
        TRD_clsprc <- c(TRD_clsprc, TRD_clssd)
    }
    TRD_clsprc_sd <- cbind(TRD_clsprc_sd, TRD_clsprc)
}
colnames(TRD_clsprc_sd) <- ls(pattern = '^stkabr_g')
