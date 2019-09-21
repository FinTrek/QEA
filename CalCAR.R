library(tidyverse)

# Input window data ===========================================================
datadir <- '~/NutSync/MyData/QEAData/'

# Specifying trem information
Accprd <- '2017-09-30'
Pretype <- '6'
weekterm <- 'wekbind'


Stkcd <- paste('^', Accprd, '.', Pretype, '.', weekterm, 
               '.*?', 'est_stkcd[.]csv$', sep='') %>% 
    dir(datadir, pattern = .) %>% 
    paste0(datadir, .) %>% 
    read_delim(delim=',', na = '')


stkest <- paste('^', Accprd, '.', Pretype, '.', weekterm, 
                '.*?', 'est_TradStat[.]csv$', sep='') %>% 
    dir(datadir, pattern = .) %>% 
    paste0(datadir, .) %>% 
    read_delim(delim=',', na = '')


stkeve <- paste('^', Accprd, '.', Pretype, '.', weekterm, 
                '.*?', 'eve_TradStat[.]csv$', sep='') %>% 
    dir(datadir, pattern = .) %>% 
    paste0(datadir, .) %>% 
    read_delim(delim=',', na = '')



## Read the result from MATLAB ==================================================
library(R.matlab)
PLSpath <- "~/NutSync/QEA/Matlab_PLS"

# Group information
PLSclus <- readMat(file.path(PLSpath, 
                             paste('group','-', Pretype, '-',Accprd,'.mat', sep = ''))) %>% `[[`(1) 
colnames(PLSclus) <- c("group")
grpnum <- length(unique(PLSclus))
# grouped stock code
PLSgrp <- data.frame()
samnum <- c()
for (i in 1:grpnum) {
    sam <- Stkcd[PLSclus==i, 1]
    samn <- nrow(sam)
    sam <- cbind(sam,c(i))
    PLSgrp <- rbind(PLSgrp, sam)
    samnum <- c(samnum,samn)
}
rm(sam,samn,PLSclus)
colnames(PLSgrp) <- c('Stkcd', 'group')
PLSgrp$Stkcd <- as.character(PLSgrp$Stkcd)


# PLS coefficients
PLScoef <- readMat(file.path(PLSpath, 
                             paste('est_PLS','-',Pretype, '-',Accprd,'.mat', sep = ''))) %>% `[[`(1)
PLScolnam <- c()
for (i in 1:grpnum) {
    colnam <- paste0('g', i, '_', c('coef', 'sd', 't'))
    PLScolnam <- c(PLScolnam ,colnam)
}
colnames(PLScoef) <- PLScolnam; rm(colnam)




## select Asset pricing model, CAPM, FF 3-factors or FF 5-factors? =================

if (nrow(PLScoef)==3) { # 5= 2(Stkcd+alpha) + 3 factors
    stkff <- subset(stkest, select=c(Stkcd, Dretwd, RiskPrem, Thr_SMB, Thr_HML))
    modeltype <- 'FF3'
} else if (nrow(PLScoef)==5) {
    stkff <- subset(stkest, select=c(Stkcd, Dretwd, RiskPrem, Five_SMB, Five_HML,
                                     Five_RMW, Five_CMA))
    modeltype <- 'FF5'
} else {
    stkff <- subset(stkest, select=c(Stkcd, Dretwd, RiskPrem))
    modeltype <- 'CAPM'
}

cdgrp <- paste(Accprd, Pretype, modeltype, weekterm,'group', sep='_') %>%
         paste0(datadir, ., '.csv')
write.csv(PLSgrp, file=cdgrp, quote=F, row.names = F)


windlen <- 240L


# the correlation coefficient between explanation variables
rownum <- windlen * sample(1:length(unique(stkff$Stkcd)), size = 1)
cor(subset(stkff[c(1:windlen)+rownum,], select = -c(Stkcd, Dretwd))); rm(rownum)


## personal function for getting the OLS estimator one by one ======================
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


# Set colunm names and data extraction strings
if (ncol(OLScoef)==5) { # 5= 2(Stkcd+alpha) + 3 factors
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML")
    coefcol <- c("MKT", "SMB", "HML")
    evencol <- c("RiskPrem", "Thr_SMB", "Thr_HML")
} else if (ncol(OLScoef)==7) {
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML", "RMW", "CMA")
    coefcol <- c("MKT", "SMB", "HML", "RMW", "CMA")
    evencol <- c("RiskPrem", "Five_SMB", "Five_HML", "Five_RMW", "Five_CMA")
} else {
    colnames(OLScoef) <- c("Stkcd", "alpha","MKT")
    coefcol <- c("MKT")
    evencol <- c("RiskPrem")
}


# solve the problem of data type (factor to numeric)
cdcoef <- paste(Accprd, Pretype, modeltype, weekterm,'OLScoef', sep='_') %>%
          paste0(datadir, ., '.csv')
write.csv(OLScoef, file=cdcoef, quote=F, row.names = F)
OLScoef <- as.data.frame(read_delim(cdcoef, delim=',', na = '')); rm(cdcoef)
OLScoef[, 3:ncol(OLScoef)] <- round(OLScoef[, 3:ncol(OLScoef)], 6)



## Calculate AR & CAR ================================================================

timeline <- c(-20:+40)
windlen <- length(timeline)


QEAabr <- data.frame('tau' = timeline)
for (z in 1:grpnum) {
    subsam <- filter(PLSgrp, group==z)$Stkcd
    stkabr <- c('tau' = timeline)
    for (i in subsam) {
        evedat <- filter(stkeve, Stkcd==i)
        w <- as.matrix(evedat[, evencol]) %*% diag(subset(OLScoef, Stkcd==i, coefcol))
        stkfit <- rowSums(w) + subset(OLScoef, Stkcd==i)$alpha
        AbRet <- subset(evedat, select=Dretwd) - stkfit
        stkabr <- cbind(stkabr, AbRet)
    }
    colnames(stkabr) <- c('tau',subsam)
    QEAabr <- cbind(QEAabr, stkabr)
}
QEAabr <- as.data.frame(QEAabr)[,-c(1)] # Abnormal returns
rm(subsam, evedat, w, stkfit, AbRet, stkabr)


# Output the abnormal returns
for (i in 1:grpnum) {
    if (i == 1) {
        paste(Accprd, Pretype, modeltype, weekterm, 'group', i, 'AR', sep='_') %>%
            paste0(datadir, ., '.csv') %>%
            write.csv(QEAabr[,c(1:(samnum[i]+1))], file=., quote=F, row.names = F)
    } else {
        paste(Accprd, Pretype, modeltype, weekterm, 'group', i, 'AR', sep='_') %>%
            paste0(datadir, ., '.csv') %>%
            write.csv(QEAabr[,c((i+sum(samnum[1:(i-1)])) : (i+sum(samnum[1:i])))], 
                      file=., quote=F, row.names = F)
    }
}



QEAcar <- as.data.frame(matrix(c(length(windlen*grpnum)),windlen,grpnum))
for (i in 1:grpnum) {
    ifelse(i==1,
           QEAabrmen <- rowMeans(QEAabr[,1+c(1:samnum[i])]),
           QEAabrmen <- rowMeans(QEAabr[,sum(samnum[1:i-1])+(i-1) + 
                                            1+c(1:samnum[i])]))
    for (t in 1:windlen) {
        ifelse(t==1,
               QEAcar[t,i] <- QEAabrmen[t],
               QEAcar[t,i] <- sum(QEAabrmen[1:t])) 
    }
}
colnames(QEAcar) <- c(paste0('g',1:grpnum,'_PLS')) # Calculative abnormal returns



QEAcar <- mutate(QEAcar,sum=rowSums(QEAcar)/2)


# Output 
paste(Accprd, Pretype, modeltype, weekterm,'CAR', sep='_') %>%
paste0(datadir, ., '.csv') %>%
write.csv(QEAcar, file=., quote=F, row.names = F)



## Path of CAR ========================================================================

titchar <- paste0('Paths of cumulative abnormal return (CAR) ', 
                  '\nattributed to quarterly earnings announcement ',
                  'arround accountting period ', Accprd)
ggcar <- data.frame(matrix(0,windlen*grpnum,3))
for (i in 1:(grpnum+1)) {
    ifelse(i==1, ggcar[(1:windlen),] <- cbind(timeline,QEAcar[,i],c(i)),
           ggcar[(i-1)*windlen + (1:windlen), ] <- cbind(timeline,QEAcar[,i],c(i)))
}
colnames(ggcar) <- c('timeline','CAR', 'group')
ggcar$group <- as.factor(ggcar$group)



library(ggplot2)

pdf(paste(Accprd, Pretype, modeltype, weekterm, 'CAR', sep='_') %>%
    paste0(datadir, ., '.pdf'))

if (grpnum+1==4) {
  ggplot(ggcar, aes(timeline, CAR, 
                    linetype = group, colour = group)) + 
    geom_line() + geom_point() +
    scale_linetype_manual(name='Group',values=c("solid", 'solid', 'solid', "dotted"),
                          labels=c('PLS_G1', 'PLS_G2', 'PLS_G3','Unclassified')) +
    scale_colour_manual(name="Group", values = c("blue", "red", 'green', "black"),
                        labels=c('PLS_G1', 'PLS_G2', 'PLS_G3','Unclassified')) +
    labs(title = titchar, x = "Time line", y = 'Cumulative Abnormal Return') + 
    theme(plot.title = element_text(size=11), 
          axis.ticks.y = element_blank())
  
} else if (grpnum+1==3) {
    ggplot(ggcar, aes(timeline, CAR, 
                      linetype = group, colour = group)) + 
        geom_line() + geom_point() +
        scale_linetype_manual(name='Group',values=c("solid", 'solid', "dotted"),
                              labels=c('PLS_G1', 'PLS_G2', 'Unclassified')) +
        scale_colour_manual(name="Group", values = c("blue", "red", "black"),
                        labels=c('PLS_G1', 'PLS_G2', 'Unclassified')) +
        labs(title = titchar, x = "Time line", y = 'Cumulative Abnormal Return') + 
        theme(plot.title = element_text(size=11), 
              axis.ticks.y = element_blank())
} else print('Group number is error')


dev.off()



## Obtain the plots of OLS estimator used above QEA-FF data ===========================

require(grid)
library(latex2exp)

pdf(paste(Accprd, Pretype, modeltype, weekterm, 'OLScoefdis', sep='_') %>%
    paste0(datadir, ., '.pdf'))

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

dev.off()





## Assure the OLS estimator in R is same with MATLAB's ==============================
# demean by function 'scale'

stkdem <- data.frame()
if(nrow(PLScoef==3)) {
  for (i in unique(stkest$Stkcd)) {
    estdat <- filter(stkest, Stkcd==i) %>%
              subset(select=charFF3)
    estdat[,c(2:5)] <- scale(subset(estdat,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,estdat)
  }
} else if (nrow(PLScoef==5)) {
  for (i in unique(stkest$Stkcd)) {
    estdat <- filter(stkest, Stkcd==i) %>%
              subset(select=charFF5)
    estdat[,c(2:7)] <- scale(subset(estdat,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,estdat)
  }
} else {
  for (i in unique(stkest$Stkcd)) {
    estdat <- filter(stkest, Stkcd==i) %>%
              subset(select=charCAPM)
    estdat[,c(2:3)] <- scale(subset(estdat,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,estdat)
  }
}

# personal function to getting the OLS estimator of demeaned data ================
OLScoefdem <- stkcoef(stkdem)
