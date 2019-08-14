load(cdimage)
######### select Asset pricing model ########
#### CAPM, FF 3-factors or FF 5-factors? ####
charCAPM <- c('Stkcd', 'Dretwd', 'RiskPrem')
charFF3 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Thr_SMB', 'Thr_HML')
charFF5 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Five_SMB', 'Five_HML',
             'Five_RMW', 'Five_CMA')

stkCAPM <- subset(stkdat, select=charCAPM)
stkFF3 <- subset(stkdat, select=charFF3)
stkFF5 <- subset(stkdat, select=charFF5)

stkff <- stkFF5

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
cdcoef <- paste(QEAperd, Pretype, modeltype, weekterm,'OLScoef', sep='_') %>%
    paste0(datadir, ., '.csv')
write.csv(OLScoef, file=cdcoef, quote=F, row.names = F)
OLScoef <- as.data.frame(read_delim(cdcoef, delim=',', na = '')); rm(necoef)
OLScoef[, 3:ncol(OLScoef)] <- round(OLScoef[, 3:ncol(OLScoef)], 6)


##################################################
############### Calculate AR & CAR ###############
##################################################
timeline <- c(-20:+40)
stkevel <- datwind(2) # event window, key
stkeve <- stkevel[[1]]; windlen <- stkevel[[2]]

if(ncol(OLScoef)==5) {
    estcol <- c("MKT", "SMB", "HML")
    evecol <- c("RiskPrem", "Thr_SMB", "Thr_HML")
} else if(ncol(OLScoef)==7) {
    estcol <- c("MKT", "SMB", "HML", "RMW", "CMA")
    evecol <- c("RiskPrem", "Five_SMB", "Five_HML", "Five_RMW", "Five_CMA")
} else {
    estcol <- c("MKT")
    evecol <- c("RiskPrem")
}
QEAabr <- data.frame('tau' = timeline)
for (z in 1:grpnum) {
    subsam <- filter(PLSgrp, group==z)$Stkcd
    stkabr <- c('tau' = timeline)
    for (i in subsam) {
        evedat <- filter(stkeve, Stkcd==i)
        w <- as.matrix(evedat[, evecol]) %*% diag(OLScoef[OLScoef$Stkcd==i, estcol])
        stkfit <- rowSums(w) + filter(OLScoef,Stkcd==i)$alpha
        AbRet <- subset(evedat, select=Dretwd) - stkfit
        stkabr <- cbind(stkabr, AbRet)
    }
    colnames(stkabr) <- c('tau',subsam)
    QEAabr <- cbind(QEAabr, stkabr)
}
QEAabr <- as.data.frame(QEAabr)[,-c(1)] # Abnorl returns
rm(subsam, evedat, w, stkfit, AbRet, stkabr)

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
colnames(QEAcar) <- c(paste0('g',1:grpnum,'_PLS'))


######### Output #########
paste(QEAperd, Pretype, modeltype, weekterm,'CAR', sep='_') %>%
    paste0(datadir, ., '.csv') %>%
    write.csv(QEAcar, file=., quote=F, row.names = F)


###########################################
############# Path of CAR  ################
###########################################
titchar <- paste0('Paths of cumulative abnormal return (CAR) ', 
                  '\nattributed to quarterly earnings announcement ',
                  'arround accountting period ', QEAperd)
ggcar <- data.frame(matrix(0,windlen*grpnum,3))
for (i in 1:grpnum) {
    ifelse(i==1, ggcar[(1:windlen),] <- cbind(timeline,QEAcar[,i],c(i)),
           ggcar[(i-1)*windlen + (1:windlen), ] <- cbind(timeline,QEAcar[,i],c(i)))
}
colnames(ggcar) <- c('timeline','CAR', 'group')
ggcar$group <- as.factor(ggcar$group)

library(ggplot2)

pdf(paste(QEAperd, Pretype, modeltype, weekterm, 'CAR', sep='_') %>%
        paste0(datadir, ., '.pdf'))

ggplot(ggcar) + geom_path(size=1, aes(timeline, CAR, colour = group))  +
    labs(title = titchar, x="Time line", colour="Group") + 
    theme(plot.title = element_text(size=13), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_colour_discrete(labels=expression(
        beta['MKT'] == PLS_G1, beta['MKT'] == PLS_G2))

dev.off()


#####################################################################
##### Obtain the plots of OLS estimator used above QEA-FF data ######
#####################################################################
require(grid)
library(latex2exp)

pdf(paste(QEAperd, Pretype, modeltype, weekterm, 'OLScoefdis', sep='_') %>%
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
