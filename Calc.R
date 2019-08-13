###############################################
######### Read the result from MATLAB #########
###############################################
library(R.matlab)
PLSpath <- ("~/NutSync/QEA/Data, Code/Matlab_PLS")

# Group information
PLSclus <- readMat(file.path(PLSpath, "group.mat")) %>% `[[`(1) 
colnames(PLSclus) <- c("group")
grpnum <- length(unique(PLSclus))
# grouped stock code
PLSgrp <- data.frame()
samnum <- c()
for (i in 1:grpnum) {
  sam <- TSN[PLSclus==i]
  samn <- length(sam)
  sam <- cbind(sam,c(i))
  PLSgrp <- rbind(PLSgrp, sam)
  samnum <- c(samnum,samn)
}
rm(sam,samn,PLSclus)
colnames(PLSgrp) <- c('Stkcd', 'group')
PLSgrp$Stkcd <- as.character(PLSgrp$Stkcd)

# PLS coefficients
PLScoef <- readMat(file.path(PLSpath, "est_PLS.mat")) %>% `[[`(1)
PLScolnam <- c()
for (i in 1:grpnum) {
  colnam <- paste0('g', i, '_', c('coef', 'sd', 't'))
  PLScolnam <- c(PLScolnam ,colnam)
}
colnames(PLScoef) <- PLScolnam; rm(colnam)
paste(QEAperd, Pretype, modeltype, weekterm,'PLScoef', sep='_') %>%
  paste0(datadir, ., '.csv') %>%
  write.csv(round(PLScoef,4), file=., quote=F, row.names = F)

########## Assure the OLS estimator in R is same with MATLAB's ##########
# demean by function 'scale'
stkdem <- data.frame()
if(nrow(PLScoef==3)) {
  for (i in unique(stkdat$Stkcd)) {
    stkeve <- filter(stkdat, Stkcd==i) %>%
              subset(select=charFF3)
    stkeve[,c(2:5)] <- scale(subset(stkeve,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,stkeve)
  }
} else if (nrow(PLScoef==5)) {
  for (i in unique(stkdat$Stkcd)) {
    stkeve <- filter(stkdat, Stkcd==i) %>%
              subset(select=charFF5)
    stkeve[,c(2:7)] <- scale(subset(stkeve,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,stkeve)
  }
} else {
  for (i in unique(stkdat$Stkcd)) {
    stkeve <- filter(stkdat, Stkcd==i) %>%
              subset(select=charCAPM)
    stkeve[,c(2:3)] <- scale(subset(stkeve,select= -Stkcd),center=T,scale=F)
    stkdem <- rbind(stkdem,stkeve)
  }
}
nedem <- paste(QEAperd, Pretype, modeltype, weekterm,'demean', sep='_') %>%
      paste0(datadir, ., '.csv') 
write.csv(stkdem, file=nedem, quote=F, row.names = F)
stkdem <- read_delim(nedem, delim=',', na = '')

# personal function to getting the OLS estimator of demeaned data
OLScoefdem <- stkcoef(stkdem) 



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
ggplot(ggcar) + geom_path(size=1, aes(timeline, CAR, colour = group))  +
  labs(title = titchar, x="Time line", colour="PLS") + 
  theme(plot.title = element_text(size=13), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_discrete(labels=expression(beta[1] == PLS_G1, beta[1] == PLS_G2))

###################### Output #######################
necar <- paste0('~/R/Data/', QEAperd, '-', modeltype, '-CAR','.RData')
write.csv(QEAcar, file=necar, quote=F, row.names = F)

