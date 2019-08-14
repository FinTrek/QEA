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
