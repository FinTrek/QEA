###############################################
######### Read the result from MATLAB #########
###############################################
library(R.matlab)
path <- ("~/NutSync/QEA/Data/Matlab_PLS")

# PLS coefficients
PLScoef <- readMat(file.path(path, "est_PLS.mat"))
PLScoef <- as.data.frame(PLScoef[[1]])
PLScolnam <- c()
for (i in 1:grpnum) {
  colnam <- paste0('g', i, '_', c('coef', 'sd', 't'))
  PLScolnam <- c(PLScolnam ,colnam)
}
rm(colnam); colnames(PLScoef) <- PLScolnam

# Group information
PLSgrp <- readMat(file.path(path, "group.mat"))
PLSgrp <- data.frame(matrix(unlist(PLSgrp), nrow=length(TSN), byrow=T),
                     stringsAsFactors=FALSE)
colnames(PLSgrp) <- c("group")
grpnum <- length(unique(PLSgrp$group))
# grouped stock code
PLSclus <- data.frame()
samnum <- c()
for (i in 1:grpnum) {
  sam <- TSN[PLSgrp$group==i]
  samn <- length(sam)
  sam <- cbind(sam,c(i))
  PLSclus <- rbind(PLSclus, sam)
  samnum <- c(samnum,samn)
}
PLSgrp <- PLSclus; rm(sam,samn,PLSclus)
colnames(PLSgrp) <- c('Stkcd', 'group')
PLSgrp$Stkcd <- as.character(PLSgrp$Stkcd)


##########################################################
#### Assure the OLS result in R is same with matlab's ####
##########################################################
charff3 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Thr_SMB', 'Thr_HML')
charff5 <- c('Stkcd', 'Dretwd', 'RiskPrem', 'Five_SMB', 'Five_HML', 
  'Five_RMW', 'Five_CMA')
# demean by function scale
# one by one
ifelse(nrow(PLScoef==3),
       stkdem <- subset(stkdat, select=charff3),
       stkdem <- subset(stkdat, select=charff5))
stkdem <- scale(subset(stkdem,select=-Stkcd),center=T,scale=F)
# personal function for getting the demeaned OLS estimator
stkcoef <- function(x) {
  g <- data.frame()
  for(i in unique(x$Stkcd)) {
    q <- filter(x, Stkcd==i)[,-1]
    stkreg <- lm(Dretwd ~ ., data = q) 
    stkregcoef <- t(as.data.frame(coef(stkreg)))
    k <- cbind(i, stkregcoef)
    g <- rbind(g,k)
  }
  return(as.data.frame(g))
}
OLScoef <- stkcoef(stkdem) 


#####################################################################
########### the statistical properties of QEA daily data ############
#####################################################################

# estimate coefficients one by one
stkff3 <- subset(stkdat, select=charff3)
stkff5 <- subset(stkdat, select=charff5)

stkff <- stkff3 # Very Important!

# the correlation coefficient between explanation variables
rownum <- estwndlen * sample(1:length(unique(stkdat$Stkcd)), size = 1)
cor(subset(stkff[c(1:estwndlen)+rownum,], select = -c(Stkcd, Dretwd)))
# Original OLS estimator within estimation window 
samcoef <- stkcoef(stkff) 
# solve the problem of data type (factor to numeric)
ifelse (ncol(samcoef)==5, # 5= 2(Stkcd+alpha) + 3 factors 
        colnames(samcoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML"),
        colnames(samcoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML", "RMW", "CMA"))
necoef <- paste0('~/R/Data/', QEAperd, '-coef', '.csv')
write.csv(samcoef, file=necoef, quote=F, row.names = F)
samcoef <- as.data.frame(read_delim(necoef, delim=',', na = ''))
samcoef[, 3:ncol(samcoef)] <- round(samcoef[, 3:ncol(samcoef)], 6)


# plot of estimation results
library(ggplot2)
require(grid)
library(latex2exp)

grid.newpage()
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
ifelse(ncol(samcoef)==5,
{ pushViewport(viewport(layout = grid.layout(2,2)))
  print(qplot(samcoef$MKT, xlab=TeX('$\\beta_1$ of MKT'), ylab = "Count",bins=100), 
      vp = vplayout(1,1:2))
  print(qplot(samcoef$SMB, xlab=TeX('$\\beta_2$ of SMB'), ylab = "Count",bins=30), 
      vp = vplayout(2,1))       
  print(qplot(samcoef$HML, xlab=TeX('$\\beta_3$ of HML'), ylab = "Count",bins=30), 
      vp = vplayout(2,2))},
{ pushViewport(viewport(layout = grid.layout(2,4)))
  print(qplot(samcoef$MKT, xlab=TeX('$\\beta_1$ of MKT'), ylab = "Count",bins=100), 
        vp = vplayout(1,1:4))
  print(qplot(samcoef$SMB, xlab=TeX('$\\beta_2$ of SMB'), ylab = "Count",bins=30), 
        vp = vplayout(2,1))       
  print(qplot(samcoef$HML, xlab=TeX('$\\beta_3$ of HML'), ylab = "Count",bins=30), 
        vp = vplayout(2,2))
  print(qplot(samcoef$RMW, xlab=TeX('$\\beta_4$ of RMW'), ylab = "Count",bins=30), 
        vp = vplayout(2,3))
  print(qplot(samcoef$CMA, xlab=TeX('$\\beta_5$ of CMA'), ylab = "Count",bins=30), 
        vp = vplayout(2,4))}
)


##################################################
############### Calculate AR & CAR ###############
##################################################
timeline <- c(-20:+40)
evewndlen <- length(timeline)
stkeve <- datwind(2) # event window, key
ifelse(ncol(samcoef)-2==3,
       {estcol <- c("MKT", "SMB", "HML")
       evecol <- c("RiskPrem", "Thr_SMB", "Thr_HML")},
       {estcol <- c("MKT", "SMB", "HML", "RMW", "CMA")
       evecol <- c("RiskPrem", "Five_SMB", "Five_HML", "Five_RMW", "Five_CMA")})
QEAabr <- data.frame('tau' = timeline)
for (z in 1:grpnum) {
  subsam <- filter(PLSgrp, group==z)$Stkcd
  stkabr <- c('tau' = timeline)
  for (i in subsam) {
    evedat <- filter(stkeve, Stkcd==i)
    w <- as.matrix(evedat[, evecol]) %*% diag(samcoef[samcoef$Stkcd==i, estcol])
    stkfit <- rowSums(w) + samcoef[samcoef$Stkcd==i, 'alpha']
    AbRet <- c(subset(estdat, Dretwd) - stkfit)
    stkabr <- cbind(stkabr, AbRet)
  }
  colnames(stkabr) <- c('tau',subsam)
  QEAabr <- cbind(QEAabr, stkabr)
}
QEAabr <- as.data.frame(QEAabr)[,-c(1)] # Abnorl returns
rm(subsam, evedat, w, stkfit, AbRet, stkabr)

QEAcar <- as.data.frame(matrix(c(length(evewndlen*grpnum)),evewndlen,grpnum))
for (i in 1:grpnum) {
  ifelse(i==1,
         QEAabrmen <- rowMeans(QEAabr[,1+c(1:samnum[i])]),
         QEAabrmen <- rowMeans(QEAabr[,sum(samnum[1:i-1])+(i-1) + 
                                        1+c(1:samnum[i])]))
  for (t in 1:evewndlen) {
    ifelse(t==1,
           QEAcar[t,i] <- QEAabrmen[t],
           QEAcar[t,i] <- sum(QEAabrmen[1:t])) 
  }
}
colnames(QEAcar) <- c(paste0('g',1:grpnum,'_PLS'))


###########################################
############# Path of CAR  ################
###########################################
ggcar <- data.frame(matrix(0,evewndlen*grpnum,3))
for (i in 1:grpnum) {
  ifelse(i==1, ggcar[(1:evewndlen),] <- cbind(timeline,QEAcar[,i],c(i)),
  ggcar[(i-1)*evewndlen + (1:evewndlen), ] <- cbind(timeline,QEAcar[,i],c(i)))
}
colnames(ggcar) <- c('timeline','CAR', 'group')
ggcar$group <- as.factor(ggcar$group)
ggplot(ggcar) + geom_path(size=1, aes(timeline, CAR, colour = group))  +
  labs(title = titchar, x="Time line", colour="PLS") + 
  theme(plot.title = element_text(size=15), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_discrete(labels=expression(beta[1] == PLScoef[1,1], beta[1] == PLScoef[1,4]))


###################### Output #######################
necar <- paste0('~/R/Data/', QEAperd, 'CAR','.RData')
write.csv(QEAcar, file=necar, quote=F, row.names = F)
write.csv(round(PLScoef,4), file = "~/R/Data/PLSrelt.csv", 
          row.names = F)
