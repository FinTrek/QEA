
library(R.matlab)
path <- ("~/NutSync/EQA/Data/Matlab_PLS")

# Group result from matlab
PLSgrp <- readMat(file.path(path, "group.mat"))
PLSgrp <- data.frame(matrix(unlist(PLSgrp), nrow=1172, byrow=T),stringsAsFactors=FALSE)
colnames(PLSgrp) <- c("group")
# grouped stock code
subsamA <- TSN[group[,1]==1]
subsamB <- TSN[group[,1]==2]
subsamC <- TSN[group[,1]==3]
subsam.N <- c()

# PLS coefficients
coefgrp <- readMat(file.path(path, "est_PLS.mat"))
coefgrp <- as.data.frame(coefgrp[[1]])
colnames(coefgrp) <- c('g1_coef', 'g1_sd', 'g1_t', 'g2_coef', 'g2_sd', 'g2_t', 
                      'g3_coef', 'g3_sd', 'g3_t')
write.csv(coefgrp, file = "PLScoef.csv", row.names = F)
#######################



#####################################################################
########### the statistical properties of QEA daily data ############
#####################################################################

# the correlation coefficient between explanation variables
cor(stknor[(1:T)+T*sample(1:length(unique(stknor[,ecode])), size = 1), c(4,7:10)])

######################### demean ##################
ecode <- c(1L)
edate <- c(2L)
demean <- function(x) {
  f <- data.frame()
  j <- data.frame()
  for (i in unique(x[,ecode])) {
    w <- filter(x, x[,ecode]==i)
    z <- colMeans(w[, - c(ecode, edate, ncol(x))])
    for (l in 1:length(z)) { w[,l+2] <- w[,l+2] - z[l] }
    j <- rbind(j, w)
  }
  return(j)
}

stkdm <- demean(stknor)


## estimate coefficients one by one
stkest <- stknor # for stkdm, acquire the result is same with matlab
stkff3 <- select(stkest, c('Stkcd', 'Dretwd', 'RiskPrem', 'Thr_SMB', 'Thr_HML'))

stkff5 <- select(stkest, c('Stkcd', 'Dretwd', 'RiskPrem', 
                           'Five_SMB', 'Five_HML', 'Five_RMW', 'Five_CMA'))

stkcoef <- function(x) {
  y <- unique(x[,ecode])
  g <- data.frame()
  for(i in 1:length(y)) {
    z <- y[i]
    q <- filter(x, x[,ecode]==z)[,-1]
    storeg <- lm(Dretwd ~ ., data = q) 
    storegcoef <- t(as.data.frame(coef(storeg)))
    k <- cbind(z, storegcoef)
    g <- rbind(g,k)
  }
  return(g)
}

samcoef <- as.data.frame(stkcoef(stkff3))
# solve the problem of data type (factor to numeric)
colnames(samcoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML")
# as for FF-5
colnames(samcoef) <- c("Stkcd", "alpha","MKT", "SMB", "HML", "RMW", "CMA")
nece <- paste0('coef-',QEAper, '.csv')
write.csv(samcoef, file=nece, quote=F, row.names = F)
samcoef <- as.data.frame(read_delim(nece, delim=',', na = ''))
samcoef[,3:ncol(samcoef)] <- round(samcoef[,3:ncol(samcoef)],6)


#####################################################################
########### the plot of estimation results ############
#####################################################################
library(ggplot2)
require(grid)
library(latex2exp)
a <- qplot(samcoef[,3], xlab=TeX('$\\beta_1$ of MKT'), ylab = "count",bins=50)
b <- qplot(samcoef[,4], xlab=TeX('$\\beta_2$ of SMB'), ylab = "count",bins=50)
c <- qplot(samcoef[,5], xlab=TeX('$\\beta_3$ of HML'), ylab = "count",bins=50)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(a, vp = vplayout(1,1))
print(b, vp = vplayout(1,2))       
print(c, vp = vplayout(1,3)) 

###################################################
############### Calculate AR & CAR ###############
#################################################

#expected value
QEAES <- function(x, y) {
  p <- c()
  for (i in 1:nrow(x)) {
    code <- x[i, ecode]
    z <- filter(y, y[,ecode]==code)
    w <- as.matrix(z[, colnum]) %*% diag(x[i, colnum])
    q <- rowSums(w) + x[i, 2]
    p <- c(p ,q)
  }
  return(p)
}


CAR <- function(x) {
  q <- list(length=ET)
  p <- c()
  z <- c()
  for(i in 1:ET) { q[[i]] <- x[seq(i,ET*N.sam,by=ET)] }
  for(i in 1:ET) { p <- c(p, mean(q[[i]])) }
  q[[ET+1]] <- p
  z[1] <- q[[ET+1]][1]
  for (i in 1:ET) { z <- c(z, z[i] + q[[ET+1]][i+1]) }
  q[[ET+1+1]] <- z
  return(q)
}


colnum <- c(3,4,5)
ET <- 61L
exdate <- 1
bhdate <- 240


for (z in c(1,2,3)) {
  subsam <- TSN[group[,1]==z]
  
  stknorab <- data.frame()
  for (i in subsam) {
    QEAsam <- filter(stknor[c(exdate:bhdate),], Stkcd==i)
    stknorab <- rbind(stknorab, QEAsam)
  }
  
  stkfit <- QEAES(samcoef, stknorab)
  abRet <- stknorab[, 2] - stkfit
  
  stkcar <- CAR(abRet)
  QEAcar <- as.data.frame(stkcar[[ET+1+1]][1:ET])
  
  QEAcargrp <- rbind(QEAcargrp, QEAcar)
}

QEAcargrp$group <- as.factor(c(rep(1:3,each=61)))
QEAcargrp <- cbind(time=rep(-30:+30,3), QEAcargrp)

#####
write.csv(CAR.g, file = "CAR.g.csv", row.names = F)
#####

# plot
ggplot(CAR.g[CAR.g[,2]==1,], aes(x=seq(-20,+20), y=CAR)) + geom_line()
ggplot(CAR.g, aes(time, CAR, colour = group)) + geom_path() 


timeline <- seq(-30,+30)
ggplot() + geom_line(aes(x=timeline, y=CAR), CAR.g[CAR.g[,2]==1,], color="red") +  
  geom_line(aes(x=timeline, y=CAR), CAR.g[CAR.g[,2]==2,], color="blue") +
  geom_line(aes(x=timeline, y=CAR), CAR.g[CAR.g[,2]==3,], color="black") + 
  xlab("Time line")
