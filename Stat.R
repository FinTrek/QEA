#####################################################################
########### the statistical properties of QEA daily data ############
#####################################################################
# demean
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
stkest <- stkdm
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

# plot
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

# the correlation coefficient between explanation variables
cor(stknor[(1:T)+T*sample(1:length(unique(stknor[,ecode])), size = 1), c(4,7:10)])

