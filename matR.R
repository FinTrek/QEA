
library(R.matlab)
path <- ("~/NutSync/EQA/Data/Matlab_PLS")

# group
group <- readMat(file.path(path, "group.mat"))
group <- data.frame(matrix(unlist(group), nrow=1172, byrow=T),stringsAsFactors=FALSE)
colnames(group) <- c("group")

# PLs coefficients
coef.g <- readMat(file.path(path, "est_PLS.mat"))
coef.g <- as.data.frame(coef.g[[1]])
colnames(coef.g) <- c('g1_coef', 'g1_sd', 'g1_t', 'g2_coef', 'g2_sd', 'g2_t', 
                      'g3_coef', 'g3_sd', 'g3_t')
write.csv(coef.g, file = "coef.g.csv", row.names = F)
#######################


# event window
exwind <- -30L
bhwind <- 30L
stksamab <- data.frame()
stkwnkab <- data.frame()
stkbrkab <- c()
for (i in TSN) {
  QEAgrp <- data.frame()
  stkts <- filter(FF, Stkcd == i)
  Repdate <- filter(ReptInfo, Stkcd==i)$Annodt
  stkts <- filter(stkts, TradingDate %within%
                    interval(Repdate %m+% months(-3),
                             Repdate %m+% months(3)))
  if (Repdate %in% WorkingDay) {
    n.row <- which(WorkingDay==Repdate)
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (z in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate %in% QEA.date[z])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stksamab <- rbind(stksamab, QEAgrp),
           stkbrkab <- c(stkbrkab, i))
  } else if (wday(Repdate) == 7) { 
    QEADate <- Repdate + days(2)
    ifelse(QEADate %in% WorkingDay, 
           n.row <- which(WorkingDay==QEADate), 
           stkbrkab <- c(stkbrkab, i))
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (z in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate %in% QEA.date[z])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stkwnkab <- rbind(stkwnkab, QEAgrp),
           stkbrkab <- c(stkbrkab, i))
  } else if (wday(Repdate) == 1) {
    QEADate <- Repdate + days(1)
    ifelse(QEADate %in% WorkingDay, 
           n.row <- which(WorkingDay==QEADate), 
           stkbrkab <- c(stkbrkab, i))
    QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
    for (z in 1:length(QEA.date)) {
      QEAsim <- filter(stkts, TradingDate  %in% QEA.date[z])
      QEAgrp <- rbind(QEAgrp, QEAsim)
    }
    ifelse(nrow(QEAgrp) == bhwind-exwind+1,
           stkwnkab <- rbind(stkwnkab, QEAgrp),
           stkbrkab <- rbind(stkbrkab, i))
  } else stkbrkab <- c(stkbrkab, i)
}

stktolab <- rbind(stksamab,stkwnkab)

col.factor <- 3:6
fac.name <- c("DaRet","MKT","SMB","HML")
colnames(stktolab)[col.factor] <- fac.name
########################


colnum <- c(3,4,5)
ET <- 61L

#######################

# grouped code and select subsam data
subsam <- reasam[group[,1]==1]
N.sam <- 1165L
subsam <- reasam[group[,1]==2]
N.sam <- 312L
subsam <- reasam[group[,1]==3]
N.sam <- 67

group.a <- subsam
group.b <- subsam
group.c <- subsam

PD.event.g <- stksam(subsam, PD.event)
PDcoef.g <- stksam(subsam, PDcoef)


#expected value
ES.fit <- function(x, y) {
  p <- c()
  for (i in 1:nrow(x)) {
    code <- x[i, ecode]
    z <- y[y[, ecode]==code, ]
    w <- as.matrix(z[, colnum]) %*% diag(x[i, colnum])
    q <- w[, 1] + w[, 2] + w[, 3] + x[i, 2]
    p <- c(p ,q)
  }
  return(p)
}

fit.event <- ES.fit(PDcoef.g, PD.event.g)
ab.return <- PD.event.g[, 2] - fit.event


AbRe.daily <- function(x) {
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

CAR <- AbRe.daily(ab.return)
CAR <- CAR[[ET+1+1]][1:ET]

CAR.a <- as.data.frame(CAR)
CAR.b <- as.data.frame(CAR)
CAR.c <- as.data.frame(CAR)

CAR.g <- rbind(CAR.a, CAR.b, CAR.c)
CAR.g$group <- c(rep(1:3,each=61))
CAR.g$group <- as.factor(CAR.g$group)
CAR.g <- cbind(time=rep(-30:+30,3), CAR.g)

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
