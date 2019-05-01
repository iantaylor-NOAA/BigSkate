# Note: this file produced an index of abundance for Big and Longnose skates
# from the IPHC survey. This file depends on code in "IPHC_skate_notes.R"
# which is used to create the Rdata file read at the start.
#
# notes are by Ian Taylor, 2019, based on work for dogfish in 2011
# GLM approach and code are all thanks to John Wallace

iphc.dir <- 'c:/SS/skates/indices/IPHC/'
load(file=file.path(iphc.dir, "iphc.data_4-10-2019.Rdata"))

# hook adjustment factors
iphc.haf <- read.csv(file.path(iphc.dir,
                               'TaylorI20190423-FISS-HookAdj.csv'),
                     stringsAsFactors=FALSE)
# add hook adjustment factors to iphc.US table loaded above:
iphc.US$h.adj <- NA
for(irow in 1:nrow(iphc.US)){
  iphc.US$h.adj[irow] <- iphc.haf$h.adj[iphc.haf$stlkey == iphc.US$stlkey[irow]]
}
# look at 


require(MCMCpack)
require(Hmisc)

# simple function to sort out row names
renum <- function(x, no.num = F)
  {
    if(no.num)
      rownames(x) <- rep("", nrow(x))
    else rownames(x) <- 1:nrow(x)
    x
  }


# vector of years included in the analysis
yrs <- sort(unique(iphc.US$Year))
yrs
##  [1] 1999 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
## [16] 2015 2016 2017 2018

# function to calculate index and SD of log values for use in plotting and
# for input to Stock Synthesis
skate.MCMC.Index.quants.f <- function(MCMC.FIT) {

  inv.logit <- function(x) exp(x)/(1 + exp(x))
  MCMC.FIT.DF <- as.data.frame(MCMC.FIT)

  for(iyr in 1:length(yrs)){
    yr <- yrs[iyr]
    if(iyr == 1){
      MCMC.FIT.DF[[paste0("I",yr)]] <- inv.logit( 0 + MCMC.FIT.DF[,1])
    }else{
      MCMC.FIT.DF[[paste0("I",yr)]] <- inv.logit( MCMC.FIT.DF[[paste0("Year", yr)]] +
                                                   MCMC.FIT.DF[,1])
    }
  }

  list(Index = apply(MCMC.FIT.DF[, names(MCMC.FIT.DF) %in% paste0("I", yrs)],
           MARGIN = 2, FUN = quantile, probs=c(0.50, 0.025, 0.975), type=8),
       SD.of.log = apply(MCMC.FIT.DF[, names(MCMC.FIT.DF) %in% paste0("I", yrs)],
           MARGIN = 2, FUN = function(x) sd(log(x)))
       )
}

# create data with 0 and 1 observations per hook by tranforming
# table of the number of fish observed and the number of observed hooks

# empty object
iphc.bin.BS <- NULL
iphc.bin.LN <- NULL
# fill in based on rows of counts of hooks observed and species observed
for(i in 1:nrow(iphc.US)){
  if(i %% 100 == 0){
    print(paste0(round(100 * i / nrow(iphc.US)), "%"))
  }
  if(iphc.US$Hooks.Observed[i] > 0){
    # big skate
    temp.BS <- data.frame(Year = iphc.US$Year[i],
                          Station = iphc.US$Station[i],
                          catch = c(rep(0,iphc.US$Hooks.Observed[i] -
                                          iphc.US$BigSkate.Observed[i]),
                              rep(1,iphc.US$BigSkate.Observed[i]))
                          )
    # longnose skate
    temp.LN <- data.frame(Year = iphc.US$Year[i],
                          Station = iphc.US$Station[i],
                          catch = c(rep(0,iphc.US$Hooks.Observed[i] -
                                          iphc.US$LongnoseSkate.Observed[i]),
                              rep(1,iphc.US$LongnoseSkate.Observed[i]))
                          )
    iphc.bin.BS <- rbind(iphc.bin.BS, temp.BS)
    iphc.bin.LN <- rbind(iphc.bin.LN, temp.LN)
  }
}
# make year and station into factors
iphc.bin.BS$Year <- as.factor(iphc.bin.BS$Year)
iphc.bin.LN$Year <- as.factor(iphc.bin.LN$Year)
iphc.bin.BS$Station <- as.factor(as.character(iphc.bin.BS$Station))
iphc.bin.LN$Station <- as.factor(as.character(iphc.bin.LN$Station))


# -------------------------------------------------------------------
# commenting out exploratory runs that can be skipped

## # calculate alternative inputs that only included stations
## # that have at least one occurrence
## # these are all designated "HS" (for reasons that I don't remember)
##
## # station list
## BS.St <- aggregate(list(catch=iphc.bin.BS$catch),
##                    list(Station=iphc.bin.BS$Station), sum)
## LN.St <- aggregate(list(catch=iphc.bin.LN$catch),
##                    list(Station=iphc.bin.LN$Station), sum)
## # subset for those with non-zero catch
## renum(BS.St[BS.St$catch > 0,])
## renum(LN.St[LN.St$catch > 0,])
## iphc.bin.BS.HS <- iphc.bin.BS[iphc.bin.BS$Station %in% BS.St[BS.St$catch > 0,]$Station,]
## iphc.bin.LN.HS <- iphc.bin.LN[iphc.bin.LN$Station %in% LN.St[LN.St$catch > 0,]$Station,]
##
## # run Big Skate MCMC with 100,000 samples (with and without zero value stations)
## MCMC.IPHC.Cat.100k.BS    <- MCMClogit(catch ~ Year, data=iphc.bin.BS,
##                                       mcmc=1e3, burnin=1e2, thin=1,
##                                       tune= 0.75, verbose=100)
## MCMC.IPHC.Cat.100k.BS.HS <- MCMClogit(catch ~ Year, data=iphc.bin.BS.HS,
##                                       mcmc=1e3, burnin=1e2, thin=1,
##                                       tune= 0.75, verbose=100)
## # run Longnose Skate MCMC with 100,000 samples (with and without zero value stations)
## MCMC.IPHC.Cat.100k.LN    <- MCMClogit(catch ~ Year, data=iphc.bin.LN,
##                                       mcmc=1e3, burnin=1e2, thin=1,
##                                       tune= 0.75, verbose=100)
## MCMC.IPHC.Cat.100k.LN.HS <- MCMClogit(catch ~ Year, data=iphc.bin.LN.HS, mcmc=1e3,
##                                       burnin=1e2, thin=1, tune= 0.75, verbose=100)
##
## # calculate resulting values
## Q.MCMC.BS <-  skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.100k.BS)[[1]]
## Q.MCMC.BS.HS <- skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.100k.BS.HS)[[1]]
##
## Q.MCMC.LN <-  skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.100k.LN)[[1]]
## Q.MCMC.LN.HS <- skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.100k.LN.HS)[[1]]
##
## # make plot
## png(file.path(iphc.dir, 'IPHC index for Longnose Skate.png'), width=6.5, height=5,
##     units='in', res=300)
## plot(yrs, Q.MCMC.LN[1,], type='o',ylim=range(0,1.1*Q.MCMC.LN),
##      pch=16, xlab="Year",ylab="Index", lwd=3, yaxs='i')
## arrows(yrs, Q.MCMC.LN[2,],
##        yrs, Q.MCMC.LN[3,],
##        code=3, angle=90, length=0.02)
## meancatch <- aggregate(iphc.bin.LN$catch, by=list(iphc.bin.LN$Year), FUN=mean)
## lines(as.numeric(as.character(meancatch$Group.1)), meancatch$x, col=2, lty=2, lwd=2)
## legend('topright', col=1:2, lwd=3,
##        legend=c("Standardized index", "Mean catch rate"),
##        bty='n', lty=c(1,2))
## dev.off()
##
## ## points(yrs+.02, Q.MCMC.HS[1,], type='o',col=2)
## ## arrows(yrs+.02, Q.MCMC.HS[2,],
## ##        yrs+.02, Q.MCMC.HS[3,],
## ##        code=3, angle=90, length=0.02,col=2)
##
## # rescale the alternative one by the means
## Q.MCMC.BS.HS2 <- Q.MCMC.BS.HS*mean(Q.MCMC.BS)/mean(Q.MCMC.BS.HS)
## Q.MCMC.LN.HS2 <- Q.MCMC.LN.HS*mean(Q.MCMC.LN)/mean(Q.MCMC.LN.HS)
## ## points(yrs+.02, Q.MCMC.HS2[1,], type='o',col=4)
## ## arrows(yrs+.02, Q.MCMC.HS2[2,],
## ##        yrs+.02, Q.MCMC.HS2[3,],
## ##        code=3, angle=90, length=0.02,col=4)
##
## # makes little difference to remove stations with no Big Skate catch,
## # so will stick with all stations (as was the case for dogfish)
## range(Q.MCMC/Q.MCMC.HS2 )
## ## [1] 0.9584989 1.1375390
## range(Q.MCMC.LN/Q.MCMC.LN.HS2 )
## ## [1] 0.9962408 1.0170049



# running again with 1 million samples longer series:
MCMC.IPHC.Cat.1mil.LN <- MCMClogit(catch ~ Year, data=iphc.bin.LN,
                                   mcmc=1e6, burnin=1e3, thin=1e3,
                                   tune= 0.75, verbose=1000)
MCMC.IPHC.Cat.1mil.BS <- MCMClogit(catch ~ Year, data=iphc.bin.BS,
                                   mcmc=1e6, burnin=1e3, thin=1e3,
                                   tune= 0.75, verbose=1000)

Q.MCMC_1mil.LN <-  skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.1mil.LN)[[1]]
Q.MCMC_1mil.BS <-  skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.1mil.BS)[[1]]


# write tables in format required by SS
BS2 <- skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.1mil.BS)
index.BS2 <- data.frame(Year=yrs, Seas=7, Fleet=999, Index=BS2$Index[1,], SD.of.log=BS2$SD.of.log)
rownames(index.BS2) <- 1:nrow(index.BS2)
write.csv(index.BS2,
          file = file.path(iphc.dir, 'IPHC.index.BigSkate_4-22-2019.csv'),
          row.names = FALSE)

LN2 <- skate.MCMC.Index.quants.f(MCMC.IPHC.Cat.1mil.LN)
index.LN2 <- data.frame(Year=yrs, Month=7, Fleet=999, Index=LN2$Index[1,], SD.of.log=LN2$SD.of.log)
rownames(index.LN2) <- 1:nrow(index.LN2)
write.csv(index.LN2,
          file = file.path(iphc.dir, 'IPHC.index.longnose_4-22-2019.csv'),
          row.names = FALSE)

# check model diagnostics
library(coda)
heidel.diag(MCMC.IPHC.Cat.100k.BS)
heidel.diag(MCMC.IPHC.Cat.1mil.BS)

heidel.diag(MCMC.IPHC.Cat.100k.LN)
heidel.diag(MCMC.IPHC.Cat.1mil.LN)



# make plot for LN
png(file.path(iphc.dir, 'IPHC index for Longnose Skate (4-22-2019).png'), width=6.5, height=5,
    units='in', res=300)
plot(yrs, Q.MCMC_1mil.LN[1,], type='o',ylim=range(0,1.1*Q.MCMC_1mil.LN),
     pch=16, xlab="Year",ylab="Index", lwd=3, yaxs='i')
arrows(yrs, Q.MCMC_1mil.LN[2,],
       yrs, Q.MCMC_1mil.LN[3,],
       code=3, angle=90, length=0.02)
meancatch <- aggregate(iphc.bin.LN$catch, by=list(iphc.bin.LN$Year), FUN=mean)
lines(as.numeric(as.character(meancatch$Group.1)), meancatch$x, col=2, lty=2, lwd=2)
legend('topright', col=1:2, lwd=3,
       legend=c("Standardized index", "Mean catch rate"),
       bty='n', lty=c(1,2))
dev.off()


# make plot for BS
png(file.path(iphc.dir, 'IPHC index for Big Skate (4-22-2019).png'), width=6.5, height=5,
    units='in', res=300)
plot(yrs, Q.MCMC_1mil.BS[1,], type='o',ylim=range(0,1.1*Q.MCMC_1mil.BS),
     pch=16, xlab="Year",ylab="Index", lwd=3, yaxs='i')
arrows(yrs, Q.MCMC_1mil.BS[2,],
       yrs, Q.MCMC_1mil.BS[3,],
       code=3, angle=90, length=0.02)
meancatch <- aggregate(iphc.bin.BS$catch, by=list(iphc.bin.BS$Year), FUN=mean)
lines(as.numeric(as.character(meancatch$Group.1)), meancatch$x, col=2, lty=2, lwd=2)
legend('topright', col=1:2, lwd=3,
       legend=c("Standardized index", "Mean catch rate"),
       bty='n', lty=c(1,2))
dev.off()
