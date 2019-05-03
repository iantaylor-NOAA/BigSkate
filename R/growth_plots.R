stop("don't source this file")

## # directory where files 
bio.dir <- 'C:/SS/skates/bio/'

# load previously extracted data
# extraction code is in /R/survey_extration_skates.R
load(file = file.path(bio.dir, '../data/BigSkate_survey_extractions_4-22-2019.Rdata'))


table(bio.WCGBTS.BS$Sex)
##    F    M    U 
## 2480 2993   12 
WLdata <- bio.WCGBTS.BS[bio.WCGBTS.BS$Sex != 'U',] # exclude 12 unsexed fish

# fit linear models
WL_lm1 <- lm(log(Weight) ~ log(Length_cm)*Sex, data=WLdata)
WL_lm2 <- lm(log(Weight) ~ log(Length_cm) + Sex, data=WLdata)

# summary shows that sex terms are not significant: Pr(>|t|) = 0.622
summary(WL_lm1)
summary(WL_lm2)

# restore unsexed data
WLdata <- bio.WCGBTS.BS
WL_lm3 <- lm(log(Weight) ~ log(Length_cm), data=WLdata)
summary(WL_lm3)
WL_Apar <- exp(WL_lm3$coefficients[1])
WL_Bpar <- WL_lm3$coefficients[2]
print(WL_Apar,digits=5)
## (Intercept) 
##  7.4924e-06 
print(WL_Bpar,digits=5)
## log(Length_cm) 
##         2.9925 

# confirm correct interpretation of coefficients
# by looking at predicted weight of 100 cm skate
mean(WLdata$Weight[round(WLdata$Length_cm) %in% 95:105], na.rm=TRUE)
## [1] 7.188169
## (Intercept) 
##    7.236605 
WL_Apar*100^WL_Bpar

# random order in which to plot the points
samp <- sample(1:nrow(WLdata))

colvec <- rep(rgb(0,1,0,.1), nrow(bio.WCGBTS.BS))
colvec[bio.WCGBTS.BS$Sex=="F"] <- rgb(1,0,0,.1)
colvec[bio.WCGBTS.BS$Sex=="M"] <- rgb(0,0,1,.1)

table(bio.WCGBTS.BS$Sex)
##    F    M    U 
## 2480 2993   12 


png(file.path(bio.dir, 'Big Skate length at age WCGBTS.png'), width=5, height=5,
    units='in', res=300)
par(mar=c(4,4,1,1))
plot(jitter(bio.WCGBTS.BS$Age), bio.WCGBTS.BS$Length_cm,
     cex=2, xlab="Age (years)", ylab="Length (cm)", pch=16, col=colvec,
     xlim=c(0,13), ylim=c(0,200), xaxs='r', yaxs='i', xpd=TRUE, las=1)
n <- sum(!is.na(bio.WCGBTS.BS$Age) &
           !is.na(bio.WCGBTS.BS$Length_cm))
years <- range(bio.WCGBTS.BS$date_dim.year[!is.na(bio.WCGBTS.BS$Length_cm) &
                                          !is.na(bio.WCGBTS.BS$Age)])
dev.off()

## png(file.path(bio.dir, 'Big Skate weight at length.png'), width=5, height=5,
##     units='in', res=300)
## par(mar=c(4,4,1,1))

png(file.path(bio.dir, 'Big Skate bio relationships.png'), width=7, height=4,
    pointsize=10, units='in', res=300)
par(mar=c(4,4,1,1), mfrow=c(1,2))

plot(bio.WCGBTS.BS$Length_cm, bio.WCGBTS.BS$Weight,
     cex=2, xlab="Length (cm)", ylab="Weight (kg)", pch=16, col=colvec,
     xlim=c(0,200), ylim=c(0,50), xaxs='i', yaxs='i', xpd=TRUE, las=1)
n <- sum(!is.na(bio.WCGBTS.BS$Length_cm) & !is.na(bio.WCGBTS.BS$Weight))
years <- range(bio.WCGBTS.BS$date_dim.year[!is.na(bio.WCGBTS.BS$Length_cm) &
                                          !is.na(bio.WCGBTS.BS$Weight)])

nf <- sum(!is.na(bio.WCGBTS.BS$Weight) & !is.na(bio.WCGBTS.BS$Length_cm) &
            bio.WCGBTS.BS$Sex=="F")
nm <- sum(!is.na(bio.WCGBTS.BS$Weight) & !is.na(bio.WCGBTS.BS$Length_cm) &
            bio.WCGBTS.BS$Sex=="M")
lines(0:200, WL_Apar*(0:200)^WL_Bpar, lty = 3, lwd = 3)
legend('topleft', pt.cex=2, col = c(rgb(1,0,0,.3), rgb(0,0,1,.3), 1),
       pch=c(16,16,NA), lty=c(NA,NA,3), lwd=c(NA,NA,3),
       legend=c(paste0("Females (n =", nf, ")"),
           paste0("Males (n =", nm, ")"),
           "Estimated relationship"),
           ## paste0("Wt = ", format(WL_Apar, digits=5),
           ##        " * Len ^ ", format(WL_Bpar, digits=5))),
       bty='n')

## dev.off()

## png(file.path(bio.dir, 'Big Skate length at width.png'), width=5, height=5,
##     units='in', res=300)
## par(mar=c(4,4,1,1))

# darker colors because there are very few samples
colvec2 <- rep(rgb(0,1,0,.3), nrow(bio.WCGBTS.BS))
colvec2[bio.WCGBTS.BS$Sex=="F"] <- rgb(1,0,0,.3)
colvec2[bio.WCGBTS.BS$Sex=="M"] <- rgb(0,0,1,.3)
plot(bio.WCGBTS.BS$Width_cm, bio.WCGBTS.BS$Length_cm,
     cex=2, xlab="Disc width (cm)", ylab="Length (cm)", pch=16, col=colvec2,
     xlim=c(0,140), ylim=c(0,200), xaxs='i', yaxs='i', xpd=TRUE, las=1)
n <- sum(!is.na(bio.WCGBTS.BS$Length_cm) & !is.na(bio.WCGBTS.BS$Width_cm))
nf <- sum(!is.na(bio.WCGBTS.BS$Width_cm) & !is.na(bio.WCGBTS.BS$Length_cm) &
            bio.WCGBTS.BS$Sex=="F")
nm <- sum(!is.na(bio.WCGBTS.BS$Width_cm) & !is.na(bio.WCGBTS.BS$Length_cm) &
            bio.WCGBTS.BS$Sex=="M")

# intercept was -2 cm, very minor difference, excluding that option
#lm.lenwidth1 <- lm(bio.WCGBTS.BS$Length_cm ~ bio.WCGBTS.BS$Width_cm)
lm.lenwidth2 <- lm(bio.WCGBTS.BS$Length_cm ~ 0 + bio.WCGBTS.BS$Width_cm)
##abline(lm.lenwidth1, lwd=3)

abline(lm.lenwidth2, lwd=3, lty=3, col=1)
summary(lm.lenwidth2)
legend('topleft', pt.cex=2, col = c(rgb(1,0,0,.3), rgb(0,0,1,.3), 1),
       pch=c(16,16,NA), lty=c(NA,NA,3), lwd=c(NA,NA,3),
       legend=c(paste0("Females (n =", nf, ")"),
           paste0("Males (n =", nm, ")"),
           "Estimated relationship"),
           ## paste0("Len = ", format(lm.lenwidth2$coefficients, digits=5),
           ##        " * Wd")),
       bty='n')
dev.off()
file.copy(file.path(bio.dir, 'Big Skate bio relationships.png'),
          file.path(bio.dir, '../BigSkate_Doc/Figures/Big Skate bio relationships.png'))

#R-squared:
summary(WL_lm3)$adj.r.squared
## [1] 0.9913042
summary(lm.lenwidth2)$adj.r.squared
## [1] 0.9983096



# add points
points(WLdataLength_cm[samp], WLdata$Weight[samp], col=colvec[samp], pch=16)
lines(x.vec, exp(WL_lm$coefficients[1])*x.vec^WL_lm$coefficients[2], lwd=2)
lines(x.vec, exp(WL_lm$coefficients[1])*x.vec^WL_lm$coefficients[2], lwd=2)
print(exp(WL_lm$coefficients[1]),digits=5)
print(WL_lm$coefficients[2],digits=5)
dev.off()

# plot on a log scale
plot(0,
     xlim=log(c(15, 1.5*max(bio.WCGBTS.BS$Length_cm, na.rm=TRUE))),
     ylim=log(c(0.05, 1.5*max(bio.WCGBTS.BS$Weight, na.rm=TRUE))),
     xaxs='i', yaxs='i')

# add points
points(log(bio.WCGBTS.BS$Length_cm[samp]),
       log(bio.WCGBTS.BS$Weight[samp]), col=colvec[samp], pch=16)
abline(WL_lm3, lty=2)





## ###############
## Kevin Dunn
## Paul Kujala

## Alaska Crab Bait market

## 30 fathoms and in, definitely inside the teens and twenties
## generally speaking, people used to spend much more time in the teens and twenties (fathoms)
## than they do now, like in the past 6, 7 years

## prior to IQ, you had 2-month trip limits, and you had deep limints and shallow limits,
## and everybody chose whether to fish deep or shallow for the 2-month period
## depending on what their plans were for shrimping and what they could catch how fast

## much more market for sand sole and starry flounder in the 80s than today
## "don't hardly fish them anymore"

## if you chose the outside limits (outside 200 fathoms) and you were targeting dover,
## then you would be catching mostly longnose
## if you were on the inside limits, you would catch more big skates

## you don't get paid for your labor winging skates (3x higher price, but 1/3 the volume)
## it's just a way to save space in the fish hold and save ice
## that was more in the 90s, nobody does that now


## overall trawl effort wouldn't be a better guage of total catch of longnose
## low dover quotas in the 80s during trip limits, and when you did reach your limit
## they were thrown over


## ##
## reach out to Pete Leipzig about changes in skate markets over time
## Greg Lippert puts out tables of disposition of catch
