### maturity analysis for Longnose Skate and Big Skate
### most of the code below is thanks to Melissa Head, 2019
### additions by Ian Taylor

dir <- 'C:/SS/skates/bio/maturity'

Skatemat <- read.csv(file.path(dir, "ODFW_WDFW_Skatematurity_2009_2019.csv"))
wcgbtsmat <- read.csv(file.path(dir, "BSKT_WCGBTS_maturity data.csv"))

#############################################################################
mat.table <- function(bins, len, mature){
  # table to summarize maturity proportions by length bin
  mat.table <- data.frame(bin=bins,
                          count=NA,
                          prop=NA)
  # loop over length bins
  for(irow in 1:(nrow(mat.table) - 1)){
    len.min <- mat.table$bin[irow]   # min length
    len.max <- mat.table$bin[irow+1] # max length
    sub <- len >= len.min & len < len.max # TRUE/FALSE vector to subset of data
    mat.table$count[irow] <- sum(sub) # get count
    if(mat.table$count[irow] > 0){ # get proportion mature if count > 0
      mat.table$prop[irow] <- mean(mature[sub])
    }
  }

  return(mat.table)
}

#############################################################################

plot.mat <- function(mat.table, mat.glm, scale=0.5){
  # function to plot maturity points vs. GLM fit
  par(mar=c(4,4,1,1))
  # make empty plot
  plot(0, type='n', xlim=range(mat.table$bin), ylim=c(0,1),
       xlab="Length (cm)", ylab="Proportion mature", yaxs='r',
       axes=FALSE)
  axis(1)
  axis(2, at=seq(0,1,.25), las=1)
  # add text with sample sizes
  points(x = mat.table$bin,
         y = mat.table$prop,
         cex = scale*sqrt(mat.table$count),
         pch = 16,
         col = gray(level=0, alpha=0.2))
  text(x = mat.table$bin,
       y = mat.table$prop,
       labels = mat.table$count)
  box() # replace black border around plot that got covered up by grey circles
  # get predicted values from the GLM
  predict <- predict(mat.glm,
                     newdata=data.frame(length = 1:200),
                     type='response')
  coeff <- mat.glm$coefficients
  lines(1:200, predict, col=2, lwd=3)
  abline(v = -coeff[1]/coeff[2], lty=3, h=0.5)
}



####Subset Longnose Females###
L<-subset(Skatemat,Species=="Longnose Skate")
L_fm<- subset(L,Sex=="Female")


###glm fit###

fit.mat.glm.LN <- glm (maturity ~ 1 + length,
                       data <-data.frame(length = L_fm$Length_cm,
                                         maturity = L_fm$Biological_maturity),
                       family = binomial(link ="logit"))

summary(fit.mat.glm.LN)
# confirm that coefficients match values reported below from Melissa
fit.mat.glm.LN$coefficients
## (Intercept)      length 
## -12.4456163   0.0954535 

Aglm<- -12.44562
Bglm<- 0.09545
n <-721

# input values for SS (-A/B & -B):
-fit.mat.glm.LN$coeff[1]/fit.mat.glm.LN$coeff[2]
## (Intercept) 
##    130.3841 
-fit.mat.glm.LN$coeff[2]
##     length 
## -0.0954535 


####Table of maturity by length bin####
mat.table.LN <- mat.table(bins=seq(5,165,5),
                          len=L_fm$Length_cm,
                          mature=L_fm$Biological_maturity)
#### make plot
plot.mat(mat.table.LN, fit.mat.glm.LN)

#L50 glm = 130.389 cm##

# predict(fit.mat.glm, newdata=data.frame(length = 1:150), type='response')

####Subset Longnose Males###
L<-subset(Skatemat,Species=="Longnose Skate")
L_m<- subset(L,Sex=="Male")


####Table of maturity by length bin####
mat.table.LN.m <- mat.table(bins=seq(5,165,5),
                            len=L_m$Length_cm,
                            mature=L_m$Biological_maturity)

###glm fit###

fit.mat.glm.LN.m <- glm (maturity ~ 1 + length,
                         data = data.frame(length = L_m$Length_cm,
                             maturity = L_m$Biological_maturity),
                         family = binomial(link ="logit"))

summary(fit.mat.glm.LN.m)

Aglm<- -8.435004 
Bglm<- 0.078413
n <-428

#L50 glm = 107.5715 cm##


plot.mat(mat.table.LN.m, fit.mat.glm.LN.m)


####Subset Big skate Females#######

Big <- subset(Skatemat,Species=="Big Skate")
Big_f<- subset(Big[,c("Length_cm", "Biological_maturity", "Sex")], Sex=="Female")

#### process WCGBTS samples for consideration as well
Big_f_wcgbts <- wcgbtsmat[grep("Ova", wcgbtsmat$triennial_maturity_dim.maturity_description),]
Big_f_wcgbts$Biological_maturity <-
  Big_f_wcgbts$triennial_maturity_dim.maturity_name %in% c("Adult/Mature",
                                                           "Egg Cases Present")
Big_f_wcgbts <- data.frame(Length_cm = Big_f_wcgbts$length_cm,
                           Biological_maturity = Big_f_wcgbts$Biological_maturity,
                           Sex = "Female")


###glm fit###

fit.mat.glm.BS <- glm (maturity ~ 1 + length,
                       data = data.frame(length = Big_f$Length_cm,
                           maturity = Big_f$Biological_maturity),
                       family = binomial(link ="logit"))

summary(fit.mat.glm.BS)

Aglm<- -19.98167
Bglm<- 0.13358
n <-278

#L50 glm =  149.5858 cm##


### glm fit with WCGBTS samples added
Big_f2 <- rbind(Big_f, Big_f_wcgbts)
fit.mat.glm.BS2 <- glm (maturity ~ 1 + length,
                       data = data.frame(length = Big_f2$Length_cm,
                           maturity = Big_f2$Biological_maturity),
                       family = binomial(link ="logit"))




####Table of maturity by length bin####
mat.table.BS <- mat.table(bins=seq(5,200,5),
                          len=Big_f$Length_cm,
                          mature=Big_f$Biological_maturity)
mat.table.BS2 <- mat.table(bins=seq(5,200,5),
                           len=Big_f2$Length_cm,
                           mature=Big_f2$Biological_maturity)
#### make plot
plot.mat(mat.table.BS, fit.mat.glm.BS, scale=.8)

png(file.path(dir, "BigSkate_maturity.png"), res=300, units='in', width=6.5, height=5,
    pointsize=10)
par(mar=c(4,4,1,1))
plot.mat(mat.table.BS2, fit.mat.glm.BS2, scale=1.0)
dev.off()
file.copy(file.path(dir, "BigSkate_maturity.png"),
          file.path(dir, "../../BigSkate_Doc/Figures/BigSkate_maturity.png"))

# -19.5017       0.1316  
-fit.mat.glm.BS2$coeff[1]/fit.mat.glm.BS2$coeff[2]
## (Intercept) 
##    148.2453 
-fit.mat.glm.BS2$coeff[2]
##     length 
## -0.1315502 

####Subset Big skate Males#######

Big<-subset(Skatemat,Species=="Big Skate")
Big_m<- subset(Big,Sex=="Male")


###glm fit###

fit.mat.glm.BS.m <- glm (maturity ~ 1 + length,
                         data = data.frame(length = Big_m$Length_cm,
                             maturity = Big_m$Biological_maturity),
                         family = binomial(link ="logit"))

summary(fit.mat.glm.BS.m)

Aglm<- -27.79483 
Bglm<- 0.26002
n <-310

#L50 glm =  106.895cm##

####Table of maturity by length bin####
mat.table.BS.m <- mat.table(bins=seq(5,200,5),
                            len=L_m$Length_cm,
                            mature=L_m$Biological_maturity)
plot.mat(mat.table.BS.m, fit.mat.glm.BS.m)
