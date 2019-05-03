### process survey length and age data to get compositions

# load previously extracted data
# extraction code is in /R/survey_extration_skates.R
load(file = 'c:/SS/skates/data/BigSkate_survey_extractions_4-22-2019.Rdata')
library(nwfscSurvey)

strata <- CreateStrataDF.fn(
    names=c("shallow_s", "deep_s","shallow_n", "deep_n"), 
    depths.shallow = c( 55, 183,  55, 183),
    depths.deep    = c(183, 549, 183, 549),
    lats.south     = c( 32,  32,  42,  42),
    lats.north     = c( 42,  42,  49,  49))
##        name      area Depth_m.1 Depth_m.2 Latitude_dd.1 Latitude_dd.2
## 1 shallow_s 187251226        55       183            32            42
## 2    deep_s 182594511       183       549            32            42
## 3 shallow_n 208174619        55       183            42            49
## 4    deep_n 106872334       183       549            42            49

# design-based indices
biomass.WCGBTS <- Biomass.fn(dir = 'c:/SS/skates/indices/WCGBTS', 
                            dat = catch.WCGBTS.BS,  
                            strat.df = strata, 
                            printfolder = "",
                            outputMedian = TRUE)
biomass.Tri <- Biomass.fn(dir = 'c:/SS/skates/indices/Triennial',
                          dat = catch.Tri.BS,  
                          strat.df = strata, 
                          printfolder = "", 
                          outputMedian = TRUE)


## par(mfrow=c(2,2))
PlotBio.fn(dir = getwd(), 
           dat = biomass.Tri,  
           main = "Big Skate, Triennial survey")
PlotBio.fn(dir = getwd(), 
           dat = biomass.WCGBTS,  
           main = "Big Skate, NWFSC shelf-slope bottom trawl survey")

### comps
# convert disc widths to lengths using regression from /R/growth_plots.R
# L = 1.3399*W

table(is.na(bio.WCGBTS.BS$Length_cm), is.na(bio.WCGBTS.BS$Width_cm))
  ##       FALSE TRUE
  ## FALSE    95 5135
  ## TRUE    251    4

# subset to 251 samples that have width but not length
sub <- is.na(bio.WCGBTS.BS$Length_cm) & !is.na(bio.WCGBTS.BS$Width_cm)
## table(sub)
## sub
## FALSE  TRUE 
##  5234   251 
bio.WCGBTS.BS$Length_cm[sub] <- 1.3399*bio.WCGBTS.BS$Width_cm[sub]
table(is.na(bio.WCGBTS.BS$Length_cm), is.na(bio.WCGBTS.BS$Width_cm))
  ##       FALSE TRUE
  ## FALSE   346 5135
  ## TRUE      0    4

table(is.na(bio.Tri.BS$Lengths$Length_cm), is.na(bio.Tri.BS$Lengths$Width_cm))
# subset to 6 samples that have width but not length from the triennial
sub <- is.na(bio.Tri.BS$Lengths$Length_cm) & !is.na(bio.Tri.BS$Lengths$Width_cm)
## table(sub)
## sub
## FALSE  TRUE 
##   181     6 
bio.Tri.BS$Lengths$Length_cm[sub] <- 1.3399*bio.Tri.BS$Lengths$Width_cm[sub]
table(is.na(bio.Tri.BS$Lengths$Length_cm), is.na(bio.Tri.BS$Lengths$Width_cm))
  ##       FALSE TRUE
  ## FALSE     6  181


len.bins <- seq(5, 200, 5)
dir <- 'c:/SS/skates/bio/survey_comps'

# Calculate the effN for WCGBTS
n = GetN.fn(dir=dir, dat = bio.WCGBTS.BS, type = "length",
    species = "others", printfolder = "WCGBTS_comps")
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.
# "The effN sample size is calculated as 2.38 multiplied by the number of tows in each year."

# Expand and format length composition data for SS
LFs <- SurveyLFs.fn(dir = file.path(dir, 'WCGBTS_comps'),
                    datL = bio.WCGBTS.BS, datTows = catch.WCGBTS.BS,  
                    strat.df = strata, lgthBins = len.bins, gender = 3, 
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 0, 
                    nSamps = n)
#### did rounding to 2 digits past decimal in Excel
## value.columns <- c(paste0("F",len.bins), paste0("M",len.bins))
## LFs[,value.columns] <- round(LFs[,value.columns], 2)

# Calculate the effN for Triennial
n.tri <- GetN.fn(dir=dir, dat = bio.Tri.BS$Lengths, type = "length",
    species = "others", printfolder = "Triennial_comps")
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.
## n2 = GetN.fn(dir=dir, dat = bio.Tri.BS$Lengths, type = "length",
##     species = "shelfrock", printfolder = "Triennial_comps2")
## n.LN = GetN.fn(dir=dir, dat = bio.Tri.LN$Lengths, type = "length",
##     species = "shelfrock", printfolder = "Triennial_comps_LN")


# convert width to length for 
# Expand and format length composition data for SS
LFs.tri <- SurveyLFs.fn(dir = file.path(dir, 'Triennial_comps'),
                        datL = bio.Tri.BS$Lengths, datTows = catch.Tri.BS,  
                        strat.df = strata, lgthBins = len.bins, gender = 3, 
                        sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 0, 
                        nSamps = n.tri)
##### sexRatioStage = 1 caused error (github issue #20)
LFs.tri2 <- SurveyLFs.fn(dir = file.path(dir, 'Triennial_comps'),
                         datL = bio.Tri.BS$Lengths, datTows = catch.Tri.BS,  
                         strat.df = strata, lgthBins = len.bins, gender = 3, 
                         sexRatioStage = 1, sexRatioUnsexed = 0.5, maxSizeUnsexed = 0, 
                         nSamps = n.tri)

# remove unsexed fish for unexpanded lengths
lengths.sexed <- bio.Tri.BS$Lengths[bio.Tri.BS$Lengths$Sex %in% c("F","M"),]

# unexpanded lengths
# copy expanded table already created by SurveyLFs.fn
LFs.tri.nox <- LFs.tri
# which columns have values
value.col.names <- c(paste0("F",len.bins), paste0("M",len.bins))
# set all values to NA initially
LFs.tri.nox[ , names(LFs.tri.nox) %in% value.col.names] <- NA
# loop over years, bins, sexes
for(y in c(2001,2004)){
  for(len in len.bins){
    for(sex in c("F","M")){
      colname <- paste0(sex, len)
      LFs.tri.nox[LFs.tri.nox$year == y, colname] <-
        sum(lengths.sexed$Year == y &
              lengths.sexed$Sex == sex &
                floor(lengths.sexed$Length_cm) %in% (len + 0:4))
    }
  }
}
write.csv(LFs.tri.nox,
          file=file.path(dir, 'Triennial_comps/forSS', "unexpanded_comps.csv"),
          row.names=FALSE)



# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).


PlotFreqData.fn(dir = dir, dat = LFs.tri,
                main = "Triennial",
                ylim=c(0, max(len.bins) + 4), yaxs="i",
                ylab="Length (cm)", dopng = TRUE)
PlotFreqData.fn(dir = dir, dat = LFs.tri2,
                main = "Triennial sexRatioStage = 1",
                ylim=c(0, max(len.bins) + 4), yaxs="i",
                ylab="Length (cm)", dopng = TRUE)
PlotSexRatio.fn(dir = dir, dat = len, data.type = "length",
                dopng = TRUE, main = "NWFSC Groundfish Bottom Trawl Survey")

# figure suggests evidence of different asymptotic size,
# as does comparison of histograms below
par(mfrow=c(2,1))
hist(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Sex=="F"] , breaks=len.bins)
hist(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Sex=="M"] , breaks=len.bins)




#============================================================================================
#Age Biological Data 
#============================================================================================
#age = bio
dir <- 'c:/SS/skates/bio/survey_comps'
age.bins = 0:15

n.age.WCGBTS = GetN.fn(dir = file.path(dir, 'WCGBTS_comps'),
    dat = bio.WCGBTS.BS, type = "age", species = "others", printfolder = "forSS")

# Exand and format the marginal age composition data for SS
Ages <- SurveyAFs.fn(dir = file.path(dir, 'WCGBTS_comps'),
                     datA = bio.WCGBTS.BS, datTows = catch.WCGBTS.BS,  
                     strat.df = strata, ageBins = age.bins, 
                     sexRatioStage = 2, sexRatioUnsexed = 0.50, maxSizeUnsexed = 5, 
                     gender = 3, nSamps = n.age.WCGBTS)



PlotFreqData.fn(dir = file.path(dir, 'WCGBTS_comps'),
                dat = Ages,
                ylim=c(0, max(age.bins) + 2),
                yaxs="i",
                ylab="Age (yr)",
                dopng=FALSE)
PlotVarLengthAtAge.fn(dir = file.path(dir, 'WCGBTS_comps'),
                      dat = bio.WCGBTS.BS, dopng = FALSE) 
PlotSexRatio.fn(dir = file.path(dir, 'WCGBTS_comps'),
                dat = bio.WCGBTS.BS, data.type = "age",
                dopng = FALSE, main = "WCGBTS")

#============================================================================================
# Conditional Ages
#============================================================================================
CAAL.WCGBTS.BS <- SurveyAgeAtLen.fn (dir = file.path(dir, 'WCGBTS_comps'),
                                     datAL = bio.WCGBTS.BS, datTows = catch.WCGBTS.BS, 
                                     strat.df = strata, lgthBins = len.bins,
                                     ageBins = age.bins, partition = 0)


### mean length at age
meanlen <- data.frame(age = 0:15, meanF = NA, meanM = NA,
                      NF = NA, NM = NA)
for(a in 0:15){
  meanlen$meanF[meanlen$age==a] <-
    round(mean(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Age == a &
                                   bio.WCGBTS.BS$Sex == "F"], na.rm=TRUE),1)
  meanlen$meanM[meanlen$age==a] <-
    round(mean(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Age == a &
                                   bio.WCGBTS.BS$Sex == "M"], na.rm=TRUE),1)
  meanlen$NF[meanlen$age==a] <-
    sum(bio.WCGBTS.BS$Age == a & bio.WCGBTS.BS$Sex == "F", na.rm=TRUE)
  meanlen$NM[meanlen$age==a] <-
    sum(bio.WCGBTS.BS$Age == a & bio.WCGBTS.BS$Sex == "M", na.rm=TRUE)
}

##    age     meanF     meanM  NF  NM
## 1    0  29.35714  28.61765  28  17
## 2    1  42.32716  43.49038  81  52
## 3    2  55.89754  55.44000 122 100
## 4    3  63.71053  64.95175  95 114
## 5    4  75.98039  79.50000  51  70
## 6    5  89.19643  91.42742  28  62
## 7    6 104.36842 103.41803  19  61
## 8    7 108.69231 111.73256  13  43
## 9    8 128.75000 113.66667   8  36
## 10   9 130.00000 120.42857   5   7
## 11  10 140.75000 126.60000   4   5
## 12  11 179.00000 126.00000   1   1
## 13  12 141.00000 133.00000   3   1
## 14  13 144.00000       NaN   1   0
## 15  14 168.50000       NaN   2   0
## 16  15 177.00000       NaN   1   0

# example input
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
## #_yr month fleet sex part ageerr ignore datavector(female-male)
## #                                          samplesize(female-male)
##  1971 7 1 3 0 1 2 29.8931 40.6872 44.7411 50.027 52.5794 56.1489 57.1033 61.1728 61.7417 63.368 64.4088 65.6889 67.616 68.5972 69.9177 71.0443 72.3609 32.8188 39.5964 43.988 50.1693 53.1729 54.9822 55.3463 60.3509 60.7439 62.3432 64.3224 65.1032 64.1965 66.7452 67.5154 70.8749 71.2768 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
##  1995 7 1 3 0 1 2 32.8974 38.2709 43.8878 49.2745 53.5343 55.1978 57.4389 62.0368 62.1445 62.9579 65.0857 65.6433 66.082 65.6117 67.0784 69.3493 72.2966 32.6552 40.5546 44.6292 50.4063 52.0796 56.1529 56.9004 60.218 61.5894 63.6613 64.0222 63.4926 65.8115 69.5357 68.2448 66.881 71.5122 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
##  1995 7 1 3 1 1 2 31.4253 40.0692 43.5027 48.0151 49.5317 52.5139 57.1566 55.2488 58.4423 60.548 61.446 63.9418 64.9355 64.191 66.8906 66.1213 70.2574 31.5972 37.8587 43.1353 46.4582 49.7879 53.7443 55.7443 56.3585 59.4961 58.7688 61.2203 61.6839 64.6311 67.0238 66.4088 68.8365 67.8683 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20

meanlen$meanM[is.nan(meanlen$meanM)] <- -999
meanlen.SS <- data.frame(year=2011, month=7, fleet=5, sex=3, part=0, ageerr=0, ignore=99)
meanlen.SS <- cbind(meanlen.SS,
                    t(meanlen$meanF), t(meanlen$meanM),
                    t(meanlen$NF), t(meanlen$NM))
names(meanlen.SS)[-(1:7)] <- c(paste0("F",0:15), paste0("M",0:15),
                               paste0("NF",0:15), paste0("NM",0:15))

