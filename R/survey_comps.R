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
