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
biomassWCGBTS <- Biomass.fn(dir = 'c:/SS/skates/indices/WCGBTS', 
                            dat = catchWCGBTS.BS,  
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

len.bins <- seq(5, 200, 5)
len <- bio.WCGBTS.BS
catch <- catchWCGBTS.BS
dir <- 'c:/SS/skates/bio/survey_comps'

# Calculate the effN
n = GetN.fn(dir=dir, dat = len, type = "length",
    species = "others", printfolder = "WCGBTS_comps")
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.

# Expand and format length composition data for SS
LFs <- SurveyLFs.fn(dir = dir, datL = len, datTows = catchWCGBTS.BS,  
                    strat.df = strata, lgthBins = len.bins, gender = 3, 
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 0, 
                    nSamps = n)


# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).


PlotFreqData.fn(dir = dir, dat = LFs,
                main = "NWFSC Groundfish Bottom Trawl Survey",
                ylim=c(0, max(len.bins) + 4), yaxs="i",
                ylab="Length (cm)", dopng = TRUE)
PlotSexRatio.fn(dir = dir, dat = len, data.type = "length",
                dopng = TRUE, main = "NWFSC Groundfish Bottom Trawl Survey")

# figure suggests evidence of different asymptotic size,
# as does comparison of histograms below
par(mfrow=c(2,1))
hist(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Sex=="F"] , breaks=len.bins)
hist(bio.WCGBTS.BS$Length_cm[bio.WCGBTS.BS$Sex=="M"] , breaks=len.bins)

