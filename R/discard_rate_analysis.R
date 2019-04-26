# load some WCGOP data from a few years ago
if(FALSE){
  load('C:/data/WCGOP/Observer_Catch_Data_2002_2015.Rdat')
}

# list of all species
all_species <- unique(OBCatch$species)
# list of skate species
skate_species <- all_species[grep("skate", tolower(all_species))]

# subset big table to list of skate species
OBCatch_skates <- OBCatch[OBCatch$species %in% skate_species,]
# sum catch by species
total_MT  <- aggregate(OBCatch_skates$MT,  by=list(OBCatch_skates$species), FUN=sum)
total_RET_MT <- aggregate(OBCatch_skates$RET_MT, by=list(OBCatch_skates$species), FUN=sum)
total_DIS_MT <- aggregate(OBCatch_skates$DIS_MT, by=list(OBCatch_skates$species), FUN=sum)

# combine aggregated catch into 1 table
total_table <- data.frame(total_MT$Group.1, MT=total_MT$x,
                          RET_MT=total_RET_MT$x, DIS_MT=total_DIS_MT$x)
# round values
total_table[,-1] <- round(total_table[,-1])
# calculate discard rate
total_table$disc_rate <- round(total_table$DIS_MT / total_table$MT,2)

# print table ordered by total
total_table[order(total_table$MT, decreasing=TRUE),]

##             total_MT.Group.1   MT RET_MT DIS_MT disc_rate
## 8             Longnose Skate 5956   4458   1498      0.25
## 11                Skate Unid 3437   3044    393      0.11
## 4                  Big Skate  912    466    447      0.49
## 10           Sandpaper Skate  311      1    310      1.00
## 5                Black Skate  152      1    151      0.99
## 6           California Skate   61      1     59      0.97
## 2             Aleutian Skate   12      0     12      1.00
## 7              Deepsea Skate    4      0      4      1.00
## 12              Starry Skate    3      0      3      1.00
## 13               White Skate    1      0      1      1.00
## 1               Alaska Skate    0      0      0       NaN
## 3               Bering Skate    0      0      0       NaN
## 9  Roughshoulder/Broad Skate    0      0      0       NaN

# three groupings:
species == "Big Skate"
species %in% c("Big Skate", "Skate Unid")
species %in% c("Big Skate", "Skate Unid", "Longnose Skate")



dir.dis <- 'C:/SS/skates/discards/'
files <- dir(file.path(dir.dis, 'WCGOP discard rates'))

cs_3spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big longnose_unid_skates_OB_DisRatios_boot_cs_big_longnose_2019-04-26.csv"))
ncs_3spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big longnose_unid_skates_OB_DisRatios_boot_ncs_2019-04-26.csv"))
cs_2spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big skate_and_unid_OB_DisRatios_boot_cs_2019-04-26.csv"))
ncs_2spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big skate_and_unid_OB_DisRatios_boot_ncs_2019-04-26.csv"))
cs_1spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big skate_alone_OB_DisRatios_boot_cs_2019-04-26.csv"))
ncs_1spec <- read.csv(file.path(dir.dis, 'WCGOP discard rates',
  "big skate_alone_OB_DisRatios_boot_ncs_2019-04-26.csv"))


# examination of the data shows trawl to swamp all non-trawl gears,
# and non-catch-shares to be insignificant once the catch-share period begins in 2011

# next steps were to combine the Trawl only estimates across the different groups
# with NCS for the years prior to 2011 and the CS for the years after
# plot was written to C:\SS\skates\discards\
head(ncs_2spec)
# rates for SS
SSinputs <- rbind(ncs_2spec[ncs_2spec$ryear < 2011 & ncs_2spec$gear3 == "Trawl",
                            c('ryear','Observed_Ratio','CV.Boot_Ratio')],
                  data.frame(cs_2spec[cs_2spec$ryear %in% 2011:2014 &
                                        cs_2spec$gear3 == "Trawl",
                                      c('ryear','Observed_Ratio'),],
                             CV.Boot_Ratio = 0.01),
                  data.frame(cs_1spec[cs_1spec$ryear >= 2015 &
                                        cs_1spec$gear3 == "Trawl",
                                      c('ryear','Observed_Ratio'),],
                             CV.Boot_Ratio = 0.01))

SSinputs[,2:3] <- round(SSinputs[,2:3], 3)
SSinputs
##     ryear Observed_Ratio CV.Boot_Ratio
## 3    2002          0.628         0.063
## 5    2003          0.326         0.158
## 7    2004          0.399         0.128
## 9    2005          0.218         0.178
## 11   2006          0.204         0.173
## 13   2007          0.236         0.181
## 15   2008          0.060         0.259
## 17   2009          0.092         0.264
## 19   2010          0.060         0.256
## 31   2011          0.099         0.010
## 6    2012          0.104         0.010
## 91   2013          0.124         0.010
## 12   2014          0.119         0.010
## 121  2015          0.129         0.010
## 14   2016          0.156         0.010
## 16   2017          0.140         0.010
