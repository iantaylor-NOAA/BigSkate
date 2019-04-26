
#install.packages("devtools")
devtools::install_github("nwfsc-assess/PacFIN.Utilities")
library(PacFIN.Utilities)

#Example file
?PacFIN_Example

#Define length and age bins

BSKT.LBINS <- seq(5, 200, by = 5)
BSKT.ABINS <- 0:15

#==============================================================
#===================  Load PacFIN BDS  ========================
#==============================================================

if(Sys.info()["user"] == "Ian.Taylor"){
  pacfin.dir <- "C:/SS/skates/bio/PacFIN_comps/"
}

# Load John Wallace's BDS .dmp file to R console
load(file.path(pacfin.dir, "PacFIN.BSKT.bds.24.Apr.2019.dmp"))

PacFIN.BSKT.BDS <- PacFIN.BSKT.bds.24.Apr.2029

#Select data only for one state, line below is for OR. This is for exploration purposes
#PacFIN.BSKT.BDS <- PacFIN.BSKT.bds.30.Aug.2018[PacFIN.BSKT.bds.30.Aug.2018$SOURCE_AGID %in% 'O',]

#==============================================================
#========  CONVERT ALL LENGTHS TYPES TO TOTAL LENGTH  =========
#==============================================================

# To convert from from disc width to total lengths
# (conversion is derived from WCGBTS data)

sub <- PacFIN.BSKT.BDS$FISH_LENGTH_TYPE %in% "A"
PacFIN.BSKT.BDS$FISH_LENGTH[sub] <- 1.3399 * PacFIN.BSKT.BDS$FISH_LENGTH[sub]

# To convert to from interspiracular width to total lengths
# Females or unsexed (only 2 unsexed fish with interspiracular width)
sub <- PacFIN.BSKT.BDS$FISH_LENGTH_TYPE %in% "R" & PacFIN.BSKT.BDS$SEX %in% c("F","U")
PacFIN.BSKT.BDS$FISH_LENGTH[sub] <- 12.111 + 9.761 * PacFIN.BSKT.BDS$FISH_LENGTH[sub]

# Males
sub <- PacFIN.BSKT.BDS$FISH_LENGTH_TYPE %in% "R" & PacFIN.BSKT.BDS$SEX %in% "F"
PacFIN.BSKT.BDS$FISH_LENGTH[sub] <- 3.824 + 10.927 * PacFIN.BSKT.BDS$FISH_LENGTH[sub]

#Check how many samples are in the data
table(PacFIN.BSKT.BDS$SAMPLE_NO)

#Clean PacFIN bds file
# note that all lengths types have already been converted to total length above
PacFIN.BSKT.BDS <- cleanPacFIN(PacFIN.BSKT.BDS,
                               keep_length_type = c("A","F","R","T"),
                               keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT"))

write.csv(PacFIN.BSKT.BDS, file = file.path(pacfin.dir, "PacFIN.BSKT.BDS_cleaned.csv"))

#==============================================================
#==================  Stratification  ==========================
#==============================================================

table(PacFIN.BSKT.BDS$geargroup) 
 ## HKL  TWL 
 ## 168 7537 

PacFIN.BSKT.BDS$mygear <- PacFIN.BSKT.BDS$geargroup

# this line doesn't change anything
PacFIN.BSKT.BDS$mygear[ PacFIN.BSKT.BDS$mygear != c("HKL")] <- "TWL"

PacFIN.BSKT.BDS$stratification <- paste(PacFIN.BSKT.BDS$state,
                                        PacFIN.BSKT.BDS$mygear, sep=".")

table(PacFIN.BSKT.BDS$stratification)
## CA.HKL CA.TWL OR.HKL OR.TWL WA.HKL WA.TWL 
##     24   1277     91   5005     53   1255 

#==============================================================
#=====================  Expansion  ============================
#==============================================================

# Expansion 1 (WL parameters estimated from WCGBTS data)
PacFIN.BSKT.BDS <- getExpansion_1(PacFIN.BSKT.BDS, maxExp = 0.95,
                                  Exp_WA = TRUE,
                                  fa = 7.4924e-06, fb = 2.9925,
                                  ma = 7.4924e-06, mb = 2.9925,
                                  ua = 7.4924e-06, ub = 2.9925)

# Expansion 2
#Read in catch file
Catch <- read.csv(file.path(pacfin.dir,
                            'Length comps/Landings - PacFIN BDS/Catch.csv'))
head(Catch)
summary(Catch)
PacFIN.BSKT.BDS <- getExpansion_2(PacFIN.BSKT.BDS, Catch,
                                  Convert=TRUE, maxExp = 0.95)

#Add column for final sample sizes
#No expansion
#PacFIN.BSKT.BDS$Final_Sample_Size = 1

#One-stage expantion
#PacFIN.BSKT.BDS$Final_Sample_Size = PacFIN.BSKT.BDS$Expansion_Factor_1

#Two-stage expantion
PacFIN.BSKT.BDS$Final_Sample_Size <-
  PacFIN.BSKT.BDS$Expansion_Factor_1 * PacFIN.BSKT.BDS$Expansion_Factor_2

#write csv. file of clean BDS file, with final samples sizes added

write.csv(PacFIN.BSKT.BDS, file.path(pacfin.dir, "PacFIN.BSKT.BDS_expanded2.csv"))

# Generate Length Comps

Lcomps <- getComps(PacFIN.BSKT.BDS, Comps="LEN")

Lcomps = doSexRatio(Lcomps)

# Write length comps into csv. file

# Females and males separately


writeComps(Lcomps, fname = file.path(pacfin.dir, "PacFIN.BSKT.BDS_length_comps.csv"),
           lbins = BSKT.LBINS,
           partition = 2, ageErr = NA, returns = "FthenM",
           dummybins = FALSE, sum1 = TRUE,
           overwrite = FALSE, verbose = TRUE)

#==============================================================
#========  AGES  =========
#==============================================================

## Ages:

##      'Adata = cleanAges(Pdata)'

##      'Adata = getExpansion_1(Adata)'

##      'Adata = getExpansion_2(Adata, Catch.XMPL, Convert=T)'

##      'Adata$Final_Sample_Size = Adata$Expansion_Factor_1 *
##      Adata$Expansion_Factor_2'

PacFIN.BSKT.BDS.ages <- PacFIN.BSKT.BDS[!is.na(PacFIN.BSKT.BDS$age) &
                                          PacFIN.BSKT.BDS$age >= 0,]
Acomps = getComps(PacFIN.BSKT.BDS.ages, Comps="AGE")

# skipping 4 out of 65 unsexed fish for now
## Acomps = doSexRatio(Acomps)

marginal_ages <- writeComps(Acomps, fname = file.path(pacfin.dir, "PacFIN.BSKT.BDS_age_comps.csv"),
                            abins = BSKT.ABINS,
                            partition = 2, ageErr = 1, returns = "FthenM",
                            dummybins = FALSE, sum1 = TRUE,
                            overwrite = TRUE, verbose = TRUE)

# Age-at-Length:

PacFIN.BSKT.BDS.AAL <- PacFIN.BSKT.BDS.ages
PacFIN.BSKT.BDS.AAL$Final_Sample_Size <- 1 

ALcomps <- getComps(PacFIN.BSKT.BDS.AAL, Comps="AAL")
# bin into 5cm bins by rounding down the length values
ALcomps$lengthcm <- 5*floor(ALcomps$lengthcm / 5)

#ALcomps <- doSexRatio(ALcomps) 

CAAL_F <- writeComps(ALcomps,
                     fname = file.path(pacfin.dir, "PacFIN.BSKT.BDS_AAL_comps.csv"),
                     abins = BSKT.ABINS,
                     lbins = BSKT.LBINS,
                     partition = 2, ageErr = 1, returns = "Fout",
                     dummybins = FALSE, sum1 = FALSE,
                     overwrite = TRUE, verbose = TRUE)
CAAL_M <- writeComps(ALcomps,
                     fname = file.path(pacfin.dir, "PacFIN.BSKT.BDS_AAL_comps.csv"),
                     abins = BSKT.ABINS,
                     lbins = BSKT.LBINS,
                     partition = 2, ageErr = 1, returns = "Mout",
                     dummybins = FALSE, sum1 = FALSE,
                     overwrite = TRUE, verbose = TRUE)

marginal_ages2 <- data.frame(marginal_ages[,c(1,2,4:6)],
                            LbinLo = -1, LbinHi = -1,
                             marginal_ages[,-(1:6)])
# turn off likelihood for marginals
marginal_ages2$fleet <- -1

names(CAAL_F) <- names(marginal_ages2)
names(CAAL_M) <- names(marginal_ages2)
all_ages <- rbind(marginal_ages2, CAAL_F, CAAL_M)
ages_fixed <- data.frame(year = all_ages$fishyr,
                         month = 7,
                         all_ages[,!names(all_ages) %in% c("Ntows")])
names(ages_fixed)[names(ages_fixed) %in% paste0("A",0:15)] <- paste0("F",0:15)
names(ages_fixed)[names(ages_fixed) %in% paste0("A",0:15,".1")] <- paste0("M",0:15)
write.csv(ages_fixed,
          file = file.path(pacfin.dir, "PacFIN.BSKT.BDS_AAL_comps_forSS.csv"),
          row.names=FALSE)
