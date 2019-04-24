#################

# Install package from GitHub using devtools function
install.packages("devtools")
devtools::install_github("nwfsc-assess/nwfscAgeingError")

# Load package
library(nwfscAgeingError)

# File where the Punt et al. (2008) model (pre-compiled in ADMB) resides
SourceFile <- paste(system.file("executables", package="nwfscAgeingError"),"/",sep="")
mydir <- 'c:/SS/skates/bio/ageing/double_reads' # directory on Ian's computer

#################

rawdat <- read.csv(file=file.path(mydir, "Double_Reads_All_Specimens_BSKT_qry.csv"),
                   stringsAsFactors = FALSE)
dat <- data.frame(SpecimenID = unique(rawdat$SpecimenID),
                  tjohnson = -999,
                  pmcdonald = -999,
                  cgburski = -999,
                  marrington = -999,
                  bmatta = -999)
for(irow in 1:nrow(dat)){
  ID <- dat$SpecimenID[irow]
  # subset rows of raw data file for this ID
  rawdat_sub <- rawdat[rawdat$SpecimenID == ID, ]
  for(jrow in 1:nrow(rawdat_sub)){
    reader1 <- rawdat_sub$Original_AgerID[jrow]
    age1 <- rawdat_sub$Original_Age_Estimate[jrow]
    reader2 <- rawdat_sub$DoubleRead_AgerID[jrow]
    age2 <- rawdat_sub$DoubleRead_Age_Estimate[jrow]

    dat[irow, reader1] <- age1
    dat[irow, reader2] <- age2
  }
}

# remove IDs
dat <- dat[,-1]
datNAs <- dat
datNAs[datNAs == -999] <- NA
Nreaders <- ncol(dat)

# make table formatted as required by ageing error software
Reads2 = data.frame(count=1, dat[1,])
# loop over rows of original data
for(RowI in 2:nrow(dat)){
  DupRow <- NA
  # loop over all previous rows looking for duplicates of the current row
  for(PreviousRowJ in 1:nrow(Reads2)){
    # if all values match, take note of previous row number
    if(all(dat[RowI,1:Nreaders]==
           Reads2[PreviousRowJ,1:Nreaders+1])){
      DupRow = PreviousRowJ
    }
  }
  # if no duplicate found, add new row
  if(is.na(DupRow)){
    # Add new row to ChinaReads2
    Reads2 <- rbind(Reads2, data.frame(count=1, dat[RowI,]))
  }
  # if duplicate found, increment count
  if(!is.na(DupRow)){
    # Increment number of samples for the previous duplicate
    Reads2[DupRow,1] <- Reads2[DupRow,1] + 1
  }
}

MinAge <- 0
(MaxAge <- max(ceiling(max(Reads2[,2:(Nreaders+1)])/5)*5))
## [1] 15
#MaxAge <- 26
KnotAges <- list(NA, NA, NA, NA, NA)  # Necessary for option 5 or 6

nmods <- 15
BiasOpt.mat <- SigOpt.mat <- matrix(0,nmods,Nreaders)
BiasOpt.mat[1,] =  c(0,0,0,0,0)
BiasOpt.mat[2,] =  c(0,0,0,0,0)
BiasOpt.mat[3,] =  c(0,0,0,0,0)
BiasOpt.mat[4,] =  c(0,0,1,-3,-3)
BiasOpt.mat[5,] =  c(0,0,1,-3,-3)
BiasOpt.mat[6,] =  c(0,0,1,-3,-3)
BiasOpt.mat[7,] =  c(1,-1,0,0,0)
BiasOpt.mat[8,] =  c(1,-1,0,0,0)
BiasOpt.mat[9,] =  c(1,-1,0,0,0)
BiasOpt.mat[10,] = c(0,0,2,-3,-3)
BiasOpt.mat[11,] = c(0,0,2,-3,-3)
BiasOpt.mat[12,] = c(0,0,2,-3,-3)
BiasOpt.mat[13,] = c(2,-1,0,0,0)
BiasOpt.mat[14,] = c(2,-1,0,0,0)
BiasOpt.mat[15,] = c(2,-1,0,0,0)

SigOpt.mat[1,] =  c(1,-1,-1,-1,-1)
SigOpt.mat[2,] =  c(2,-1,-1,-1,-1)
SigOpt.mat[3,] =  c(3,-1,-1,-1,-1)
SigOpt.mat[4,] =  c(1,-1,-1,-1,-1)
SigOpt.mat[5,] =  c(2,-1,-1,-1,-1)
SigOpt.mat[6,] =  c(3,-1,-1,-1,-1)
SigOpt.mat[7,] =  c(1,-1,-1,-1,-1)
SigOpt.mat[8,] =  c(2,-1,-1,-1,-1)
SigOpt.mat[9,] =  c(3,-1,-1,-1,-1)
SigOpt.mat[10,] = c(1,-1,-1,-1,-1)
SigOpt.mat[11,] = c(2,-1,-1,-1,-1)
SigOpt.mat[12,] = c(3,-1,-1,-1,-1)
SigOpt.mat[13,] = c(1,-1,-1,-1,-1)
SigOpt.mat[14,] = c(2,-1,-1,-1,-1)
SigOpt.mat[15,] = c(3,-1,-1,-1,-1)

# subset for CARE study with all 5 readers
Reads3 <- Reads2[Reads2$bmatta != -999,]
model.aic <- as.data.frame(matrix(NA, nrow(BiasOpt.mat), 4))
colnames(model.aic) <- c("Run","AIC","AICc","BIC")
model.name<-c("B1_S1_CARE","B1_S2_CARE","B1_S3_CARE",
              "B2_S1_CARE","B2_S2_CARE","B2_S3_CARE",
              "B3_S1_CARE","B3_S2_CARE","B3_S3_CARE",
              "B4_S1_CARE","B4_S2_CARE","B4_S3_CARE",
              "B5_S1_CARE","B5_S2_CARE","B5_S3_CARE")

for(i in 1:nmods)
{
  setwd(mydir)
  DateFile <- file.path(mydir, model.name[i])
  dir.create(DateFile)
  BiasOpt =BiasOpt.mat[i,]
  SigOpt = SigOpt.mat[i,]
  RunFn(Data=Reads3, SigOpt=SigOpt,KnotAges =KnotAges, BiasOpt=BiasOpt,
        NDataSets=1, MinAge=MinAge, MaxAge=MaxAge, RefAge=10,
        MinusAge=1, PlusAge=15,
        SaveFile=DateFile,
        AdmbFile=SourceFile, EffSampleSize=0, Intern=FALSE,
        JustWrite=FALSE, CallType="shell") #,ExtraArgs=" -ams 2341577272 -est")

  ## RunFn(Data=AgeReads3, SigOpt=SigOpt[1:2], KnotAges=KnotAges[1:2], BiasOpt=BiasOpt[1:2],
  ##       NDataSets=1, MinAge=MinAge, MaxAge=MaxAge, RefAge=10,
  ##       MinusAge=1, PlusAge=30, SaveFile=DateFile, AdmbFile=SourceFile,
  ##       EffSampleSize=0, Intern=FALSE, JustWrite=FALSE, CallType="shell",
  ##       ExtraArgs=" -ams 400000000 -est")

  
  info <- PlotOutputFn(Data=Reads3, MaxAge=MaxAge, SaveFile=DateFile, PlotType="PNG")
  Df <- as.numeric(scan(file.path(DateFile,"agemat.par"),comment.char="%", what="character", quiet=TRUE)[6])
  Nll <- as.numeric(scan(file.path(DateFile,"agemat.par"),comment.char="%", what="character", quiet=TRUE)[11])
  n <- sum(ifelse(Reads3[,-1]==-999,0,1))
  Aic <- 2*Nll + 2*Df
  Aicc <- Aic + 2*Df*(Df+1)/(n-Df-1)
  Bic <- 2*Nll + Df*log(n)
  run.name <- strsplit(DateFile,"/")[[1]]
  run.name <- tail(run.name,1)
  model.aic[i,1] <- run.name
  model.aic[i,-1] <- c(Aic,Aicc,Bic)
}

# clean up some stuff that isn't working right in the loop above
## for(icol in 2:4){
##   model.aic[,icol] <- as.numeric(model.aic[,icol])
## }
## model.aic$Run <- model.name

save(model.aic, file = file.path(mydir, "model_selection_CARE.dmp"))




############# just 2 readers

dat2 <- dat[,1:2]
Nreaders <- 2
# make table formatted as required by ageing error software
Reads4 = data.frame(count=1, dat2[1,])
# loop over rows of original data
for(RowI in 2:nrow(dat2)){
  DupRow <- NA
  # loop over all previous rows looking for duplicates of the current row
  for(PreviousRowJ in 1:nrow(Reads4)){
    # if all values match, take note of previous row number
    if(all(dat2[RowI,1:Nreaders]==
           Reads4[PreviousRowJ,1:Nreaders+1])){
      DupRow = PreviousRowJ
    }
  }
  # if no duplicate found, add new row
  if(is.na(DupRow)){
    # Add new row to ChinaReads4
    Reads4 <- rbind(Reads4, data.frame(count=1, dat2[RowI,]))
  }
  # if duplicate found, increment count
  if(!is.na(DupRow)){
    # Increment number of samples for the previous duplicate
    Reads4[DupRow,1] <- Reads4[DupRow,1] + 1
  }
}

MinAge <- 0
(MaxAge <- max(ceiling(max(Reads4[,2:(Nreaders+1)])/5)*5))
## [1] 15
MaxAge <- 20
KnotAges <- list(NA, NA)  # Necessary for option 5 or 6


nmods <- 6
BiasOpt.mat <- SigOpt.mat <- matrix(0,nmods,Nreaders)
BiasOpt.mat[1,] =  c(0,0)
BiasOpt.mat[2,] =  c(0,0)
BiasOpt.mat[3,] =  c(0,0)
BiasOpt.mat[4,] =  c(1,0)
BiasOpt.mat[5,] =  c(1,0)
BiasOpt.mat[6,] =  c(1,0)

SigOpt.mat[1,] =  c(1,-1)
SigOpt.mat[2,] =  c(2,-1)
SigOpt.mat[3,] =  c(3,-1)
SigOpt.mat[4,] =  c(1,-1)
SigOpt.mat[5,] =  c(2,-1)
SigOpt.mat[6,] =  c(3,-1)

model.aic <- as.data.frame(matrix(NA, nrow(BiasOpt.mat), 4))
colnames(model.aic) <- c("Run","AIC","AICc","BIC")
model.name<-c("B1_S1maxage20","B1_S2maxage20","B1_S3maxage20",
              "B2_S1maxage20","B2_S2maxage20","B2_S3maxage20")
for(i in 1:nmods)
{
  setwd(mydir)
  DateFile <- file.path(mydir, model.name[i])
  dir.create(DateFile)
  BiasOpt =BiasOpt.mat[i,]
  SigOpt = SigOpt.mat[i,]
  RunFn(Data=Reads4, SigOpt=SigOpt,KnotAges =KnotAges, BiasOpt=BiasOpt,
        NDataSets=1, MinAge=MinAge, MaxAge=MaxAge, RefAge=10,
        MinusAge=1, PlusAge=15,
        SaveFile=DateFile,
        AdmbFile=SourceFile, EffSampleSize=0, Intern=FALSE,
        JustWrite=FALSE, CallType="shell") #,ExtraArgs=" -ams 2341577272 -est")

  info <- PlotOutputFn(Data=Reads4, MaxAge=MaxAge, SaveFile=DateFile, PlotType="PNG")
  Df <- as.numeric(scan(file.path(DateFile,"agemat.par"),comment.char="%", what="character", quiet=TRUE)[6])
  Nll <- as.numeric(scan(file.path(DateFile,"agemat.par"),comment.char="%", what="character", quiet=TRUE)[11])
  n <- sum(ifelse(Reads4[,-1]==-999,0,1))
  Aic <- 2*Nll + 2*Df
  Aicc <- Aic + 2*Df*(Df+1)/(n-Df-1)
  Bic <- 2*Nll + Df*log(n)
  run.name <- strsplit(DateFile,"/")[[1]]
  run.name <- tail(run.name,1)
  model.aic[i,1] <- run.name
  model.aic[i,-1] <- c(Aic,Aicc,Bic)
}

## model.aic[order(model.aic$AIC),]
##             Run     AIC     AICc      BIC
## 6 B2_S3maxage20 2912.68 2921.911 2967.050
## 5 B2_S2maxage20 2913.86 2923.091 2968.230
## 3 B1_S3maxage20 2927.22 2935.481 2978.871
## 2 B1_S2maxage20 2928.76 2937.021 2980.411
## 4 B2_S1maxage20 3041.24 3048.595 3090.173
## 1 B1_S1maxage20 3051.60 3058.111 3097.814

save(model.aic, file = file.path(mydir, "model_selection_2readers_maxage20.dmp"))

