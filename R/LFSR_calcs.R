# from TPL
## SRZ_0=log(1.0/(SSB_virgin_adj/Recr_virgin_adj));
## srz_min=SRZ_0*(1.0-steepness);
## SRZ_surv=mfexp((1.-pow((SSB_curr_adj/SSB_virgin_adj),SR_parm_work(3)) )*(srz_min-SRZ_0)+SRZ_0);  //  survival
## NewRecruits=SSB_curr_adj*SRZ_surv;
## exp_rec(y,1)=NewRecruits;   // expected arithmetic mean recruitment

## df <- SS_output('c:/SS/modeltesting/Version_3.30.13_beta_Mar8/Spinydogfish_2011/')
ssb0 <- df$derived_quants["SSB_Virg","Value"]
R0 <- exp(df$parameters["SR_LN(R0)","Value"])
zfrac <- df$parameters["SR_surv_zfrac","Value"]
beta <- df$parameters["SR_surv_Beta","Value"]

ssb2017 <- subset(df$recruit, Yr==2017, select=SpawnBio)

  lfr <- function(ssb, ssb0, R0, zfrac, beta) {

    z0 <- log(R0 / ssb0)
    zmax <- z0 + zfrac * (-z0)

    surv <- exp((1 - (ssb / ssb0) ^ beta) * (zmax - z0) + z0)

    rec <- ssb * surv

    return(rec)
  }


  lfr2 <- function(ssb, ssb0, R0, zfrac, beta) {

    z0 <- log(R0 / ssb0)
    zmin <- z0 * (1 - zfrac)
    
    surv <- exp((1 - (ssb / ssb0) ^ beta) * (zmin - z0) + z0)

    rec <- ssb * surv

    return(rec)
  }

lfr2(ssb=ssb2017, ssb0=ssb0,  R0 = R0, zfrac=zfrac, beta=beta)

# read dogfish model
df <- SS_output('c:/SS/modeltesting/Version_3.30.13_beta_Mar8/Spinydogfish_2011/')
# vector of ages
ages <- 0:df$accuage
# unfished equilibrium age structure
equilage <- as.numeric(df$natage[df$natage$Era=="VIRG" & df$natage$Sex==1 &
                                   df$natage$"Beg/Mid" == "B", paste(ages)])
# maturity*fecundity at age
fec <- df$endgrowth$"Mat*Fecund"[df$endgrowth$Sex==1]
# expected lifetime spawning output per recruit
sum(fec*equilage/equilage[1])
# [1] 5.984951


plot(0:95, fec)
lines(0:95, fec*equilage/equilage[1], col=2)
lines(0:95, equilage/equilage[1], col=3)
sum(fec*equilage/equilage[1])


fec <- bs21fec60$endgrowth$"Mat*Fecund"[bs21fec60$endgrowth$Sex==1]
equilage <- as.numeric(bs21fec60$natage[bs21fec60$natage$Era=="VIRG" & bs21fec60$natage$Sex==1 &
                                   bs21fec60$natage$"Beg/Mid" == "B", paste(0:20)])
plot(fec*equilage/equilage[1])
sum(fec*equilage/equilage[1])

