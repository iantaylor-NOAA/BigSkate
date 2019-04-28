# note: this file has some data processing and code to make maps,
# this is for Longnose Skate and Big Skate in 2019 based on similar work
# for spiny dogfish in 2011
# to get index of abundance, see file called "IPHC Binomial GLM skates.R"

# read files from IPHC
iphc.dir <- 'c:/SS/skates/indices/IPHC/'
iphc.skate <- read.csv(file.path(iphc.dir,
                                 'TaylorI20190318_FISS-skate-spp sets.csv'),
                       stringsAsFactors=FALSE)
iphc.fish  <- read.csv(file.path(iphc.dir,
                                 'TaylorI20190318_FISS-skate-spp fish.csv'),
                       stringsAsFactors=FALSE)

# load R packages
require(maps)
require(mapdata)

# load information on EEZ for U.S. west coast
source('c:/SS/skates/R/US.Can.boundary.R')
eez.outer <- read.csv('C:/data/maps/EEZ_polygon_lat_lon.csv')

# make map
map('worldHires', #regions=c("Canada","Mexico"),
    #xlim = c(-180, -116.8), ylim = c(32, 60),   # include Alaska
    #xlim = c(-130, -116.8), ylim = c(32, 49.5), # whole west coast
    #xlim = c(-130, -116.8), ylim = c(37, 49.5), # down to San Francisco
    xlim = c(-128, -122), ylim = c(41.9, 49.5),   # north coast only
    col='grey', fill=TRUE, interior=TRUE, lwd=1)

# add US/Canada boundary
lines(US.CAN.lon, US.CAN.lat, lty=2)

# add some text
text(-120, 50, "Canada")
text(-120, 48, "U.S.")

# add IPHC survey points
points(iphc.skate$MidLon,
       iphc.skate$MidLat,
       col=2, pch=16, cex=.5)

# convert NULL to NA and character to numeric
for(col in c("End.Lat", "End.Lon", "End.Depth",
              "Hooks.Retrieved", "Hooks.Fished", "Soak.Time")){
  iphc.skate[[col]][iphc.skate[[col]]=="NULL"] <- NA
  iphc.skate[[col]] <- as.numeric(iphc.skate[[col]])
}

# check eez.polygon
## polygon(eez.outer, col=3)
## map('worldHires', regions=c("Canada","USA", "Mexico"),
##     add=TRUE,
##     col='grey', fill=TRUE, interior=TRUE, lwd=1)
## polygon(eez.outer)

# check for stations on the U.S. west coast
# (excludes Puget Sound, which are also in IPHC.Reg.Area=="2A")
iphc.skate$in.eez <- sp::point.in.polygon(point.x = iphc.skate$MidLon,
                                          point.y = iphc.skate$MidLat,
                                          pol.x = eez.outer$lon,
                                          pol.y = eez.outer$lat)
# confirm filtering
points(iphc.skate$MidLon[iphc.skate$in.eez==1],
       iphc.skate$MidLat[iphc.skate$in.eez==1],
       col=2, pch=16, cex=.3)
good.stations <- unique(iphc.skate$Station[iphc.skate$in.eez==1])
bad.stations <- unique(iphc.skate$Station[iphc.skate$in.eez==0])
# look deeper at the one area which is sometimes in or out
good.stations[good.stations %in% bad.stations]
## [1] 1084
table(iphc.skate$in.eez[iphc.skate$Station==1084])
## 0  1 
## 2 17 
points(iphc.skate$MidLon[iphc.skate$Station==1084],
       iphc.skate$MidLat[iphc.skate$Station==1084],
       col=3, pch=16, cex=.3)

## # filter to only points in the U.S.
## iphc.US <- iphc.skate[iphc.skate$in.eez==1,]
# filter to only the good stations (includes 2 observations in Canada at 1084)
iphc.US <- iphc.skate[iphc.skate$Station %in% good.stations,]

# note that hooks observed is usually around 20%
table(iphc.US$Year, round(iphc.US$Hooks.Observed/iphc.US$Hooks.Retrieved,1))
  ##      0.1 0.2 0.3 0.8   1
  ## 1999   0  84   0   0   0
  ## 2001   0  84   0   0   0
  ## 2002   0  84   0   0   0
  ## 2003  84   0   0   0   0
  ## 2004   0  84   0   0   0
  ## 2005   0  84   0   0   0
  ## 2006   0  84   0   0   0
  ## 2007   0  84   0   0   0
  ## 2008   0  84   0   0   0
  ## 2009   0  83   1   0   0
  ## 2010   0  84   0   0   0
  ## 2011   0 119   2   0   0
  ## 2012   0  84   0   1  10
  ## 2013   0  96   0   0  14
  ## 2014   0 134   0   0  14
  ## 2015   0  82   1   0  12
  ## 2016   1  82   0   0  12
  ## 2017   0 133   0   0  30
  ## 2018   0  89   0   0   6

# note relatively few sets off Washington and Oregon. Note: Area 2A based on map:
# http://www.iphc.washington.edu/images/iphc/surveyregions_big.gif

table(iphc.skate$Year, iphc.skate$IPHC.Reg.Area)
      
  ##       2A  2B  2C  3A  3B  4A  4B  4C  4D  4E CLS
  ## 1998   0 128 124 376 232 112  73   0   0   0   0
  ## 1999  84 170 124 375 232  66  86   0   0   0   0
  ## 2000   0 129 123 374 231 113  90   0  50   0   0
  ## 2001  84 170 123 374 231 113  90   0  50   0   0
  ## 2002  84 170 123 378 231 114  89   0  52   0   0
  ## 2003  84 170 123 374 231 113  89   0  49   0   0
  ## 2004  84 170 123 374 232 112  89   0  49   0   0
  ## 2005  84 170 123 374 228 110  89   0  49   0   0
  ## 2006  84 170 123 374 231 114  89  28  92  22  17
  ## 2007  84 170 123 374 231 113  89  20  58   0   0
  ## 2008  84 170 123 375 230 113  89  20  58   0   0
  ## 2009  84 170 123 374 232 113  89  20  58   0   0
  ## 2010  84 170 123 374 231 113  89  21  58   0   0
  ## 2011 136 170 123 374 231 113  89  20  58   0   0
  ## 2012  97 170 123 374 232 113  89  20  58   0   0
  ## 2013 112 170 123 374 231 113  89  20  58   0   0
  ## 2014 163 170 123 374 231 192  90  20  58   0   0
  ## 2015  96 170 123 374 231 114  89  28  92  23  17
  ## 2016  96 170 123 376 231 113  90  20 142   0   0
  ## 2017 178 166 123 374 231 113 202  20  58   0   0
  ## 2018 110 297 165 370 231 113  90  20  58   0   0

# filter to only stations that were visited prior to 2011
regular.stations <- unique(iphc.US$Station[iphc.US$Year == 2010])
iphc.US <- iphc.US[iphc.US$Station %in% regular.stations,]

# check for ineffective sets (NEED TO FOLLOW UP TO KNOW WHAT THESE MEAN)
table(iphc.US$Year[iphc.US$Effective=="N"],
      iphc.US$Ineffective.Code[iphc.US$Effective=="N"])
  ##      DS GI PP ST
  ## 2004  1  0  0  0
  ## 2010  1  0  0  0
  ## 2012  0  2  0  0
  ## 2016  0  0  0  1
  ## 2018  0  0  1  0

# make map for just stations to be used:
map('worldHires', #regions=c("Canada","Mexico"),
    xlim = c(-128, -122), ylim = c(41.9, 49.5),   # north coast only
    col='grey', fill=TRUE, interior=TRUE, lwd=1)

# add US/Canada boundary
lines(US.CAN.lon, US.CAN.lat, lty=2)

# add some text
text(-120, 50, "Canada")
text(-120, 48, "U.S.")

# add IPHC survey points
points(iphc.US$MidLon,
       iphc.US$MidLat,
       col=2, pch=16, cex=.5)


###############################################################3
# work with data on skate samples
###############################################################3

table(iphc.fish$Common.Name)

  ##        Alaska Skate        Aleutian Skate          Bering Skate 
  ##                2860                  4089                   547 
  ## Bering/Alaska Skate Bering/Aleutian Skate             Big Skate 
  ##                  13                    12                  4066 
  ##         Black Skate       Butterfly Skate       Commander Skate 
  ##                  99                    34                    30 
  ##       Deepsea Skate        Flathead Skate          Golden Skate 
  ##                   9                     6                     5 
  ##       Leopard Skate        Longnose Skate             Mud Skate 
  ##                 259                 11526                     9 
  ##       Okhotsk Skate   Roughshoulder Skate       Sandpaper Skate 
  ##                   3                    26                   160 
  ##        Starry Skate        unident. Skate   Whiteblotched Skate 
  ##                  58                  1805                  1242 
  ##     Whitebrow Skate 
  ##                  11 

# unidentified designation was most common in 1998 to 2003
table(iphc.fish$Year, iphc.fish$Common.Name=="unident. Skate")
      
  ##      FALSE TRUE
  ## 1998   372  437
  ## 1999   791  199
  ## 2000   875  183
  ## 2001   789  162
  ## 2002   766  212
  ## 2003   879  192
  ## 2004  1127   61
  ## 2005  1260   51
  ## 2006  1232   30
  ## 2007  1092   35
  ## 2008  1362   19
  ## 2009  1539   48
  ## 2010  1564   26
  ## 2011  1399   23
  ## 2012  1201   23
  ## 2013  1373    6
  ## 2014  1519   10
  ## 2015  1609   32
  ## 2016  1452    8
  ## 2017  1318   33
  ## 2018  1545   15

# filter for just U.S. west coast
iphc.fish.US <- iphc.fish[iphc.fish$Station %in% unique(iphc.US$Station),]

# use of unidentified is rare on west coast, can probably be assumed to be
# something rare, and neither Big Skate nor Longnose Skate
table(iphc.fish.US$Common.Name, iphc.fish.US$Year)
  ##                 1999 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
  ## Bering Skate       0    0    0    0    0    0    0    0    0    0    0    0
  ## Big Skate         15    8    2    4   13   13    1    4    2    3   10   13
  ## Black Skate        0    1    6    0    0    0    0    0    0    0    0    0
  ## Longnose Skate    43   42   21   32   43   41   25   25   37   27   33   27
  ## Sandpaper Skate    0    0    0    0    0    0    0    0    0    0    0    0
  ## unident. Skate     0    0    0    3    0    0    0    0    0    0    1    0
                 
  ##                 2012 2013 2014 2015 2016 2017 2018
  ## Bering Skate       0    0    0    0    0    2    0
  ## Big Skate          3    8   13   12   16   16    8
  ## Black Skate        0    0    0    0    0    0    0
  ## Longnose Skate    32   23   21   30   29   34   40
  ## Sandpaper Skate    0    0    0    0    2    0    0
  ## unident. Skate     0    0    0    0    0    0    0

# add species columns to table with info on each set
iphc.US$BigSkate.Observed <- 0
iphc.US$LongnoseSkate.Observed <- 0
iphc.US$OtherSkate.Observed <- 0

# loop over samples to fill in rows
for(irow in 1:nrow(iphc.fish.US)){
  row <- iphc.fish.US[irow,]
  jrow <- which(iphc.US$Year == row$Year &
                  iphc.US$Station == row$Station &
                    iphc.US$Set.No. == row$Set.No.)
  if(length(jrow) > 1){
    cat('multiple values: irow =', irow, ' jrow =', jrow)
  }
  # check for Big Skate samples
  if(row$Common.Name=="Big Skate"){
    iphc.US$BigSkate.Observed[jrow] <-
      iphc.US$BigSkate.Observed[jrow] + row$Number.Observed
  }
  # check for Longose Skate samples
  if(row$Common.Name=="Longnose Skate"){
    iphc.US$LongnoseSkate.Observed[jrow] <-
      iphc.US$LongnoseSkate.Observed[jrow] + row$Number.Observed
  }
  # check for all other skate
  if(!row$Common.Name %in% c("Big Skate", "Longnose Skate")){
    iphc.US$OtherSkate.Observed[jrow] <-
      iphc.US$OtherSkate.Observed[jrow] + row$Number.Observed 
  }
}

##### repeat for all stations
iphc.skate$BigSkate.Observed <- 0
iphc.skate$LongnoseSkate.Observed <- 0
iphc.skate$OtherSkate.Observed <- 0

# loop over samples to fill in rows
for(irow in 1:nrow(iphc.fish)){
  row <- iphc.fish[irow,]
  jrow <- which(iphc.skate$Year == row$Year &
                  iphc.skate$Station == row$Station &
                    iphc.skate$Set.No. == row$Set.No.)
  if(length(jrow) > 1){
    cat('multiple values: irow =', irow, ' jrow =', jrow)
  }
  # check for Big Skate samples
  if(row$Common.Name=="Big Skate"){
    iphc.skate$BigSkate.Observed[jrow] <-
      iphc.skate$BigSkate.Observed[jrow] + row$Number.Observed
  }
  # check for Longose Skate samples
  if(row$Common.Name=="Longnose Skate"){
    iphc.skate$LongnoseSkate.Observed[jrow] <-
      iphc.skate$LongnoseSkate.Observed[jrow] + row$Number.Observed
  }
  # check for all other skate
  if(!row$Common.Name %in% c("Big Skate", "Longnose Skate")){
    iphc.skate$OtherSkate.Observed[jrow] <-
      iphc.skate$OtherSkate.Observed[jrow] + row$Number.Observed 
  }
}



# confirm that totals match
sum(iphc.US$BigSkate.Observed)
## [1] 355
sum(iphc.fish.US$Number.Observed[iphc.fish.US$Common.Name=="Big Skate"])
## [1] 355
sum(iphc.US$LongnoseSkate.Observed)
## [1] 1261
sum(iphc.fish.US$Number.Observed[iphc.fish.US$Common.Name=="Longnose Skate"])
## [1] 1261
sum(iphc.US$OtherSkate.Observed)
## [1] 27
sum(iphc.fish.US$Number.Observed) - 355 - 1261
## [1] 27

### check for whole coast table
sum(iphc.skate$BigSkate.Observed)
## [1] 10781
sum(iphc.fish$Number.Observed[iphc.fish$Common.Name=="Big Skate"])
## [1] 10781
sum(iphc.skate$LongnoseSkate.Observed)
## [1] 43539
sum(iphc.fish$Number.Observed[iphc.fish$Common.Name=="Longnose Skate"])
## [1] 43539
sum(iphc.skate$OtherSkate.Observed)
## [1] 40849
sum(iphc.fish$Number.Observed) - 10781 - 43539
## [1] 40849

# catch of skates as proportion of hooks observed
iphc.US$BigSkate.prop <- iphc.US$BigSkate.Observed / iphc.US$Hooks.Observed
iphc.US$LongnoseSkate.prop <- iphc.US$LongnoseSkate.Observed / iphc.US$Hooks.Observed
iphc.US$OtherSkate.prop <- iphc.US$OtherSkate.Observed / iphc.US$Hooks.Observed

iphc.skate$BigSkate.prop <- iphc.skate$BigSkate.Observed / iphc.skate$Hooks.Observed
iphc.skate$LongnoseSkate.prop <- iphc.skate$LongnoseSkate.Observed / iphc.skate$Hooks.Observed
iphc.skate$OtherSkate.prop <- iphc.skate$OtherSkate.Observed / iphc.skate$Hooks.Observed

### save data.frames
save(iphc.US, iphc.skate, file=file.path(iphc.dir, "iphc.data_4-10-2019.Rdata"))


### make map with panel for each year
for(spec in c("Big Skate","Longnose Skate")){
  spec.short <- gsub(pattern=" ", replacement="", x=spec)
  png(file.path(iphc.dir, paste0('IPHC_',spec.short,'_map.png')), width=10, height=10,
      units='in', res=300)
  par(mfrow=c(2,10),mar=rep(0,4),oma=c(2,4,4,2))
  scale <- 15
  for(y in sort(unique(iphc.US$Year))){
    map('worldHires',xlim=c(-125.8,-123.8),ylim=c(41.8,48.7),
        mar=rep(.2,4),fill=TRUE,col='grey80')
    good <- iphc.US$Year==y
    colname <- paste0(spec.short, '.prop')
    vals <- iphc.US[[colname]][good]
    points(iphc.US$MidLon[good], iphc.US$MidLat[good],
           col=rgb(1,0,0,.3), cex=sqrt(vals)*scale, pch=16)
    points(iphc.US$MidLon[good], iphc.US$MidLat[good],
           col=1, cex=0.2, pch=16)
    mtext(line=.3,y,font=1)
    box()
    if(y==min(iphc.US$Year)){
      propvec <- c(0,0.01,0.02,0.05,0.1)
      n <- length(propvec)
      lonvec <- rep(-125.3,n)
      latvec <- seq(42,43,length=n)
      points(lonvec,latvec,col=rgb(1,0,0,.3),cex=sqrt(propvec)*scale, pch=16)
      points(lonvec,latvec,col=4,cex=0.2, pch=16)
      text(lonvec-.05,latvec,100*propvec,pos=2)
      at = axis(2,lab=F)
      # add degree symbol as suggested at http://tolstoy.newcastle.edu.au/R/e2/help/07/03/12710.html
      axis(2,at=at,lab=parse(text=paste(at,"*degree","~N",sep="")),las=1)
      rect(-126,41.6,-125,43.2)
    }
    # add a vertical axis for 2nd row
    if(par()$mfg[1] == 2 & par()$mfg[2] == 1){
      axis(2,at=at,lab=parse(text=paste(at,"*degree","~N",sep="")),las=1)
    }
    #at2 = c(-124,-125)
    #axis(1,at=at2,lab=parse(text = paste(format(abs(at2)), "*degree","~W", sep = "")),las=1)
  }
  mtext(text=paste(spec, "per 100 observed hooks in IPHC longline survey"),
        side=3, line=1, outer=TRUE, font=2, cex=1.8)
  dev.off()
}


### make big map for whole coast
for(spec in c("Big Skate","Longnose Skate")){
  spec.short <- gsub(pattern=" ", replacement="", x=spec)
  png(file.path(iphc.dir, paste0('IPHC_coast_',spec.short,'_map.png')), width=10, height=7.5,
      units='in', res=300)
  #windows(width=10, height=7.5) #, mar=c(6,6,1,1))
  scale <- 5
  map('worldHires',xlim=c(-179,-120),ylim=c(35,62),
      mar=c(6,6,1,1), fill=TRUE,col='grey80')
  colname <- paste0(spec.short, '.prop')
  good <- 1:nrow(iphc.skate)
  vals <- iphc.skate[[colname]][good]
  points(iphc.skate$MidLon[good], iphc.skate$MidLat[good],
         col=rgb(1,0,0,.1), cex=sqrt(vals)*scale, pch=16)
  good <- which(iphc.skate$Year==2017)
  points(iphc.skate$MidLon[good], iphc.skate$MidLat[good],
         col=4, cex=0.2, pch=16)
  box()

  propvec <- c(0,0.01,0.02,0.05,0.1)
  n <- length(propvec)
  lonvec <- rep(-170,n)
  latvec <- seq(42,45,length=n)
  points(lonvec,latvec,col=rgb(1,0,0,.3),cex=sqrt(propvec)*scale, pch=16)
  points(lonvec,latvec,col=1,cex=0.2, pch=16)
  text(lonvec-.05,latvec,100*propvec,pos=2)
  # add degree symbol as suggested at http://tolstoy.newcastle.edu.au/R/e2/help/07/03/12710.html
  at = axis(1,lab=F)
  axis(1,at=at,lab=parse(text=paste(abs(at),"*degree","~W",sep="")),las=1)
  at2 = axis(2,lab=F)
  axis(2,at=at2,lab=parse(text = paste(format(at),"*degree","~N",sep="")),las=1)
  ## }
  mtext(text=paste(spec, "per 100 observed hooks in IPHC longline survey"),
        side=3, line=1, outer=FALSE, font=2, cex=1.8)
  dev.off()
}
