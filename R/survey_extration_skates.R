if(FALSE){
  devtools::install_github("nwfsc-assess/nwfscSurvey", build_vignettes=TRUE)
  library(nwfscSurvey)
}

# WGBTS for Big Skate
catch.WCGBTS.BS <- PullCatch.fn(Name = "big skate", SurveyName = "NWFSC.Combo")
bio.WCGBTS.BS   <- PullBio.fn(Name = "big skate", SurveyName = "NWFSC.Combo")
catch.Tri.BS <- PullCatch.fn(Name = "big skate", SurveyName = "Triennial")
bio.Tri.BS   <- PullBio.fn(Name = "big skate", SurveyName = "Triennial")
catch.WCGBTS.Starry <- PullCatch.fn(Name = "starry flounder", SurveyName = "NWFSC.Combo")
catch.Tri.Starry <- PullCatch.fn(Name = "starry flounder", SurveyName = "Triennial")

catch.Tri.LN <- PullCatch.fn(Name = "longnose skate", SurveyName = "Triennial")
bio.Tri.LN   <- PullBio.fn(Name = "longnose skate", SurveyName = "Triennial")

# save data for offline use
save(catch.WCGBTS.BS, bio.WCGBTS.BS,
     catch.Tri.BS,    bio.Tri.BS,
     file = 'c:/SS/skates/data/BigSkate_survey_extractions_4-22-2019.Rdata')


# WGBTS for Longnose Skate
catch.combo.LN <- PullCatch.fn(Name = "longnose skate", SurveyName = "NWFSC.Combo")
bio.combo.LN   <- PullBio.fn(Name = "longnose skate", SurveyName = "NWFSC.Combo")

catch.tri.CA <- PullCatch.fn(Name = "California skate", SurveyName = "Triennial")
catch.combo.CA <- PullCatch.fn(Name = "California skate", SurveyName = "NWFSC.Combo")

# AFSC Slope (no data)
catch.AFSC.Slope.BS <- PullCatch.fn(Name = "big skate", SurveyName = "AFSC.Slope")
## No data returned by the warehouse for the filters given.
##  Make sure the year range is correct for the project selected and the input name is correct,
##  otherwise there may be no data for this species from this project.
## Error in PullCatch.fn(Name = "big skate", SurveyName = "AFSC.Slope") :
  
# NWFSC Slope (depth started at 183m, too few positive tows)
catch.NWFSC.Slope.BS <- PullCatch.fn(Name = "big skate", SurveyName = "NWFSC.Slope")
table(catch.NWFSC.Slope.BS$Year, catch.NWFSC.Slope.BS$cpue_kg_km2 > 0)
  ##      FALSE TRUE
  ## 1998   301    0
  ## 1999   324    0
  ## 2000   329    0
  ## 2001   333    1
  ## 2002   424    2

# NWFSC Shelf (2001 only, narrow range of latitudes)
catch.NWFSC.Shelf.BS <- PullCatch.fn(Name = "big skate", SurveyName = "NWFSC.Shelf")
table(catch.NWFSC.Shelf.BS$Year, catch.NWFSC.Shelf.BS$cpue_kg_km2 > 0)
  ##      FALSE TRUE
  ## 2001    49   22

# Video survey (?)
catch.NWFSC.Video.BS <- PullCatch.fn(Name = "big skate", SurveyName = "NWFSC.Video")
table(catch.NWFSC.Video.BS$Year, catch.NWFSC.Video.BS$cpue_kg_km2 > 0)
  ##      FALSE TRUE
  ## 2009    27   21


          
