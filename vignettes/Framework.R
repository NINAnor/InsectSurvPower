## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = T
)

## ---- include = F--------------------------------------------------------
require(tidyverse)
require(sf)
#require(ggplot2)
require(SurveyPower)

## ------------------------------------------------------------------------


system.time(test5yearsTrend <- createOccNorm(map10km, 
                              intercept = 10,
                              sigmaFylke = 0, 
                              sigmaKommune = 0.5, 
                              sigmaGrid = 0.5,
                              nYears = 5,
                              interceptTrend = -0.1,
                              sigmaFylkeTrend = 0,
                              sigmaKommuneTrend = 0,
                              sortGrid = F, 
                              sortFylke = F, 
                              sortKommune = F)
            )


