library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

hmapper <- function(hurr){
  
  rmap = map_counties(storm = hurr, metric = "rainfall") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap = map_counties(storm = hurr, metric = "wind") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  expos = map_rain_exposure(storm =hurr, 
                            rain_limit = 175, 
                            dist_limit = 500, 
                            days_included =-5:3) +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ml <-  list(rmap, wmap, expos)
  names(ml) <- c("rmap", "wmap", "expos")
  
  return(ml)
}





