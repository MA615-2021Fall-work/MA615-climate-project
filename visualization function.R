library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)

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

#rainfall
map_counties(storm = "Ike-2008", metric= "rainfall", days_included = -1:0) +
  ggtitle("Rain Ike")


#wind speed(m/s)
map_counties(storm = "Ike-2008", metric = "wind")


#wind duration(minutes)
map_counties("Ike-2008", metric = "wind", wind_var = "sust_dur")

#exposed distance
map_counties(storm = "Ike-2008", metric = "distance")

#exposed distance < 75km
map_distance_exposure(storm = "Ike-2008", dist_limit = 75)

#rain exposure(rain>175mm)
map_rain_exposure(storm ="Ike-2008", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3)


#wind exposure(wind > 17.5m/s)
map_wind_exposure(storm = "Ike-2008",
                  wind_limit = convert_wind_speed(34, "knots","mps"))


#flood event
map_event_exposure(storm = "Ike-2008", event_type = "flood")

#tornado event
map_event_exposure(storm = "Ike-2008", event_type = "tornado")

#flood event + hurr track
Ike_map <- map_event_exposure(storm = "Ike-2008", event_type = "flood")

map_tracks(storms = "Ike-2008",
           plot_object =Ike_map,
           plot_points =TRUE,
           color ="darkgray"
)



map_counties(storm = "Ike-2008", metric = "wind")
