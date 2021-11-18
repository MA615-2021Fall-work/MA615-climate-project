library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)
library(ggplot2)


source("data clean.R")
source("map_counties_j.R")


get_bundle <- function() {
  #rainfall
  map_counties_j(storm = "Ike-2008", metric= "rainfall", days_included = -2:2) +
    ggtitle("Rainfall(mm) map for Hurricane Ike-2008 with the tract")
  
  #wind speed(m/s)
  map_counties_j(storm = "Ike-2008", metric = "wind")+
    ggtitle("Wind speed(m/s) map for Hurricane Ike-2008 with the tract")
  
  #wind duration(minutes)
  map_counties("Ike-2008", metric = "wind", wind_var = "sust_dur")+
    ggtitle("Wind duration(minutes) map for Hurricane Ike-2008 with the tract")
  
  #exposed distance
  map_counties_j(storm = "Ike-2008", metric = "distance")+
    ggtitle("Exposed distance(km) for Hurricane Ike-2008 with the tract")
  
  
}

map_counties_j(storm = "Ike-2008", metric = "wind")+
  ggtitle("Wind speed(m/s) map for Hurricane Ike-2008 with the tract")





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




wind_speed_tsp <- function(list){
  colors <- c("GRRT2" = "brown1", "42043" = "aquamarine2", "GNJT2" = "deeppink", "42035" = "darkorange", "EPTT2" = "blue", "MGPT2" =  "blueviolet", "CLLT2" = "brown", "RLOT2" = "chartreuse")
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(WSPD),color = "GRRT2"), alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(WSPD),color="42043"), alpha = 0.4)+
    geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(WSPD),color="GNJT2"),alpha = 0.5)+
    geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(WSPD),color="42035"),alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(WSPD),color="EPTT2"),alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(WSPD),color="MGPT2"),alpha = 0.8)+
    geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(WSPD),color="CLLT2"),alpha = 0.9)+
    geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(WSPD),color="RLOT2"),alpha = 1)+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ggtitle("Wind speed time series plot for Ike-2008 at different buoy")+
    labs(x = "Date and time",
         y = "Wind speed(m/s)",
         color = "Buoy station") +
    scale_color_manual(values = colors)
}


gust_wind_speed_tsp <- function(list){
  colors <- c("GRRT2" = "brown1", "42043" = "aquamarine2", "GNJT2" = "deeppink", "42035" = "darkorange", "EPTT2" = "blue", "MGPT2" =  "blueviolet", "CLLT2" = "brown", "RLOT2" = "chartreuse")
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(GST),color = "GRRT2"), alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(GST),color="42043"), alpha = 0.4)+
    geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(GST),color="GNJT2"),alpha = 0.5)+
    geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(GST),color="42035"),alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(GST),color="EPTT2"),alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(GST),color="MGPT2"),alpha = 0.8)+
    geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(GST),color="CLLT2"),alpha = 0.9)+
    geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(GST),color="RLOT2"),alpha = 1)+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ggtitle("Gust wind speed time series plot for Ike-2008 at different buoy")+
    labs(x = "Date and time",
         y = "Gust wind speed(m/s)",
         color = "Buoy station") +
    scale_color_manual(values = colors)
}



wind_speed_tsp(get_result())
gust_wind_speed_tsp(get_result())

