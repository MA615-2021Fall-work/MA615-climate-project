library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)
library(gganimate)


source("data clean.R")
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

clean_buoy_list <- list(GRRT2_IKE_clean, T_42043_IKE_clean,GNJT2_IKE_clean,T_42035_IKE_clean,EPTT2_IKE_clean,MGPT2_IKE_clean,CLLT2_IKE_clean,RLOT2_IKE_clean)

wind_speed_tsp <- function(list){
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(WSPD)),color = "brown1", alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(WSPD)),color="aquamarine2", alpha = 0.4)+
    geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(WSPD)),color="deeppink",alpha = 0.5)+
    geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(WSPD)),color="darkorange",alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(WSPD)),color="blue",alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(WSPD)),color="blueviolet",alpha = 0.8)+
    geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(WSPD)),color="brown",alpha = 0.9)+
    geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(WSPD)),color="chartreuse",alpha = 1)+
    ylab("Wind speed(m/s)")+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ggtitle("Wind speed time series plot for Ike-2008 at different buoy")
}


gust_wind_speed_tsp <- function(list){
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(GST)), color = "brown1",alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(GST)),color="aquamarine2", alpha = 0.4)+
    geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(GST)),color="deeppink",alpha = 0.5)+
    geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(GST)),color="darkorange",alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(GST)),color="blue",alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(GST)),color="blueviolet",alpha = 0.8)+
    geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(GST)),color="brown",alpha = 0.9)+
    geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(GST)),color="chartreuse",alpha = 1)+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ylab("Gust wind speed(m/s)") +
    ggtitle("Gust wind speed time series plot for Ike-2008 at different buoy")
  
}




