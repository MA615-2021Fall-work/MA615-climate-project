library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)
library(ggplot2)
library(sf)
library(ggforce)

addRepo("geanders")

source("data clean.R")
source("map_counties_j.R")
source("map_buoy.R")


hmapper <- function(hurr){

  location <- data.frame("buoy" = c("GRRT2","42043","GNJT2","42035","EPTT2","MGPT2","CLLT2","RLOT2"), "longi" = c(-94.896,-94.899,-94.725,-94.413,-94.917,-94.985,-95.67,-94.513), "lati" = c(29.302,28.982,29.357,29.232,29.481,29.682,29.563,29.515))
  
  lati <- location$lati
  longi <- location$longi
  buoy <- location$buoy
  
  trackmap = map_tracks(storms = "Ike-2008",
                          plot_points =TRUE,
                          color ="darkgray"
  )+ggtitle("The track for Ike-2008")+geom_point(aes(x=longi, y = lati), color="darkorange")
  
  
  rmap = map_counties_j(storm = "Ike-2008", metric= "rainfall", days_included = -2:2) +
    ggtitle("Rainfall(mm) map for Hurricane Ike-2008 with the tract")+
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap = map_counties_j(storm = "Ike-2008", metric = "wind")+
    ggtitle("Wind speed(m/s) map for Hurricane Ike-2008 with the tract")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  wdmap <- map_counties("Ike-2008", metric = "wind", wind_var = "sust_dur")+
    ggtitle("Wind duration(minutes) map for Hurricane Ike-2008 with the tract")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  expos = map_counties_j(storm = "Ike-2008", metric = "distance")+
    ggtitle("Exposed distance(km) for Hurricane Ike-2008 with the tract")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  flood = map_event_exposure(storm = "Ike-2008", event_type = "flood")+
    ggtitle("Floods events during Ike-2008")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  tornado  = tornade_event <- map_event_exposure(storm = "Ike-2008", event_type = "tornado")+
    ggtitle("Tonarto events during Ike-2008")+
    theme(plot.title = element_text(hjust = 0.5))
  
  buoymap <- map_counties_j(storm = "Ike-2008", metric = "rainfall")+
    ggtitle("Rainfall(mm) map for Hurricane Ike-2008 with buoy stations")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point(aes(x=longi, y = lati, color= buoy),size = 2)+
    #geom_label(label = buoy)+
    coord_cartesian(xlim = c(-97, -92), ylim = c(28,33))
  
  ml <-  list(trackmap,rmap, wmap, wdmap,expos,flood,tornado,buoymap)
  names(ml) <- c("trackmap","rmap","wmap", "wdmp","expos","flood","tornado","buoymap")
  
  return(ml)
}

mapps_Ike <- hmapper("Ike-2008")





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
    scale_color_manual(values = colors)+theme(axis.text.x = element_text(angle = 90))
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
    scale_color_manual(values = colors)+theme(axis.text.x = element_text(angle = 90))
}



atmp_tsp <- function(list){
  colors_atmp <- c("GRRT2" = "brown1", "42043" = "aquamarine2", "42035" = "darkorange", "EPTT2" = "blue", "MGPT2" =  "blueviolet", "CLLT2" = "brown")
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(ATMP),color = "GRRT2"), alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(ATMP),color="42043"), alpha = 0.4)+
    #geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(ATMP),color="GNJT2"),alpha = 0.5)+
    geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(ATMP),color="42035"),alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(ATMP),color="EPTT2"),alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(ATMP),color="MGPT2"),alpha = 0.8)+
    geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(ATMP),color="CLLT2"),alpha = 0.9)+
    #geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(ATMP),color="RLOT2"),alpha = 1)+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ggtitle("Air temperature time series plot for Ike-2008 at different buoy")+
    labs(x = "Date and time",
         y = "Air temperature(C)",
         color = "Buoy station") +
    scale_color_manual(values = colors_atmp)+theme(axis.text.x = element_text(angle = 90))
}


press_tsp <- function(list){
  color_press <- c("GRRT2" = "brown1", "42043" = "aquamarine2", "GNJT2" = "deeppink", "EPTT2" = "blue", "MGPT2" =  "blueviolet")
  ggplot() +
    geom_line(data = data.frame(list[1]), aes(datetime, as.numeric(PRES),color = "GRRT2"), alpha = 0.3) +
    geom_line(data = data.frame(list[2]), aes(datetime, as.numeric(PRES),color="42043"), alpha = 0.4)+
    geom_line(data = data.frame(list[3]), aes(datetime, as.numeric(PRES),color="GNJT2"),alpha = 0.5)+
    #geom_line(data = data.frame(list[4]), aes(datetime, as.numeric(PRES),color="42035"),alpha = 0.6)+
    geom_line(data = data.frame(list[5]), aes(datetime, as.numeric(PRES),color="EPTT2"),alpha = 0.7)+
    geom_line(data = data.frame(list[6]), aes(datetime, as.numeric(PRES),color="MGPT2"),alpha = 0.8)+
    #geom_line(data = data.frame(list[7]), aes(datetime, as.numeric(PRES),color="CLLT2"),alpha = 0.9)+
    #geom_line(data = data.frame(list[8]), aes(datetime, as.numeric(PRES),color="RLOT2"),alpha = 1)+
    scale_x_datetime(date_breaks = "12 hours", labels = date_format("%Y-%m-%d %H:%M"))+
    ggtitle("Sea level pressure time series plot for Ike-2008 at different buoy")+
    labs(x = "Date and time",
         y = "Sea level pressure (hPa)",
         color = "Buoy station") +
    scale_color_manual(values = color_press)+theme(axis.text.x = element_text(angle = 90))
}






