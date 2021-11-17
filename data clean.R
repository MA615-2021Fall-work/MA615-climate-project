library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)
library(scales)

GRRT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=eptt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
T_42043<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=42043h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
GNJT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=gnjt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
T_42035<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=42035h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
EPTT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=eptt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")

MGPT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=mgpt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
CLLT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=cllt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
# GTOT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=gtot2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
RLOT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=rlot2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")

hurri1 <-list(GRRT2,T_42043, GNJT2, T_42035, EPTT2, MGPT2, CLLT2,RLOT2)
for (i in 1:8) {
  hurri1[[i]] <- hurri1[[i]][hurri1[[i]][,2] == "09",]
}

for (i in 1:8) {
  hurri1[[i]] <- hurri1[[i]][hurri1[[i]][,3] %in% c("11","12","13","14","15"),] %>%
    unite("date", X.YY:DD, sep = "-", na.rm = TRUE, remove = TRUE) %>%
    unite("time", hh:mm, sep = ":", na.rm = TRUE, remove = TRUE)
}



GRRT2_IKE <- data.frame(hurri1[1]) # total 1107obs and collected every 6 minutes
T_42043_IKE <- data.frame(hurri1[2]) # only have 72obs from 9/11-9/12 every half an hour
GNJT2_IKE <- data.frame(hurri1[3]) # only have 489obs from 9/11-9/13 3:30 am every 6 minutes
T_42035_IKE <- data.frame(hurri1[4]) # only have 119obs from 9/11-9/15 every hour
EPTT2_IKE <- data.frame(hurri1[5]) # total 1107obs and collected every 6 minutes 
MGPT2_IKE <- data.frame(hurri1[6]) # only have 523obs from 9/11-9/13 7:12 am every 6 minutes
CLLT2_IKE <- data.frame(hurri1[7]) # only have 48obs from 9/11-9/13 6:00 am every hour
RLOT2_IKE <- data.frame(hurri1[8]) # only have 55obs from 9/11-9/13 6:00 am every hour


# avaiable data from exposure package:
IKE_rain <- subset(rain, storm_id == "Ike-2008")
IKE_hurr_tracks <- subset(hurr_tracks,storm_id == "Ike-2008")
#IKE_landfall_county <- subset(county_centers, fips == 48167)
IKE_closest_dist <- subset(closest_dist, storm_id == "Ike-2008")
IKE_storm_winds <- subset(storm_winds, storm_id == "Ike-2008")
IKE_storm_events <- data.frame(storm_events$`Ike-2008`)
IKE_ext_tracks_wind <- subset(ext_tracks_wind, storm_id == "Ike-2008")

# extract dataset from exposure package for variogram:

Ike_wind_vgm_df <- left_join(IKE_storm_winds, county_centers, by = "fips")
Ike_rain_vgm_df <- left_join(IKE_rain, county_centers, by = "fips")

head(Ike_wind_vgm_df)
head(Ike_rain_vgm_df)


# clean extracted data from buoy for time series:
GRRT2_IKE_clean <- subset(GRRT2_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
GRRT2_IKE_clean$datetime <- as.POSIXct(paste(GRRT2_IKE_clean$date, GRRT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

T_42043_IKE_clean <- subset(T_42043_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
T_42043_IKE_clean$datetime <- as.POSIXct(paste(T_42043_IKE_clean$date, T_42043_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

GNJT2_IKE_clean <- subset(GNJT2_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
GNJT2_IKE_clean$datetime <- as.POSIXct(paste(GNJT2_IKE_clean$date, GNJT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

T_42035_IKE_clean <- subset(T_42035_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
T_42035_IKE_clean$datetime <- as.POSIXct(paste(T_42035_IKE_clean$date, T_42035_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

EPTT2_IKE_clean <- subset(EPTT2_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
EPTT2_IKE_clean$datetime <- as.POSIXct(paste(EPTT2_IKE_clean$date, EPTT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

MGPT2_IKE_clean <- subset(MGPT2_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
MGPT2_IKE_clean$datetime <- as.POSIXct(paste(MGPT2_IKE_clean$date, MGPT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC")

CLLT2_IKE_clean <- subset(CLLT2_IKE, WSPD != "99.0" & WSPD != "0.0" & GST != "99.0" & ATMP != "999.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
CLLT2_IKE_clean$datetime <- as.POSIXct(paste(CLLT2_IKE_clean$date, CLLT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC") # no WDIR data

RLOT2_IKE_clean <- subset(RLOT2_IKE, WSPD != "99.0" & WSPD != "0.0" & WDIR != "999" & GST != "99.0",select = c(date,time,WSPD,WDIR,GST,ATMP))
RLOT2_IKE_clean$datetime <- as.POSIXct(paste(RLOT2_IKE_clean$date, RLOT2_IKE_clean$time), format = "%Y-%m-%d %H:%M",tz="UTC") # no ATMP data

clean_buoy_list <- list(GRRT2_IKE_clean, T_42043_IKE_clean,GNJT2_IKE_clean,T_42035_IKE_clean,EPTT2_IKE_clean,MGPT2_IKE_clean,CLLT2_IKE_clean,RLOT2_IKE_clean)


