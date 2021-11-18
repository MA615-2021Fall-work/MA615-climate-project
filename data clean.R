library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(weathermetrics)
library(scales)
library(sp)
library(gstat)

GRRT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=eptt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
T_42043<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=42043h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
GNJT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=gnjt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
T_42035<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=42035h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
EPTT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=eptt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
MGPT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=mgpt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
CLLT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=cllt2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
RLOT2<- read.csv("https://www.ndbc.noaa.gov/view_text_file.php?filename=rlot2h2008.txt.gz&dir=data/historical/stdmet/", sep = "")
# GRRT2 total 1107obs and collected every 6 minutes
# T_42043 only have 72obs from 9/11-9/12 every half an hour
# GNJT2 only have 489obs from 9/11-9/13 3:30 am every 6 minutes
# T_42035 only have 119obs from 9/11-9/15 every hour
# EPTT2 total 1107obs and collected every 6 minutes 
# MGPT2 only have 523obs from 9/11-9/13 7:12 am every 6 minutes
# CLLT2 only have 48obs from 9/11-9/13 6:00 am every hour
# RLOT2 only have 55obs from 9/11-9/13 6:00 am every hour

# put them into a list
hurri1 <-list(GRRT2,T_42043, GNJT2, T_42035, EPTT2, MGPT2, CLLT2,RLOT2)

# wrangle our data sets by choosing the data between 9.11 and 9.15, dropping unrecorded value,
# replacing the data format to a data time column.
for (i in 1:length(hurri1)) {
  hurri1[[i]]$"buoy" <- rep(c("GRRT2","42043","GNJT2","42035","EPTT2","MGPT2","CLLT2","RLOT2")[i], nrow(hurri1[[i]]))
  hurri1[[i]] <- filter(hurri1[[i]], MM == '09',
                        as.numeric(DD) %in% c(11:15), WSPD != "99.0",
                        WSPD != "0.0", GST != "99.0")
  hurri1[[i]] <- unite(hurri1[[i]], "date", X.YY:DD, sep = "-", na.rm = TRUE, remove = TRUE)
  hurri1[[i]] <- unite(hurri1[[i]], "time", hh:mm, sep = ":", na.rm = TRUE, remove = TRUE)
  hurri1[[i]] <- select(hurri1[[i]], date, time, WSPD, GST, ATMP,PRES)
  hurri1[[i]]$datetime <- as.POSIXct(paste(hurri1[[i]]$date, hurri1[[i]]$time),
                                     format = "%Y-%m-%d %H:%M",tz="UTC")
}


# get our Ike data from the exposure package:
IKE_rain <- subset(rain, storm_id == "Ike-2008")
IKE_hurr_tracks <- subset(hurr_tracks,storm_id == "Ike-2008")
#IKE_landfall_county <- subset(county_centers, fips == 48167)
IKE_closest_dist <- subset(closest_dist, storm_id == "Ike-2008")
IKE_storm_winds <- subset(storm_winds, storm_id == "Ike-2008")
IKE_storm_events <- data.frame(storm_events$`Ike-2008`)
IKE_ext_tracks_wind <- subset(ext_tracks_wind, storm_id == "Ike-2008")

Ike_wind_vgm_df <- left_join(IKE_storm_winds, county_centers, by = "fips")
Ike_rain_vgm_df <- left_join(IKE_rain, county_centers, by = "fips")

# function that returns our list of data sets
get_result <- function() {
  return(hurri1)
}

