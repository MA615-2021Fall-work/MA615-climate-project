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
  hurri1[[i]] <- hurri1[[i]][hurri1[[i]][,3] %in% c("11","12","13","14","15"),]
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
IKE_landfall_county <- subset(county_centers, fips == 48167)
IKE_closest_dist <- subset(closest_dist, storm_id == "Ike-2008")
IKE_storm_winds <- subset(storm_winds, storm_id == "Ike-2008")
IKE_storm_events <- data.frame(storm_events$`Ike-2008`)
IKE_ext_tracks_wind <- subset(ext_tracks_wind, storm_id == "Ike-2008")




