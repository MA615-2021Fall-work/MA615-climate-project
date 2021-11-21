



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


coordinates(Ike_wind_vgm_df) <- ~longitude+latitude
v <- variogram((vmax_sust) ~ 1, data=na.omit(Ike_wind_vgm_df))
v_fit_same <- fit.variogram(v, vgm("Gau"))



lzn.dir = variogram(vmax_sust~1, Ike_wind_vgm_df, alpha = c(0, 45, 90, 135))
lzndir.fit = vgm( 50,"Gau",10, 1,anis = c(45, .4))
v_fit <- fit.variogram(lzn.dir, lzndir.fit)


pred_grid1 <- data.frame(Ike_wind_vgm_df@coords)
pred_grid2 <- expand.grid(seq(max(data.frame(Ike_wind_vgm_df@coords)[,1]),min(data.frame(Ike_wind_vgm_df@coords)[,1]),by = -1), seq(max(data.frame(Ike_wind_vgm_df@coords)[,2]),min(data.frame(Ike_wind_vgm_df@coords)[,2]),by = -1))
colnames(pred_grid2) <- c("longitude", "latitude")
coordinates(pred_grid2) <-  ~ longitude+ latitude
krig_hurri<-krige(formula= vmax_sust ~ 1, Ike_wind_vgm_df, pred_grid2, model=v_fit)

hurri_predicted <-  krig_hurri %>% as.data.frame %>%
  ggplot(aes(x=longitude, y=latitude)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "white", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() 


pred_grid1 <- data.frame(Ike_wind_vgm_df@coords)
coordinates(pred_grid1) <-  ~ longitude+ latitude
krig_hurri2<-krige(formula= vmax_sust ~ 1, Ike_wind_vgm_df, pred_grid1, model=v_fit)
proj4string(krig_hurri2) <- CRS("+init=epsg:4326")
tmap_mode("view")
hurri_map <- tm_shape(krig_hurri2) +
  tm_bubbles(col = "var1.pred", palette = "-RdYlBu", size = .3, alpha = .5)
