
library(gstat) # variogram estimation


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

# Helper functions
gaussian_variogram <- function (n, ps, r)
  function (h) n + ps * (1 - exp(-(h / r) ^ 2))

# solves `A * x = v` where `C = chol(A)` is the Cholesky factor:
chol_solve <- function (C, v) backsolve(C, backsolve(C, v, transpose = TRUE))

kriging_smooth_gaussian <- function (formula, data, ...) {
  v <- variogram(formula, data)
  v_fit <- fit.variogram(v, vgm("Gau", ...))
  v_f <- gaussian_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])

  Sigma <- v_f(as.matrix(dist(coordinates(Ike_rain_vgm_df)))) # semivariogram  data=Ike_rain_vgm_df!!!!!!
  Sigma <- sum(v_fit$psill) - Sigma # prior variance
  tau2 <- v_fit$psill[1] # residual variance
  C <- chol(tau2 * diag(nrow(Ike_rain_vgm_df)) + Sigma)
  y <- model.frame((precip_max)~1, Ike_rain_vgm_df)[, 1] # response
  x <- model.matrix((precip_max)~1, Ike_rain_vgm_df)  #formula=(precip_max)~1 ????????????????????????????????????
  # generalized least squares:
  beta <- coef(lm.fit(backsolve(C, x, transpose = TRUE),
                      backsolve(C, y, transpose = TRUE))) # prior mean

  Sigma_inv <- chol2inv(chol(Sigma))
  C <- chol(Sigma_inv + diag(nrow(Ike_rain_vgm_df)) / tau2)
  # posterior mean (smoother):
  mu <- drop(chol_solve(C, y / tau2 + Sigma_inv %*% x %*% beta))
  list(smooth = mu, prior_mean = beta)
}


# Example: Meuse river study

library(sp) # spatial point data frames
data(Ike_rain_vgm_df)
coordinates(Ike_rain_vgm_df) <- ~ longitude + latitude
proj4string(Ike_rain_vgm_df) <- CRS("+init=epsg:4326")  #28992
Ike_rain_vgm_df$response <- log(Ike_rain_vgm_df$precip_max)

library(tmap) # thematic map plotting
tmap_mode("view")
tm_shape(Ike_rain_vgm_df) +
  tm_bubbles(col = "response", palette = "-RdYlBu", size = .3, alpha = .5)


vgm_rain <- variogram(precip_max ~ 1, data=Ike_rain_vgm_df)
vgm_rain_fit <- fit.variogram(v, vgm(psill=250,model="Gau",nugget = 50,range=200))
v_f <- gaussian_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])

vv<-variogram(precip_max ~ 1, data=Ike_rain_vgm_df,alpha=c(0,45,90,135))
vv_fit<-fit.variogram(vv, model=vgm("Gau"))
vv_f <- gaussian_variogram(vv_fit$psill[1], vv_fit$psill[2], vv_fit$range[2])


#Variogram map
TheGStat<-gstat(id="precip_max",formula=precip_max~1,data=Ike_rain_vgm_df)
Thevgm=variogram(TheGStat,map=TRUE,cutoff=1500,width=300)
plot(Thevgm,threshold=10,main="Variogram Map",xlab="x",ylab="y")

