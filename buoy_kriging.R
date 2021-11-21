library(dplyr)
library(gstat)
library(base)
library(geoR)
library(ggplot2)
library(scales)

source("data clean.R")

hurri1_origin <-list(GRRT2,T_42043, GNJT2, T_42035, EPTT2, MGPT2, CLLT2,RLOT2)

for (i in 1:8) {
  hurri1_origin[[i]] <- hurri1_origin[[i]]%>%
    filter(MM %in% "09")%>%
    filter(as.numeric(DD) %in% as.character(11:15))
}

for (i in 1:8) {
  hurri1_origin[[i]]$"buoy" <- rep(c("GRRT2","42043","GNJT2","42035","EPTT2","MGPT2","CLLT2","RLOT2")[i], nrow(hurri1_origin[[i]]))
}

for (i in 1:8) {
  hurri1_origin[[i]] <- apply(hurri1_origin[[i]], 2, as.character)
}
names(hurri1_origin) <- c("GRRT2","42043","GNJT2","42035","EPTT2","MGPT2","CLLT2","RLOT2")

location <- data.frame("buoy" = c("GRRT2","42043","GNJT2","42035","EPTT2","MGPT2","CLLT2","RLOT2"), "lati" = c(29.302,28.982,29.357,29.232,29.481,29.682,29.563,29.515), "longi" = c(94.896,94.899,94.725,94.413,94.917,94.985,95.67,94.513))
location$longi <- -(location$longi)
hurri2 <- hurri1_origin%>%
  purrr::map(~as.data.frame(.x))%>% dplyr::bind_rows()
hurri2[,6:18] <- apply(hurri2[,6:18], 2, as.numeric)

for (i in 6:ncol(hurri2)-1) {
  hurri2[,i][hurri2[,i] %in% c(0, 99, 999)] <- NA
}

hurri2 <- hurri2[, c(1:5,7:8,ncol(hurri2))]
hurri2 <- na.omit(hurri2) 

hurri3 <- hurri2%>%
  group_by(buoy)%>%
  summarise(mean_ws = max(as.numeric(WSPD)), na.rm = T)

hurri3 <- merge(location, hurri3, by = "buoy") 

coordinates(hurri3) <- ~ longi + lati

semivariog <- variogram((mean_ws) ~ 1, data=na.omit(hurri3), width =0.04)
v_fit_buoy <- fit.variogram(semivariog, vgm("Gau"))


pred_grid <- expand.grid(seq(ceiling(max(location$longi)), floor(min(location$longi)), by = -0.01), seq(ceiling(max(location$lati)), floor(min(location$lati)), by = -0.01))

colnames(pred_grid) <- c("longtitude", "latitude")
coordinates(pred_grid) <-  ~ longtitude + latitude
krig_buoy<-krige(formula= mean_ws ~ 1, hurri3, pred_grid, model=v_fit_buoy)



predict_wspd <- krig_buoy %>% as.data.frame %>%
  ggplot(aes(x=longtitude, y=latitude)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  geom_point(mapping = aes(x = longi, y = lati), data = location,size=4) +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  ggtitle("Predict landfall point wind speed using buoy's data")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


tmap_mode("view")
buoy_kriging_map <- tm_shape(krig_bouy) +
  tm_bubbles(col = "var1.pred", palette = "-RdYlBu", size = .3, alpha = .5)

