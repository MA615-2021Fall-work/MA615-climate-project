
buoymap <- function (storms, plot_object = NULL, padding = 2, plot_points = FALSE, 
          alpha = 1, color = "firebrick") 
{
  #hasData()
  if (is.null(plot_object)) {
    plot_object <- default_map()
  }
  map_dim <- apply(matrix(c(-106.65037, 25.12993, -67.00742, 
                            47.48101), byrow = TRUE, ncol = 2), MARGIN = 2, function(x) range(x) + 
                     c(-1, 1) * padding)
  tracks <- hurricaneexposuredata::hurr_tracks %>% dplyr::select(.data$latitude, 
                                                                 .data$longitude, .data$storm_id, .data$date) %>% dplyr::filter(as.character(.data$storm_id) %in% 
                                                                                                                                  storms & .data$longitude > !!map_dim[1, 1] & .data$longitude < 
                                                                                                                                  !!map_dim[2, 1] & .data$latitude > !!map_dim[1, 2] & 
                                                                                                                                  .data$latitude < !!map_dim[2, 2]) %>% dplyr::mutate(date = lubridate::ymd_hm(.data$date))
  splt_tracks <- split(tracks, tracks$storm_id)
  full_tracks <- lapply(splt_tracks, interp_track)
  full_tracks <- do.call("rbind", full_tracks)
  out <- plot_object + ggplot2::geom_path(data = full_tracks, 
                                          ggplot2::aes(x = .data$longitude, y = .data$latitude), alpha = alpha, color = color)
  if (plot_points) {
    out <- out + ggplot2::geom_point(data = tracks, ggplot2::aes(x = .data$longitude, 
                                                                 y = .data$latitude, group = .data$storm_id), alpha = alpha)
  }
  if (!("CoordMap" %in% class(out$coordinates))) {
    out <- out + ggplot2::coord_map()
  }
  return(out)
}