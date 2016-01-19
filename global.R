library(data.table)
library(leaflet)
library(ggplot2)
library(sp)

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  df_lines <- df[, .SD[c(seq(1, .N, 20), .N)], by = ndowid]
  unq_id <- unique(df[, ndowid])
  map <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
  
  for (n in unq_id) {
    d <- df_lines[ndowid == n]
    dp <- d[, .SD[c(1, .N)]]
    
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2, color = "black", opacity = .4)
    map <- addCircleMarkers(map, lng = dp$long_x, lat = dp$lat_y,
                            stroke = FALSE, radius = 4, color = "navy",
                            fillOpacity = 1,
                            popup = paste("NDOW ID:", n))
  }
  return(map)
}

Calculate_NSD <- function(dat) {
  dat <- as.data.frame(dat)
  #dat$timestamp <- as.character(dat$timestamp)
  #dat$timestamp <- as.POSIXlt.character(dat$timestamp, 
  #                                      format = ("%m/%d/%Y %H:%M:%S %p"))
  dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
  geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                      as.numeric(dat$lat_y)),
                                proj4string = sp::CRS("+proj=longlat"))
  utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
  colnames(utmcoord) <- c("Easting", "Northing")
  dat <- cbind(dat, utmcoord)
  unq_id <- unique(dat$ndowid)
  df <- data.frame()
  for (i in unq_id) {
    x <- dat[dat$ndowid == i, ]
    x$NSD <- (x$Easting - x$Easting[1])**2 + (x$Northing - x$Northing[1])**2
    df <- rbind(df, x)
  }
  return(df)
}

Plot_NSD <- function(dataframe) {
  p <- ggplot(dataframe, aes(x = date, y = NSD, group = ndowid)) +
    geom_line(color = 'firebrick4', size = .75) +
    facet_wrap(~ndowid) +
    labs(y = 'Net Squared Displacement') +
    theme(panel.background = element_rect(fill = 'grey90'),
          plot.background = element_rect(fill = 'grey90'),
          panel.grid.major.x = element_line(color = 'grey70', size = 1, linetype = 'dotted'),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = 'grey50', size = 14),
          axis.text.x = element_text(color = 'grey50', size = 10),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}