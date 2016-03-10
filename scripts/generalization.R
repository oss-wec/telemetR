dat <- read.csv('data/mink.csv')
df <- dat[, c(3, 4, 5, 10)]
names(df) <- c('timestamp', 'long_x', 'lat_y', 'ndowid')

dm <- DeviceMapping(df)

df <- 6, 11, 7 , 13
df <- df[df$ndowid %in% c(6, 11, 7), ]

coord_conv <- function(df, conversion = 'utm') {
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  conv <- SpatialPoints(cbind(as.numeric(df$long_x), as.numeric(df$lat_y)),
                        proj4string = CRS('+proj=longlat'))
  conv <- as.data.frame(spTransform(conv, CRS('+proj=utm +zone=16')))
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}

get_ud <- function(ud, percent) {
  ud_list <- list(length(percent))
  for (i in seq_along(percent)) {
    ctr <- getverticeshr(x = ud, percent = percent[i])
    ctr@proj4string <- CRS('+proj=utm +zone=16')
    ctr <- spTransform(ctr, CRS('+proj=longlat'))
    ctr <- geojson_list(ctr)
    ud_list[[i]] <- ctr
  }
  geojson <- geojson_json(Reduce(`+`, ud_list))
  return(geojson)
}

get_mud <- function(ud) {
  gjm_list <- list()
  print(paste('n elements', length(ud)))
  print('entering for loop')
  for (i in 1:length(ud)) {
    print(paste('before ud', i, 'NDOWID', names(ud)[i]))
    gj <- get_ud(ud[[i]], c(50, 70, 90))
    print(paste('before list assignment', i))
    gjm_list[[i]] <- gj
  }
  print('exit loop')
  names(gjm_list) <- names(ud)
  return(gjm_list)
}

df <- coord_conv(as.data.table(df))
kd <- df
coordinates(kd) <- kd[, .(x, y)]
kd@proj4string <- CRS('+proj=utm +zone=16')
kd <- kernelUD(kd[, 4])
image(kd)
plot(getverticeshr(kd[[1]], 95), add = T)

hr <- get_mud(kd)

DeviceMapping_geojson(dm, hr)

leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = df$long_x, lat = df$lat_y) %>% 
  addGeoJSON(hr[1])
