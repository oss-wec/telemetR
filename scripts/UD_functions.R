get_ud <- function(ud, percent) {
  ud_list <- list(length(percent))
  for (i in seq_along(percent)) {
    ctr <- getverticeshr(x = ud, percent = percent[i])
    ctr@proj4string <- CRS('+proj=utm +zone=11')
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
    gj <- get_ud(ud[[i]], c(50, 75, 95))
    print(paste('before list assignment', i))
    gjm_list[[i]] <- gj
  }
  print('exit loop')
  names(gjm_list) <- names(ud)
  return(gjm_list)
}

## reading in data
dat <- fread('Collars.csv')
df <- dat[ndowid == 1132, ]
d <- df

## creating spatialpointsdataframe for input into kernelUD function
d <- coord_conv(d)
coordinates(d) <- d[, .(x, y)]
d@proj4string <- CRS('+proj=utm +zone=11')

## kernel density estimation
kd <- kernelUD(d[, 2])

## visualization
image(kd)
plot(getverticeshr(kd, 95), add = T)

## getting contours for leaflet map
ud <- get_ud(kd, id_list)
mud <- get_mud(kd, id_list)
## plotting in leaflet
leaflet() %>% addTiles() %>% 
  addGeoJSON(ud, weight = 1) %>% 
  addCircleMarkers(lng = df$long_x, lat = df$lat_y,
                   stroke = F, radius = 2)


## testing getting multiple UDs from multiple individuals
dat <- fread('Collars.csv')
df <- dat[ndowid %in% c(1141), ]
d <- df

## creating spatialpointsdataframe
d <- coord_conv(d)
coordinates(d) <- d[, .(x, y)]
d@proj4string <- CRS('+proj=utm +zone=11')

## kernel density estimation
kd <- kernelUD(d[, 2])

## visualization
image(kd)
image(kd[[2]])
plot(getverticeshr(kd[[2]], 90), add = T)

## getting contours for leaflet map
ud <- get_mud(kd)

## plotting in leaflet with leaflet functions
dm <- DeviceMapping(df)
DeviceMapping_geojson(dm, ud)


## browian bridge
df <- dat[ndowid == 1576, ]
d <- coord_conv(df)
coordinates(d) <- d[, .(x, y)]
d@proj4string <- CRS('+proj=utm +zone=11')
traj <- to_ltraj(d)
bb <- estimate_bbmm(traj)
ud <- get_ud(bb, c(75, 95))
dm <- DeviceMapping(df)
DeviceMapping_geojson(dm, ud)
