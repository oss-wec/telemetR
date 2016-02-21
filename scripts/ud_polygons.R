get_ud <- function(ud, percent) {
  ud_list <- list(length(percent))
  for (i in seq_along(percent)) {
    ctr <- getverticeshr(x = ud, percent = percent[i])
    ctr@proj4string <- CRS('+proj=longlat')
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

# c(1141, 1142) works
df <- dat[ndowid %in% c(1132, 1135, 1142, 1141), ]
d <- df
coordinates(df) <- df[, .(long_x, lat_y)]
df@proj4string <- CRS('+proj=longlat')
kd <- kernelUD(df[, 2], h = 'href', same4all = T, grid = 100)
dm <- DeviceMapping(d)
gj <- get_mud(kd)
DeviceMapping_geojson(dm, gj)

seq_along(kd)
kd[[seq_along(kd)[1]]]
