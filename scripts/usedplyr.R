library(readr)
library(dplyr)
library(lubridate)
library(sp)
library(leaflet)
library(adehabitatHR)

color_pal <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6", 
               "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99", 
               "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", 
               "#5574A6", "#3B3EAC")

dat <- read_csv('Collars.csv', n_max = 1000)
dat$rowno <- 1:nrow(dat)
dat$ndowid[50:100] <- 2
dat_animal <- read_csv('Animals.csv')

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  df_lines <- df[, .SD[c(seq(1, .N, 20), .N)], by = ndowid]
  unq_id <- unique(df[, ndowid])
  pal <- rep_len(color_pal, length(unq_id))
  map <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap",
                                        options = providerTileOptions(attribution = NA))
  
  for (i in 1:length(unq_id)) {
    d <- df_lines[ndowid == unq_id[i]]
    dp <- d[, .SD[c(1, .N)]]
    
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2, color = pal[i], opacity = .4)
    map <- addCircleMarkers(map, lng = dp$long_x, lat = dp$lat_y,
                            stroke = FALSE, radius = 4, color = pal[i],
                            fillOpacity = 1,
                            popup = paste("NDOW ID:", unq_id[i]))
  }
  return(map)
}

df <- dat %>% group_by(ndowid, as_date(timestamp)) %>% slice(1)

CollarMap_dplyr <- function(dataframe) {
  df <- dataframe %>% 
    filter(!(is.na(long_x) | is.na(lat_y))) %>% 
    arrange(ndowid, timestamp) %>% 
    group_by(ndowid, as_date(timestamp)) %>% 
    slice(1) %>% 
    ungroup()
  ids <- unique(df$ndowid)
  pal <- pal <- rep_len(color_pal, length(ids))

  map <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(attribution = NA))
  for (i in seq_along(ids)) {
    d <- df %>% filter(ndowid == ids[i])
    dp <- d[c(1, nrow(d)), ]
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2,
                        color = pal[i],
                        opacity = .4) %>% 
           addCircleMarkers(map, lng = dp$long_x, lat = dp$lat_y,
                            stroke = FALSE,
                            radius = 4,
                            color = pal[i],
                            fillOpacity = 1)
  }
  return(map)
}
CollarMap_dplyr(dat)


# PAGE 2
## move_df
move_df <- eventReactive(input$ac_UpdateMap, {
  df <- coord_conv(df_subset())
  df[, ':=' (dist = move.dist(x, y),
             R2n = move.r2n(x, y),
             mth = month(timestamp),
             hr = hour(timestamp),
             dt = move.dt(timestamp)), by = ndowid]
  df[, ':=' (sig.dist = cumsum(dist),
             speed = move.speed(dist, dt)), by = ndowid]
  #p <- movement_eda(df, plot_var = input$y.input, type = input$fig.type)
  #return(list(df, p))
  return(df)
})

### STEP 1: coordinate conversion
xyConv <- function(df, xy = c('long_x', 'lat_y'), CRSin = '+proj=longlat',
                   CRSout = '+proj=utm +zone=11') {
  df <- df[complete.cases(df[, xy]), ]
  conv <- SpatialPoints(coordinates(cbind('x' = df[, xy[1]],
                                          'y' = df[, xy[2]])),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  
  return(df)
}
df <- xyConv(dat)

### STEP 2: move_params
moveDist <- function(x, y) {
  dist <- c(0, sqrt((x[-1] - x[-length(x)])**2 + 
                      (y[-1] - y[-length(y)])**2))
  return(dist) # same unit as input (meters)
}
moveNSD <- function(x, y) {
  r2n <- (x - x[1])**2 + (y - y[1])**2
  r2n <- (r2n - min(r2n)) / (max(r2n) - min(r2n))
  return(r2n)
}
moveDt <- function(time) {
  dt <- c(0, unclass(time[-1]) - unclass(time[-length(time)]))
  return(dt / 3600) # seconds
}
moveSpeed <- function(dist, time) {
  speed <- (dist / 1000) / (time)
  speed[is.nan(speed)] <- 0
  return(speed)
}

move <- df %>% 
  arrange(ndowid, timestamp) %>% 
  group_by(ndowid) %>% 
  mutate(distance = moveDist(x, y),
         sigDist = cumsum(distance),
         NSD = moveNSD(x, y),
         dtime = moveDt(timestamp),
         Speed = moveSpeed(distance, dtime),
         yr = year(timestamp),
         mth = month(timestamp),
         dy = day(timestamp),
         hr = hour(timestamp)) %>% 
  ungroup()
         
# map points
mapPoints <- function(map, df) {
  ids <- unique(df$ndowid)
  pal <- rep_len(color_pal, length(ids))
  layers <- list()
  
  for(i in seq_along(ids)) {
    dat <- df %>% filter(ndowid == ids[i])
    map <- addPolylines(map, lng = dat$long_x, lat = dat$lat_y,
                        group = as.character(ids[i]),
                        color = pal[i], weight = 1) 
    map <- addCircleMarkers(map, lng = dat$long_x, lat = dat$lat_y,
                            group = as.character(ids[i]), color = pal[i],
                            radius = 3, stroke = FALSE,fillOpacity = .3,
                            popup = paste(sep = "<br>",
                                          paste("<b>NDOW ID:</b> ", ids[i]),
                                          paste("<b>timestamp:</b> ", dat$timestamp),
                                          paste("<b>LocID</b>: ", dat$locid)))
    layers <- c(layers, as.character(ids[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}

mp <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap')
mapPoints(mp, move)
df <- dat %>% filter(ndowid == 292)
# HOMERANGE ESTIMATION
estHomeRange <- function(df, xy, id, method) {
  coords <- data.frame(df[, xy])
  sdf <- SpatialPointsDataFrame(coordinates(coords),
                                data = data.frame(df))
  if (method == 'mcp') {
    hr <- mcp(sdf[, id], percent = 95)
  } else if (method == 'kd') {
    hr <- kernelUD(sdf[, id])
  } else if (method == 'bb') {
    hr <- as.ltraj(data.frame(df[, c('x', 'y')]), date = df$timestamp, id = df[, id])
    sig1 <- liker(hr, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
    hr <- kernelbb(hr, sig1[[1]]$sig1, 40, grid = 100, byburst = TRUE)
  }
  return(hr)
}

pp <- estHomeRange(move, c('long_x', 'lat_y'), 'ndowid', 'kd')
image(pp)
pp <- estHomeRange(move, c('long_x', 'lat_y'), 'ndowid', 'mcp')
plot(pp)
dat <- xyConv(dat)
pp <- estHomeRange(df, c('long_x', 'lat_y'), 'ndowid', 'bb')  
image(pp)
plot(getverticeshr(pp, 50), add = T)
summary(bsig)

hr <- as.ltraj(data.frame(dat[, c('x', 'y')]), date = dat$timestamp, id = dat[, 'ndowid'])
sig1 <- liker(hr, sig2 = 40, rangesig1 = c(0, 10), plotit = T)
hr <- kernelbb(hr, c(3.4, 6.1), 40, grid = 100, byburst = TRUE, same4all = F)
image(hr)
image(hr[[1]])
plot(getverticeshr(hr[[1]], 95), add = T)
