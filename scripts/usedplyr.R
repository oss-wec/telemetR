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

dat <- read_csv('Collars.csv', n_max = 3000)
dat$rowno <- 1:nrow(dat)
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

# HOMERANGE ESTIMATION
estHomeRange <- function(df, xy, id, method, CRSin = '+init=epsg:26911') {
  coords <- data.frame(df[, xy])
  sdf <- SpatialPointsDataFrame(coordinates(coords),
                                data = data.frame(df),
                                proj4string = CRS(CRSin))
  if (method == 'mcp') {
    hr <- mcp(sdf[, id], percent = 95)
  } else if (method == 'kd') {
    hr <- kernelUD(sdf[, id])
  } else if (method == 'bb') {
    hr <- as.ltraj(coords, date = df$timestamp, id = df[, id])
    sig1 <- liker(hr, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
    sig1 <- unlist(lapply(1:length(sig1), function(i) sig1[[i]]$sig1))
    hr <- kernelbb(hr, sig1, 40, grid = 100, byburst = TRUE)
  }
  return(hr)
}

hr <- estHomeRange(df, xy = c('x', 'y'), id = 'ndowid', 'mcp')
plot(hr)
hr <- estHomeRange(df, xy = c('x', 'y'), id = 'ndowid', 'kd')
image(hr)
hr <- estHomeRange(df, xy = c('x', 'y'), id = 'ndowid', 'bb')
image(hr[[1]])

getContours <- function(ud, pct) {
  ids <- as.character(pct)
  x <- getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z
  
  cl <- lapply(pct, function(x) { 
    contourLines(x = xyl$x, y = xyl$y, z, nlevels = 1, levels = x)
  })
  
  plys <- lapply(seq_along(cl), function(i) {
    Polygons(lapply(seq_along(cl[[i]]), function(j) {
      m <- cl[[i]][[j]]
      ply <- cbind(m$x, m$y)
      ply <- rbind(ply, ply[1, ])
      Polygon(ply)
    }), ID = ids[i])
  })
  
  plys <- lapply(plys, function(x) checkPolygonsHoles(x))
  splys <- SpatialPolygons(plys)
  dff <- data.frame(id = ids)
  row.names(dff) <- ids
  spdf <- SpatialPolygonsDataFrame(splys, dff)
  return(spdf)
}

factor(df$ndowid)
df <- xyConv(dat)
df <- filter(df, ndowid == 896)
coords <- data.frame(df[, c('x', 'y')])
hr <- as.ltraj(coords, date = df$timestamp, id = df$ndowid)
hr
plot(hr)
sig <- liker(hr, sig2 = 40, rangesig1 = c(1, 10))
bb <- kernelbb(hr, sig1 = 3.28, sig2 = 40, same4all = FALSE, grid = 100)
image(bb)
plot(getverticeshr(bb, 95), add = T)
plot(hr)
