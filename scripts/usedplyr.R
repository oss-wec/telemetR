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
    hr <- kernelbb(hr, sig1[[1]]$sig1, 40, grid = 100, byburst = TRUE)
  }
  return(hr)
}

hr <- estHomeRange(df, xy = c('x', 'y'), id = 'ndowid', 'mcp')
plot(hr)
hr <- estHomeRange(df, xy = c('x', 'y'), id = 'ndowid', 'kd')
image(hr)
x <- lapply(hr, function(x) getContours(x, c(50, 90)))
plot(x[[1]])


df <- xyConv(dat)
move <- df %>%
  group_by(ndowid) %>%
  mutate(Distance = moveDist(x, y),
         sigDist = cumsum(Distance),
         NSD = moveNSD(x, y),
         dTime = moveDt(timestamp),
         Speed = moveSpeed(Distance, dTime),
         Year = year(timestamp),
         Month = month(timestamp),
         Day = day(timestamp),
         Hour = hour(timestamp)) %>%
  ungroup()
