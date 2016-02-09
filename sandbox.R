library(data.table)
library(leaflet)
library(sp)
library(adehabitatHR)
library(ggplot2)

dat_animal <- fread("data/collared_animals.csv")
dat_animal[Species == "DBHS"]

head(dat_animal[, Collar_Date])
head(strptime(dat_animal[, Collar_Date], format = "%m/%d/%Y"))

dat_animal$Collar_Date <- as.Date(strptime(dat_animal$Collar_Date, format = "%m/%d/%Y"))
dat_animal$Fate_Date <- as.POSIXct(strptime(dat_animal$Fate_Date, format = "%m/%d/%Y"))

d <- dat_animal[dat_animal$Species == "MULD" &
                dat_animal$Collar_Date >= "2000-01-01" &
                dat_animal$Collar_Date <= "2015-10-16", ]

df <- dat[, .SD[c(1,.N)], by = ndowid]
df <- dat[, .SD[seq(1, .N, 10)], by = ndowid]
levels(as.factor(df$ndowid))

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  df <- df[order(ndowid, timestamp)]

  unq_id <- unique(df$ndowid)
  dat <- split(df, df$ndowid)
  

  m <- leaflet() %>% addTiles()
  layers <- list()
  for (i in 1:length(unq_id)) {
    terminus <- d[[i]][, .SD[c(1, .N)]] 
    
    m <- addCircleMarkers(m, lng = dat[[i]]$long_x, lat = dat[[i]]$lat_y,
                          stroke = FALSE, color = "navy", fillOpacity = .9,
                          radius = 4, group = paste(dat[[i]]$ndowid),
                          popup = paste(dat[[i]]$ndowid))
    m <- addCircleMarkers(m, lng = terminus$long_x, lat = terminus$lat_y,
                          color = "red")
    layers <- c(layers, unq_id[i]) 
  }
    m <- addLayersControl(m, overlayGroups = layers,
                          options = layersControlOptions(collapsed = F))
  return(m)
}

CollarMap(df)

terminus <- d[[1]][, .SD[c(1, .N)]]

for (i in 1:length(unq_id)) {
  print(i)
  terminus_x <- c(dat[[i]]$long_x[1], dat[[i]]$long_x[length(dat[[i]])])
  terminus_y <- c(dat[[i]]$lat_y[1], dat[[i]]$lat_y[length(dat[[i]])])
  print(terminus_x); print(terminus_y)
  #     m <- addCircleMarkers(m, lng = terminus_x, lat = terminus_y,
  #                           stroke = TRUE, color = "red", fillOpacity = .9,
  #                           radius = 4, group = paste(dat[[i]]$ndowid),
  #                           popup = paste(dat[[i]]$ndowid))
  m <- addPolylines(m, lng = dat[[i]]$long_x, lat = dat[[i]]$lat_y,
                    color = "black", weight = 2)
  layers <- c(layers, unq_id[i]) 
}
#   m <- addLayersControl(m, overlayGroups = layers,
#                         options = layersControlOptions(collapsed = F))
return(m)

##############################################
# SUBSETTING ALL COLLARS FOR WORKING EXAMPLE #
##############################################

dat <- fread("data/AllCollars.csv")
df <- dat[, .SD[seq(1, .N, 20)], by = ndowid]  #THIS IS THE DATA.TABLE WAY TO SUBSET EACH NDOWID 
dat[, .SD[c(1, .N)]]
write.csv(df, "WorkingExample2.csv")

seq(1, 10, 4)

##############################
# WORKING EXAMPLE OF THE MAP #
##############################

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  unq_id <- unique(df[, ndowid])
  
  map <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
  layer_group <- list()
  
  for (i in 1:(length(unq_id) - 1)) {
    d <- df[ndowid == unq_id[i]]
    print(i); print(unq_id[i])
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2, color = "black", opacity = 1)
    map <- addCircleMarkers(map, lng = d$long_x, lat = d$lat_y,
                            stroke = FALSE, radius = 4, color = "navy",
                            fillOpacity = 1, 
                            group = paste(unq_id[i]))
    layer_group <- c(layer_group, paste(unq_id[i]))
  }
  map <- addLayersControl(map, 
                          overlayGroups = layer_group,
                          options = layersControlOptions(collapsed = TRUE))
  return(map)
}

dat <- fread("data/WorkingExample.csv")

df_lines <- dat[, .SD[c(seq(1, .N, 20), .N)], by = ndowid]
df_points <- df_lines[, .SD[c(1, .N)], by = ndowid]

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  df_lines <- df[, .SD[c(seq(1, .N, 20), .N)], by = ndowid]
  unq_id <- unique(df[, ndowid])
  
  map <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
  
  for (i in 1:(length(unq_id) - 1)) {
    d <- df_lines[ndowid == unq_id[i]]
    dp <- d[, .SD[c(1, .N)]]
    
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2, color = "black", opacity = .4)
    map <- addCircleMarkers(map, lng = dp$long_x, lat = dp$lat_y,
                            stroke = FALSE, radius = 4, color = "navy",
                            fillOpacity = 1)
  }
  return(map)
}
CollarMap(dat)

### NEW DATA INCLUDED SPECIES AND MANAGEMENT AREA

df <- fread("data/WorkingExample2.csv")


#######################################
# USING TEXT INPUT TO GENERATE A LIST #
#######################################

test <- "831, 3669"
tstrsp <- strsplit(test, ", ")
t <- tstrsp[[1]]
class(t)
tnum <- as.numeric(t)
tnum

t.df <- df[ndowid %in% tnum, ]

#######################################
# TESTING CollarMap WITH SUBSET, %in% #
#######################################

test <- "3669, 831"
test.spl <- strsplit(test, ", ")
test.spl <- lapply(test.spl, as.numeric)
test.spl <- test.spl[[1]]
t.df <- df[ndowid %in% test.spl, ]

CollarMap(t.df)

#############################
# USING ALL THE COLLAR DATA #
#############################

dat <- fread("data/AllCollars (2).csv")

##############################
# TESTING MIGRATION ANALYSIS #
##############################

#TESTING FUNCTION
dat <- read.csv("CollarData.csv")
nsd <- Calculate_NSD(dat)
plot(nsd$date, nsd$NSD)

dat$timestamp <- as.character(dat$timestamp)
dat$timestamp <- as.POSIXlt.character(dat$timestamp, 
                                      format = ("%m/%d/%Y %H:%M:%S %p"))

#PLOTTING NDS
ggplot2::ggplot(nsd, ggplot2::aes(timestamp, NSD)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()


Plot_NSD <- function(dataframe) {
  unq_id <- unique(dataframe$ndowid)
  p <- ggplot2::ggplot(nsd, ggplot2::aes(x = timestamp, y = NSD)) +
    ggplot2::geom_line(color = 'red') +
    ggplot2::theme_bw()
  return(p)
}

Plot_NSD(nsd)

#TESTING MULTIPLE ANIMALS
dat <- read.csv("CollarData (2).csv")
nsd <- Calculate_NSD(dat)
Plot_NSD(nsd)

#PRETTY PLOT
ggplot(nsd, aes(x = timestamp, y = NSD, group = ndowid)) +
  geom_line(color = 'firebrick4', size = .75) +
  theme_fivethirtyeight()

p <- ggplot(dataframe, aes(x = timestamp, y = NSD, group = ndowid)) +
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

Plot_NSD <- function(dataframe) {
  p <- ggplot(dataframe, aes(x = timestamp, y = NSD, group = ndowid)) +
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

Plot_NSD(nsd)

#TRYING TO AVERAGE HERD NSD
df <- nsd[, c(3, 11, 14)]
df$month <- month(df$date)
df$day <- day(df$date)
df$year <- year(df$date)

df_gb <- df %>% 
  group_by(date) %>% 
  summarize(m_nsd = mean(NSD, na.rm = TRUE))

ggplot(df_gb, aes(x = as.character(date), y = m_nsd)) +
  geom_line()


# RUNNING WITH DATA FROM APP. APP IS ERRORING FOR DATA.TABLE REASONS??
dat <- read.csv("CollarData (3).csv")

##############################
# USING HTML WIDGETS FOR NSD #
##############################
library(metricsgraphics)
library(htmltools)
library(htmlwidgets)
source('https://raw.githubusercontent.com/kissmygritts/NDOW_NBGDS/master/Calculate_NSD.R')

dat <- read.csv('CollarData.csv')
dat.nsd <- Calculate_NSD(dat)

tmp <- dat.nsd[, .(date = as.Date(date), NSD)]

tmp %>% 
  mjs_plot(x = date, y = NSD) %>% 
  mjs_line(color = "red") %>% 
  mjs_axis_x(xax_format = "date") %>% 
  mjs_axis_y(show = FALSE) %>% 
  mjs_labs(y_label = "Net Square Displacement")
  
# MULTIPLE NDOWIDS

dat <- read.csv('CollarData (2).csv')
levels(as.factor(dat[, 'ndowid']))
dat <- Calculate_NSD(dat)
levels(as.factor(dat[, ndowid]))

plot_nsd_mjs <- function(dat) {
  # add nsd check here, if false calculate nsd
  dat <- as.data.table(dat, .(ndowid, date, NSD))
  dat[, date := as.Date(date)]
  unq <- unique(dat[, ndowid])
  lapply(unq, function(x) {
    mjs_plot(dat[ndowid == x], x = date, y = NSD, 
             width = 900, height = 400) %>% 
      mjs_line() %>% 
      mjs_axis_x(xax_format = "date") %>% 
      mjs_labs(x_label = paste("NDOW ID: ", x))
  }) -> mjs_plots
  return(mjs_plots)
}

p <- plot_nsd_mjs(dat)

html_print(mjs_grid(p, nrow = 5, ncol = 1))

dat[, date := as.Date(date)]

################
# adehabitatLT #
################
library(adehabitatLT)
library(adehabitatHR)
dat <- read.csv("CollarData.csv")
dat$timestamp <- as.POSIXct(dat$timestamp)

dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = sp::CRS("+proj=longlat"))
utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
colnames(utmcoord) <- c("Easting", "Northing")
dat <- cbind(dat, utmcoord)
rm(utmcoord); rm(geocoord)

traj <- as.ltraj(dat[, 12:13], date = dat$timestamp, id = dat$ndowid)
df.traj <- traj[[1]]

plot(traj)

## brownian bridge in adehabitatHR
bb.traj <- kernelbb(traj, sig1 = 6.23, sig2 = 58, grid = 50)
bb.traj

# merging utilization polygons
ud50 <- getverticeshr(bb.traj, 50)
ud90 <- getverticeshr(bb.traj, 90)
conts <- rbind(ud50, ud90, makeUniqueIDs = T)
conts@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
conts.latlong <- spTransform(conts, CRS("+proj=longlat"))

leaflet(conts.latlong) %>% addProviderTiles(("Esri.WorldImagery")) %>% 
  addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4) %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)

# plotting on leaflet map.
ud50@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
ud50.latlong <- spTransform(ud50, CRS("+proj=longlat"))
ud90@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
ud90.latlong <- spTransform(ud90, CRS("+proj=longlat"))

# teasing out multipolygon sp::SPDFs for leaflet mapping
ud50.latlong@polygons[[1]]@Polygons[[1]]@coords

leaflet(ud50.latlong) %>% addProviderTiles(("Esri.WorldTerrain")) %>% addPolygons()

leaflet() %>% addProviderTiles(("Esri.WorldTopoMap")) %>% 
  addPolygons(lat <- ud50.latlong@polygons[[1]]@Polygons[[1]]@coords[, 1],
              lng <- ud50.latlong@polygons[[1]]@Polygons[[1]]@coords[, 2], stroke = F, color = "navy") %>% 
  addPolygons(lat <- ud50.latlong@polygons[[1]]@Polygons[[2]]@coords[, 1],
              lng <- ud50.latlong@polygons[[1]]@Polygons[[2]]@coords[, 2], stroke = F, color = "navy") %>%
  addPolygons(lat <- ud90.latlong@polygons[[1]]@Polygons[[1]]@coords[, 1],
              lng <- ud90.latlong@polygons[[1]]@Polygons[[1]]@coords[, 2], stroke = F, color = "navy") %>% 
  addPolygons(lat <- ud90.latlong@polygons[[1]]@Polygons[[2]]@coords[, 1],
              lng <- ud90.latlong@polygons[[1]]@Polygons[[2]]@coords[, 2], stroke = F, color = "navy") %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)
  
## Brownian Bridge with all 3494 locations
dat <- read.csv("CollarData3494.csv")
dat$timestamp <- as.POSIXct(dat$timestamp)
dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = sp::CRS("+proj=longlat"))
utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
colnames(utmcoord) <- c("Easting", "Northing")
dat <- cbind(dat, utmcoord)
rm(utmcoord); rm(geocoord)

traj <- as.ltraj(dat[, 12:13], date = dat$timestamp, id = dat$ndowid)
bb.traj <- kernelbb(traj, sig1 = 6.23, sig2 = 58, grid = 50)

ud50 <- getverticeshr(bb.traj, 50)
ud90 <- getverticeshr(bb.traj, 90)
conts <- rbind(ud50, ud90, makeUniqueIDs = T)
conts@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
conts.latlong <- spTransform(conts, CRS("+proj=longlat"))

leaflet(conts.latlong) %>% addProviderTiles(("Esri.WorldTopoMap")) %>% 
  addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4) %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)

## Brownian Bridge with all 4133 locations
dat <- read.csv("CollarData4133.csv")
dat$timestamp <- as.POSIXct(dat$timestamp, format('%Y-%m-%d %H:%M:%S'))
dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = sp::CRS("+proj=longlat"))
utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
colnames(utmcoord) <- c("Easting", "Northing")
dat <- cbind(dat, utmcoord)
rm(utmcoord); rm(geocoord)

traj <- as.ltraj(dat[, 12:13], date = dat$timestamp, id = dat$ndowid)
bb.traj <- kernelbb(traj, sig1 = 6.23, sig2 = 58, grid = 50)

ud50 <- getverticeshr(bb.traj, 50)
ud90 <- getverticeshr(bb.traj, 90)
conts <- rbind(ud50, ud90, makeUniqueIDs = T)
conts@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
conts.latlong <- spTransform(conts, CRS("+proj=longlat"))

leaflet(conts.latlong) %>% addProviderTiles(("Esri.WorldTopoMap")) %>% 
  addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4) %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)

## FULL BROWNIAN BRIDGE EXAMPLE, WITH ESTIMATED SIG1 
library(adehabitatHR)
library(leaflet)

# cleaning data for ltraj conversion
dat <- read.csv("CollarData3494.csv")
dat$timestamp <- as.POSIXct(dat$timestamp)
dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = sp::CRS("+proj=longlat"))
utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=utm +zone=11")))
colnames(utmcoord) <- c("Easting", "Northing")
dat <- cbind(dat, utmcoord)
rm(utmcoord); rm(geocoord)

# convert to ltraj and estimate BBMM UD
traj <- as.ltraj(dat[, 12:13], date = dat$timestamp, id = dat$ndowid)
lik <- liker(traj, sig2 = 40, rangesig1 = c(1, 10))
bb <- kernelbb(traj, sig1 = 5, sig2 = 40, grid = 150)

# extract UD polygons and plot using leaflet
ud95 <- getverticeshr(bb, 95)
ud95@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
ud95.latlong <- spTransform(ud95, CRS("+proj=longlat"))
leaflet(ud95.latlong) %>% addProviderTiles(("Esri.WorldTopoMap")) %>% 
  addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4) %>% 
  addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, color = "red", stroke = F, radius = 3)

# encapsulating the munging, then the functions
library(adehabitatHR)
library(leaflet)
dat <- read.csv("CollarData3494.csv")

estimate_bbmm <- function(dat) {
  dat <- as.data.frame(dat)
  dat <- dat[complete.cases(dat[, c("long_x", "lat_y", "timestamp")]), ] 
  dat$timestamp <- as.POSIXct(dat$timestamp)
  
  coord_conv <- SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = CRS("+proj=longlat"))
  coord_conv <- as.data.frame(spTransform(coord_conv, CRS("+proj=utm +zone=11")))
  colnames(coord_conv) <- c("Easting", "Northing")
  dat <- cbind(dat, coord_conv)
  
  traj <- as.ltraj(dat[, c("Easting", "Northing")], date = dat$timestamp, id = dat$ndowid)
  sig1 <- liker(traj, sig2 = 40, rangesig1 = c(1, 100), plotit = FALSE)
  bb <- kernelbb(traj, sig1[[1]]$sig1, 40, grid = 500)
  return(bb)
}

get_ud <- function(bb, pct_ud) {
  ud <- getverticeshr(bb, pct_ud)
  ud@proj4string <- CRS("+proj=utm +zone=11 +datum=WGS84")
  ud <- spTransform(ud, CRS("+proj=longlat"))
  return(ud)
}

map_ud <- function(dat, ud) {
  dat <- dat[complete.cases(dat[, c("long_x", "lat_y")]), ]
  map <- leaflet(ud) %>% addProviderTiles("Esri.WorldTopoMap") %>% 
    addCircleMarkers(lng = dat$long_x, lat = dat$lat_y, 
                     color = "red", stroke = F, radius = 3) %>% 
    addPolygons(stroke = F, color = "midnightblue", fillOpacity = .4,
                group = "BBMM 95%")
  return(map)
}

# testing 
system.time(x <- estimate_bbmm(dat))
image(x)
plot(getverticeshr(x, 95), add = T, lwd = 2)
p <- get_ud(x, 95)
map_ud(dat, p)

# more testing, error fixes with date time. Fuck date time
dat <- read.csv("CollarData4133.csv")
traj <- to_ltraj(dat)
traj[duplicated(traj$timestamp), ]

dat[c(4741, 10326), ]
as.character(dat[10325:10327, 'timestamp'])
as.POSIXct(dat[10325, 'timestamp'], format = '%Y-%m-%d %H:%M:%S')
??tz
# for some reason the as.POSIXct call isn't properly converting the datetime for area 7??
# The timestamp error had to do with daylight savings time issues.

# WORKING/TESTING KERNELBB ON MULD 
dat <- read.csv("CollarData4133.csv")
dat$timestamp <- as.POSIXct(dat$timestamp, format = '%Y-%m-%d %H:%M:%S')
dat <- dat[dat$timestamp >= "2014-06-01" & dat$timestamp <= "2014-07-01", ]
DeviceMapping(dat)

traj <- to_ltraj(dat)
liker(traj, sig2 = 60, rangesig1 = c(1, 10))
system.time(bb <- kernelbb(traj, sig1 = 2.7027, sig2 = 40, grid = 100))
image(bb)
plot(getverticeshr(bb, 99), add = T, lwd = 2)

ud <- get_ud(bb, 99)
map_ud(dat, ud)

movement_eda(traj[[1]], "sig.dist", color = "royalblue4")
movement_eda(traj[[1]], "R2n", color = "royalblue4")
movement_eda(traj[[1]], "dist", "point", color = "royalblue4")
movement_eda(traj[[1]], "dist", "hist", "royalblue4")

system.time(x <- estimate_bbmm(dat))
image(x)
plot(getverticeshr(x, 99), add = T, lwd = 2)
p <- get_ud(x, 99)
map_ud(dat, p)






######################
# EDA figures in app #
######################
dat <- read.csv("CollarData3494.csv")

to_ltraj <- function(dat) {
  dat <- as.data.frame(dat)
  dat <- dat[complete.cases(dat[, c("long_x", "lat_y", "timestamp")]), ] 
  dat$timestamp <- as.POSIXct(dat$timestamp)
  
  coord_conv <- SpatialPoints(cbind(as.numeric(dat$long_x),
                                    as.numeric(dat$lat_y)),
                              proj4string = CRS("+proj=longlat"))
  coord_conv <- as.data.frame(spTransform(coord_conv, CRS("+proj=utm +zone=11")))
  colnames(coord_conv) <- c("Easting", "Northing")
  dat <- cbind(dat, coord_conv)
  
  traj <- as.ltraj(dat[, c("Easting", "Northing")], date = dat$timestamp, id = dat$ndowid)
  return(traj)
}

traj <- to_ltraj(dat)[[1]]   #returns data frame from trajectory
traj$cummdist <- cumsum(traj$dist)

# these are the figures that I want to include in the web app
plot(traj$date, traj$cummdist)
plot(traj$date, traj$R2n)
plot(traj$date, traj$dist)
plot(traj$date, (traj$dist/traj$dt))
hist(traj$dist)

ggplot(traj, aes(x = traj$date, y = traj$cummdist)) +
  geom_line()
ggplot(traj, aes(x = date, y = R2n)) +
  geom_line()
ggplot(traj, aes(x = date, y = dist)) +
  geom_line()
ggplot(traj, aes(x = date, y = dist/(dt * (1/60) * (1/60)))) +
  geom_point()
ggplot(traj, aes(x = dist)) +
  geom_histogram()

# generalizing plot functions

movement_eda <- function(dat, plot_var, type = 'line') {
  p <- ggplot(dat)
  if(type == 'hist'){
    p <- p + geom_histogram(aes_string(x = plot_var),
                            binwidth = 500, fill = 'firebrick4', color = 'white')
  } else if (type == 'line'){
    p <- p + geom_line(aes_string(x = 'date', y = plot_var),
                                  color = 'firebrick4', size = .75)
  } else if (type == 'point'){
    p <- p + geom_point(aes_string(x = 'date', y = plot_var),
                        color = 'firebrick4', size = 1.2)
  }
  p <- p + theme(panel.background = element_rect(fill = 'white'),
           plot.background = element_rect(fill = 'white'),
           panel.grid.major.x = element_line(color = 'grey75', size = .5, linetype = 'dotted'),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(color = 'grey75', size = .5, linetype = 'dotted'),
           panel.grid.minor.y = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_text(color = 'grey50', size = 14),
           axis.text.x = element_text(color = 'grey50', size = 10),
           axis.text.y = element_text(color = 'grey50', size = 10),
           axis.ticks = element_blank(),
           strip.background = element_blank(),
           strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}

library(gridExtra)
grid.arrange(movement_eda(traj, "cummdist"),
             movement_eda(traj, "R2n"),
             movement_eda(traj, "dist", "point"),
             movement_eda(traj, "dist", "hist"), ncol = 1)

movement_eda(traj, "sig.dist", color = "royalblue4")
movement_eda(traj, "R2n", color = "royalblue4")
movement_eda(traj, "dist", "point", color = "firebrick4")
movement_eda(traj, "dist", "hist", "royalblue4")

# multiple animals for the movement figures
dat <- read.csv("CollarData.csv")
dat2 <- read.csv("CollarData4133.csv")

traj1 <- to_ltraj(dat)[[1]]
traj2 <- to_ltraj(dat2)[[1]]
traj1$ndowid <- 3494
traj2$ndowid <- 4133
df <- rbind(traj1, traj2)

movement_eda(df, 'dist', 'point', 'firebrick4')

###############################################################################
#Trying to facet result
library(data.table)
dat <- fread("AllCollars.csv", nrows = 100000)
df <- dat[dat$ndowid %in% c(292, 831), ]

to_ltraj <- function(dat) {
  dat <- as.data.frame(dat)
  dat$timestamp <- as.POSIXct(dat$timestamp, format = '%Y-%m-%d %H:%M:%S')
#  dat <- dat[complete.cases(dat[, c("long_x", "lat_y", "timestamp")]), ] 
#   coord_conv <- SpatialPoints(cbind(as.numeric(dat$long_x),
#                                     as.numeric(dat$lat_y)),
#                               proj4string = CRS("+proj=longlat"))
#   coord_conv <- as.data.frame(spTransform(coord_conv, CRS("+proj=utm +zone=11")))
#   colnames(coord_conv) <- c("Easting", "Northing")
#  dat <- cbind(dat, coord_conv)
#   
#   traj <- as.ltraj(dat[, c("Easting", "Northing")], date = dat$timestamp, id = dat$ndowid)
#   traj[[1]]$sig.dist <- cumsum(traj[[1]]$dist)
#   return(traj)
  return(dat)
}

traj <- to_ltraj(df)

movement_eda <- function(dat, plot_var){
  ggplot(dat, aes_string(x = 'date', y = plot_var, group = 'ndowid', color = 'ndowid')) +
#    facet_grid(~ndowid, drop = T) +
    geom_point()
}
movement_eda(df, 'R2n')
