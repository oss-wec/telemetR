library(readr)
library(sp)
library(magrittr)
library(dplyr)
library(leaflet)
library(maps)
library(spatstat)
library(GISTools)
library(ggplot2)
library(rgeos)
library(rgdal)

# read data
dat <- read_csv('ccdata.csv')
nv <- read_csv('C:/Users/mgritts/Dropbox/Data/Geography/Nevada/NVdataframe.csv')

# create unique species list
unq_species <- distinct(dat, SpeciesCode)

# exp plots
datgb <- dat %>% group_by(ObsYear) %>% 
  summarize(n = n(),
            total = sum(Total))

ggplot(datgb, aes(ObsYear, total)) + geom_line() +theme_bw()

dat$ObsDate <- lubridate::mdy(dat$ObsDate)
dat$year <- lubridate::year(dat$ObsDate)
dat$month <- lubridate::month(dat$ObsDate)

dat14 <- dat %>% 
  filter(year == 2014)

dat14_gb <- dat14 %>% 
  group_by(SpeciesName) %>% 
  summarize(n = n(),
            total = sum(Total))
ggplot(dat14_gb, aes(x = SpeciesName, y = total)) + geom_bar(stat = 'identity')

dat14month <- dat14 %>% 
  group_by(month) %>% 
  summarize(n = n(),
            total = sum(Total))
ggplot(dat14month, aes(x = month, y = total)) + geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 1:12)


gb <- dat %>% 
  group_by(SpeciesName, year, month) %>% 
  summarize(n = n(),
            total = sum(Total))
ggplot(gb, aes(x = year, y = total, group = SpeciesName, color = SpeciesName)) + 
  geom_point() 

ggplot(dat, aes(x = ENAD83, y = NNAD83)) + geom_point() + coord_equal() + theme_bw()

## ECOREGION LINES
nv <- readRDS('ComCol_Shiny/NV_Outline.rda')
nv@proj4string@projargs
eco <- readOGR('../../Data/GIS/EcoRegions', 'CEC_NorthAmericanEcoRegions_EPA')

nvlaea <- spTransform(nv, CRSobj = CRS(eco@proj4string@projargs))
nv_eco <- gIntersection(nvlaea, eco, byid = TRUE, drop_lower_td = TRUE)
plot(nv_eco)

ind <- eco$OID_ %in% c(141, 137, 135, 69, 139)
aoi <- eco[ind, ]
plot(aoi)
plot(nvlaea, add = T)

ids <- rownames(data.frame(aoi))
aoi2 <- gIntersection(aoi, nvlaea, byid = TRUE, id = ids)

aoidf <- data.frame(aoi)
aoidf$id <- ids
nv_er <- SpatialPolygonsDataFrame(aoi2, aoidf)
plot(nv_er, col = 'green')

saveRDS(nv_er, 'NV_EcoRegion.rda')

nv_eco <- readRDS('NV_EcoRegion.rda')
plot(nv_eco)
?gUnaryUnion

p1 <- SpatialPolygons(list(nv_eco@polygons[[1]]))
p2 <- SpatialPolygons(list(nv_eco@polygons[[2]]))
p3 <- SpatialPolygons(list(nv_eco@polygons[[3]]))
p4 <- SpatialPolygons(list(nv_eco@polygons[[4]]))
p5 <- SpatialPolygons(list(nv_eco@polygons[[5]]))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
plot(p5)

un23 <- gUnion(p2, p3, drop_lower_td = T)
plot(un23)
un23 <- gUnionCascaded(SpatialPolygons(list(un23@polygons[[1]])))

pp <- un23@polygons[[1]]
spp <- SpatialPolygons(list(pp))
plot(spp)
sss <- gUnaryUnion(spp)
plot(sss)
plot(nv_eco)

## 1 sqmile hex
