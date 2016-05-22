library(lubridate)
library(readr)
library(leaflet)
library(magrittr)
library(sp)
library(dplyr)
library(ggplot2)
library(adehabitatHR)
library(dplyr)

dat <- read_csv("data/AllCollars.csv")
df <- filter(dat, ndowid %in% c('1140', '1139'))
df <- xyConv(df)


## wrap homerange estimation functions
estHR <- function(xy, id, dat, method = 'kd', CRSin = '+init=epsg:26911') {
  spdf <- SpatialPointsDataFrame(coordinates(df[, xy]),
                                 data = as.data.frame(dat),
                                 proj4string = CRS(CRSin))
  ud <- kernelUD(spdf[, id])
  return(ud)
}
sdf_points <- estHR(xy = c('x', 'y'), id = 'ndowid', dat = df)
image(sdf_points)

## wrap contour functions
getPoly <- function(ud, pct = 90) {
  plyg <- lapply(ud, function(x) {
    tryCatch({
      getverticeshr(x, pct)
    }, error = function(error) NA)
    })
  return(plyg)
}
x <- getPoly(sdf_points)

## wrapping lapply function from above into a function
getP <- function(ud, pct) {
  p <- tryCatch({
    getverticeshr(ud, pct)
  }, error = function(error) NA)
}
p <- getP(sdf_points[[2]], 90)
plot(p)
## testing lapply function 
x <- lapply(sdf_points, function(x) getP(x, 90))
plot(x[[1]]); plot(x[[2]])

###############################################################################
## LOOKING AT THE GETVERTICESHR CODE TO RECREATE FOR MULTIPLE PCTS
data("puechabonsp")
dat <- puechabonsp$relocs
kud <- kernelUD(dat[,1])

## skipping verifications
x <- getvolumeUD(kud[[1]])

xyma <- coordinates(x)
xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
ud <- as.image.SpatialGridDataFrame(x[,1])$z

## list of coordinates for contour
re <- contourLines(x = xyl$x,
                   y = xyl$y,
                   ud, nlevels = 1, levels = 60)
# ## visualize contourLines output
 plot(re[[1]])
# polygon(re[[2]]); polygon(re[[1]])

## check for holes in the list

## required functions for checking for holes
.arcp <- function(xy){
  if (nrow(xy) < 3)
    return(0);
  x.segmat <- cbind(xy, rbind(xy[2:nrow(xy), ],
                              xy[1, ]))
  abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3]
          * x.segmat[,2])) / 2
}

.arcpspdf <- function(spdf){
  lar <- unlist(lapply(polygons(spdf)@polygons,
                       function(x) unlist(lapply(x@Polygons, function(y)
                         .arcp(y@coords)))))
  lhol <- unlist(lapply(polygons(spdf)@polygons,
                        function(x) unlist(lapply(x@Polygons, function(y)
                          y@hole))))
  sum(lar[!lhol])-sum(lar[lhol])
}

areaa <- unlist(lapply(re, function(y) {
  ttmp <- cbind(y$x,y$y)
  ttmp <- rbind(ttmp, ttmp[1,])
  .arcpspdf(SpatialPolygons(list(Polygons(list(Polygon(ttmp)), 1))))
}))

## the tmpp calls above are to create a data.frame repeating the
## first point as the last to close the polygon
# ttmp <- cbind(re[[1]]$x, re[[1]]$y)
# ttmp <- rbind(ttmp, ttmp[1, ])
## .arcpspdf is from within adehabitatHR package 
spdf <- SpatialPolygons(list(Polygons(list(Polygon(ttmp)), 1)))
plot(spdf)
lar <- unlist(lapply())
