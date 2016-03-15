# minimum convex polygon methods. Multiple contours and multiple individuals.

library(adehabitatHR)
library(leaflet)
library(sp)
library(data.table)
library(geojsonio)

# loading data and subsetting
dat <- fread('Collars.csv')
df <- dat[ndowid %in% c(1132, 1135), ]
df <- coord_conv(df)

# what is the difference between SpatialPoints and coordinates.
cp <- SpatialPoints(df[, .(x, y)], CRS('+proj=utm +zone=11'))
coordinates(df) <- df[, .(x, y)]

cp <- mcp(df[, 2], percent = 95)
plot(cp)
df.cp <- as.data.frame(cp)
mcp.area(df[, 2], unin = 'm', unout = 'ha')
?mcp

for (i in seq_along(cp@polygons)) {
  print(i)
}


