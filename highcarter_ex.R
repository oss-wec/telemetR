library(data.table, verbose = FALSE)
library(highcharter)
library(sp)
library(lubridate)

move.r2n <- function(x, y) {
  r2n <- (x - x[1])**2 + (y - y[1])**2
  return(r2n)
}

coord_conv <- function(df, conversion = 'utm') {
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  conv <- SpatialPoints(cbind(as.numeric(df$long_x), as.numeric(df$lat_y)),
                        proj4string = CRS('+proj=longlat'))
  conv <- as.data.frame(spTransform(conv, CRS('+proj=utm +zone=11')))
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)
  return(df)
}

dat <- fread("S:/MGritts/telemetR/Collars.csv")
df <- dat[ndowid == 1139]
df <- coord_conv(df)
df[, R2n := move.r2n(x, y)]
df$timestamp <- ymd_hms(df$timestamp)

highchart() %>% 
  hc_xAxis(data = datetime_to_timestamp(df$timestamp)) %>% 
  hc_add_series(data = df$R2n)

highchart() %>% 
  hc_add_series_times_values(dates = as.Date(df$timestamp), values = df$R2n)
  