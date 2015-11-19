library(data.table)
library(leaflet)

CollarMap <- function(dataframe) {
  df <- as.data.table(dataframe)
  df <- df[complete.cases(df[, .(long_x, lat_y)])]
  df_lines <- df[, .SD[c(seq(1, .N, 20), .N)], by = ndowid]
  unq_id <- unique(df[, ndowid])
  map <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
  
  for (n in unq_id) {
    d <- df_lines[ndowid == n]
    dp <- d[, .SD[c(1, .N)]]
    
    map <- addPolylines(map, lng = d$long_x, lat = d$lat_y,
                        weight = 2, color = "black", opacity = .4)
    map <- addCircleMarkers(map, lng = dp$long_x, lat = dp$lat_y,
                            stroke = FALSE, radius = 4, color = "navy",
                            fillOpacity = 1)
  }
  return(map)
}


