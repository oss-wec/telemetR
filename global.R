library(data.table)
library(leaflet)

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


