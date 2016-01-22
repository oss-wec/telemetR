DeviceMapping <- function(dataframe, basemap = "Esri.WorldTopoMap") {
  
    
  dat <- dataframe[complete.cases(dataframe[, c("long_x", "lat_y")]), ]
  unique.id <- unique(dat$ndowid)
  pal <- ggthemes::gdocs_pal()(20)
  device.map <- leaflet() %>% 
    addProviderTiles(basemap, group = "topo") %>% 
    addProviderTiles("MapQuestOpen.Aerial", group = "satelite")
  layer.group <- list()
  
  for(i in 1:length(unique.id)) {
    df <- dat[dat$ndowid == unique.id[i], ]
    df <- df[order(df$timestamp), ]
    device.map <- addPolylines(device.map, 
                               lng = df$long_x, lat = df$lat_y,
                               group = as.character(unique.id[i]),
                               color = "grey",
                               weight = 2
    )
    device.map <- addCircleMarkers(device.map,
                                   lng = df$long_x, lat = df$lat_y,
                                   group = as.character(unique.id[i]),
                                   radius = 4,
                                   stroke = FALSE,
                                   fillOpacity = .8,
                                   color = pal[i],
                                   popup = paste(sep = "<br>",
                                                 paste("<b>NDOW ID:</b> ", unique.id[i]),
                                                 paste("<b>timestamp:</b> ", df$timestamp),
                                                 paste("<b>LocID</b>: ", df$locid))
                          )
    layer.group <- c(layer.group, as.character(unique.id[i]))
  } 
  device.map <- addLayersControl(device.map,
                                baseGroup = c("topo", "satellite"),
                                overlayGroups = layer.group)
  return(device.map)
}
DeviceMapping(dat)


#   for(i in 1:length(unique.id)) {
#     df <- dat[dat$ndowID == unique.id[i], ]
#     df <- df[order(df$timestamp), ]
#     if (shape == "points") {

#     } else if (shape == "lines") {
#       device.map <- addPolylines(device.map,
#                                  lng = df$long_x, lat = df$lat_y,
#                                  group = unique.id[i],
#                                  weight = 2,
#                                  color = pal[i])
#     }
#     layer.group <- c(layer.group, unique.id[i])
#   }
#   device.map <- addLayersControl(device.map,
#                                  baseGroup = c("topo", "satellite"),
#                                  overlayGroups = layer.group,
#                                  options = layersControlOptions(collapsed = FALSE))
#   return(device.map)
# }

dat <- read.csv('CollarData (2).csv')
DeviceMapping(dat)

## I WANT TO TRY AND USE LAPPLY TO CONSTRUCT THE MAP...
# builder function for lapply to call
build_leaflet_layers <- function(x, df, map, geometry = "points") {
  df <- df[df$ndowid == x, ]
  map <- addCircleMarkers(map,
                          lng = df$long_x, lat = df$lat_y,
                          group = as.character(x),
                          radius = 3,
                          stroke = FALSE,
                          fillOpacity = .8,
                          color = "navy",
                          popup = as.character(x))
}

device_map <- function(dataframe) {
  dat <- dataframe[complete.cases(dataframe[, c("long_x", "lat_y")]), ]
  unique.id <- unique(dat$ndowid)
  device.map <- leaflet() %>% 
    addProviderTiles('Esri.WorldTopoMap')
  device.map <- lapply(unique.id, function(x) build_leaflet_layers(x, dat, device.map, "points"))
  return(device.map)
}

device_map(dat)

leaflet() %>% addTiles() %>% addCircleMarkers(dat$long_x, dat$lat_y)

## USING DATA.TABLE TO ONLY PLOT EVERY 20 POINTS, THE LINE IS GOING TO BE EVERY POINT
# I don't know if this is really necessary. It looks okay, but isn't really that great, 
# may be better when there are a ton of points. The big change here however is that I'm using 
# data.table for the dataframe manipulations. I'll leave it in, but comment out the line.
DeviceMapping <- function(dataframe, basemap = "Esri.WorldTopoMap") {
  dat <- as.data.table(dataframe)
  dat <- dat[complete.cases(dat[, .(long_x, lat_y)])]
  unique.id <- unique(dat$ndowid)
  pal <- ggthemes::gdocs_pal()(20)
  
  device.map <- leaflet() %>% 
    addProviderTiles(basemap, group = "topo") %>% 
    addProviderTiles("MapQuestOpen.Aerial", group = "satelite")
  layer.group <- list()
  
  for(i in 1:length(unique.id)) {
    df <- dat[ndowid == unique.id[i]]
    device.map <- addPolylines(device.map, 
                               lng = df$long_x, lat = df$lat_y,
                               group = as.character(unique.id[i]),
                               color = "grey",
                               weight = 2
    )
    #df <- df[, .SD[c(seq(1, .N, 5), .N)]]
    device.map <- addCircleMarkers(device.map,
                                   lng = df$long_x, lat = df$lat_y,
                                   group = as.character(unique.id[i]),
                                   radius = 3,
                                   stroke = FALSE,
                                   fillOpacity = .5,
                                   color = pal[i],
                                   popup = paste(sep = "<br>",
                                                 paste("<b>NDOW ID:</b> ", unique.id[i]),
                                                 paste("<b>timestamp:</b> ", df$timestamp),
                                                 paste("<b>LocID</b>: ", df$locid))
    )
    layer.group <- c(layer.group, as.character(unique.id[i]))
  } 
  device.map <- addLayersControl(device.map,
                                 baseGroup = c("topo", "satellite"),
                                 overlayGroups = layer.group)
  return(device.map)
}

DeviceMapping(dat)
