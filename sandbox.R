library(data.table)

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
write.csv(df, "WorkingExample.csv")

seq(1, 10, 4)
