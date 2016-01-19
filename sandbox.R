library(data.table)
library(leaflet)

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

