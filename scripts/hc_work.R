source('../global.R')

df <- dat %>% filter(ndowid %in% c(1135:1145))
df <- coord_conv(df)
df$timestamp <- lubridate::ymd_hms(df$timestamp)
df <- df %>% arrange(ndowid, timestamp)
df$nds <- move.r2n(df$x, df$y)
movement_eda(df, 'nds')

## normalising nsd
nds_v <- df$nds
df$nds_norm <- (nds_v - min(nds_v)) / (max(nds_v) - min(nds_v))
movement_eda(df, 'nds_norm')

## split - apply - combine with dplyr
df <- df %>% group_by(ndowid) %>% mutate(nsd = move.r2n(x, y), 
                                         ts = date(timestamp))

## plot 
movement_eda(df, 'nsd')

## plot with highcharter
d1 <- df %>% filter(ndowid == 1137) %>% mutate(ts = date(timestamp)) %>% group_by(ts) %>% slice(1)
d2 <- df %>% filter(ndowid == 1136) %>% mutate(ts = date(timestamp)) %>% group_by(ts) %>% slice(1)
d3 <- df %>% filter(ndowid == 1138) %>% mutate(ts = date(timestamp)) %>% group_by(ts) %>% slice(1)
highchart() %>% 
  hc_add_series_times_values(dates = as.Date(d1$timestamp), values = d1$nsd, color = color_pal[1]) %>%    
  hc_add_series_times_values(dates = as.Date(d2$timestamp), values = d2$nsd, color = color_pal[2]) %>% 
  hc_add_series_times_values(dates = as.Date(d3$timestamp), values = d3$nsd, color = color_pal[3])

## all together now
df <- dat %>% filter(ndowid %in% c(1135:1145))
df <- coord_conv(df)
df$timestamp <- lubridate::ymd_hms(df$timestamp)

df <- df %>% 
  arrange(ndowid, timestamp) %>% 
  group_by(ndowid) %>% 
  mutate(nsd = move.r2n(x, y),
         ts = date(timestamp)) %>% 
  group_by(ndowid, ts) %>% 
  slice(1)

ids <- df %>% select(ndowid) %>% extract2('ndowid') %>% unique() 
hc <- highchart()
for(i in seq_along(ids)) {
  d <- df %>% filter(ndowid == ids[i])
  hc <- hc_add_series_times_values(hc, dates = d$ts, values = d$nsd, color = color_pal[i])
}
hc
