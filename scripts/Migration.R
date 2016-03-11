library(data.table)
library(ggplot2)
library(sp)
library(lubridate)
library(fasttime)
library(dplyr)
library(viridis)
library(ggthemes)
library(gridExtra)
library(changepoint)


dat <- fread('Collars.CSV')
dat$timestamp <- dat[, fastPOSIXct(timestamp)]
df <- dat[ndowid == 1576, ]

## convert coordinates and movement parameters
df <- coord_conv(df)
df[, dist := move.dist(x, y), by = ndowid]
df[, sig.dist := cumsum(dist), by = ndowid]
df[, R2n := move.r2n(x, y), by = ndowid]
df[, sqrtR2n := sqrt(move.r2n(x, y)), by = ndowid]
df[, dt := move.dt(timestamp), by = ndowid]
df[, speed := move.speed(dist, dt), by = ndowid]
df[, hr := hour(timestamp), by = ndowid]
df[, mth := month(timestamp), by = ndowid]
df[, d.mth := lubridate::day(timestamp), by = ndowid]
df[, locnum := 1:.N, by = ndowid]
d <- df


## NSD plot
r2n_plot <- movement_eda(df, 'R2n')
r2n_plot
movement_eda(df, 'sqrtR2n')
grid.arrange(r2n_plot, movement_eda(df, 'sqrtR2n'))

## NSD inlfection points
dR2n <- df$R2n[-length(df$R2n)] - df$R2n[-1]
dR2n <- c(0, dR2n)
r2n.sigma <- dR2n / sd(dR2n)

df[, dr2n := dR2n, ]
df[, sr2n := r2n.sigma, ]
str(df)

## visualizing R2n
hist(dR2n)
ggplot(df, aes(x = timestamp, y = dr2n)) + geom_point()
summary(dR2n)
hist(r2n.sigma)
ggplot(df, aes(x = timestamp, y = sr2n)) + geom_point() + geom_hline(yintercept = c(2, -2, 3, -3, 6, -6))
summary(r2n.sigma)

gridExtra::grid.arrange(r2n_plot, ggplot(df, aes(x = timestamp, y = sr2n)) + geom_point())

## subsetting data for just migration time
df.mig <- df[timestamp < as.POSIXct('2013-07-01'), ]
movement_eda(df.mig, 'R2n')
df.mig <- df.mig[timestamp > as.POSIXct('2013-04-01'), ]
movement_eda(df.mig, 'R2n')
df.mig <- df.mig[timestamp > as.POSIXct('2013-04-05'), ]
movement_eda(df.mig, 'R2n')
df.mig <- df.mig[timestamp < as.POSIXct('2013-05-10'), ]
g.nsd <- movement_eda(df.mig, 'R2n', 'point')
g.nd <- movement_eda(df.mig, 'sqrtR2n', 'point')
gridExtra::grid.arrange(g.nsd, g.nd, nrow = 2)


## one point per day
df.mig$date <- as.Date(df.mig$timestamp)
df.oneday <- df.mig[, .SD[1], by = date]
movement_eda(df.oneday, 'R2n')

## breakpoint
hist(df.mig$R2n)
hist(df.mig$sqrtR2n)
hist(df.mig$dist)
bp <- cpt.meanvar(df.mig$R2n)
plot(bp)
cpts(bp)
bp <- cpt.meanvar(df$R2n, method = 'BinSeg')
plot(bp)
plot.ts(bp)
cpts(bp)


bp.pelt <- cpt.meanvar(df$R2n, method = 'PELT', Q = 10)
cpts(bp.pelt)
plot(bp.pelt)


## TSD (Time scaled distance)
x <- df.mig$x
y <- df.mig$y 
v <- df.mig$speed
t <- df.mig$timestamp
s = 0.04
tsd <- sqrt((x - x[1])**2 + (y - y[1])**2 + 
           (s * max(v, na.rm = T) * 
           (as.integer(abs(difftime(t[1], t, units = 'secs')))) ))
tsd[1] <- 0
tsd.df <- data.frame(tsd = tsd, index = 1:length(tsd))
plot(x = tsd.df$index, y = tsd.df$tsd, type = 'l')
