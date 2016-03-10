library(data.table)
library(ggplot2)
library(sp)
library(lubridate)
library(fasttime)
library(dplyr)
library(viridis)
library(ggthemes)


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


## break point detection

