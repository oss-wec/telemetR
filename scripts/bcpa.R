## CREATING DATA OBJECT FOR ANALYSIS
df.vt <- df[, .(timestamp, x, y)]
df.vt <- df.vt[Time < '2014-07-01', ]
df.vt <- df.vt[Time >= '2014-01-01', ]
names(df.vt) <- c('Time', 'X', 'Y')
df.track <- MakeTrack(df.vt$X, df.vt$Y, df.vt$Time)
plot(df.track)

df.bp <- GetVT(df.track)
hist(df.bp$V, breaks = 20, col = 'grey')
hist(df.bp$Theta, breaks = 20, col = 'grey')

df.ws <- WindowSweep(df.bp, 'log(S)', windowsize = 100, K = 2, windowstep = 5)
head(df.ws$ws)
plot(df.ws, type = 'smooth')
plot(df.ws, type = 'smooth')
plot(df.ws, type = 'flat')
plot(df.ws, type = 'flat', clusterwidth = 1)

ChangePointSummary(df.ws, clusterwidth = 3)
PathPlot(df.track, df.ws, type = 'flat', main = 'flat bcpa')
PathPlot(df.track, df.ws, type = 'smooth', main = 'smooth bcpa')
PhasePlot(df.ws, 'smooth')
DiagPlot(df.ws, 'flat')

plot(df.bp$T.POSIX, df.bp$V * cos(df.bp$Theta), type = 'l')
plot(df.bp$T.POSIX, df.bp$dT)
plot(df.bp$T.POSIX, df.bp$V)
