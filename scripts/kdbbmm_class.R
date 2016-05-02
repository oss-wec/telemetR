## kernelUD and kernelbb troubleshooting

data("puechabonsp")
dat <- puechabonsp[[2]]
dat <- as.data.frame(dat)
brock <- dat[dat$Name == 'Brock',]
brock <- droplevels(brock)

## kernelUD 
b_spdf <- SpatialPointsDataFrame(coordinates(cbind(brock$X, brock$Y)),
                                 data = brock)
kd <- kernelUD(b_spdf[, 1])
class(kd)
names(kd)

## kernelbb
da <- as.POSIXct(strptime(as.character(dat$Date), "%y%m%d"))
traj <- as.ltraj(cbind(dat$X, dat$Y), da, id = dat$Name)
bb <- kernelbb(traj, 6.23, 58, grid = 50)
class(bb)
length(names(bb))

length(bb)
bb@h
