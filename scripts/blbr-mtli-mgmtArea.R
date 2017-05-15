dat <- read_csv("Collars.csv")
dat_animal <- read_csv("Animals.csv")

levels(as.factor(dat_animal$spid)) 

dat_animal[dat_animal$spid %in% c('BLBR', 'MTLI'), 'mgmtarea']
dat_animal[dat_animal$spid %in% c('BLBR', 'MTLI'), 'mgmtarea'] <- 30

dat[dat$species %in% c('BLBR', 'MTLI'), 'mgmtarea']
dat[dat$species %in% c('BLBR', 'MTLI'), 'mgmtarea'] <- 30

write_csv(dat, 'collars-out.csv')
write_csv(dat_animal, 'animals-out.csv')
