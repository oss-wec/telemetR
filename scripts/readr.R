# using readr instead of data.table to read the data? 
library(readr)
dat <- read_csv("Collars.csv")
dat <- as.data.table(dat)
