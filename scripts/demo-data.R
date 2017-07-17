sub_dat <- dat %>% 
  filter(timestamp <= lubridate::ymd('2013-12-31') & 
         species %in% c('MULD', 'DBHS', 'RMEL'))
write_csv(sub_dat, 'testdata.csv')


ids <- sub_dat %>% 
  extract2('ndowid') %>% 
  unique()

sub_animal <- dat_animal %>% 
  filter(spid %in% c('MULD', 'DBHS', 'RMEL') &
         ndowid %in% ids)
write_csv(sub_animal, 'testanimal.csv')
