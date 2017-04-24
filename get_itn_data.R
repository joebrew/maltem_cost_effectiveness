library(tidyverse)
itn <- read_excel('data/vector_control_ministry/ITN_Mozambique.xls',
                  skip = 3)
itn <- itn[,!is.na(names(itn))]

# Clean up names
itn <-
  itn %>%
  rename(province = PROVINCIA,
         district = DISTRITOS,
         month = `Mes de distribuic`,
         nets = `Redes Distribuidas`) %>%
  dplyr::select(province,
                district, 
                year,
                month) %>%
  filter(province %in% c('Gaza', 'Maputo'))

# Write data
write_csv(itn, 'data/outputs/itn.csv')
