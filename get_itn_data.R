library(tidyverse)
itn <- read_csv('data/vector_control_ministry/ITN_Mozambique_clean.csv',
                skip = 0)
itn <- itn[,!is.na(names(itn))]

# Clean up names
itn <-
  itn %>%
  rename(nets = ITN_distributed) %>%
  dplyr::select(province,
                district, 
                year,
                month,
                nets) %>%
  filter(province %in% c('Gaza', 'Maputo'))

# Make cap
itn <- itn %>%
  mutate(district = toupper(district))

# Fix names
itn <-
  itn %>%
  mutate(district = ifelse(district == 'XAI-XAI', 'XAI-XAI DISTRICT',
                                  ifelse(district == 'CIDADE DA MATOLA', 'MATOLA', district)))


# Make monthly
itn <- itn %>%
  group_by(province, district, year, month) %>%
  summarise(nets = sum(nets)) %>%
  ungroup

