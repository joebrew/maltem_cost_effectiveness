library(tidyverse)

irs <- read_csv('data/vector_control_ministry/IRS_Mozambique_clean - IRS_Maputo.csv')
irs$number_of_days_activity <- NULL

# Keep only gaza and maputo
irs <- irs %>%
  filter(province %in% c('Gaza', 'Maputo'))

# Standardize names to match those in bes
irs$district <- toupper(irs$district)
irs_districts <- sort(unique(irs$district))

irs <- 
  irs %>%
  mutate(district = ifelse(district == 'C.MATOLA', 'MATOLA',
                           ifelse(district == 'C.XAI-XAI', 'XAI-XAI CITY',
                                  ifelse(district == 'D.XAI-XAI',
                                         'XAI-XAI DISTRICT',
                                         ifelse(district == 'MANJACAZE',
                                                'MANDLAKAZI',
                                                ifelse(district == 'MATUTUÍNE MOZAL', 'MATUTUINE',
                                                       ifelse(district == 'XINAVANE', 'MANHICA', NA)))))))

# Clean up
irs <- 
  irs %>%
  filter(!is.na(district)) %>%
  group_by(province, 
           district,
           year) %>%
  summarise(houses_irs = sum(as.numeric(as.character(gsub(',', '', houses))), na.rm = TRUE),
            people_irs = sum(as.numeric(as.character(gsub(',', '', people))), na.rm = TRUE)) %>%
  ungroup

