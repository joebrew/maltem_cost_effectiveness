library(tidyverse)
library(readr)
library(readxl)

irs <- read_csv('data/vector_control_ministry/IRS_mozambique_clean_final.csv')

# Fix Matola
irs$district[irs$district == 'M.Matola'] <- 'Matola'
irs$province[irs$district == 'Matola'] <- 'Maputo'

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
                                                ifelse(district == 'MATUTUÍNE MOZAL', 'MATUTUINE',
                                                       ifelse(district == 'XINAVANE', 'MANHICA', district))))))
irs <- 
  irs %>%
  mutate(district = ifelse(district == 'MANHIÇA', 'MANHICA',
                           ifelse(district == 'MAT MOZAL', 'MATOLA',
                                  ifelse(district == 'MATUTUÍNE', 'MATUTUINE',
                                         ifelse(district == 'XXAI CIDADE', 'XAI-XAI CITY', 
                                                ifelse(district == 'XXAI DISTRITO', 'XAI-XAI DISTRICT', 
                                                       ifelse(district == 'ZONA 1A*', 'MATOLA', district)))))))

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

