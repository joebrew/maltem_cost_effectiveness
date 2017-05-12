# Get BES data
source('get_bes_data.R')

# Get a date helper for standardizing dates
wh <- create_date_helper()
wh$month <- as.numeric(format(wh$date, '%m'))

# MANHICA OPD ########################################
load('misc_data/opd_cleaned.RData')
opd_manhica <- opd %>%
  filter(malaria) %>%
  filter(date >= min(bes$date)) %>%
  mutate(age_group = ifelse(age < (5 * 12), '0-4',
                            ifelse(age >= (5 * 12), '5+',
                                   NA))) %>%
  dplyr::select(age_group, date) %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m')),
         week = as.numeric(format(date, '%U')) + 1) %>%
  group_by(year, week, age_group) %>%
  summarise(cases = n()) %>%
  ungroup %>%
  mutate(district = 'MANHICA',
         source = 'OPD')
opd_manhica_under5 <-
  opd_manhica %>%
  filter(age_group == '0-4') %>%
  group_by(year, week, district, source) %>%
  summarise(cases = sum(cases, na.rm = TRUE))

# MAGUDE OPD ###########################################
# Read in old OPD Magude data, sent by Bea
old_opd <- readstata13::read.dta13('misc_data/opd_magude_old/survmag_rrs_dis.dta')
old_opd <-
  old_opd %>%
  dplyr::select(yr,
                week,
                mal_hf) %>%
  rename(year = yr,
         cases = mal_hf)

opd_magude_all <- old_opd
opd_magude_all$district <- 'MAGUDE'
opd_magude_all$source <- 'OPD'
rm(old_opd)

# Combine all sources
comparison_magude <-
  opd_magude_all %>%
  bind_rows(bes %>%
              filter(district == 'MAGUDE',
                     disease == 'MALARIA') %>%
              group_by(year, week, district) %>%
              summarise(cases = sum(cases, na.rm = TRUE)) %>%
              ungroup %>%
              mutate(source = 'BES')) %>%
  left_join(wh) %>%
  group_by(year, month, source, district) %>%
  summarise(date = first(date),
            cases = sum(cases, na.rm = TRUE))

comparison_manhica <-
  opd_manhica_under5 %>%
  bind_rows(bes %>%
              filter(district == 'MANHICA',
                     age_group == '0-4',
                     disease == 'MALARIA') %>%
              group_by(year, week, district) %>%
              summarise(cases = sum(cases, na.rm = TRUE)) %>%
              ungroup %>%
              mutate(source = 'BES')) %>%
  left_join(wh) %>%
  group_by(year, month, source, district) %>%
  summarise(date = first(date),
            cases = sum(cases, na.rm = TRUE))

g1 <- ggplot(data = comparison_magude,
       aes(x = date,
           y = cases,
           color = source)) +
  geom_line(alpha = 0.8) +
  labs(title = 'Magude: all cases',
       x = 'Date',
       y = 'Cases') +
  theme_cism() +
  scale_color_manual(name = 'Source',
                     values = c('darkgreen', 'darkorange'))

g2 <- ggplot(data = comparison_manhica,
       aes(x = date,
           y = cases,
           color = source)) +
  geom_line(alpha = 0.8) +
  labs(title = 'Manhica: 0-4 only',
       x = 'Date',
       y = 'Cases') +
  theme_cism() +
  scale_color_manual(name = 'Source',
                     values = c('darkgreen', 'darkorange'))

library(cism)
Rmisc::multiplot(g1, g2, cols = 1) 

g1
ggsave('~/Desktop/magude.png')
g2
ggsave('~/Desktop/manhica.png')
