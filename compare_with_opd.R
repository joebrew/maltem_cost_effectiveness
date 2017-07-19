library(cism)
# Get BES data
source('get_bes_data.R', encoding = "UTF-8")

# Define date truncation
date_truncate <- 
  function (date_object, level = c("month", "quarter", "year")) 
  {
    if (is.null(level)) {
      stop("You must provide a level argument of either \"month\", \"quarter\" or \"year\".")
    }
    date_object <- as.Date(date_object)
    if (sum(!is.na(date_object)) == 0) {
      return(date_object)
    }
    if (level == "month") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y-%m"), "-01"))
      return(return_object)
    }
    if (level == "quarter") {
      q_month <- (((((as.numeric(format(date_object, "%m"))) - 
                       1)%/%3) + 1) * 3) - 2
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), ifelse(nchar(q_month[!is.na(return_object)]) == 
                                                                                    2, "-", "-0"), q_month, "-01"))
      return(return_object)
    }
    if (level == "year") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), "-01-01"))
      return(return_object)
    }
  }

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

# Comparison manhica
x <- 
  comparison_manhica %>%
  ungroup %>%
  group_by(district, source) %>%
  mutate(avg = mean(cases)) %>%
  ungroup %>%
  mutate(y = cases / avg * 100)
ggplot(data = x,
       aes(x = date,
           y = y, 
           color = source)) +
  geom_line(alpha = 0.8) +
  labs(title = 'Manhica: 0-4 only',
       subtitle = 'Percentage of total period average',
       x = 'Date',
       y = 'Percent') +
  theme_cism() +
  scale_color_manual(name = 'Source',
                     values = c('darkgreen', 'darkorange'))
y <- spread(x %>%
              dplyr::select(date, source, cases) %>%
              filter(!duplicated(paste0(date, source))),
           key = source,
           value = cases)
z <- y %>% filter(!is.na(OPD),
                  !is.na(BES))
cor(z$OPD, z$BES)
ggplot(data = z, aes(x = OPD, y = BES)) +
  geom_point() +
  labs(title = 'Manhica: 0-4 only',
       subtitle = 'Correlation in number of cases') +
  theme_cism() 

# GET incidence in ij
x <- 
  opd %>%
  filter(!is.na(date)) %>%
  mutate(date >= as.Date('2010-01-01')) %>%
  mutate(year_month = date_truncate(date, 'month')) %>%
  mutate(month = as.numeric(format(date, '%m'))) %>%
  filter(place == 'Ilha Josinha') %>%
  mutate(age_group = ifelse(age < 12, '0-0.99',
                            ifelse(age < 60, '1-4.99',
                                   ifelse(age >= 60, '5+', NA)))) %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  filter(year >= 2010) %>%
  group_by(date, age_group) %>%
  summarise(visits = n(),
            malaria = length(which(malaria))) %>%
  ungroup %>%
  mutate(p = malaria / visits * 100)

ggplot(data = x %>% filter(age_group != '5+'),
       aes(x = date,
           y = malaria,
           group = age_group,
           color = age_group)) +
  geom_line(alpha = 0.6) +
  # geom_point() +
  theme_cism() +
  labs(x = 'Age group',
       y = 'Daily malaria cases')
ggsave('~/Desktop/daily.png')

write_csv(x, '~/Desktop/daily.csv')

# GET incidence in ij
x <- 
  opd %>%
  filter(!is.na(date)) %>%
  mutate(date >= as.Date('2010-01-01')) %>%
  mutate(year_month = lendable::date_truncate(date, 'month')) %>%
  mutate(month = as.numeric(format(date, '%m'))) %>%
  filter(place == 'Ilha Josinha') %>%
  mutate(age_group = ifelse(age < 12, '0-0.99',
                            ifelse(age < 60, '1-4.99',
                                   ifelse(age >= 60, '5+', NA)))) %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year, year_month, month, age_group) %>%
  summarise(visits = n(),
            malaria = length(which(malaria))) %>%
  ungroup %>%
  mutate(p = malaria / visits * 100) %>%
  filter(year >= 2010)

ggplot(data = x, #%>% filter(age_group != '5+'),
       aes(x = year_month,
           y = malaria,
           group = age_group,
           color = age_group)) +
  geom_line() +
  geom_point() +
  theme_cism() +
  labs(x = 'Age group',
       y = 'Monthly malaria cases')

ggsave('~/Desktop/monthly.png')

by_month <- 
  x %>%
  group_by(month, year, age_group) %>%
  summarise(malaria = sum(malaria)) %>% 
  ungroup

by_month$year <- factor(by_month$year)
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(by_month$year)))
ggplot(data = by_month,
       aes(x = month,
           y = malaria,
           group = year,
           color = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~age_group) +
  scale_color_manual(name = 'Year',
                     values = cols) +
  geom_vline(xintercept = 4, lty = 2) +
  theme_cism()
ggsave('~/Desktop/monthly_overlapping_years.png')


by_year <- x %>%
  group_by(year, age_group) %>%
  summarise(malaria = sum(malaria)) %>%
  ungroup

ggplot(data = by_year,
       aes(x = year,
           y = malaria,
           color= age_group)) +
  geom_point() +
  geom_line() +
  theme_cism() +
  labs(x = 'Year',
       y = 'Malaria cases',
       title = 'Yearly malaria cases') +
  scale_color_manual(name = 'Age group',
                     values = c('red', 'orange', 'blue'))
ggsave('~/Desktop/yearly.png')
