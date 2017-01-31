# Libraries
library(tidyverse)

# BES data
source('get_bes_data.R')

# Precipitation data
source('get_weather_data.R')

# IRS data
source('get_irs_data.R')

# Join malaria cases with precipitation
df <-
  left_join(x = df,
            y = precipitation_weekly,
            by = c('district', 'year', 'week'))

# Join with IRS
df <- 
  left_join(x = df,
            y = irs %>%
              dplyr::select(-houses) %>%
              rename(people_irs = people),
            by = c('district', 'year')) 

# Get IRS coverage
df <- df %>%
  group_by(district, year, week) %>%
  mutate(district_population = sum(population, na.rm = TRUE)) %>%
  mutate(irs_coverage = people_irs / district_population * 100)


write_csv(df, 'data/outputs/master.csv')

library(cism)
ggplot(data = df,
       aes(x = date,
           y = precipitation)) +
  geom_line(alpha = 0.8,
            color = 'darkgreen') +
  facet_wrap(~district) +
  labs(x = 'Date',
       y = 'Milimeters',
       title = 'Rainfall',
       subtitle = 'Province of Maputo') +
  theme_cism()


cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$year)))
ggplot(data = df %>%
         mutate(day_of_year = as.numeric(format(date, '%j'))),
       aes(x = day_of_year,
           y = precipitation)) +
  geom_line(alpha = 0.8,
            aes(color = factor(year))) +
  facet_wrap(~district) +
  labs(x = 'Day of year',
       y = 'Milimeters',
       title = 'Rainfall',
       subtitle = 'Province of Maputo') +
  theme_bw() +
  scale_color_manual(name = 'Year',
                     values = cols)


# IRS coverage
x <- df %>%
  group_by(district, year) %>%
  summarise(irs_coverage = mean(irs_coverage))

ggplot(data = x,
       aes(x = year, 
           y = irs_coverage,
           color = district,
           group = district)) +
  geom_line()
