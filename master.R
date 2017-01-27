library(tidyverse)
df <- read_csv('data/cleaned/cases.csv')
precip <- read_csv('weather_data/aggregated/precipitation_weekly.csv')

df$month <- as.numeric(format(df$date, '%m'))
df$day <- as.numeric(format(df$date, '%d'))

df <-
  left_join(x = df,
            y = precip,
            by = c('district', 'year', 'week'))

write_csv(df, 'data/cleaned/cases_with_precipitation.csv')
