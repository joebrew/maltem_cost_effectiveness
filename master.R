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
