# Libraries
library(tidyverse)

# BES data
source('get_bes_data.R', encoding = "UTF-8")

# Precipitation data
source('get_weather_data.R')

# IRS data
source('get_irs_data.R')

df <-		
  left_join(x = df,		
            y = weather,		
            by = c('district', 'date'))

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
  ungroup %>%
  mutate(irs_coverage = people_irs / district_population * 100) %>%
  # Remove unecessary variables
  dplyr::select(-people_irs, -district_population)

# Save a copy
write_csv(df, 'data/outputs/master.csv')

# 
# 
# library(cism)
# ggplot(data = df,
#        aes(x = date,
#            y = precipitation)) +
#   geom_line(alpha = 0.8,
#             color = 'darkgreen') +
#   facet_wrap(~district) +
#   labs(x = 'Date',
#        y = 'Milimeters',
#        title = 'Rainfall',
#        subtitle = 'Province of Maputo') +
#   theme_cism()
# 
# 
# cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$year)))
# ggplot(data = df %>%
#          mutate(day_of_year = as.numeric(format(date, '%j'))),
#        aes(x = day_of_year,
#            y = precipitation)) +
#   geom_line(alpha = 0.8,
#             aes(color = factor(year))) +
#   facet_wrap(~district) +
#   labs(x = 'Day of year',
#        y = 'Milimeters',
#        title = 'Rainfall',
#        subtitle = 'Province of Maputo') +
#   theme_bw() +
#   scale_color_manual(name = 'Year',
#                      values = cols)
# 
# 
# # IRS coverage
# x <- df %>%
#   group_by(district, year) %>%
#   summarise(irs_coverage = first(irs_coverage))
# 
# ggplot(data = x,
#        aes(x = year, 
#            y = irs_coverage)) +
#   geom_bar(stat = 'identity',
#            pos = 'dodge',
#            fill = 'darkorange', alpha = 0.6) +
#   facet_wrap(~district) +
#   theme_cism() +
#   xlab('Year') +
#   ylab('IRS coverage (%) of population') +
#   geom_label(aes(label = paste0(round(irs_coverage, digits = 1), '%')),
#              size = 3,
#              alpha = 0.6) +
#   labs(title = 'IRS coverage in Maputo province by district',
#        subtitle = 'Number of people considered protected as % of total population')

# # Plot malaria risk
# ggplot(data = df,
#        aes(x = date,
#            y = malaria_risk)) +
#   geom_line(alpha = 0.3) +
#   facet_wrap(~district) +
#   xlab('Date') +
#   ylab('Malaria risk') +
#   ggtitle('Estimated climatological risk of malaria',
#           'By district') +
#   geom_line(data = df %>%
#               group_by(district) %>%
#               mutate(p = p / max(p)) %>%
#               ungroup,
#             aes(x = date,
#                 y = p),
#             color = 'red',
#             alpha = 0.5)
