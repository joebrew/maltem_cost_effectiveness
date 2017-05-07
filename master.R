# Libraries
library(tidyverse)

# BES data
source('get_bes_data.R', encoding = "UTF-8")

# Precipitation data
source('get_weather_data.R')

# IRS data
source('get_irs_data.R')

# ITN data
source('get_itn_data.R')

# Get population into itn
itn <-
  left_join(x = itn,
            y = pop %>%
              group_by(year, district, province) %>%
              summarise(population = sum(population, na.rm = TRUE)) %>%
                          ungroup,
            by = c('province', 'district', 'year'))

# Get percentage coverage of itn
itn <-
  itn %>%
  mutate(itn_coverage = nets / population * 100) %>%
  dplyr::select(province, district, year, itn_coverage)

# Join irs/itn information to df
df <- left_join(x = df,
                y = itn,
                by = c('province',
                       'district', 'year'))

# Set to 0 those NAs for itn
df$itn_coverage[is.na(df$itn_coverage)] <- 0

# Get population information in irs
irs <-
  left_join(x = irs,
            y = pop %>%
              group_by(year, district, province) %>%
              summarise(population = sum(population, na.rm = TRUE)) %>%
              ungroup,
            by = c('province', 'district', 'year'))

# Get percentage coverage of irs
irs <-
  irs %>%
  mutate(irs_coverage_houses = irs_houses / population * 100,
         irs_coverage_people = irs_people / population * 100) 

# Make weeks instead of days since campaign end
irs$weeks_since_last_irs_campaign_end <- 
  round(irs$days_since_last_irs_campaign_end / 7)

# Narrow down
irs <- irs %>%
  dplyr::select(province, district, year, week,
                irs_houses, irs_people,
                weeks_since_last_irs_campaign_end,
                irs_coverage_houses,
                irs_coverage_people)

# Join irs data to df (bes + population + itn)
df <-
  left_join(x = df,
            y = irs,
            by = c("year", "week", "province", "district"))

# Join df (bes + population) to weather
df <-		
  left_join(x = df,		
            y = weather_weekly,		
            by = c('district', 'date'))

# Write data for sharing with collaborators
write_csv(df, 'data/outputs/cases_population_weather_itn_irs.csv')
write_csv(bes, 'data/outputs/cases_only.csv')
write_csv(pop, 'data/outputs/population.csv')
# Write csv for weather data
write_csv(weather_weekly, 'data/outputs/weather_weekly.csv')
write_csv(weather, 'data/outputs/weather_daily.csv')
# Write csv for itn data
write_csv(itn, 'data/outputs/itn.csv')
# Write csv for irs data
write_csv(irs, 'data/outputs/irs.csv')


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
