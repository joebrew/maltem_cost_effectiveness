# Libraries
library(tidyverse)

# BES data
source('get_bes_data.R', encoding = "UTF-8")

# Weather data
source('get_weather_data.R')

# # Lagged weather data
# source('get_wide_weather.R')

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

# Make an older and younger dataset
df_young <-df %>% filter(age_group == '0-4')
df_old <- df %>% filter(age_group == '5+')

# Write data for sharing with collaborators
write_csv(df_young, 'data/outputs/cases_population_weather_itn_irs_young_only.csv')
write_csv(df_old, 'data/outputs/cases_population_weather_itn_irs_old_only.csv')

write_csv(df, 'data/outputs/cases_population_weather_itn_irs.csv')
write_csv(bes, 'data/outputs/cases_only.csv')
write_csv(pop, 'data/outputs/population.csv')
# Write csv for weather data
write_csv(weather_weekly, 'data/outputs/weather_weekly.csv')
write_csv(weather, 'data/outputs/weather_daily.csv')
# write_csv(wide_weather, 'data/outputs/weather_wide.csv')
# Write csv for itn data
write_csv(itn, 'data/outputs/itn.csv')
# Write csv for irs data
write_csv(irs, 'data/outputs/irs.csv')

# # Join with wide weather
# df_wide <- 
#   left_join(x = df,
#             y = wide_weather)
# # Write wide weather too
# write_csv(df_wide, 'data/outputs/cases_population_weather_wide_itn_irs.csv')


# # Make a plot of Magude over time
# x <- df %>%
#   filter(district == 'MAGUDE') %>%
#   group_by(date, disease) %>% 
#   summarise(cases = sum(cases),
#             population = sum(population)) %>%
#   ungroup %>%
#   mutate(k = cases / population * 1000)
# 
# library(cism)
# ggplot(data = x %>%
#          filter(disease == 'MALARIA'),
#        aes(x = date,
#            y = k)) +
#   # geom_point() +
#   geom_line(color = 'darkorange',
#             alpha = 0.8) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Weekly incidence per 1,000',
#        title = 'Malaria cases',
#        subtitle = 'Magude, Mozambique')
# 
# 
# # Currency
# owd <- getwd()
# setwd('../lendable/app')
# library(lendable)
# mzn <- fetch_db(query = "SELECT * FROM currency WHERE currency_type ='MZN'")
# setwd(owd)
# # Get to value of mzn
# mzn$mzn <- 1/ mzn$usd 
# mzn <- mzn %>%
#   filter(date >= as.Date('2010-01-01'),
#          mzn <= 0.15)
# ggplot(data = mzn,
#        aes(x = date,
#            y = mzn)) +
#   geom_line(color = 'darkorange',
#             alpha = 0.8) +
#   labs(x = 'Date',
#        y = '?',
#        title = 'Huge reduction',
#        subtitle = 'Thanks, MALTEM!') +
#   theme_cism() 
# 
# # Map of gaza and maputo
# gm <- cism::moz1_fortified %>%
#   filter(id %in% c('Maputo', 'Gaza'))
# ggplot() +
#   geom_polygon(data = gm,
#                aes(x = long,
#                    y = lat,
#                    group = group,
#                    fill = id),
#                alpha = 0.7) +
#   geom_polygon(data = mag3_fortified,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                fill = 'red') +
#   theme_cism_map() +
#   coord_map() +
#   scale_fill_manual(name = '',
#                     values = c('black', 'grey'))
# 
# # Create model
# model_data <- df %>%
#   filter(district == 'MAGUDE',
#          disease == 'MALARIA') %>%
#   group_by(date, disease) %>% 
#   summarise(cases = sum(cases),
#             population = sum(population)) %>%
#   ungroup %>%
#   mutate(k = cases / population * 1000) %>%
#   mutate(month = format(date, '%B'))
# fit <- lm(k ~ month + date, data = model_data %>% filter(date <= '2015-06-01'))
# model_data$predicted <- predict(fit, model_data)
# model_data <- model_data %>%
#   dplyr::select(date, k, predicted) %>%
#   rename(observed = k)
# # make long
# long <- model_data %>%
#   gather(key,
#          value,
#          observed:predicted)
# 
# ggplot(data = long,
#        aes(x = date,
#            y = value,
#            color = key)) +
#   geom_line(size = 1.5,
#             alpha = 0.8) +
#   scale_y_sqrt() +
#   theme_cism() +
#   scale_color_manual(name = '',
#                      values = c('darkgreen', 'darkorange')) +
#   geom_hline(yintercept = 0) +
#   labs(x = 'Date',
#        y = 'Cases (scale: sqrt)',
#        title = 'Observed vs. predicted',
#        subtitle = 'Predicted = no MALTEM ')



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
