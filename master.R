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

# Get which number irs campaign
cum_summer <- function(x){
  x[is.na(x)] <-0
  cumsum(x)
}
irs <- irs %>%
  arrange(district, year, week) %>%
  mutate(dummy = ifelse(weeks_since_last_irs_campaign_end == 1, 1, 0)) %>%
  group_by(district) %>%
  mutate(irs_campaign = cum_summer(dummy))  %>%
  mutate(irs_campaign = ifelse(weeks_since_last_irs_campaign_end == 0, 
                               irs_campaign + 1,
                               irs_campaign))

# Create a protection variable based on decline
irs <- irs %>%
  mutate(irs_protection = irs_protect(weeks_since_last_irs_campaign_end) * irs_coverage_people) %>%
  mutate(irs_protection = ifelse(irs_protection < 0, 0,
                                 ifelse(irs_protection > 100, 100,
                                        irs_protection))) %>%
  mutate(irs_protection = ifelse(is.na(irs_protection), 0, irs_protection))

# Remove unecessary variables
irs <- irs %>%
  dplyr::select(-dummy, -irs_campaign)

# # During an IRS campaign increase protection linearly (not working yet)
# x <-
#   irs %>%
#   # filter(!is.na(irs_campaign)) %>%
#   arrange(year, week) %>%
#   group_by(district, irs_campaign) %>%
#   mutate(irs_protection = ifelse(is.na(irs_protection),
#                                  dplyr::first(irs_protection[!is.na(irs_protection)]),
#                                  irs_protection)) %>%
#   mutate(irs_protection_campaign_start = dplyr::first(irs_protection[weeks_since_last_irs_campaign_end == 0]),
#          irs_portection_campaign_end = dplyr::first(irs_protection[weeks_since_last_irs_campaign_end == 1])) %>%
#   mutate(irs_protection_campaign_weeks = length(which(weeks_since_last_irs_campaign_end == 0))) %>%
#   mutate(irs_protection_increase = irs_portection_campaign_end - irs_protection_campaign_start) %>%
#   mutate(irs_protection_increase = ifelse(irs_protection_increase < 0,
#                                           0, 
#                                           irs_protection_increase)) %>%
#   mutate(irs_protection_weekly_increase = irs_protection_increase / irs_protection_campaign_weeks) %>%
#   group_by(district, irs_campaign) %>%
#   mutate(x = irs_protection_campaign_start + cumsum(irs_protection_weekly_increase))

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

# Make irs NAs be 0
df <- df %>%
  mutate(irs_protection = ifelse(is.na(irs_protection),
                                 0,
                                 irs_protection)) 

# Get distance to south africa
source('get_distance_to_border.R')
df <- 
  left_join(x = df,
            y = distances_to_border,
            by = 'district')

# Make an older and younger dataset
df_young <-df %>% filter(age_group == '0-4')
df_old <- df %>% filter(age_group == '5+')

# Make an aggregated/pooled dataset
df_agg <- df %>%
  group_by(year, week, date, month, day,
           province, district, disease) %>%
  summarise(x = n(),
            cases = sum(cases),
            population = sum(population),
            itn_coverage = first(itn_coverage),
            irs_houses = first(irs_houses),
            irs_people = first(irs_people),
            weeks_since_last_irs_campaign_end = first(weeks_since_last_irs_campaign_end),
            irs_coverage_houses = first(irs_coverage_houses),
            irs_coverage_people = first(irs_coverage_people),
            irs_protection = first(irs_protection),
            precipitation = first(precipitation),
            temp = first(temp),
            temp_max = first(temp_max),
            temp_min = first(temp_min),
            dew_point = first(dew_point),
            wind_speed = first(wind_speed),
            wind_speed_max = first(wind_speed_max),
            distance_to_land_border = first(distance_to_land_border))

# # # Write data for sharing with collaborators
# write_csv(df_agg, 'data/outputs/cases_population_weather_itn_irs_pooled_age_groups.csv')
# write_csv(df_young, 'data/outputs/cases_population_weather_itn_irs_young_only.csv')
# write_csv(df_old, 'data/outputs/cases_population_weather_itn_irs_old_only.csv')
# write_csv(df, 'data/outputs/cases_population_weather_itn_irs.csv')
# write_csv(bes, 'data/outputs/cases_only.csv')
# write_csv(pop, 'data/outputs/population.csv')
# # Write csv for weather data
# write_csv(weather_weekly, 'data/outputs/weather_weekly.csv')
# write_csv(weather, 'data/outputs/weather_daily.csv')
# # write_csv(wide_weather, 'data/outputs/weather_wide.csv')
# # Write csv for itn data
# write_csv(itn, 'data/outputs/itn.csv')
# # Write csv for irs data
# write_csv(irs, 'data/outputs/irs.csv')

# # Join with wide weather
# df_wide <- 
#   left_join(x = df,
#             y = wide_weather)
# # Write wide weather too
# write_csv(df_wide, 'data/outputs/cases_population_weather_wide_itn_irs.csv')

# ggplot(data = df_agg,
#        aes(x = date,
#            y = irs_protection)) +
#   geom_line(color = 'darkred',
#             alpha = 0.6) +
#   facet_wrap(~district) +
#   labs(x = 'Date',
#        y = 'Protection score',
#        title = 'IRS protection by district') +
#   ggthemes::theme_hc()