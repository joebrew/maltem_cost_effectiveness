# Libraries
library(tidyverse)

# BES data
source('get_bes_data.R', encoding = "UTF-8")

# Precipitation data
source('get_weather_data.R')

# IRS data
source('get_irs_data.R')

# Join malaria cases with precipitation and weather
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
  ungroup %>%
  mutate(irs_coverage = people_irs / district_population * 100) %>%
  # Remove unecessary variables
  dplyr::select(-people_irs, -district_population)

# Save a copy
write_csv(df, 'data/outputs/master.csv')
# 
# # Generate a wide dataset
# if(file.exists('data/outputs/master_wide.csv')){
#   df_wide <- read_csv('data/outputs/master_wide.csv')
# } else {
#   df_wide <- df
#   
#   districts <- sort(unique(df$district))
#   years <- sort(unique(df$year))
#   
#   # Generate irs lag
#   df_wide$irs_coverage_last_year <- NA
#   for (i in 1:nrow(df)){
#     this_year <- df$year[i]
#     this_district <- df$district[i]
#     df_wide$irs_coverage_last_year[i] <-
#       df_wide$irs_coverage[df_wide$district == this_district &
#                         df_wide$year == this_year - 1][1]
#   }
#   
#   # Generate precipitation variables
#   starter <- seq(120, 1, by = -10)
#   ender <- seq(1, 120, by = 10)
#   
#   # Create empty columns
#   for (s in starter){
#     for (e in ender){
#       if(s >= e){
#         df_wide[,paste0('precipitation_from_',
#                    s, '_to_',
#                    e, '_days_ago')] <- 
#           NA
#         df_wide[,paste0('max_precipitation_from_',
#                    s, '_to_',
#                    e, '_days_ago')] <- 
#           NA
#         df_wide[,paste0('min_precipitation_from_',
#                    s, '_to_',
#                    e, '_days_ago')] <- 
#           NA
#         df_wide[,paste0('rainy_days_from_',
#                    s, '_to_',
#                    e, '_days_ago')] <- 
#           NA
#         df_wide[,paste0('dry_days_from_',
#                    s, '_to_',
#                    e, '_days_ago')] <- 
#           NA
#       }
#     }
#   }
#   
#   # Populate emtpy columns
#   for (s in starter){
#     for (e in ender){
#       message(paste0(s, '  ', e))
#       if(s >= e){
#         for (i in 1:nrow(df_wide)){
#           this_district <- df_wide$district[i]
#           this_date <- df_wide$date[i]
#           
#           the_data <-
#             precipitation_daily %>%
#             filter(date >= (this_date - s),
#                    date <= (this_date - e),
#                    district == this_district) %>%
#             summarise(the_sum = sum(precipitation),
#                       the_max = max(precipitation),
#                       the_min = min(precipitation),
#                       dry_days = length(which(precipitation == 0)),
#                       wet_days = length(which(precipitation > 0))) 
#           
#           # Plug in the results
#           df_wide[i,paste0('precipitation_from_',
#                       s, '_to_',
#                       e, '_days_ago')] <- 
#             the_data$the_sum
#           df_wide[i,paste0('max_precipitation_from_',
#                       s, '_to_',
#                       e, '_days_ago')] <- 
#             the_data$the_max
#           df_wide[i,paste0('min_precipitation_from_',
#                       s, '_to_',
#                       e, '_days_ago')] <- 
#             the_data$the_min
#           df_wide[i,paste0('rainy_days_from_',
#                       s, '_to_',
#                       e, '_days_ago')] <- 
#             the_data$wet_days
#           df_wide[i,paste0('dry_days_from_',
#                       s, '_to_',
#                       e, '_days_ago')] <- 
#             the_data$dry_days
#         }
#       }
#     }
#   }
#   # Remove 2010 rows
#   df_wide <-
#     df_wide %>%
#     filter(year >= 2011)
#   
#   # Write a csv
#   write_csv(df_wide, 'data/outputs/master_wide.csv')
# }
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
