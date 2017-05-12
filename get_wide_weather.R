# This should be run after "get_weather_data.R"
# It assumes object in memory from that
# source('get_weather_data.R')

library(tidyverse)

# Generate a wide weather dataset
if('mozambican_weather_wide.csv' %in% dir('noaa_data/')){
  wide_weather <- read_csv('noaa_data/mozambican_weather_wide.csv')
} else {
  
  # Trim down weekly weather 
  weather_weekly <- 
    weather_weekly %>%
    filter(!is.na(date)) %>%
    filter(date >= '2010-01-01')
  
  # Generate time ranges
  starter <- seq(100, 0, by = -10)
  ender <- seq(0, 100, by = 10)
  
  counter <- 0
  combos <- expand.grid(s = starter,
                        e = ender)
  combos <- combos %>% filter(s >= e)
  total <- nrow(combos)

  # Create empty columns
  for (s in starter){
    for (e in ender){
      if(s >= e){
        weather_weekly[,paste0('precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('min_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('rainy_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('dry_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('min_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_dew_point_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_wind_speed_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_wind_speed_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
      }
    }
  }
  
  # Populate those columns with real values
  for (s in starter){
    for (e in ender){
      
      if(s >= e){
        counter <- counter + 1
        
        nrww <- nrow(weather_weekly)
        for (i in 1:nrww){
          message(paste0('Combination ',
                         counter,
                         ' of ',
                         total,
                         ' combinations',
                         '. Date ',
                         i, 
                         ' of ',
                         nrww, 
                         ' dates.'
                         ))
          
          this_district <- weather_weekly$district[i]
          this_date <- weather_weekly$date[i]
          
          the_data <-
            weather %>%
            filter(date >= (this_date - s),
                   date <= (this_date - e),
                   district == this_district) %>%
            summarise(the_sum = sum(precipitation, na.rm = TRUE),
                      the_max = max(precipitation, na.rm = TRUE),
                      the_min = min(precipitation, na.rm = TRUE),
                      dry_days = length(which(precipitation == 0)),
                      wet_days = length(which(precipitation > 0)),
                      max_temp = max(temp_max, na.rm = TRUE),
                      min_temp = min(temp_min, na.rm = TRUE),
                      avg_temp = mean(temp, na.rm = TRUE),
                      avg_dew_point = mean(dew_point, na.rm = TRUE),
                      avg_wind_speed = mean(wind_speed, na.rm = TRUE),
                      wind_speed_max = max(wind_speed_max, na.rm = TRUE))
          
          # Plug in the results
          weather_weekly[i,paste0('precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_sum
          weather_weekly[i,paste0('max_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_max
          weather_weekly[i,paste0('min_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_min
          weather_weekly[i,paste0('rainy_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$wet_days
          weather_weekly[i,paste0('dry_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$dry_days
          weather_weekly[i,paste0('min_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$min_temp
          weather_weekly[i,paste0('max_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$max_temp
          weather_weekly[i,paste0('avg_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$avg_temp
          weather_weekly[i,paste0('avg_dew_point_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$avg_dew_point
          weather_weekly[i,paste0('avg_wind_speed_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$avg_wind_speed
          weather_weekly[i,paste0('max_wind_speed_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$wind_speed_max
        }
      }
    }
  }
  save.image('~/Desktop/backup.RData')

  # Write csv
  wide_weather <- weather_weekly
  readr::write_csv(wide_weather, 
            'noaa_data/mozambican_weather_wide.csv')
}

