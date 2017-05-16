# This should be run after "get_weather_data.R"
# It assumes object in memory from that
# source('get_weather_data.R')

library(tidyverse)



# Generate a wide weather dataset
if('mozambican_weather_wide.csv' %in% dir('noaa_data/')){
  wide_weather <- read_csv('noaa_data/mozambican_weather_wide.csv')
} else {
  wide_weather <-
    make_weather_wide(weather = weather_weekly)
  save.image('~/Desktop/backup.RData')
  
  # Write csv
  readr::write_csv(wide_weather, 
                   'noaa_data/mozambican_weather_wide.csv')
}

