# Packages
library(tidyverse)

# Source helpers
source('helpers.R')

# If the data has already been processed, simply read it in
if('mozambican_weather.csv' %in% dir('noaa_data')){
  weather <- read_csv('noaa_data/mozambican_weather.csv')
} else {
  
  # Read in data
  a <- read_csv('noaa_data/CDO5060047274390.txt')
  counter <- 1
  for (j in 1:ncol(a)){
    if(is.na(names(a)[j])){
      names(a)[j] <- letters[counter]
      counter <- counter + 1
    }
  }
  names(a)[1] <- 'USAF'
  
  # Read in station info
  b <- read_table('noaa_data/isd-history.txt', skip = 20)
  b$USAF <- as.numeric(b$USAF)
  
  # Join
  noaa <- left_join(a, b,
                    by = c("USAF", "WBAN"))
  rm(a,b)
  
  # Make date column
  noaa$date <- as.Date(paste0(substr(noaa$YEARMODA,start = 1, stop = 4),
                              '-',
                              substr(noaa$YEARMODA,start = 5, stop = 6),
                              '-',
                              substr(noaa$YEARMODA,start = 7, stop = 8)))
  
  # Make lowercase column names
  names(noaa) <- tolower(names(noaa))
  
  # Keep only columns of interest
  noaa <- 
    noaa %>%
    dplyr::select(`station name`,
                  date,
                  temp,
                  dewp,
                  wdsp,
                  mxspd,
                  max,
                  min,
                  prcp,
                  lat,
                  lon)
  
  # Rename
  noaa <-
    noaa %>%
    rename(station_name = `station name`)
  
  # Clean up NAs
  noaa <- data.frame(noaa)
  for (j in 3:ncol(noaa)){
    noaa[,j] <- ifelse(detect_noaa_na(noaa[,j]),
                       NA,
                       noaa[,j])
  }
  
  # Convert to number
  convert_to_number <-
    function(x){
      x <- regmatches(x, gregexpr("[[:digit:]]+", x))
      if(length(unlist(x)) == 0){
        y <- NA
      } else {
        y <- lapply(x, function(z){
          if(length(z) == 2){
            out <- as.numeric(paste0(z[1], '.', z[2]))
          } else {
            out <- unlist(z)[1]
          }
          return(out)
        })
      }
      return(as.numeric(unlist(y)))
    }
  
    
  # Clean up column types
  noaa <-
    noaa %>%
    mutate(max = convert_to_number(`max`),
           min = convert_to_number(`min`),
           prcp = convert_to_number(prcp))
  
  # Interpolate for our locations
  library(sp)
  x <- cism::moz2
  x <- x[x@data$NAME_1 %in% c('Maputo', 'Gaza'),]
  coords <- data.frame(coordinates(x))
  names(coords) <- c('x', 'y')
  coords$district <- x@data$NAME_2
  
  # Standardize names
  coords$district <- toupper(coords$district)
  coords$district[coords$district == 'CHÓKWÈ'] <- 'CHOKWE'
  coords$district[coords$district == 'GUIJÁ'] <- 'GUIJA'
  coords$district[coords$district == 'XAI-XAI'] <- 'XAI-XAI DISTRICT'
  coords$district[coords$district == 'MANHIÇA'] <- 'MANHICA'
  coords$district[coords$district == 'MATUTUÍNE'] <- 'MATUTUINE'
  # coords <- read_csv('public_data/coordinates_of_maputo_districts.csv')
  
  # For each location, get weather
  source('helpers.R')
  results <- list()
  for (i in 1:nrow(coords)){
    this_district <- coords$district[i]
    message('Estimating weather for: ', this_district)
    
    this_location <- coords[i,]
    this_weather <- get_weather_for_location(noaa = noaa,
                                             lng = this_location$x,
                                             lat = this_location$y)
    # Add in a column for the district
    this_weather$district <- this_district
    
    # Convert from fareinheit to celcius, inches to mm, and rename
    this_weather <- this_weather %>%
      mutate(temp = f_to_c(temp),
             temp_max = f_to_c(max),
             temp_min = f_to_c(min),
             dew_point = f_to_c(dewp),
             precipitation = i_to_m(prcp),
             wind_speed = wdsp,
             wind_speed_max = mxspd) %>%
      # Keep only columns of interest
      dplyr::select(district, date,
                    precipitation,
                    temp, temp_max, temp_min,
                    dew_point, wind_speed,
                    wind_speed_max)
    # Combine results into a list
    results[[i]] <- this_weather
  }
  
  # Get into a dataframe
  weather <- bind_rows(results)
  
  # Write a csv for faster processing later
  write_csv(weather, 'noaa_data/mozambican_weather.csv')
}

# Get a date_helper
date_helper <- create_date_helper()

# Create weekly weather, using dates as the last saturday in the week
weather_weekly <- 
  weather %>%
  mutate(year = as.numeric(format(date, '%Y')),
         week = as.numeric(format(date, '%U'))) %>%
  dplyr::select(-date) %>%
  left_join(date_helper,
            by = c('year', 'week')) %>%
  group_by(date, district) %>%
  summarise(precipitation = mean(precipitation, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            temp_max = max(temp_max, na.rm = TRUE),
            temp_min = min(temp_min, na.rm = TRUE),
            dew_point = mean(dew_point, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            wind_speed_max = max(wind_speed_max, na.rm = TRUE)) %>%
  ungroup
  
rm(date_helper)

# Write a csv
write_csv(weather_weekly, 'data/outputs/weather_weekly.csv')
write_csv(weather, 'data/outputs/weather_daily.csv')
# library(scales)
# library(ggthemes)
# ggplot(data = weather %>% 
#          filter(district == 'MANHICA') %>% 
#          mutate(date = as.Date('2016-12-31') + day_number), 
#        aes(x = date, y = temp_max)) + 
#   geom_point(alpha = 0.3,
#              color = 'darkorange') +
#   scale_x_date(labels = date_format("%B")) +
#   labs(x = 'Fecha',
#        y = 'Temperatura maxima (C)',
#        title = 'Lo peor ya pasó',
#        subtitle = 'Temperature máxima en Manhiça, 2009-2016, interpolada de estaciones NOAA cercanas') +
#   geom_vline(xintercept = as.numeric(Sys.Date())) +
#   geom_label(data = data_frame(date = Sys.Date(),
#                                y = 22,
#                                label = 'Today'),
#              aes(x = date,
#                  y = y,
#                  label = label),
#              color = 'darkgreen',
#              alpha = 0.6) +
#   geom_hline(yintercept = seq(20, 40, 10),
#              lty = 2,
#              alpha = 0.2) +
#   geom_smooth(color = 'darkgreen') +
#   theme_cism()
# 
# 
# ggplot(data = weather %>% 
#          filter(district == 'MANHICA') %>% 
#          mutate(date = as.Date('2016-12-31') + day_number) %>%
#          mutate(date = as.Date(format(date, '%Y-%m-01'))) %>%
#          filter(date <= '2017-12-31') %>%
#          group_by(date) %>%
#          summarise(precipitation = mean(precipitation, na.rm = TRUE)), 
#        aes(x = date, y = precipitation)) + 
#   geom_bar(alpha = 0.7,
#            stat = 'identity',
#              fill = 'darkorange') +
#   scale_x_date(labels = date_format("%B")) +
#   labs(x = 'Mes',
#        y = 'Milimetros',
#        title = 'Soon, no more rain',
#        subtitle = 'Precipitación diaria media en Manhiça, 2009-2016, interpolada de estaciones NOAA cercanas') +
#   # geom_smooth(color = 'darkgreen') +
#   theme_cism()

# # Plot with mozambique
# library(cism)
# library(sp)
# cols <- ifelse(moz3@data$NAME_1 == 'Maputo',
#                'yellow',
#                ifelse(moz3@data$NAME_1 == 'Gaza',
#                       'orange',
#                       'white'))
# plot(moz3, col = cols,
#      border = adjustcolor('grey', alpha.f = 0.2))
# plot(moz0, add = TRUE)
# x <- noaa %>%
#   filter(!duplicated(lon, lat))
# points(x$lon, x$lat, 
#        col = adjustcolor('darkred', alpha.f = 0.6),
#        pch = 14)
# legend('bottomright',
#        col = c(NA, NA, adjustcolor('darkred', alpha.f = 0.6)),
#        fill = c('orange', 'yellow', NA),
#        border = NA,
#        legend = c('Gaza', 'Maputo', 'Data'),
#        bty = 'n',
#        pch = c(NA, NA, 14))
# title(main = 'Spatial distribution of available NOAA temperature data')
