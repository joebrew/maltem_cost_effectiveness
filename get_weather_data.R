# https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&resolution=40

# Packages
library(tidyverse)
library(sp)

# Source helpers
source('helpers.R')
source('weather_functions.R')

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
  
  # Since noaa has some missing days, interpolate
  left <- expand.grid(station_name = sort(unique(noaa$station_name)),
                      date = sort(unique(noaa$date)))
  noaa <- left_join(left,
                    noaa,
                    by = c('station_name', 'date'))
  # Flag estimations
  noaa$estimated <- ifelse(is.na(noaa$lat), TRUE, FALSE)
  # Performance interpolation
  x <-
    noaa %>%
    arrange(date) %>%
    group_by(station_name) %>%
    mutate(temp = zoo::na.approx(object = temp,
                                 x = date,
                                 na.rm = FALSE),
           dewp = zoo::na.approx(object = dewp,
                                 x = date,
                                 na.rm = FALSE),
           wdsp = zoo::na.approx(object = wdsp,
                                 x = date,
                                 na.rm = FALSE),
           mxspd = zoo::na.approx(object = mxspd,
                                 x = date,
                                 na.rm = FALSE),
           max = zoo::na.approx(object = max,
                                 x = date,
                                 na.rm = FALSE),
           min = zoo::na.approx(object = min,
                                 x = date,
                                 na.rm = FALSE),
           prcp = zoo::na.approx(object = prcp,
                                 x = date,
                                 na.rm = FALSE))
  
  # Fix missing lat/lons
  ll <- noaa %>%
    group_by(station_name) %>%
    summarise(lat = dplyr::first(lat[!is.na(lat)]),
              lon = dplyr::first(lon[!is.na(lon)]))
  
  noaa <- noaa %>%
    dplyr::select(-lat,
                  -lon) %>%
    left_join(ll,
              by = 'station_name')
  
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
  coords$district[coords$district == 'MANDLAKAZI'] <- 'MANJACAZE'
  
  # Add a row for matola
  matola <- data_frame(x = 32.46580,
                       y = -25.92391,
                       district = 'MATOLA')
  coords <- bind_rows(coords, 
                      matola)
  # Add a row for xai-xai city
  xxc <- coords %>% filter(district == 'XAI-XAI DISTRICT') %>%
    mutate(district = 'XAI-XAI CITY')
  coords <- bind_rows(coords,
                      xxc)
  # Get matola
  # coords2 <- read_csv('public_data/coordinates_of_maputo_districts.csv')
  
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


