# Define function for creating date helper
create_date_helper <- 
  function(){
    message('Standardizing dates so that all weekly data use ',
            'the date of the Saturday in that week (weeks are Sunday-Saturday')
    # Standardize dates (use dates ending on Saturday of each week)
    date_helper <- 
      data_frame(date = seq(as.Date('2009-01-01'),
                            as.Date('2017-12-31'),
                            by = 1))
    date_helper$dow <- weekdays(date_helper$date)
    date_helper <-
      date_helper %>%
      mutate(year = as.numeric(format(date, '%Y')),
             week = as.numeric(format(date, '%U')))
    
    date_helper <- date_helper %>%
      group_by(year, week) %>%
      mutate(has_saturday = 'Saturday' %in% dow) %>%
      filter(has_saturday) %>%
      ungroup %>%
      dplyr::select(-has_saturday)
    
    date_helper$new_date <- as.Date(NA)
    for (i in 1:nrow(date_helper)){
      counter <- i
      this_dow <- date_helper$dow[counter]
      while(this_dow != 'Saturday'){
        counter <- counter + 1
        this_dow <- date_helper$dow[counter]
      }
      date_helper$new_date[i] <- as.Date(date_helper$date[counter])
    }
    date_helper <-
      date_helper %>%
      dplyr::select(new_date, year, week) %>%
      rename(date = new_date)
    date_helper <- date_helper[!duplicated(date_helper$date),]
    return(date_helper)
  }


# Define function to replace NAs (coded in noaa as 99.9 by NOAA)
detect_noaa_na <- function(x){
  x <- as.character(x)
  y <- gsub('.', '', x, fixed = TRUE)
  oks <- y == x
  out <- unlist(lapply(strsplit(y, ''), function(x){all(x == '9')}))
  out[oks] <- FALSE
  out
}

# Define functions for converting from farenheit to celcius
f_to_c <- function(x){
  x <- x - 32
  x <- x * (5/9)
  x
}

# Define function for converting from inches to milimeters
i_to_m <- function(x){
  x <- x * 25.4
  x
}

# Define function to calculate distance from each district centroid to the weather stations
get_distance <- function (lon1, lat1, lon2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# Define function to get weather for certain location
get_weather_for_location <- function(noaa,
                                     lng, 
                                     lat){
  out <- noaa
  y <- lat
  x <- lng
  
  # Get distance to all locations
  out$distance <- NA
  for (i in 1:nrow(out)){
    out$distance[i] <-
      get_distance(lon1 = lng,
                   lat1 = lat,
                   lon2 = out$lon[i],
                   lat2 = out$lat[i])
  }
  
  # Create a weight column
  out$weight <- 1 / out$distance 
  out$weight[is.infinite(out$weight)] <- 1
  
  # Group by date and get weighted averages
  out <- 
    out %>%
    group_by(date) %>%
    summarise(lat = y,
              lon = x,
              temp = weighted.mean(temp, w = weight, na.rm = TRUE),
              dewp = weighted.mean(dewp, w = weight, na.rm = TRUE),
              wdsp = weighted.mean(wdsp, w = weight, na.rm = TRUE),
              mxspd = weighted.mean(mxspd, w = weight, na.rm = TRUE),
              max = weighted.mean(max, w = weight, na.rm = TRUE),
              min = weighted.mean(min, w = weight, na.rm = TRUE),
              prcp = weighted.mean(prcp, w = weight, na.rm = TRUE))
  return(out)
}
