library(raster)
library(R.utils)
library(tidyverse)
library(readr)

if(file.exists("weather_data/aggregated/precipitation_daily.csv") &
   file.exists("weather_data/aggregated/precipitation_weekly.csv") &
   file.exists("weather_data/aggregated/precipitation_monthly.csv")){
  precipitation_daily <- read_csv("weather_data/aggregated/precipitation_daily.csv")
  precipitation_weekly <- read_csv("weather_data/aggregated/precipitation_weekly.csv")
  precipitation_monthly <- read_csv("weather_data/aggregated/precipitation_monthly.csv")
  
} else {
  
  # Define function for creating link to data
  create_url <- function(date = '2010-01-01'){
    date <- as.Date(date)
    url <- paste0("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/",
                  format(date, '%Y'),
                  "/chirps-v2.0.",
                  format(date, '%Y'),
                  ".",
                  format(date, '%m'),
                  ".",
                  format(date, "%d"),
                  ".tif.gz")
    return(url)
  }
  
  dates <- seq(as.Date('2010-01-01'),
               as.Date('2016-12-31'),
               by = 1)
  
  # If no weather_data dir, create on
  if(!dir.exists('weather_data')){
    dir.create('weather_data')
  }
  
  for (i in 1:length(dates)){
    this_date <- dates[i]
    start_time <- Sys.time()
    try({
      # Define a file name
      file_name <- paste0('weather_data/', this_date, '.tif')
      # Skip if the data is already there
      if(!file.exists(file_name)){
        this_url <- create_url(this_date)
        # Remove the old stuff
        file.remove('temp.tif')
        file.remove('temp.tif.gz')
        # Download file
        download.file(url = this_url,
                      destfile = 'temp.tif.gz')
        # Extract
        R.utils::gunzip('temp.tif.gz')
        # Move
        file.copy(from = 'temp.tif',
                  to = file_name)
        message('---------------------------------')
        end_time <- Sys.time()
        message(paste0('That took ',
                       round(as.numeric(end_time - start_time), digits = 2),
                       ' seconds.'))
      }
    })
  }
  
  # Aggregate weather data
  ###########################
  
  ## Get a map of maputo province
  # library(cism)
  # maputo <- moz2
  # maputo <- maputo[which(maputo@data$NAME_1 == 'Maputo'),]
  # centroids <-
  #   data.frame(district = toupper(maputo@data$NAME_2),
  #              x = coordinates(maputo)[,1],
  #              y = coordinates(maputo)[,2])
  
  # Get the centroid of each district in maputo province
  if('coordinates_of_maputo_districts.csv' %in% dir('public_data')){
    centroids <- read_csv('public_data/coordinates_of_maputo_districts.csv')
  } else {
    library(ggmap)
    centroids <- data_frame(district = c('BOANE',
                                         'MAGUDE',
                                         'MANHICA',
                                         'MARRACUENE',
                                         'MATOLA',
                                         'MATUTUINE',
                                         'MOAMBA',
                                         'NAMAACHA'))
    centroids$y <- centroids$x <- NA
    for (i in 1:nrow(centroids)){
      coords <- geocode(location = paste0(centroids$district[i], 
                                          ' DISTRICT, MOZAMBIQUE'))
      centroids$x[i] <- coords[,1]
      centroids$y[i] <- coords[,2]
      write_csv(centroids, 'public_data/coordinates_of_maputo_districts.csv')
    }
  }
  
  
  # Get a place to store aggregated data
  if(!dir.exists('weather_data/aggregated')){
    dir.create('weather_data/aggregated')
  }
  
  # Read each of the files
  files <- dir('weather_data/')
  files <- files[grepl('.tif', files, fixed = TRUE)]
  
  # Go into weather data
  setwd('weather_data/')
  if(file.exists('aggregated/precipitation_daily.csv') &
     file.exists('aggregated/precipitation_weekly.csv') &
     file.exists('aggregated/precipitation_monthly.csv')){
    
    precipitation_daily <- read_csv('aggregated/precipitation_daily.csv')
    precipitation_weekly <- read_csv('aggregated/precipitation_weekly.csv')
    precipitation_monthly <- read_csv('aggregated/precipitation_monthly.csv')
  } else {
    
    # Read in each file and combine
    results <- list()
    for (i in 1:length(files)){
      this_file <- files[i]
      this_date <- as.Date(gsub('.tif', '', this_file, fixed = TRUE))
      r <- raster(this_file)
      
      # Extract the values
      x <- raster::extract(r, centroids[,2:3])
      
      # Create a dataframe output
      output <- centroids %>%
        dplyr::select(district)
      output$date <- this_date
      output$precipitation <- x
      results[[i]] <- output
      message(this_date)
    }
    
    # Add together all the results
    precipitation <- bind_rows(results)
    precipitation <- 
      precipitation %>%
      dplyr::select(date, district, precipitation)
    precipitation$year <- as.numeric(format(precipitation$date, '%Y'))
    precipitation$month <- as.numeric(format(precipitation$date, '%m'))
    precipitation$day <- as.numeric(format(precipitation$date, '%d'))
    precipitation$week <- as.numeric(format(precipitation$date, '%U')) + 1
    
    # Clean up names
    precipitation$district[precipitation$district == 'MANHIÇA'] <-
      'MANHICA'
    precipitation$district[precipitation$district == 'MATUTUÍNE'] <-
      'MATUTUINE'
    
    # Rename
    precipitation_daily <- precipitation

    
    # Define suitability for malaria,
    # according to 
    # http://www.mara-database.org/mara/docs/8.pdf
    
    # Get 30 day lag of rainfall
    districts <- sort(unique(precipitation_daily$district))
    results_list <- list()
    for (d in 1:length(districts)){
      this_district <- districts[d]
      these_data <- precipitation_daily %>%
        filter(district == this_district) %>%
        mutate(lag_30 = NA)
      for (i in 30:nrow(these_data)){
        these_data$lag_30[i] <-
          sum(these_data$precipitation[(i-30):i], na.rm = TRUE)
      }
      results_list[[d]] <- these_data
    }
    # Overwrite daily precipitaiton
    precipitation_daily <- bind_rows(results_list)
    
    # Define whether suitable (ie, greater than 80mm)
    precipitation_daily <-
      precipitation_daily %>%
      mutate(precipitation_risk = lag_30 >= 80)
    

    # Get temperature data 
    # (don't have district specific, but probably similar for whole area)
    get_weather <- function(station = "FQMA", # CDG, BGT, ATL, JFK
                            start_year = 2000,
                            end_year = 2016,
                            plot = FALSE,
                            save = TRUE,
                            load_saved = TRUE){
      
      require(data.table)
      
      # Define a filename
      file_name <- paste0('weather_',
                          station,
                          '_',
                          start_year,
                          '_',
                          end_year,
                          '.RData')
      
      if(load_saved & file_name %in% dir()){
        load(file_name)
      } else {
        
        # Format station name
        station <- toupper(gsub(" ", "%20", station))
        
        # Adjust dates
        start_date <- as.Date(paste0(start_year, '-01-01'))
        end_date <- as.Date(paste0(end_year, '-12-31'))
        if(end_date > Sys.Date()){
          end_date <- Sys.Date() - 1
        }
        
        # Parse date components
        start_day <- as.numeric(format(start_date, "%d"))
        start_month <- as.numeric(format(start_date, "%m"))
        start_year <- as.numeric(format(start_date, "%Y"))
        end_day <- as.numeric(format(end_date, "%d"))
        end_month <- as.numeric(format(end_date, "%m"))
        end_year <- as.numeric(format(end_date, "%Y"))
        
        # Get years
        years <- start_year:end_year
        
        # For each year, get the data and store in list
        results_list <- list()
        
        for (i in 1:length(years)){
          try({
            this_year <- years[i]
            this_start_month <- 1
            this_start_day <- 1
            if(this_year == end_year){
              this_end_month <- as.numeric(format(end_date, '%m'))
              this_end_day <- as.numeric(format(end_date, '%m'))
            } else {
              this_end_month <- 12
              this_end_day <- 31
            }
            # Define link format for airports
            link <- paste0("http://www.wunderground.com/history/airport/",
                           station,
                           "/", this_year, 
                           "/", this_start_month, 
                           "/", this_start_day, 
                           "/CustomHistory.html?dayend=", this_end_day, 
                           "&monthend=", this_end_month, 
                           "&yearend=", this_year, 
                           "&req_city=NA&req_state=NA&req_statename=NA&format=1")
            
            #     # Read in data from link
            df <- suppressWarnings(fread(link))
            names_df <- names(df)
            df <- data.frame(df)
            names(df) <- names_df
            
            # Keep only the first 20 columns (through cloud cover)
            df <- df[,1:21]
            
            # Fix date
            names(df)[1] <- 'date'
            df$date <- as.Date(df$date, format = '%Y-%m-%d')
            # Fix other names
            names(df) <-
              tolower(gsub(' |[/]', '_', names(df)))
            
            # Keep only certain columns
            df <- df[,!grepl('sea_level|visibility|wind|gust|dew', names(df))]
            
            #   # Standardize names
            names(df) <- c("date",
                           "temp_max",
                           "temp_mean",
                           "temp_min",
                           "humidity_max",
                           "humidity_mean",
                           "humidity_min",
                           "precipitation",
                           "cloud_cover")
            #   
            # Add a location column
            df$location <- toupper(as.character(station))
            
            # print url source
            message(paste0('Data retrieved for ', this_year))
            
            # Stick results into list
            results_list[[i]] <- df
          })
        }
        
        # Bind together results
        x <- do.call('rbind', results_list)
        
      }
      
      # Save if applicable
      if(save){
        save(x, file = file_name)
      }
      return(x)
    }
    setwd('weather_data/temperature/')
    temperature <- get_weather(station = "FQMA", 
                               start_year = 2009,
                               end_year = 2016) 
    setwd('../..')
    
    # Get weather risk
    temperature <-
      temperature %>%
      mutate(temperature_risk = 
               ifelse(temp_min <= 18 | 
                        temp_max >= 40,
                      0,
                      ifelse(temp_min <= 22 |
                               temp_max >= 32,
                             0.5,
                             1)))
      
    
    # Join to daily precipitation
    precipitation_daily <-
      precipitation_daily %>%
      left_join(temperature %>%
                  dplyr::select(-precipitation, -location),
                by = 'date')
    
    # Make a malaria risk 
    precipitation_daily <-
      precipitation_daily %>%
      mutate(malaria_risk = 
               (precipitation_risk +
               temperature_risk)  / 
               2)
    
    # Aggregate
    precipitation_weekly <- 
      precipitation_daily %>%
      group_by(year, week, district) %>%
      summarise(precipitation = sum(precipitation, na.rm = TRUE),
                precipitation_risk = length(which(precipitation_risk)) / n(),
                avg_temp_max = mean(temp_max, na.rm = TRUE),
                max_temp = max(temp_max, na.rm = TRUE),
                avg_temp_mean = mean(temp_mean, na.rm = TRUE),
                avg_temp_min = mean(temp_min, na.rm = TRUE),
                min_temp = min(temp_min, na.rm = TRUE),
                avg_humidity_max = mean(humidity_max, na.rm = TRUE),
                max_humidity = max(humidity_max, na.rm = TRUE),
                avg_humidity_mean = mean(humidity_mean, na.rm = TRUE),
                avg_humidity_min = mean(humidity_min, na.rm = TRUE),
                min_humidity = min(humidity_min, na.rm = TRUE),
                temperature_risk = mean(temperature_risk, na.rm = TRUE),
                malaria_risk = mean(malaria_risk, na.rm = TRUE))
    precipitation_monthly <- 
      precipitation_daily %>%
      group_by(year, month, district) %>%
      summarise(precipitation = sum(precipitation, na.rm = TRUE),
                precipitation_risk = length(which(precipitation_risk)) / n(),
                avg_temp_max = mean(temp_max, na.rm = TRUE),
                max_temp = max(temp_max, na.rm = TRUE),
                avg_temp_mean = mean(temp_mean, na.rm = TRUE),
                avg_temp_min = mean(temp_min, na.rm = TRUE),
                min_temp = min(temp_min, na.rm = TRUE),
                avg_humidity_max = mean(humidity_max, na.rm = TRUE),
                max_humidity = max(humidity_max, na.rm = TRUE),
                avg_humidity_mean = mean(humidity_mean, na.rm = TRUE),
                avg_humidity_min = mean(humidity_min, na.rm = TRUE),
                min_humidity = min(humidity_min, na.rm = TRUE),
                temperature_risk = mean(temperature_risk, na.rm = TRUE),
                malaria_risk = mean(malaria_risk, na.rm = TRUE))
    
      # Store the results
    write_csv(precipitation_daily, 'aggregated/precipitation_daily.csv')
    write_csv(precipitation_weekly, 'aggregated/precipitation_weekly.csv')
    write_csv(precipitation_monthly, 'aggregated/precipitation_monthly.csv')
    
    setwd('..')
  }
  
  
  
  # Remove unecessary objects
  rm(centroids,
     dates, file_name, files, i,
     start_time, this_date, create_url,
     temperature,
     these_data, d,
     districts, results_list, this_district, get_weather)
}


