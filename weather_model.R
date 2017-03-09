# BES data
source('get_bes_data.R', encoding = "UTF-8")

# Aggregate by age
df <- df %>%
  group_by(date, district) %>%
  summarise(year = first(year),
            month = first(month),
            week = first(week),
            day= first(day),
            cases = sum(cases),
            population = sum(population)) %>%
  mutate(p = cases / population,
         pk = cases / population * 1000)

# Precipitation data
source('get_weather_data.R')

# Get locations
coords <- read_csv('public_data/coordinates_of_maputo_districts.csv')

# Join to data
df <- left_join(x = df,
                y = coords,
                by = 'district')

# Rename
df$lng <- df$x
df$lat <- df$y

# Generate a wide weather dataset
if('wide_weather.csv' %in% dir()){
  wide_weather <- read_csv('weather_data/aggregated/wide_weather.csv')
} else {
  
  districts <- sort(unique(precipitation_weekly$district))
  years <- sort(unique(precipitation_weekly$year))
  
  # Generate precipitation variables
  starter <- seq(120, 0, by = -5)
  ender <- seq(0, 120, by = 5)
  
  counter <- 0
  total <- length(starter) * length(ender)
  # Create empty columns
  for (s in starter){
    for (e in ender){
      if(s >= e){
        precipitation_weekly[,paste0('precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('max_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('min_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('rainy_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('dry_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('min_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('max_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        precipitation_weekly[,paste0('avg_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
      }
    }
  }
  
  # Populate emtpy columns
  for (s in starter){
    for (e in ender){
      counter <- counter + 1
      if(s >= e){
        for (i in 1:nrow(precipitation_weekly)){
          message(paste0(counter,
                         ' of ',
                         total,
                         ': ',
                         i))
          
          this_district <- precipitation_weekly$district[i]
          this_date <- precipitation_weekly$date[i]
          
          the_data <-
            precipitation_daily %>%
            filter(date >= (this_date - s),
                   date <= (this_date - e),
                   district == this_district) %>%
            summarise(the_sum = sum(precipitation),
                      the_max = max(precipitation),
                      the_min = min(precipitation),
                      dry_days = length(which(precipitation == 0)),
                      wet_days = length(which(precipitation > 0)),
                      max_temp = max(temp_max),
                      min_temp = min(temp_min),
                      avg_temp = mean(temp_mean))
          
          # Plug in the results
          precipitation_weekly[i,paste0('precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_sum
          precipitation_weekly[i,paste0('max_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_max
          precipitation_weekly[i,paste0('min_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_min
          precipitation_weekly[i,paste0('rainy_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$wet_days
          precipitation_weekly[i,paste0('dry_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$dry_days
          precipitation_weekly[i,paste0('min_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$min_temp
          precipitation_weekly[i,paste0('max_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$max_temp
          precipitation_weekly[i,paste0('avg_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$avg_temp
        }
      }
    }
  }
  save.image('~/Desktop/backup.RData')
  # Remove 2010 rows
  precipitation_weekly <-
    precipitation_weekly %>%
    filter(year >= 2011)
  
  # Write csv
  wide_weather <- precipitation_weekly
  write_csv(wide_weather, 
            'weather_data/aggregated/wide_weather.csv')
}
  
 