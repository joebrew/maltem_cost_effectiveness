library(raster)
library(R.utils)

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
  message(this_date)
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
