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
  create.dir('weather_data')
}

for (i in 1:length(dates)){
  this_date <- dates[i]
  message(this_date)
  try({
    # Define a file name
    file_name <- paste0('weather_data/', this_date, '.RData')
    # Skip if the data is already there
    if(!file.exists(file_name)){
      this_url <- create_url(this_date)
      # Download file
      download.file(url = this_url,
                    destfile = 'temp.tif.gz')
      # Extract
      R.utils::gunzip('temp.tif.gz')
      # Read as raster
      r <- raster('temp.tif')
      # Save
      save(r,
           file = file_name)
      values(r)[values(r) == -9999] <- NA
      # Remove the old stuff
      file.remove('temp.tif')
      file.remove('temp.tif.gz')
    }
  })
}
