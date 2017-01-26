library(raster)
setwd('weather_data/')
files <- dir()
for (i in 1:length(files)){
  this_file <- files[i]
  this_date <- gsub('.tif', '', this_file)
  r <- raster(this_file)
  values(r)[values(r) == -9999] <- NA
  plot(r, 
       main = this_date)
  message(this_date)
  Sys.sleep(2)
}
