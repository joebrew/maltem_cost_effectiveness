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
# Make spatial
coords <- coords %>%
  mutate(lng = x,
         lat = y)
coordinates(coords) <- ~x+y
proj4string(coords) <- proj4string(cism::moz3)

# Get South Africa and Swaziland
sa <- cism::africa
sa <- sa[sa@data$COUNTRY %in% c('South Africa', 'Swaziland'),]

# Get projected versions
sa_proj <- spTransform(sa,
                       CRS("+init=epsg:3347"))
coords_proj <- spTransform(coords,
                           CRS("+init=epsg:3347"))

# Get distance to south Africa
coords@data$distance_to_land_border <- NA
for (i in 1:nrow(coords)){
  this_distance <- rgeos::gDistance(coords_proj[i,], sa_proj)
  coords@data$distance_to_land_border[i] <- this_distance / 10000
}
distances_to_border <- coords@data
distances_to_border <-
  distances_to_border %>%
  dplyr::select(-lat, -lng)
