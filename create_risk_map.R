# Prepare main data
source('master.R')

# Get locations
coords <- read_csv('public_data/coordinates_of_maputo_districts.csv')

# Join to data
df <- left_join(x = df,
                y = coords,
                by = 'district')

# Rename
df$lng <- df$x
df$lat <- df$y

# Get map of Maputo province
library(cism)
library(automap)
library(maptools)
map <- moz1
map <- map[map@data$NAME_1 == 'Maputo',]
map_fortified <- broom::tidy(map, region = 'NAME_1')

map2 <- moz2
map2 <- map2[map2@data$NAME_1 == 'Maputo',]
map2_fortified <- broom::tidy(map2, region = 'NAME_2')

# Define function for risk map
risk_map <- function(data,
                     map = map){
  
  # Jitter the data
  data <- data %>%
    mutate(lng = jitter(lng, factor = 50),
           lat = jitter(lat, factor = 50))
  
  # Group by location and get overall
  grouped <- data %>%
    group_by(lng, lat) %>%
    summarise(cases = sum(cases),
              population = sum(population)) %>%
    ungroup %>%
    mutate(pk = cases / population * 1000)
  
  # Perform kriging
  # Krieging
  sp_df <- grouped
  coordinates(sp_df) <- ~lng + lat
  proj4string(sp_df) <- proj4string(map)
  sp_df <- sp_df[!is.na(sp_df$pk),]
  sp_points <- SpatialPoints(sp_df)
  
  # Create a fine grid
  pixels_per_side = 150
  bottom.left = bbox(map)[,1]  #apply(sp_points@coords,2,min)
  top.right = bbox(map)[,2] #apply(sp_points@coords,2,max)
  margin = abs((top.right-bottom.left))/10
  bottom.left = bottom.left-margin
  top.right = top.right+margin
  pixel.size = abs(top.right-bottom.left)/pixels_per_side
  g = GridTopology(cellcentre.offset=bottom.left,
                   cellsize=pixel.size,
                   cells.dim=c(pixels_per_side,pixels_per_side))
  
  grid_points = SpatialPoints(g)
  proj4string(grid_points) <- proj4string(map)
  in_points = !is.na(over(grid_points,polygons(map)))
  fit_points = SpatialPoints(as.data.frame(grid_points)[in_points,])
  proj4string(fit_points) <- proj4string(map)
  
  # make projected before krieging
  new_pro <- CRS("+init=epsg:3036")
  proj4string(fit_points) <-
    proj4string(map) <- 
    proj4string(sp_df) <- new_pro
  
  # Do kriging
  krig = autoKrige(pk~1, sp_df, new_data=fit_points)
  interp_data = as.data.frame(krig$krige_output)
  colnames(interp_data) = c("longitude","latitude","pred","var","stdev")
  
  # Set up map plot
  map_base_aesthetics = aes(x=longitude, y=latitude, group=group)
  map_base = geom_polygon(data=map, map_base_aesthetics)
  borders = geom_polygon(data=map, map_base_aesthetics, color="black", fill=NA)
  
  nbin=20
  
  g <-
    ggplot(data=interp_data, aes(x=longitude, y=latitude)) + 
    geom_polygon(data = map_fortified, aes(x = long,
             y = lat,
             # z = p_positive,
             group = group),
             alpha = 0.6) +
    # coord_map() +
    geom_tile(aes(fill=pred),color=NA) +
    # stat_contour(aes(z=pred), bins=nbin, color="#999999") +
    scale_fill_gradient2(name = 'Hotspot\nindex',
                         # midpoint=mean(interp_data$pred),
                         limits = c(-1,7),
                         midpoint = 2,
                         low="white",
                         mid="yellow",
                         high="red") +
    # borders +
    theme_cism() +
    labs(title = '',
         subtitle = 'Interpolated average weekly incidence per 1,000, Krieging method',
         x = 'Longitude',
         y = 'Latitude')
  return(g)
}

# years <- 2010:2016
# for (i in 1:length(years)){
#   message(i)
#   data <- df %>%
#     filter(year == years[i])
#   r <- risk_map(data = data,
#            map = map) +
#     labs(title = years[i])
#   assign(paste0('g', i),
#          r)
# }

# Rmisc::multiplot(g1, 
#                  g2, 
#                  g3,
#                  g4,
#                  g5,
#                  g6,
#                  g7,
#                  cols = 2)

# Make one for each date
dates <- sort(unique(df$date))
dates <- dates[dates >= '2016-01-01']
for (i in 1:length(dates)){
  message(dates[i])
  data <- df %>%
    filter(date == dates[i])
  r <- risk_map(data = data,
                map = map) +
    labs(title = format(dates[i], '%B %d, %Y')) +
    geom_polygon(data = map2_fortified,
                 aes(x = long,
                     y = lat,
                     group = group),
                 color = 'black',
                 fill = NA)
  r
  ggsave(paste0('risk_map_animation/', dates[i], '.png'))
  # assign(paste0('g', i),
         # r)
}

# Static map
data <- df %>%
  filter(date >= '2016-12-15')
r <- risk_map(data = data,
              map = map) +
  labs(title = 'December 2016') +
  geom_polygon(data = map2_fortified,
               aes(x = long,
                   y = lat,
                   group = group),
               color = 'black',
               fill = NA)
r
# ggplot_list <- Filter(function(x) is(x, "ggplot"), mget(ls()))

# Run command line: convert -delay 10 -loop 0 *.png result.gif