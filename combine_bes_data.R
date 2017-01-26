library(tidyverse)
library(readxl)

# Read in modulo basico data
mb <- read.csv('data/modulo_basico/Compiled_Dados Provincia Maputo.csv', skip = 2)

# Clean it up ----------

# clean up column names
names(mb)[1:3] <- c('year', 'district', 'age')
names(mb)[4:ncol(mb)] <-
  gsub('X',
       'week_',
       names(mb)[4:ncol(mb)])

# interpolate year
for (i in 1:nrow(mb)){
  mb$year[i] <-
    ifelse(is.na(mb$year[i]),
           mb$year[i-1],
           mb$year[i])
}

# make long
mb <- gather(mb,
             key,
             value,
             week_1:week_53)

# Clean up week
names(mb)[names(mb) == 'key'] <- 'week'
mb$week <- as.numeric(gsub('week_', '', mb$week))

# Remove duplicates
mb <- mb[!duplicated(mb[,c('year', 'district', 'age', 'week')]),]

# Read in the sis-ma data ---------
d2016 <- read_excel('data/dhes2/BES de 2016 SIS-MA.xls', skip = 1)
d2017 <- read_excel('data/dhes2/BES de 2017 SIS-MA.xls', skip = 1)
d2 <- bind_rows(d2016, d2017)
rm(d2016, d2017)

# Fix column names
names(d2) <- c('period',
               'district',
               'age_0_4',
               'age_5_plus')

# Generate more columns
d2$year <- as.numeric(substr(d2$period, 1, 4))
d2$week <- as.numeric(substr(d2$period, 6, nchar(d2$period)))

# Make long
d2 <- gather(d2,
             key, value, age_0_4:age_5_plus)

# Fix names
names(d2)[names(d2) == 'key'] <- 'age'
d2$age <- ifelse(d2$age == 'age_0_4', '0-4 anos',
                 ifelse(d2$age == 'age_5_plus', '5 anos +',
                        NA))

d2 <- d2[,names(mb)]


# Specify sources
d2$src <- 'SIS-MA'
mb$src <- 'MB'

# Combine data
df <- bind_rows(d2, mb)

# Make uppercase district
df$district <- toupper(df$district)

# Standardize district names
df$district[df$district == 'MANHIÇA'] <- 'MANHICA'
df$district[df$district == 'MATUTUÌNE'] <- 'MATUTUINE'

# When two different data sources, use the average
df <- df %>%
  group_by(year, district, age, week) %>%
  summarise(value = mean(value, na.rm = TRUE))

# Get a "date" column (approximate)
df$date <- as.Date(paste0(df$year, '-01-01')) + (7 * (df$week -1))

df_adjusted <- df
df_adjusted$cases <- df_adjusted$value


# # Get the overlap period
# df <- df %>%
#   left_join(
#     df %>%
#       group_by(year, district, age, week) %>%
#       summarise(overlap = length(unique(src)) > 1),
#     by = c("year", "district", "age", "week")
#   )
# 
# # Arrange by date
# df <- df %>%
#   arrange(year, week)
# 

# # Remove the 0 cases in 2016 or 2017 (these are when systems were not yet
# # or were no longer operational)
# df <- df %>%
#   mutate(flag = year %in% 2016:2017 &
#            (value == 0 | is.na(value))) %>%
#   filter(!flag) %>%
#   dplyr::select(-flag)
# 
# # Get a scaling factor for adjusting dhes cases to MB
# scale_data <- df %>%
#            filter(overlap) %>%
#            group_by(date, district, src) %>%
#            summarise(value = sum(value, na.rm = TRUE)) %>%
#   # group by place and see
#   group_by(district) %>%
#   summarise(d_over_m = sum(value[src == 'SIS-MA'], na.rm = TRUE) / 
#               sum(value[src == 'MB'], na.rm = TRUE))
# 
# # Manually add in NAMAACHA
# scale_data <-
#   bind_rows(scale_data,
#             data_frame(district = 'NAMAACHA',
#                        d_over_m = 1))
# 
# # Get an id column
# df$id <- paste0(df$year,
#                 df$week,
#                 df$district,
#                 df$age)
# # Get adjusted cases using the scaling factor
# top <- df %>%
#   filter(src == 'MB') %>%
#   mutate(cases = value)
# bottom <- df %>%
#   filter(src == 'SIS-MA') %>%
#   mutate(src = 'SIS-MA adjusted') %>%
#   left_join(scale_data,
#             by = 'district') %>%
#   mutate(cases = value / d_over_m) %>%
#   dplyr::select(-d_over_m) %>%
#   filter(!id %in% unique(top$id))
# df_adjusted <-
#   bind_rows(top,
#             bottom) %>%
#   dplyr::select(-id, -overlap)

# Reorder columns
df_adjusted <-
  df_adjusted %>%
  dplyr::select(date,
                year,
                week,
                district,
                age,
                # value, # no longer need this
                cases)



# Read in population data
read_population <- function(sheet = 1){
  x  <- read_excel('data/Projeccoes_distritais_2007_20240/Província  Maputo - - Distritos.xls', skip = 0,sheet = sheet)
  
  # Get year
  # the_year <- as.numeric(substr(names(x)[1], nchar(names(x)[1]) - 3, nchar(names(x)[1])))
  the_year <- sheet + 2006
  
  # Get where each district starts
  district_starts <- which(apply(x[,1], 1, function(y){grepl('Quadro', y)}))
  # Manually add in the first one
  district_starts <- c(0, district_starts)
  
  # Get the actual districts
  districts <- c(names(x)[1],
                 x[district_starts,1] %>% unlist)
  districts <- as.character(districts)
  temp <- unlist(lapply(strsplit(districts, ' de '), function(x){x[3]}))
  districts <-
    toupper(substr(temp, 1, nchar(temp) -6))
  # Read each district
  results_list <- list()
  for (j in 1:length(district_starts)){
    this_district  <- read_excel('data/Projeccoes_distritais_2007_20240/Província  Maputo - - Distritos.xls', 
                                 skip = district_starts[j] + 2,
                                 sheet = 1)[,1:2]
    # Remove total lines
    this_district <- this_district[3:nrow(this_district),]
    # Remove other districts
    this_district <- this_district[1:18,]
    # Make age groups
    this_district$age_group <- NA
    this_district$age_group[1:2] <- '0-4 anos'
    this_district$age_group[3:nrow(this_district)] <- '5 anos +'
      # ifelse(this_district$Idade %in% c('0', '1-4'),
      #        '0-4 anos',
      #        '5 anos +')
    # Group and agg
    this_district <-
      this_district %>%
      group_by(age_group) %>%
      summarise(population = sum(as.numeric(as.character(Total))))
    
    # Add a year column
    this_district$year <- the_year
    
    # Add a district column
    this_district$district <- districts[j]
    
    # Add to results list
    results_list[[j]] <- this_district
  }
  results <- bind_rows(results_list)
  return(results)
}

final_list <- list()
for (sh in 1:12){
  message(sh)
  final_list[[sh]] <- read_population(sheet = sh)
}
pop <- bind_rows(final_list)

# Get pop only for our years of interest
# (there's a bug in 2026 anyway)
pop <- pop %>% filter(year >= 2010 &
                        year <= 2017)

# Standardizenames
pop <- pop %>%
  rename(age = age_group)

# Fix encoding
pop$district[pop$district == 'MANHIÇA'] <- 'MANHICA'

# Join population to data
df_adjusted  <- df_adjusted %>%
  left_join(pop,
            by = c("year", "district", 'age'))



# Make incidence
df_adjusted <- 
  df_adjusted %>%
  mutate(p = cases / population) %>%
  mutate(pk = p * 1000)

# Write data
write_csv(df_adjusted, 'data/cleaned/cases.csv')

# See incidence
library(cism)
ggplot(data = df_adjusted,
       aes(x = date,
           y = pk)) +
  geom_line(aes(color = age)) +
  facet_wrap(~district) +
  labs(x = 'Date',
       y = 'Incidence (cases per 1,000)',
       title = 'Malaria incidence over time by district',
       subtitle = 'Cases per 1,000 inhabitants') +
  scale_color_manual(name = 'Age',
                     values = c('darkgreen', 'darkorange')) +
  theme_cism()

# See chart fo reach district
districts <- sort(unique(df_adjusted$district))
for (i in 1:length(districts)){
  g <- ggplot(data = df_adjusted %>%
           filter(district == districts[i]),
         aes(x = date,
             y = pk)) +
    geom_line(aes(color = age)) +
    labs(x = 'Date',
         y = 'Incidence (cases per 1,000)',
         title = paste0('Incidence in ', districts[i]),
         subtitle = 'Cases per 1,000 inhabitants') +
    scale_color_manual(name = 'Age',
                       values = c('darkgreen', 'darkorange')) +
    theme_cism()
  print(g)
  Sys.sleep(1)
}















# 
# # View the overlap period
# ggplot(data = df %>%
#          filter(overlap) %>%
#          group_by(date, district, src) %>%
#          summarise(value = sum(value, na.rm = TRUE)),
#        aes(x = date,
#            y = value,
#            color = src)) +
#   geom_line() +
#   facet_wrap(~district,
#              scales = "free") +
#   labs(title = 'Concordance of SIS-MA and MB data',
#        subtitle = 'During the period for which both systems were active',
#        x = 'Date',
#        y = 'Cases') +
#   scale_color_manual(name = 'Source',
#                      values = c('red', 'green'))

# See what happens per place
temp <- df_adjusted %>%
  mutate(magude = ifelse(district == 'MAGUDE',
                         'Magude',
                         'Rest of Maputo province')) %>%
  group_by(date, magude) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  arrange(date) %>%
  group_by(magude) %>%
  mutate(avg = mean(cases[lubridate::year(date) %in% 2010:2015], 
                         na.rm = TRUE)) %>%
  ungroup %>%
  mutate(p = cases / avg * 100)
library(cism)
ggplot(data = temp,
       aes(x = date,
           y = p,
           color = magude)) +
  geom_line(alpha = 0.8) +
  # geom_smooth(alpha = 0.2) +
  scale_color_manual(name = '',
                     values = c('darkblue', 'darkorange')) +
  theme_cism() +
  labs(x = 'Date',
       y = 'Cases (as percentage of 2010-2015 average)',
       title = 'The impact of MALTEM',
       subtitle = 'CISM')


# Pre post
temp$time <- 
  ifelse(temp$date <= '2016-02-01', 'Pre-MALTEM',
         'Post-MALTEM')

temp %>%
  group_by(magude, time) %>%
  summarise(cases = sum(cases),
            p = mean(p))
