library(tidyverse)
library(readxl)

# Source helpers
source('helpers.R')

# Define which files we have
gaza_dir <- 'data/surveillance_ministry/BES_Gaza/modulo_basico/'
mb_files_gaza <- dir(gaza_dir,
                all.files = TRUE,
                recursive = TRUE,
                include.dirs = TRUE,
                pattern = '.xlsx')
maputo_dir <- 'data/surveillance_ministry/BES_Maputo/Modulo Básico/'
mb_files_maputo <- dir(maputo_dir,
                     all.files = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = '.xlsx')
mb_files <- c(mb_files_gaza,
              mb_files_maputo)

# Go through each file and combine
mb_list <- list()
for(i in 1:length(mb_files)){
  message(i)
  # Specify the file path
  this_file <- mb_files[i]
  # Specify the year
  this_year <-  regmatches(this_file, gregexpr("[[:digit:]]+", this_file))
  this_year <- as.numeric(unlist(this_year))
  # Specify the district
  this_district <- gsub('[[:digit:]]+', 
                        '',
                        this_file)
  this_district <- gsub('/|.xlsx| ',
                        '',
                        this_district)
  # Specify the province
  this_province <-
    ifelse(this_file %in% mb_files_gaza,
           'Gaza',
           'Maputo')
  # Read in the data
  this_file_path <- 
    paste0(
      ifelse(this_province == 'Gaza',
             gaza_dir,
             maputo_dir),
      this_file)
  these_data <- 
    read_excel(this_file_path,
               skip = 5)
  # Keep only those columns referring to malaria or diarrhea
  good_columns <- which(grepl('mal|diarr', tolower(these_data[1,])))
  good_columns <- sort(c(1,
                         good_columns,
                         good_columns + 2))
  these_data <- these_data[,good_columns]
  # Remove those rows not associated with a week
  names(these_data)[1] <- 'week'
  names(these_data)[2:ncol(these_data)] <-
    these_data[1,2:ncol(these_data)]
  for(j in 2:ncol(these_data)){
    if(is.na(names(these_data)[j])){
      names(these_data)[j] <-
        names(these_data)[j - 1]
    }
  }
  # Add the age to the names
  names(these_data)[2:ncol(these_data)] <-
    paste0(names(these_data)[2:ncol(these_data)],
           '_',
           these_data[2,2:ncol(these_data)])
  
  # Keep only weekly observations
  these_data <-
    these_data %>%
    filter(grepl('Semana ', week))
  
  # Make long
  these_data <-
    these_data %>%
    gather(key,
           value,
           `MALÁRIA_0-4 anos`:`DIARREIA_5-14 anos +`)
  
  # Create an age_group column
  these_data$age_group <-
    unlist(lapply(strsplit(these_data$key, '_'), function(x){x[2]}))
  
  # Create a disease column
  these_data$disease <- 
    unlist(lapply(strsplit(these_data$key, '_'), function(x){x[1]}))
    
  # Remove accents:
  these_data$disease <- 
    stringi::stri_trans_general(these_data$disease, "Latin-ASCII")
  
  # Add year, province, and district columns
  these_data$year <- this_year
  these_data$province <- this_province
  these_data$district <- this_district
  
  # Keep only variables of interest
  these_data <- 
    these_data %>%
    dplyr::select(year, 
                  week, 
                  province,
                  district,
                  disease,
                  age_group,
                  value)
  
  # Remove those with no age group
  these_data <- 
    these_data %>%
    filter(!is.na(age_group) | age_group == 'NA')
  
  # Recode age group
  these_data$age_group <-
    ifelse(grepl('0-4', these_data$age_group), '0-4',
           '5+')
  
  # Add results to the list
  mb_list[[i]] <- these_data
}

# Combine all data
mb <- bind_rows(mb_list)

# Fix value
mb$value <- as.numeric(as.character(mb$value))

# Recode week
mb$week <- 
  as.numeric(unlist(lapply(strsplit(mb$week, ' |/'), function(x){x[2]})))

#######- SISMA
# Define which files we have
gaza_dir <- 'data/surveillance_ministry/BES_Gaza/SIS-MA/'
sm_files_gaza <- dir(gaza_dir,
                     all.files = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = '.xls')
maputo_dir <- 'data/surveillance_ministry/BES_Maputo/SIS-MA 2016_2017/'
sm_files_maputo <- dir(maputo_dir,
                       all.files = TRUE,
                       recursive = TRUE,
                       include.dirs = TRUE,
                       pattern = '.xls')
sm_files <- c(sm_files_gaza,
              sm_files_maputo)

# Go through each file and cosmine
sm_list <- list()
for(i in 1:length(sm_files)){
  message(i)
  # Specify the file path
  this_file <- sm_files[i]
  
  # Specify the province
  this_province <-
    ifelse(this_file %in% sm_files_gaza,
           'Gaza',
           'Maputo')
  
  # If gaza, read in the gaza format
  if(this_file == 'BES_GAZA_2014_2016.xlsx'){
    # Read in the data
    this_file_path <- 
      paste0(
        ifelse(this_province == 'Gaza',
               gaza_dir,
               maputo_dir),
        this_file)
    these_data <- 
      read_excel(this_file_path,
                 skip = ifelse(this_file == "SIS-MA_2016-17.xls",
                               1,
                               0))
    
    # Rename columns to get in the same format as mb data
    these_data <-
      these_data %>%
      rename(year = Ano,
             week = Semana,
             district = Distrito,
             province = Provincia,
             district = Distrito,
             value = `Positive cases`) %>%
      dplyr::select(-value)
    
    # Make long
    these_data <-
      these_data %>%
      gather(age_group,
             value,
             `0 to 4`:`5 or more`) %>%
      mutate(age_group = ifelse(age_group == '0 to 4',
                                '0-4',
                                '5+'))
    
    # Specify that this is malaria
    these_data <-
      these_data %>%
      mutate(disease = 'MALARIA')
    
    
    # Keep only variables of interest
    these_data <- 
      these_data %>%
      dplyr::select(year, 
                    week, 
                    province,
                    district,
                    disease,
                    age_group,
                    value)
  } else {
    # Read in the maputo format
    # Read in the data
    this_file_path <- 
      paste0(
        ifelse(this_province == 'Gaza',
               gaza_dir,
               maputo_dir),
        this_file)
    these_data <- 
      read_excel(this_file_path,
                 skip = 1)
    
    these_data$province <- this_province
    
    if(this_file == "SIS-MA_2016-17.xls"){
      these_data <-
        these_data %>%
        rename(Period = Semana,
               `Organisation unit` = `DISTRITO DE XAI-XAI`)
    }
    
    # Get year and week out of period
    these_data$year <- as.numeric(substr(these_data$Period, 1, 4))
    these_data$week <- as.numeric(substr(these_data$Period, 6, nchar(these_data$Period)))
    
    # Rename columns to get in the same format as mb data
    these_data <-
      these_data %>%
      rename(district = `Organisation unit`,
             `0-4` = `BES - MALÁRIA 0-4 anos, CASOS`,
             `5+` = `BES - MALÁRIA 5+ anos, CASOS`) 
    
    # Make long
    these_data <-
      these_data %>%
      gather(age_group,
             value,
             `0-4`:`5+`) 
    
    # Specify that this is malaria
    these_data <-
      these_data %>%
      mutate(disease = 'MALARIA')
    
    # Remove accents from district
    these_data$district <- 
      stringi::stri_trans_general(these_data$district, "Latin-ASCII")
    
    # Keep only variables of interest
    these_data <- 
      these_data %>%
      dplyr::select(year, 
                    week, 
                    province,
                    district,
                    disease,
                    age_group,
                    value)
  }
  
  # Add results to the list
  sm_list[[i]] <- these_data
}

# Cosmine all data
sm <- bind_rows(sm_list)

# Standardize district names before combining sm and mb
mb$district <- toupper(mb$district)

# all_districts <- sort(unique(c(mb$district,
#                              sm$district)))
# Write a csv, create the re-coding, then
# read it back in
# write_csv(data_frame(district = all_districts,
#                      new_district = NA),
#           'data/surveillance_ministry/district_names_matcher.csv')
district_matcher <-
  read_csv('data/surveillance_ministry/district_names_matcher.csv')

# Apply the new districts
mb <-
  mb %>%
  left_join(district_matcher,
            by = 'district') %>%
  mutate(district = new_district) %>%
  dplyr::select(-new_district)
sm <-
  sm %>%
  left_join(district_matcher,
            by = 'district') %>%
  mutate(district = new_district) %>%
  dplyr::select(-new_district)

# Specify the source before combining
mb$src <- 'MB'
sm$src <- 'SM'

# Combine
bes <- bind_rows(mb,
                 sm)

# See if there are disagreements by src
x = bes %>%
  group_by(year, week, province, district, age_group, disease) %>%
  summarise(n = n(),
            min_value = min(value),
            max_value = max(value)) %>%
  ungroup %>%
  mutate(difference = max_value - min_value) 

# There are. So we average
bes <- bes %>%
  group_by(year, week, province, district, age_group, disease) %>%
  summarise(has_mb = length(which(src == 'MB')) == 1,
            has_sm = length(which(src == 'SM')) == 1,
            cases = round(mean(value,
                         na.rm = TRUE))) %>%
  ungroup %>%
  filter(!is.na(year),
         !is.na(week),
         !is.na(province),
         !is.na(district),
         !is.na(age_group)) %>%
  mutate(cases = ifelse(is.na(cases), 0, cases)) 

# Read in population data
read_population <- function(sheet = 1,
                            file = 'data/Projeccoes_distritais_2007_20240/Província  Maputo - - Distritos.xls'){
  
  # Define which district
  if(grepl('Gaza', file)){
    gaza <- TRUE
  } else {
    gaza <- FALSE
  }
  
  if(gaza){
    # Gaza province - ------------------------------------
    
    x  <- read_excel(file, skip = 0,sheet = sheet)
    
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
      this_district  <- read_excel(file, 
                                   skip = district_starts[j] + 2,
                                   sheet = 1)[,1:2]
      # Remove total lines
      this_district <- this_district[3:nrow(this_district),]
      # Remove other districts
      this_district <- this_district[1:18,]
      # Make age groups
      this_district$age_group <- NA
      this_district$age_group[1:2] <- '0-4'
      this_district$age_group[3:nrow(this_district)] <- '5+'
      # Group and agg
      this_district <-
        this_district %>%
        group_by(age_group) %>%
        summarise(population = sum(as.numeric(as.character(Total)))) %>%
        ungroup
      
      # Add a year column
      this_district$year <- the_year
      
      # Add a district column
      this_district$district <- districts[j]
      
      # Add to results list
      results_list[[j]] <- this_district
    }
    results <- bind_rows(results_list)
    return(results)
  } else {
    # Maputo province - ------------------------------------
    
    x  <- read_excel(file, skip = 0,sheet = sheet)
    
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
      this_district  <- read_excel(file, 
                                   skip = district_starts[j] + 2,
                                   sheet = 1)[,1:2]
      # Remove total lines
      this_district <- this_district[3:nrow(this_district),]
      # Remove other districts
      this_district <- this_district[1:18,]
      # Make age groups
      this_district$age_group <- NA
      this_district$age_group[1:2] <- '0-4'
      this_district$age_group[3:nrow(this_district)] <- '5+'
      # ifelse(this_district$Idade %in% c('0', '1-4'),
      #        '0-4 anos',
      #        '5 anos +')
      # Group and agg
      this_district <-
        this_district %>%
        group_by(age_group) %>%
        summarise(population = sum(as.numeric(as.character(Total)))) %>%
        ungroup 
      
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
}

final_list <- list()
for (sh in 1:12){
  message(sh)
  final_list[[sh]] <- read_population(sheet = sh)
}
pop_maputo <- bind_rows(final_list)
pop_maputo$province <- 'Maputo'

# Read for Gaza too
final_list <- list()
for (sh in 1:12){
  message(sh)
  final_list[[sh]] <- read_population(sheet = sh,
                                      file = 'data/Projeccoes_distritais_2007_20240/Gaza - Distritos.xls')
}
pop_gaza <- bind_rows(final_list) %>% filter(district != 'GAZA')
pop_gaza$province <- 'Gaza'

# Combine all populations
pop <- bind_rows(pop_maputo, pop_gaza)

# Get pop only for our years of interest
# (there's a bug in 2026 anyway)
pop <- pop %>% filter(year >= 2010 &
                        year <= 2017)

# Standardize district names
pop$district[pop$district == 'BILENE MACIA'] <- 'BILENE'
pop$district[pop$district == 'MANDLACAZE'] <- 'MANDLAKAZI'
pop$district[pop$district == 'MANHIÇA'] <- 'MANHICA'
pop$district[pop$district == 'MASSINGIRA'] <- 'MASSINGIR'
pop$district[pop$district == 'XAI_XAI'] <- 'XAI-XAI CITY'
pop$district[pop$district == 'XAI-XAI'] <- 'XAI-XAI DISTRICT'
bes$district[bes$district == 'MANJACAZE'] <- 'MANDLAKAZI'

# Get a date helper for standardizing weekly dates to saturday
date_helper <- create_date_helper()

bes <-
  bes %>%
  left_join(date_helper,
            by = c('year', 'week'))

# Get some date objects
bes$month <- as.numeric(format(bes$date, '%m'))
bes$day <- as.numeric(format(bes$date, '%d'))

# Keep only through end of 2016
bes <- bes %>%
  filter(date <= '2016-12-31')

# reorder
bes <- bes %>%
  ungroup %>%
  dplyr::select(year,
                week,
                date,
                month,
                day,
                province,
                district,
                age_group,
                disease,
                cases) %>%
  arrange(date, province, district, age_group)

# Join population to data
df  <- bes %>%
  left_join(pop %>% dplyr::select(-province),
            by = c("year", "district",  'age_group'))

# Make incidence
df <- 
  df %>%
  mutate(p = cases / population) %>%
  mutate(pk = p * 1000)

# Remove unecessary objects
rm(district_matcher,
   mb,
   pop_gaza,
   pop_maputo,
   sm,
   these_data,
   x,
   final_list,
   gaza_dir,
   good_columns,
   i,
   j,
   maputo_dir,
   mb_files,
   mb_files_gaza,
   mb_files_maputo,
   mb_list,
   sh,
   sm_files,
   sm_files_gaza,
   sm_files_maputo,
   sm_list,
   this_district,
   this_file,
   this_file_path,
   this_province,
   this_year)

# # See incidence
# library(cism)
# ggplot(data = df,
#        aes(x = date,
#            y = pk)) +
#   geom_line(aes(color = age)) +
#   facet_wrap(~district) +
#   labs(x = 'Date',
#        y = 'Incidence (cases per 1,000)',
#        title = 'Malaria incidence over time by district',
#        subtitle = 'Cases per 1,000 inhabitants') +
#   scale_color_manual(name = 'Age',
#                      values = c('darkgreen', 'darkorange')) +
#   theme_cism()
# 
# # See chart fo reach district
# districts <- sort(unique(df$district))
# for (i in 1:length(districts)){
#   g <- ggplot(data = df %>%
#            filter(district == districts[i]),
#          aes(x = date,
#              y = pk)) +
#     geom_line(aes(color = age)) +
#     labs(x = 'Date',
#          y = 'Incidence (cases per 1,000)',
#          title = paste0('Incidence in ', districts[i]),
#          subtitle = 'Cases per 1,000 inhabitants') +
#     scale_color_manual(name = 'Age',
#                        values = c('darkgreen', 'darkorange')) +
#     theme_cism()
#   print(g)
#   Sys.sleep(1)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # # View the overlap period
# # ggplot(data = df %>%
# #          filter(overlap) %>%
# #          group_by(date, district, src) %>%
# #          summarise(value = sum(value, na.rm = TRUE)),
# #        aes(x = date,
# #            y = value,
# #            color = src)) +
# #   geom_line() +
# #   facet_wrap(~district,
# #              scales = "free") +
# #   labs(title = 'Concordance of SIS-MA and MB data',
# #        subtitle = 'During the period for which both systems were active',
# #        x = 'Date',
# #        y = 'Cases') +
# #   scale_color_manual(name = 'Source',
# #                      values = c('red', 'green'))
# 
# # See what happens per place
# temp <- df %>%
#   mutate(magude = ifelse(district == 'MAGUDE',
#                          'Magude',
#                          'Rest of Maputo province')) %>%
#   group_by(date, magude) %>%
#   summarise(cases = sum(cases, na.rm = TRUE)) %>%
#   arrange(date) %>%
#   group_by(magude) %>%
#   mutate(avg = mean(cases[lubridate::year(date) %in% 2010:2015], 
#                          na.rm = TRUE)) %>%
#   ungroup %>%
#   mutate(p = cases / avg * 100)
# library(cism)
# ggplot(data = temp,
#        aes(x = date,
#            y = p,
#            color = magude)) +
#   geom_line(alpha = 0.8) +
#   # geom_smooth(alpha = 0.2) +
#   scale_color_manual(name = '',
#                      values = c('darkblue', 'darkorange')) +
#   theme_cism() +
#   labs(x = 'Date',
#        y = 'Cases (as percentage of 2010-2015 average)',
#        title = 'The impact of MALTEM',
#        subtitle = 'CISM')
# 
# 
# # Pre post
# temp$time <- 
#   ifelse(temp$date <= '2016-02-01', 'Pre-MALTEM',
#          'Post-MALTEM')
# 
# temp %>%
#   group_by(magude, time) %>%
#   summarise(cases = sum(cases),
#             p = mean(p))
