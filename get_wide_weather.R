# This should be run after "get_weather_data.R"
# It assumes object in memory from that
source('get_weather_data.R')

library(tidyverse)

# Generate a wide weather dataset
if('mozambican_weather_wide.csv' %in% dir('noaa_data/')){
  wide_weather <- read_csv('noaa_data/mozambican_weather_wide.csv')
} else {
  
  # Trim down weekly weather 
  weather_weekly <- 
    weather_weekly %>%
    filter(!is.na(date)) %>%
    filter(date >= '2010-01-01')
  
  # Generate time ranges
  starter <- seq(120, 5, by = -5)
  ender <- seq(5, 120, by = 5)
  
  counter <- 0
  combos <- expand.grid(s = starter,
                        e = ender)
  combos <- combos %>% filter(s >= e)
  total <- nrow(combos)

  # Create empty columns
  for (s in starter){
    for (e in ender){
      if(s >= e){
        weather_weekly[,paste0('precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('min_precipitation_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('rainy_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('dry_days_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('min_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_temp_from_',
                                     s, '_to_',
                                     e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_dew_point_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('avg_wind_speed_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
        weather_weekly[,paste0('max_wind_speed_from_',
                               s, '_to_',
                               e, '_days_ago')] <-
          NA
      }
    }
  }
  
  # Populate those columns with real values
  for (s in starter){
    for (e in ender){
      
      if(s >= e){
        counter <- counter + 1
        
        nrww <- nrow(weather_weekly)
        for (i in 1:nrww){
          message(paste0('Combination ',
                         counter,
                         ' of ',
                         total,
                         ' combinations',
                         '. Date ',
                         i, 
                         ' of ',
                         nrww, 
                         ' dates.'
                         ))
          
          this_district <- weather_weekly$district[i]
          this_date <- weather_weekly$date[i]
          
          the_data <-
            weather %>%
            filter(date >= (this_date - s),
                   date <= (this_date - e),
                   district == this_district) %>%
            summarise(the_sum = sum(precipitation, na.rm = TRUE),
                      the_max = max(precipitation, na.rm = TRUE),
                      the_min = min(precipitation, na.rm = TRUE),
                      dry_days = length(which(precipitation == 0)),
                      wet_days = length(which(precipitation > 0)),
                      max_temp = max(temp_max, na.rm = TRUE),
                      min_temp = min(temp_min, na.rm = TRUE),
                      avg_temp = mean(temp, na.rm = TRUE),
                      avg_dew_point = mean(dew_point, na.rm = TRUE),
                      avg_wind_speed = mean(wind_speed, na.rm = TRUE),
                      wind_speed_max = max(wind_speed_max, na.rm = TRUE))
          
          # Plug in the results
          weather_weekly[i,paste0('precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_sum
          weather_weekly[i,paste0('max_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_max
          weather_weekly[i,paste0('min_precipitation_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$the_min
          weather_weekly[i,paste0('rainy_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$wet_days
          weather_weekly[i,paste0('dry_days_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$dry_days
          weather_weekly[i,paste0('min_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$min_temp
          weather_weekly[i,paste0('max_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$max_temp
          weather_weekly[i,paste0('avg_temp_from_',
                                        s, '_to_',
                                        e, '_days_ago')] <-
            the_data$avg_temp
          weather_weekly[i,paste0('avg_dew_point_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$avg_dew_point
          weather_weekly[i,paste0('avg_wind_speed_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$avg_wind_speed
          weather_weekly[i,paste0('max_wind_speed_from_',
                                 s, '_to_',
                                 e, '_days_ago')] <-
            the_data$wind_speed_max
        }
      }
    }
  }
  save.image('~/Desktop/backup.RData')
  # Remove 2010 rows
  weather_weekly <-
    weather_weekly %>%
    filter(year >= 2011)
  
  # Write csv
  wide_weather <- weather_weekly
  write_csv(wide_weather, 
            'noaa_data/mozambican_weather_wide.csv')
}

# # Source weather functions
# source('weather_functions.R')
# 
# #### Joe's script for RF, Ridge, Lasso, and enet
# 
# ##########
# # load libraries 
# ##########
# library(caret)
# library(glmnet)
# library(randomForest)
# library(kernlab)
# library(pROC)
# library(Metrics)
# library(doParallel)
# library(nnet)
# library(dplyr)
# 
# ##########
# # get data
# ##########
# mod_dat <- 
#   left_join(df,
#             wide_weather %>%
#               dplyr::select(-date, -precipitation_risk),
#             by = c('year', 'week', 'district')) %>%
#   ungroup %>%
#   filter(date >= '2011-01-01',
#          date <= '2012-01-01') %>%
#   filter(!is.na(date))
# 
# 
# # Remove all NA columns
# mod_dat <- data.frame(mod_dat)
# flag <- FALSE
# nas <- c()
# for (j in 1:ncol(mod_dat)){
#   flag[j] <- all(is.na(mod_dat[,j]))
#   nas[j] <- length(which(is.na(mod_dat[,j])))
# }
# mod_dat <- mod_dat[,!flag]
# 
# ##########
# # Run Models 
# ##########
# 
# ##########
# # Run Random Forest
# ##########
# # EXPLANATION 
# # line by line comments are in joe_functions.R
# # nfolds argument is how many folds you want on your training data to tune parameters like tuneGrid in caret package
# # cutoff is where you want to split your training and test data (70% train, 30% test)
# # iterations is how many times you you make that split.
# rfResults <- rfPred(model_data = mod_dat,
#                     nfolds = 5,
#                     cutoff = 0.7,
#                     iterations = 10)
# 
# #  This extracts predictions and real values from all iterations 
# # from result list and plots them against each other
# plotModel(rfResults, main = 'Random Forest Regression')
# 
# 
# #########
# # Run enet
# #########
# # few steps in enet
# # 1) optimize model to choose alpa - between 0-1 that determines the proportion of shrinkage (lasso and ridge)
# # 2) once alpha is chosen, optimize model for value of lambda (the actual penalization parameter), that is lamda for which there is lowest error
# # on training data (this lambda MUST BE USED WHEN TESTING DATA. i coded it that way, so dont worry about 
# # doing that by hand)
# # 3) once alpha and lambda is chosen, model is trained with alpha and 100 lambdas. 
# # 4) when test on on test data, it returns predictions for all 100 values of lambda, but code
# # subsets predictions based on the lambda used in training data - those are your predictions
# 
# # EXPLANATION 
# # N_CV_REPEATS is how many times you want to run model to choose alpha. if just 1 it will 
# # return a vector of alphas and you simply get the min. if you choose 5 it will return a matrix 
# # with 5 columns and you avg across them and then find min. its all in the code
# # nfolds is how many folds you want on training data 
# # cutoff is where you want to split your training and test data (70% train, 30% test)
# # iterations is how many times you you make that split.
# enetResults <- enetPred(mod_dat,
#                         N_CV_REPEATS = 2,
#                         nfolds = 5,
#                         cutoff = 0.7, 
#                         iterations = 10)
# 
# plotModel(enetResults, main = 'Enet Regression')
# 
# ##########
# # Run Lasso
# ##########
# # Literally exact same steps as enet, but instead of using the model to find an 'optimal' alpha, you
# # simply set it to 1 (if its lass, 0 if ridge)
# 
# # EXPLANATION 
# # alpha indicates if it will be lasso or ridge
# # nfolds is how many folds you want on training data 
# # cutoff is where you want to split your training and test data (70% train, 30% test)
# # iterations is how many times you you make that split.
# lassoResutls <- regPred(mod_dat, 
#                         alpha = 1, 
#                         nfolds = 10, 
#                         cutoff = 0.7, 
#                         iterations = 10)
# 
# plotModel(lassoResutls, main = 'Lasso Regression')
# 
# ##########
# # Run ridge
# ##########
# # Literally exact same steps as enet, but instead of using the model to find an 'optimal' alpha, you
# # simply set it to 0 (if its ridge, 1 if ridge)
# 
# # EXPLANATION 
# # alpha indicates if it will be lasso or ridge
# # nfolds is how many folds you want on training data 
# # cutoff is where you want to split your training and test data (70% train, 30% test)
# # iterations is how many times you you make that split.
# ridgeResutls <- regPred(mod_dat, 
#                         alpha = 0, 
#                         nfolds = 10, 
#                         cutoff = 0.7, 
#                         iterations = 10)
# 
# 
# plotModel(ridgeResutls, main = 'Ridge Regression')
