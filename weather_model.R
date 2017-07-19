# Methods: RF, Ridge, Lasso, and enet

library(tidyverse)
# Source weather functions
source('weather_functions.R')

# Source wide weather
source('get_wide_weather.R')

# Get bes data
source('get_bes_data.R')

##########
# load libraries
##########
library(caret)
library(glmnet)
library(randomForest)
library(kernlab)
library(pROC)
library(Metrics)
library(doParallel)
library(nnet)
library(dplyr)

# Prepare data for modeling
make_factors_from_characters <- function(x){
  x <- data.frame(x)
  for (j in 1:ncol(x)){
    this_class <- class(x[,j])
    if(this_class == 'character'){
      x[,j] <- factor(x[,j])
    }
  }
  return(x)
}
df <- make_factors_from_characters(df)
wide_weather <- make_factors_from_characters(wide_weather)

# Remove NA dates
df <- df %>% filter(!is.na(date))
wide_weather <- wide_weather %>% filter(!is.na(date))

# Get training data
mod_dat <-
  left_join(df %>%
              filter(!date %in% unique(df$date)[which(!unique(df$date) %in% unique(wide_weather$date))]),
            wide_weather,
            by = c('date', 'district')) %>%
  ungroup %>%
  filter(date >= '2010-01-01',
         date <= '2016-01-01') 

# Force conversion to factors
mod_dat <- make_factors_from_characters(mod_dat)

# Remove all NA columns and all columns with no variation
mod_dat <- data.frame(mod_dat)
flag <- FALSE
no_variation <- FALSE
nas <- c()
for (j in 1:ncol(mod_dat)){
  flag[j] <- all(is.na(mod_dat[,j]))
  nas[j] <- length(which(is.na(mod_dat[,j])))
  no_variation[j] <- length(unique(mod_dat[,j])) == 1
}
mod_dat <- mod_dat[,!flag]
mod_dat <- mod_dat[,!no_variation]

# Remove other columns which are too predictive
mod_dat <- 
  mod_dat %>%
  dplyr::select(-year, -week, -district, -p, -cases, -population, -month, -day, -date)

# # Keep only a subsample of predictors
# mod_dat <- mod_dat[,1:100]

# Keep only complete obs
mod_dat <- mod_dat[complete.cases(mod_dat),]

# Remove variables having to do with dew point and wind speed
mod_dat <- mod_dat[,!grepl('wind_speed|dew_point', names(mod_dat))]

# Run model
if('fit.RData' %in% dir()){
  load('fit.RData')
} else {
  fit <- randomForest(pk ~ .,
                      data = mod_dat)
  save(fit,
       file = 'fit.RData')
}

# Predict too
real_data <- df %>%
  left_join(wide_weather,
            by = c('district', 'date'))
real_data$predicted <- predict(fit, real_data)

# Remove irrelevant colunns
real_data <- real_data %>% dplyr::select(date, 
                           district,
                           age,
                           pk,
                           predicted,
                           year,
                           month,
                           week, 
                           day)

real_data_short <- real_data

# Make long
real_data <- real_data %>%
  gather(key, value, pk:predicted)

# Name nicer
real_data$key <- ifelse(real_data$key == 'pk', 'Observed',
                        'Predicted')

ggplot(data = real_data %>%
         filter(district %in% c('MANHICA')) %>%
         filter(date <= '2016-01-01'),
       aes(x = date,
           y = value,
           group = key,
           color = key)) +
  geom_line(alpha = 0.6) +
  facet_grid(district ~ age,
             scales = 'free_y') +
  scale_color_manual(name = '',
                     values = c('darkgreen', 'darkorange'))

ggplot(data = real_data_short,
       aes(x = predicted,
           y = pk)) +
  geom_point(alpha = 0.1)

# Get importance
imp <- importance(fit)
imp <- data.frame(imp)
imp$variable <- row.names(imp)
names(imp)[1] <- 'importance'
imp <- imp %>% dplyr::select(variable, importance)
imp$importance <- imp$importance / sum(imp$importance) * 100
imp <- imp %>% arrange(desc(importance))
imp
##########
# Run Models
##########

##########
# Run Random Forest
##########
# EXPLANATION
# line by line comments are in joe_functions.R
# nfolds argument is how many folds you want on your training data to tune parameters like tuneGrid in caret package
# cutoff is where you want to split your training and test data (70% train, 30% test)
# iterations is how many times you you make that split.
rfResults <- rfPred(model_data = mod_dat,
                    nfolds = 5,
                    cutoff = 0.7,
                    iterations = 10)

#  This extracts predictions and real values from all iterations
# from result list and plots them against each other
plotModel(rfResults, main = 'Random Forest Regression')


#########
# Run enet
#########
# few steps in enet
# 1) optimize model to choose alpa - between 0-1 that determines the proportion of shrinkage (lasso and ridge)
# 2) once alpha is chosen, optimize model for value of lambda (the actual penalization parameter), that is lamda for which there is lowest error
# on training data (this lambda MUST BE USED WHEN TESTING DATA. i coded it that way, so dont worry about
# doing that by hand)
# 3) once alpha and lambda is chosen, model is trained with alpha and 100 lambdas.
# 4) when test on on test data, it returns predictions for all 100 values of lambda, but code
# subsets predictions based on the lambda used in training data - those are your predictions

# EXPLANATION
# N_CV_REPEATS is how many times you want to run model to choose alpha. if just 1 it will
# return a vector of alphas and you simply get the min. if you choose 5 it will return a matrix
# with 5 columns and you avg across them and then find min. its all in the code
# nfolds is how many folds you want on training data
# cutoff is where you want to split your training and test data (70% train, 30% test)
# iterations is how many times you you make that split.
enetResults <- enetPred(mod_dat,
                        N_CV_REPEATS = 2,
                        nfolds = 5,
                        cutoff = 0.7,
                        iterations = 10)

plotModel(enetResults, main = 'Enet Regression')

##########
# Run Lasso
##########
# Literally exact same steps as enet, but instead of using the model to find an 'optimal' alpha, you
# simply set it to 1 (if its lass, 0 if ridge)

# EXPLANATION
# alpha indicates if it will be lasso or ridge
# nfolds is how many folds you want on training data
# cutoff is where you want to split your training and test data (70% train, 30% test)
# iterations is how many times you you make that split.
lassoResutls <- regPred(mod_dat,
                        alpha = 1,
                        nfolds = 10,
                        cutoff = 0.7,
                        iterations = 10)

plotModel(lassoResutls, main = 'Lasso Regression')

##########
# Run ridge
##########
# Literally exact same steps as enet, but instead of using the model to find an 'optimal' alpha, you
# simply set it to 0 (if its ridge, 1 if ridge)

# EXPLANATION
# alpha indicates if it will be lasso or ridge
# nfolds is how many folds you want on training data
# cutoff is where you want to split your training and test data (70% train, 30% test)
# iterations is how many times you you make that split.
ridgeResutls <- regPred(mod_dat,
                        alpha = 0,
                        nfolds = 10,
                        cutoff = 0.7,
                        iterations = 10)


plotModel(ridgeResutls, main = 'Ridge Regression')
