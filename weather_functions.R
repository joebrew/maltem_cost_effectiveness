##########
# This script has the code for models 


##########
# Random Forest 
##########
rfPred <- function(model_data,
                   nfolds,
                   cutoff,
                   iterations) 
{
  
  model <- list()
  best_features <- list()
  importance <- list()
  train.mse <- list()
  test.mse <- list()
  train.predictions <- list()
  test.predictions <- list()
  train.ground_truth <- list()
  test.ground_truth <- list()
  test_acc <- list()
  test_stats  <- list()
  
  # remove all rows with NAs - you can also impute
  model_data <- model_data[complete.cases(model_data),]
  
  # Make sure it's a dataframe
  model_data <- data.frame(model_data)
  
  ## get features - remove outcome and date (because it has too many factor levels for the models)
  feature_index <- !grepl('cases|date|district', colnames(model_data))
  selected_features <- colnames(model_data)[feature_index]
  
  for (i in 1:iterations) {
    
    set.seed(i)
    
    train_index <- sample(nrow(model_data), nrow(model_data) *cutoff, replace = F)
    
    # set numer of folds and parameters for training data
    fitControl <- trainControl( 
      method = "repeatedcv",  # could train on boostrap resample, here use repeated cross validation.
      number = nfolds,      
      repeats = 1,
      allowParallel = TRUE
    )
    
    y <- model_data$cases[train_index] # gets training outcome
    
    # mtry: Number of variables randomly sampled as candidates at each split.
    # ntree: Number of trees to grow - these are standard settings for RF
    mtry <- sqrt(ncol(model_data[train_index, selected_features]))
    tunegrid <- expand.grid(.mtry=mtry)
    
    model[[i]] <- train(x = model_data[train_index, selected_features]
                        , y = y
                        , method = "rf"
                        , trControl = fitControl
                        , tuneGrid = tunegrid
                        , importance = T
                        , verbose = FALSE)
    
    temp <- varImp(model[[i]])[[1]]
    importance[[i]] <- cbind(rownames(temp), temp$Overall)
    
    
    test.predictions[[i]] <- predict(model[[i]] 
                                     , newdata = model_data[-train_index, selected_features])
    
    train.predictions[[i]] <- predict(model[[i]] 
                                      , newdata = model_data[train_index, selected_features])
    
    # get ground truth for training and testing data
    train.ground_truth[[i]] <- model_data$cases[train_index]
    test.ground_truth[[i]] <- model_data$cases[-train_index]
    
    # get root mean squared error
    train.mse[[i]] <- rmse(unlist(train.predictions[[i]]), unlist(train.ground_truth[[i]]))
    test.mse[[i]] <- rmse(unlist(test.predictions[[i]]), unlist(test.ground_truth[[i]]))
    
    print(i)
    
  }
  
  return(list(train.mse, test.mse,  train.predictions, test.predictions, train.ground_truth, 
              test.ground_truth, test_acc, test_stats, model, importance))
  
}


##########
# Elastic Net
##########
enetPred <- function(model_data,
                     N_CV_REPEATS,
                     nfolds,
                     cutoff,
                     iterations) {
  
  # get lists to store results
  model <- list()
  best_features <- list()
  importance <- list()
  train.mse <- list()
  test.mse <- list()
  train.predictions <- list()
  test.predictions <- list()
  train.ground_truth <- list()
  test.ground_truth <- list()
  
  # remove all rows with NAs - you can also impute
  model_data <- model_data[complete.cases(model_data),]
  
  
  ## make factor variables numeric as they will need to be coerced to a matrix for enet to read it
  # first find factors
  factor_index <- sapply(model_data,is.factor)
  
  # loop through data and change factors to numeric (for some reason apply wasn't working for me!)
  if(length(which(factor_index)) > 0){
    for (col in 1:length(which(factor_index))) {
      fac_dat <- model_data[, factor_index]
      fac_dat[, col] <- as.numeric(fac_dat[, col])
      model_data[, factor_index] <- fac_dat
    }
  }
  
  ## get features - remove outcome and date (because it has too many factor levels for the models)
  feature_index <- !grepl('cases|date', colnames(model_data))
  selected_features <- colnames(model_data)[feature_index]
  
  for (i in 1:iterations){
    
    # set seed and get training index
    set.seed(i)
    train_index <- sample(nrow(model_data), nrow(model_data) *cutoff, replace = F)
    
    # get training outcome 
    y <- model_data$cases[train_index] # gets training outcome
    
    ###### ENET
    
    # create vector and list to store best alpha on training data. alpha is the parameter that choses the 
    # the optimal proportion lambda, the tuning parameter for L1 (ridge) and L2 (lasso)
    elastic_net.cv_error = vector()
    elastic_net.cv_model = list()
    elastic_net.ALPHA <- c(1:9) / 10 # creates possible alpha values for model to choose from
    
    # set parameters for training model
    type_family <- 'gaussian'
    
    
    # create error matrix for for opitmal alpha that can run in parraellel if you have bigger data 
    # or if you have a high number fo N_CV_REPEATS
    temp.cv_error_matrix <- foreach (temp = 1:N_CV_REPEATS, .combine=rbind, .errorhandling="stop") %do% {      
      for (alpha in 1:length(elastic_net.ALPHA)) # for i in 1:9 - the model will run 9 times
      {      
        elastic_net.cv_model[[alpha]] = cv.glmnet(x = as.matrix(model_data[train_index, selected_features])
                                                  , y =  y
                                                  , alpha = elastic_net.ALPHA[alpha] # first time with 0.1 and so on
                                                  , type.measure = 'deviance'
                                                  , family = type_family
                                                  , standardize = FALSE 
                                                  , nfolds = nfolds 
                                                  , nlambda = 100
                                                  , parallel = TRUE
        )
        elastic_net.cv_error[alpha] = min(elastic_net.cv_model[[alpha]]$cvm)
      }
      elastic_net.cv_error # stores 9 errors    
    }
    
    if (N_CV_REPEATS == 1) {
      temp.cv_error_mean = temp.cv_error_matrix
    } else {
      temp.cv_error_mean = apply(temp.cv_error_matrix, 2, mean) # take the mean of the 5 iterations  
      # as your value for alpha
    }
    
    # stop if you did not recover error for any models 
    stopifnot(length(temp.cv_error_mean) == length(elastic_net.ALPHA))
    
    # get index of best alpha (lowest alpha) - alpha is values 0.1-0.9
    temp.best_alpha_index = which(min(temp.cv_error_mean) == temp.cv_error_mean)[length(which(min(temp.cv_error_mean) == temp.cv_error_mean))] 
    print(paste("Best ALPHA:", elastic_net.ALPHA[temp.best_alpha_index])) # print the value for alpha
    
    temp.non_zero_coeff = 0
    temp.loop_count = 0
    # loop runs initially because temp.non_zero coefficient <3 and then stops 
    # usually after one iteration because the nzero variable selected by lambda is greater that 3. if it keeps looping
    # it they are never greater than 1, then the model does not converge. 
    while (temp.non_zero_coeff < 1) { 
      elastic_net.cv_model = cv.glmnet(as.matrix(model_data[train_index, selected_features])
                                       , y
                                       , alpha = elastic_net.ALPHA[temp.best_alpha_index]
                                       , type.measure = 'deviance'
                                       , family = type_family
                                       , standardize=FALSE
                                       , nlambda = 100
                                       , nfolds = nfolds
                                       , parallel = TRUE
      )
      
      # get optimal lambda - the tuning parameter for ridge and lasso
      # THIS IS IMPORTANT BECAUSE WHEN YOU TRAIN THE MODEL ON 100 SEPERATE VALUES OF LAMBDA
      # AND WHEN YOU TEST THE MODEL IT WILL RETURN PREDCITION FOR ALL THOSE VALUES (1-100). YOU NEED TO 
      # GRAB THE PREDICTION WITH SAME LAMBDA THAT YOU TRAINED ON. ITS ALL IN THE CODE, BUT JUST WANTED TO 
      # GIVE YOU REASONS
      temp.min_lambda_index = which(elastic_net.cv_model$lambda == elastic_net.cv_model$lambda.min) 
      
      # # number of non zero coefficients at that lambda    
      temp.non_zero_coeff = elastic_net.cv_model$nzero[temp.min_lambda_index] 
      temp.loop_count = temp.loop_count + 1
      
      # set seed for next loop iteration
      as.numeric(Sys.time())-> t 
      set.seed((t - floor(t)) * 1e8 -> seed) 
      if (temp.loop_count > 10) {
        print("diverged")
        temp.min_lambda_index = 50 # if it loops more than 5 times, then model did not converge
        break
      }
    }# while loop ends 
    print(temp.non_zero_coeff)  
    
    model[[i]] = glmnet(x = as.matrix(model_data[train_index,  selected_features])
                        ,y =  y
                        ,alpha = elastic_net.ALPHA[temp.best_alpha_index]
                        ,standardize=FALSE
                        ,nlambda = 100
                        ,family = type_family)
    
    # This returns 100 prediction with 1-100 lambdas
    temp_test.predictions <- predict(model[[i]], as.matrix(model_data[-train_index, selected_features,]),
                                     type = 'response')
    
    test.predictions[[i]] <- temp_test.predictions[, temp.min_lambda_index]
    
    temp_train.predictions <- predict(model[[i]], as.matrix(model_data[train_index, selected_features]),
                                      type = 'response')
    
    train.predictions[[i]] <- temp_train.predictions[, temp.min_lambda_index]
    
    # get groud truth and root mean squared error
    train.ground_truth[[i]] <- model_data$cases[train_index]
    test.ground_truth[[i]] <- model_data$cases[-train_index]
    train.mse[[i]] <- rmse(unlist(train.predictions[[i]]), unlist(train.ground_truth[[i]]))
    test.mse[[i]] <- rmse(unlist(test.predictions[[i]]), unlist(test.ground_truth[[i]]))
    
    print(i)
    
  }
  
  
  return(list(train.mse, test.mse,  train.predictions, test.predictions, train.ground_truth, 
              test.ground_truth, model, importance))
  
}

##########
# Lasso & Ridge
##########
# ONLY DIFFERENCE BETWEEN THE TWO IS THE VALUE YOU GIVE ALPHA IN MODEL
# 1 FOR LASSO AND 0 FOR RIDGE

# the alpha argument should be 1 or 0
# you can give a higher number for nfolds because enet, lasso, ridge, learn very quickly to set 
# variables to zero and therefore is a lot faster than random forest and other models
regPred <- function(model_data,
                    alpha,
                    nfolds,
                    cutoff,
                    iterations) {
  
  # get lists to store results
  model <- list()
  best_features <- list()
  importance <- list()
  train.mse <- list()
  test.mse <- list()
  train.predictions <- list()
  test.predictions <- list()
  train.ground_truth <- list()
  test.ground_truth <- list()
  
  # remove all rows with NAs - you can also impute
  model_data <- model_data[complete.cases(model_data),]
  
  ## make factor variables numeric as they will need to be coerced to a matrix for enet to read it
  # first find factors
  factor_index <- sapply(model_data,is.factor)
  
  # loop through data and change factors to numeric (for some reason apply wasn't working for me!)
  for (col in 1:length(which(factor_index))) {
    fac_dat <- model_data[, factor_index]
    fac_dat[, col] <- as.numeric(fac_dat[, col])
    model_data[, factor_index] <- fac_dat
  }
  
  ## get features - remove outcome and date (because it has too many factor levels for the models)
  feature_index <- !grepl('cases|date', colnames(model_data))
  selected_features <- colnames(model_data)[feature_index]
  
  for (i in 1:iterations){
    
    # set seed and get training index
    set.seed(i)
    train_index <- sample(nrow(model_data), nrow(model_data) *cutoff, replace = F)
    
    # get training outcome 
    y <- model_data$cases[train_index] # gets training outcome
    
    # set parameters for training model
    type_family <- 'gaussian'
    
    temp.non_zero_coeff = 0
    temp.loop_count = 0
    # loop runs initially because temp.non_zero coefficient <3 and then stops 
    # usually after one iteration because the nzero variable selected by lambda is greater that 3. if it keeps looping
    # it they are never greater than 1, then the model does not converge. 
    while (temp.non_zero_coeff < 1) {      
      temp.cv_model = cv.glmnet(as.matrix(model_data[train_index, selected_features])
                                , y = y
                                , alpha = alpha
                                , type.measure = "deviance"
                                , family = type_family
                                , standardize = FALSE
                                , nlambda = 100
                                , nfolds = nfolds
                                , parallel = TRUE
      )
      temp.min_lambda_index = which(temp.cv_model$lambda == temp.cv_model$lambda.min)
      temp.non_zero_coeff = temp.cv_model$nzero[temp.min_lambda_index]
      temp.loop_count = temp.loop_count + 1
      as.numeric(Sys.time())-> t 
      set.seed((t - floor(t)) * 1e8 -> seed) 
      #print(paste0("seed: ", seed))
      if (temp.loop_count > 5) {
        print("diverged")
        temp.min_lambda_index = 50
        break
      }    
    }  
    print(temp.non_zero_coeff)  
    
    model[[i]] = glmnet(x = as.matrix(model_data[train_index,  selected_features])
                        ,y =  y
                        ,alpha = alpha # this means lasso, would be between 0-1 if enet
                        ,standardize=FALSE
                        ,nlambda = 100
                        ,family = type_family)
    
    # This returns 100 prediction with 1-100 lambdas
    temp_test.predictions <- predict(model[[i]], as.matrix(model_data[-train_index, selected_features,]),
                                     type = 'response')
    
    # get predictions with same lambda used in training 
    test.predictions[[i]] <- temp_test.predictions[, temp.min_lambda_index]
    
    temp_train.predictions <- predict(model[[i]], as.matrix(model_data[train_index, selected_features]),
                                      type = 'response')
    
    train.predictions[[i]] <- temp_train.predictions[, temp.min_lambda_index]
    
    # get groud truth and root mean squared error
    train.ground_truth[[i]] <- model_data$cases[train_index]
    test.ground_truth[[i]] <- model_data$cases[-train_index]
    train.mse[[i]] <- rmse(unlist(train.predictions[[i]]), unlist(train.ground_truth[[i]]))
    test.mse[[i]] <- rmse(unlist(test.predictions[[i]]), unlist(test.ground_truth[[i]]))
    
    print(i)
    
  }
  
  
  return(list(train.mse, test.mse,  train.predictions, test.predictions, train.ground_truth, 
              test.ground_truth, model, importance))
  
}


##########
# function that takes you model results and plot predictions vs real values and 
# a corresponding correlation
##########

plotModel <- function(result_list,
                      main) 
{
  
  # plot predictions against ground truth
  plot(unlist(result_list[[4]]), unlist(result_list[[6]]), 
       bty = 'n',
       col = adjustcolor('blue', alpha.f = 0.6),
       pch = 16,
       xlab = 'Predictions',
       ylab = 'Real Age of Diagnosis',
       main = main)
  abline(0,1, lty = 3)
  corr <- round(cor(unlist(result_list[[4]]), unlist(result_list[[6]])), 2)
  legend("topleft", legend = paste0('correlation = ', corr), cex = 1, bty = 'n')
}

########
# Define function for getting wide weather
make_weather_wide <- function(weather,
                              remove_before = '2010-01-01',
                              go_forward = FALSE){
  
  # Generate time ranges
  lowest <- 20
  highest <- 100
  starter <- seq(highest, lowest, by = lowest * -1)
  ender <- seq(lowest, highest, by = lowest)
  
  # Add rows to weather
  if(go_forward){
    new_row <- weather[1,]
    max_date <- max(weather$date)
    new_row[,c('temp_max','temp','temp_min',
               'precipitation')] <- NA
    new_rows <- list()
    for(i in 1:(lowest*2)){
      this_row <- new_row
      this_row$date <- max_date + i
      new_rows[[i]] <- this_row
    }
    new_rows <- bind_rows(new_rows)
    weather <- bind_rows(weather,
                         new_rows)
  }
  
  
  counter <- 0
  combos <- expand.grid(s = starter,
                        e = ender)
  combos <- combos %>% filter(s > e)
  total <- nrow(combos)
  
  # Create empty columns
  for (s in starter){
    for (e in ender){
      if(s > e){
        weather[,paste0('precipitation_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('max_precipitation_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('min_precipitation_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('rainy_days_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('dry_days_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('min_temp_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('max_temp_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        weather[,paste0('avg_temp_from_',
                        s, '_to_',
                        e, '_days_ago')] <-
          NA
        # weather[,paste0('avg_dew_point_from_',
        #                        s, '_to_',
        #                        e, '_days_ago')] <-
        #   NA
        # weather[,paste0('avg_wind_speed_from_',
        #                        s, '_to_',
        #                        e, '_days_ago')] <-
        #   NA
        # weather[,paste0('max_wind_speed_from_',
        #                        s, '_to_',
        #                        e, '_days_ago')] <-
        #   NA
      }
    }
  }
  
  # Trim down weekly weather 
  # Start on certain row of weather
  start_row <- min(which(weather$date >= remove_before))

  # Populate those columns with real values
  for (s in starter){
    for (e in ender){
      
      if(s > e){
        counter <- counter + 1
        
        nrww <- nrow(weather)
        for (i in start_row:nrww){
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
          
          this_district <- weather$district[i]
          this_date <- weather$date[i]
          
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
                      avg_temp = mean(temp, na.rm = TRUE))#,
          # avg_dew_point = mean(dew_point, na.rm = TRUE),
          # avg_wind_speed = mean(wind_speed, na.rm = TRUE),
          # wind_speed_max = max(wind_speed_max, na.rm = TRUE))
          
          # Plug in the results
          weather[i,paste0('precipitation_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$the_sum
          weather[i,paste0('max_precipitation_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$the_max
          weather[i,paste0('min_precipitation_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$the_min
          weather[i,paste0('rainy_days_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$wet_days
          weather[i,paste0('dry_days_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$dry_days
          weather[i,paste0('min_temp_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$min_temp
          weather[i,paste0('max_temp_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$max_temp
          weather[i,paste0('avg_temp_from_',
                           s, '_to_',
                           e, '_days_ago')] <-
            the_data$avg_temp
          # weather[i,paste0('avg_dew_point_from_',
          #                         s, '_to_',
          #                         e, '_days_ago')] <-
          #   the_data$avg_dew_point
          # weather[i,paste0('avg_wind_speed_from_',
          #                         s, '_to_',
          #                         e, '_days_ago')] <-
          #   the_data$avg_wind_speed
          # weather[i,paste0('max_wind_speed_from_',
          #                         s, '_to_',
          #                         e, '_days_ago')] <-
          #   the_data$wind_speed_max
        }
      }
    }
  }
  
  # Trim down
  weather <- 
    weather %>%
    filter(!is.na(date)) %>%
    filter(date >= as.Date(remove_before))
  
  return(weather)
}
