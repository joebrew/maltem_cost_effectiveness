library(tidyverse)
library(readr)
library(readxl)

# When not in campaign, get days since last campaign
if('irs_data_cleaned.RData' %in% dir('data/binaries')){
  load('data/binaries/irs_data_cleaned.RData')
} else {
  
  irs <- read_csv('data/vector_control_ministry/IRS_mozambique_clean_final.csv')
  
  # Fix Matola
  irs$district[irs$district == 'M.Matola'] <- 'Matola'
  irs$province[irs$district == 'Matola'] <- 'Maputo'
  
  # Keep only gaza and maputo
  irs <- irs %>%
    filter(province %in% c('Gaza', 'Maputo'))
  
  # Standardize names to match those in bes
  irs$district <- toupper(irs$district)
  # irs$province <- toupper(irs$province)
  irs_districts <- sort(unique(irs$district))
  
  # Clean dates
  irs$start_date <- irs$end_date <- NA
  irs$start_date <- as.Date(paste0(irs$start_year, 
                                   '-',
                                   irs$start_month,
                                   '-',
                                   irs$start_day))
  irs$end_date <- as.Date(paste0(irs$end_year, 
                                 '-',
                                 irs$end_month,
                                 '-',
                                 irs$end_day))
  
  irs$end_date[is.na(irs$end_date)] <-
    irs$start_date[is.na(irs$end_date)] +
    irs$number_of_days_activity[is.na(irs$end_date)]
  
  irs$start_date[is.na(irs$start_date)] <-
    irs$end_date[is.na(irs$start_date)] -
    irs$number_of_days_activity[is.na(irs$start_date)]
  
  # For those days with no start/end date, estimate
  irs$no_irs_date <- is.na(irs$start_date)
  
  # The average is 3 months, and it's usually at end of year
  # so we're guessing Oct 1 through Dec 31
  irs$start_date <- as.character(irs$start_date)
  irs$end_date <- as.character(irs$end_date)
  irs$start_date <-
    ifelse(is.na(irs$start_date),
           paste0(irs$start_year, '-10-01'),
           as.character(irs$start_date))
  irs$end_date <-
    ifelse(is.na(irs$end_date),
           paste0(irs$start_year, '-12-31'),
           as.character(irs$end_date))
  irs$start_date <- as.Date(irs$start_date)
  irs$end_date <- as.Date(irs$end_date)
  
  # If the end date is before the start date, 
  # take 3 months after the start date
  irs$end_date[irs$start_date > irs$end_date] <-
    irs$start_date[irs$start_date > irs$end_date] + 90
  
  irs <- 
    irs %>%
    mutate(district = ifelse(district == 'C.MATOLA', 'MATOLA',
                             ifelse(district == 'C.XAI-XAI', 'XAI-XAI CITY',
                                    ifelse(district == 'D.XAI-XAI',
                                           'XAI-XAI DISTRICT',
                                           ifelse(district == 'MANJACAZE',
                                                  'MANDLAKAZI',
                                                  ifelse(district == 'MATUTUÍNE MOZAL', 'MATUTUINE',
                                                         ifelse(district == 'XINAVANE', 'MANHICA', district)))))))
  irs <- 
    irs %>%
    mutate(district = ifelse(district == 'MANHIÇA', 'MANHICA',
                             ifelse(district == 'MAT MOZAL', 'MATOLA',
                                    ifelse(district == 'MATUTUÍNE', 'MATUTUINE',
                                           ifelse(district == 'XXAI CIDADE', 'XAI-XAI CITY', 
                                                  ifelse(district == 'XXAI DISTRITO', 'XAI-XAI DISTRICT', 
                                                         ifelse(district == 'ZONA 1A*', 'MATOLA', district)))))))
  
  # Select notable columns
  irs <- 
    irs %>%
    dplyr::select(province,
                  district,
                  start_date,
                  end_date,
                  houses,
                  people,
                  no_irs_date) %>%
    filter(people > 0)
  
  # Create a data helper to fill out full dataset
  source('helpers.R')
  dh <- create_date_helper()
  
  # Convert into a weekly format
  results_list <- list()
  for(i in 1:nrow(irs)){
    message(i)
    these_data <- irs[i,]
    # get dates from start to finish
    the_dates <-
      seq(these_data$start_date,
          these_data$end_date,
          by = 1)
    # get the year/week for those dates
    the_years <- as.numeric(format(the_dates, '%Y'))
    the_weeks <- as.numeric(format(the_dates, '%U')) +1
    
    # Make a dataframe
    out <- data_frame(date = the_dates,
                      irs_campaign = TRUE)
    # Keep only saturdays to be compatible with other datasets
    out <- out[weekdays(out$date) == 'Saturday',]
    
    out <-
      out %>%
      mutate(irs_campaign = ifelse(is.na(irs_campaign),
                                   FALSE,
                                   irs_campaign)) %>%
      mutate(province = these_data$province,
             district = these_data$district,
             irs_houses = these_data$houses,
             irs_people = these_data$people,
             irs_no_date = these_data$no_irs_date)
    rm(these_data)
    
    # Add to results list
    results_list[[i]] <- out
  }
  irs <- bind_rows(results_list)
  
  # Group by date and distrcit and combine
  irs <- irs %>%
    group_by(date, district, province, irs_campaign) %>%
    summarise(irs_houses = sum(irs_houses),
              irs_people = sum(irs_people)) %>%
    ungroup
  
  # Get an expanded grid of dates and districts
  dh <- create_date_helper()
  eg <- expand.grid(date = dh$date,
                    district = sort(unique(irs$district)))
  
  # Join eg to irs helper to get year, week
  irs <- 
    left_join(eg, irs)
  
  irs <- irs %>%
    group_by(district) %>% 
    mutate(province = ifelse(is.na(province), first(province[!is.na(province)]), province)) %>%
    ungroup
  
  # Fill up the NAs with 0s
  irs$irs_campaign[is.na(irs$irs_campaign)] <- FALSE
  
  # Arrange by date and district
  irs <-
    irs %>%
    arrange(district, date)
  
  irs$days_since_last_irs_campaign_end <- 0
  for (i in 1:nrow(irs)){
    message(i)
    these_data <- irs[i,]
    # If not currently in a campaign...
    if(!these_data$irs_campaign){
      # Get the district's last campaign end date
      last_campaign <- irs %>%
        filter(district == these_data$district) %>%
        filter(irs_campaign) %>%
        filter(date <= these_data$date) %>%
        filter(date == max(date)) 
      if(nrow(last_campaign) == 0){
        irs$days_since_last_irs_campaign_end[i] <- NA
      } else {
        last_campaign <- last_campaign[1,]
        irs$days_since_last_irs_campaign_end[i] <-
          as.numeric(irs$date[i] - last_campaign$date)
        irs$irs_houses[i] = last_campaign$irs_houses
        irs$irs_people[i] = last_campaign$irs_people
      }
    }
  }
  save(irs,
       file = 'data/binaries/irs_data_cleaned.RData')
}
irs$irs_no_date <- NULL
