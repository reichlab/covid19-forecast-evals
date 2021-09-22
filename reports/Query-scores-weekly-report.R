#Script to query scores

#Load Libraries
library(scoringutils)
library(covidHubUtils)
library(tidyverse)
library(lubridate)
library(DT)
library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(scico)
library(dplyr)
library(MMWRweek)


library(htmltools)

# new libraries
library(crosstalk)
library(plotly)


#Get Date Boundaries

the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips) #states, us and territories

US_fips <- hub_locations %>%  filter(geo_type == "state") %>% filter(abbreviation %in% datasets::state.abb) %>% pull(fips) #only 50 states 

n_weeks_eval <- 10 #weeks included in evaluation
n_weeks_submitted <- 5 #number of weeks needed for inclusion if no longer submitting
n_weeks_history <- 26 # number of weeks for historical data

#Important dates used
last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = 0))
first_eval_sat <- last_eval_sat  - 7*(n_weeks_eval - 1)  #First Evaluated Date

last_submission_date <- last_eval_sat  - 5 #Last submission date
first_submission_date <- first_eval_sat - 11  #First submission date

first_mon_cutoff <- first_eval_sat - 5

last_1wk_target_end_date <- as.Date(calc_target_week_end_date(last_submission_date, horizon = 1)) #last 1 week ahead horizon
first_1wk_target_end_date  <- as.Date(calc_target_week_end_date(first_submission_date, horizon = 0)) #first 1 week ahead horizon

first_4wk_target_end_date  <- as.Date(calc_target_week_end_date(first_submission_date, horizon = 4)) #first horizon with all 4 target weeks evaluated 
last_4wk_target_end_date <- as.Date(calc_target_week_end_date(last_submission_date, horizon = 4))

forecast_mon <- lubridate::floor_date(Sys.Date(), unit = "week") + 1      #Even when running on Tuesday, will be Monday date (used so that there are not duplicated values for a forecast that has submitted multiple times in a week)
first_mon_history <- forecast_mon - 7*n_weeks_history #First monday for historical data
mondays <- seq(first_mon_history, last_eval_sat, by = "week")
eval_sat <- c(first_eval_sat, last_eval_sat) #range of dates evaluated 

models_primary_secondary <- get_model_designations(source = "zoltar") %>% filter(designation %in% c("secondary", "primary")) %>% pull(model)


#function to load truth data for all 2 targets
truth_function <- function(x) {
  load_truth(
    truth_source = "JHU",
    target_variable = c(x),
    truth_end_date = Sys.Date(),
    temporal_resolution = "weekly",
    locations = the_locations)
}

# function to clean the datasets and add in columns to count the number of weeks, horizons, and locations
mutate_scores <- function(x) {
  x %>%
    group_by(model, location, horizon, score_name) %>% #Add count of weeks
    mutate(n_weeks = n(),
           n_weeks_3wksPrior = sum(target_end_date >= (last_eval_sat - 2*7) & horizon == "1"),
           n_weeks_10wksPrior =sum(target_end_date >= first_eval_sat & horizon == "1")) %>%
    ungroup() %>%
    group_by(model, location, target_end_date, score_name) %>% #Add count of horizons
    mutate(n_horizons = n()) %>%
    ungroup() %>%
    group_by(model, horizon,  target_end_date, score_name) %>% #Add count of locations
    mutate(n_locations = n()) %>%
    ungroup()  %>%
    mutate(submission_sat = as.Date(calc_target_week_end_date(forecast_date, horizon=0))) 
}

#iterate function
forecasts_inc_function <- function(x,y) {map_dfr(
  mondays[x:y], function(the_weeks) {
    load_forecasts(
      models = c(models_primary_secondary),
      date = the_weeks,
      date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc death"),
      source = "zoltar")
  }
)
}

###########################
# CASES
# Load truth data
truth_dat_case_all <- truth_function("inc case") 

#filter for last 6 months
truth_dat_case <- truth_dat_case_all  %>%
  filter(target_end_date >= first_mon_history)

#query forecast data from zoltar for past 6 month submission weeks. 
forecasts_case <- map_dfr(
  mondays, function(the_weeks) {
    load_forecasts(
      models = c(models_primary_secondary),
      dates = the_weeks,
      date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc case"),
      source = "zoltar")
  }
) 

#manually add in COVIDhub ensembles
forecasts_case_cte <- 
    load_forecasts(
      models = "COVIDhub-trained_ensemble",
      date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc case"),
      source = "zoltar")

forecasts_case_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:4, "wk ahead inc case"),
    source = "zoltar")

forecasts_case_all <- bind_rows(forecasts_case,forecasts_case_cte,forecasts_case_c4e)

#ensure there are no duplicates
forecasts_case_update <- unique(forecasts_case_all) 

#covidhub utils function to score the data
score_case <- score_forecasts(forecasts = forecasts_case_update,
                              truth = truth_dat_case,
                              return_format = "long",
                              use_median_as_point = TRUE)

# clean the datasets and add in columns to count the number of weeks, horizons, and locations
score_case_all <- mutate_scores(score_case)

#write rda to save scores and truth (this will be taken out if we use a csv pipeline)
save(score_case_all, file = "reports/score_case_all.rda")
save(truth_dat_case, file = "reports/truth_dat_case.rda")
save(truth_dat_case_all, file = "reports/truth_dat_case_all.rda")

###########################
# DEATHS
# Load truth data
truth_dat_inc_all <- truth_function("inc death") 

#filter for last 6 months
truth_dat_inc <- truth_dat_inc_all  %>%
  filter(target_end_date >= first_mon_history)

#query forecast data from zoltar for past 6 month submission weeks. 
forecasts_inc1 <- forecasts_inc_function("1","10")
forecasts_inc2 <- forecasts_inc_function("11","20")
forecasts_inc3 <- forecasts_inc_function("21","26")

forecasts_inc <- rbind(forecasts_inc1,forecasts_inc2,forecasts_inc3) %>% filter(model %in% models_primary_secondary)

#manually add in COVIDhub ensembles
forecasts_inc_cte <- 
  load_forecasts(
    models = "COVIDhub-trained_ensemble",
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:4, "wk ahead inc death"),
    source = "zoltar")

forecasts_inc_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:4, "wk ahead inc death"),
    source = "zoltar")

forecasts_inc_all <- bind_rows(forecasts_inc,forecasts_inc_cte,forecasts_inc_c4e)

#ensure there are no duplicates
forecasts_inc_update <- unique(forecasts_inc_all)

#covidhub utils function to score the data
score_inc <- score_forecasts(forecasts = forecasts_inc_update,
                             truth = truth_dat_inc,
                             return_format = "long",
                             use_median_as_point = TRUE)

# clean the datasets and add in columns to count the number of weeks, horizons, and locations
score_inc_all <- mutate_scores(score_inc)

#write rda to save scores (this will be taken out if we use a csv pipeline)
save(score_inc_all, file = "reports/score_inc_all.rda")
save(truth_dat_inc, file = "reports/truth_dat_inc.rda")
save(truth_dat_inc_all, file = "reports/truth_dat_inc_all.rda")




###########################
# HOSPITALIZATION
# Load truth data
truth_dat_hosp_all <- load_truth(
  truth_source = "HealthData",
  target_variable = c("inc hosp"),
  truth_end_date = Sys.Date(),
  temporal_resolution = "daily",
  locations = the_locations)

#filter for last 6 months
truth_dat_hosp <- truth_dat_hosp_all  %>%
  filter(target_end_date >= first_mon_history) %>%
    mutate(MMWRweek(target_end_date))


#convert to 7 day average by week
truth_dat_hosp_wk <- truth_dat_hosp %>%
  group_by(model,target_variable,MMWRyear,MMWRweek, location) %>% summarize(hosp_07da = sum(value)) %>%
  mutate(target_end_date = MMWRweek2Date(MMWRyear,MMWRweek) + 6) %>%
  rename (value=hosp_07da)

#query forecast data from zoltar for past 6 month submission weeks. 
forecasts_hosp <- map_dfr(
  mondays, function(the_weeks) {
    load_forecasts(
      models = c(models_primary_secondary),
      dates = the_weeks,
      date_window_size = 1,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:28, "day ahead inc hosp"),
      source = "zoltar")
  }
) 

#reduce size
forecasts_hosp_x <- forecasts_hosp %>%
  select(model, forecast_date, location, horizon, temporal_resolution, target_variable, target_end_date, type, quantile, value) %>%
  mutate(MMWRweek(target_end_date))

#divide into smaller groups
forecasts_hosp_x1 <- forecasts_hosp_x %>%
  filter(forecast_date <= mondays[7])

forecasts_hosp_x2 <- forecasts_hosp_x %>%
  filter(forecast_date > mondays[7] & forecast_date <= mondays[12])

forecasts_hosp_x3 <- forecasts_hosp_x %>%
  filter(forecast_date > mondays[12] & forecast_date <= mondays[19])

forecasts_hosp_x4 <- forecasts_hosp_x %>%
  filter(forecast_date > mondays[19] & forecast_date <= mondays[24])

forecasts_hosp_x5 <- forecasts_hosp_x %>%
  filter(forecast_date > mondays[24])

#ensure there are no duplicates in each smaller group (may have to increase memory limit memory.limit(24000))
forecasts_hosp_x1_update <- unique(forecasts_hosp_x1)
forecasts_hosp_x2_update <- unique(forecasts_hosp_x2)
forecasts_hosp_x3_update <- unique(forecasts_hosp_x3)
forecasts_hosp_x4_update <- unique(forecasts_hosp_x4)
forecasts_hosp_x5_update <- unique(forecasts_hosp_x5)

forecasts_hosp_update <- rbind(forecasts_hosp_x1_update,forecasts_hosp_x2_update,forecasts_hosp_x3_update,forecasts_hosp_x4_update,forecasts_hosp_x5_update)

#convert to 7 day average by week
forecasts_hosp_x_wk <- forecasts_hosp_update %>%
  group_by(model,target_variable,forecast_date,type,quantile,MMWRyear,MMWRweek, location) %>% summarize(hosp_07da = sum(value)) %>%
  mutate(target_end_date = MMWRweek2Date(MMWRyear,MMWRweek) + 6, targetyear=MMWRyear, targetweek=MMWRweek, MMWRweek(forecast_date)) %>%
  mutate(horizon = targetweek-MMWRweek + 1, temporal_resolution = "wk") %>%
  rename (value=hosp_07da)


#covidhub utils function to score the data
score_hosp <- score_forecasts(forecasts = forecasts_hosp_x_wk,
                              truth = truth_dat_hosp_wk,
                              return_format = "long",
                              use_median_as_point = TRUE)

# clean the datasets and add in columns to count the number of weeks, horizons, and locations
score_hosp_all <- mutate_scores(score_hosp)

#write rda to save scores (this will be taken out if we use a csv pipeline)
save(truth_dat_hosp, file = "reports/truth_dat_hosp.rda")
save(truth_dat_hosp_all, file = "reports/truth_dat_hosp_all.rda")
save(score_hosp_all, file = "reports/score_hosp_all.rda")
