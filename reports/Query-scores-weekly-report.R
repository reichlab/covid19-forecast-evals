#Script to query scores

#Load Libraries
library(scoringutils)
library(covidHubUtils)
library(zoltr)
library(tidyverse)

#Get Date Boundaries

the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips) #states, us and territories

US_fips <- hub_locations %>%  filter(geo_type == "state") %>% filter(abbreviation %in% datasets::state.abb) %>% pull(fips) #only 50 states 

n_weeks_eval <- 10 #weeks included in evaluation
n_weeks_submitted <- 5 #number of weeks needed for inclusion if no longer submitting

#Important dates used
last_eval_sat <- as.Date(calc_target_week_end_date(Sys.Date(), horizon = -1))
first_eval_sat <- last_eval_sat  - 7*(n_weeks_eval - 1)  #First Evaluated Date

last_submission_date <- last_eval_sat  - 5 #Last submission date
first_submission_date <- first_eval_sat - 11  #First submission date

first_mon_cutoff <- first_eval_sat - 5

last_1wk_target_end_date <- as.Date(calc_target_week_end_date(last_submission_date, horizon = 1)) #last 1 week ahead horizon
first_1wk_target_end_date  <- as.Date(calc_target_week_end_date(first_submission_date, horizon = 0)) #first 1 week ahead horizon

first_4wk_target_end_date  <- as.Date(calc_target_week_end_date(first_submission_date, horizon = 4)) #first horizon with all 4 target weeks evaluated 
last_4wk_target_end_date <- as.Date(calc_target_week_end_date(last_submission_date, horizon = 4))

eval_sat <- c(first_eval_sat, last_eval_sat) #range of dates evaluated 

models_primary_sec <- get_model_designations(source = "zoltar") %>% filter(designation %in% c("secondary", "primary")) %>% pull(model)

#function to load truth data for all 2 targets

truth_function <- function(x) {
  load_truth(
    truth_source = "JHU",
    target_variable = c(x),
    truth_end_date = Sys.Date(),
    temporal_resolution = "weekly",
    locations = the_locations)
}


truth_dat_case <- truth_function("inc case")
truth_dat_inc <- truth_function("inc death")

#query forecast data from zoltar for all submission weeks. (used so that there are not duplicated values for a forecast that has submitted multiple times in a week)

mondays <- seq(as.Date("2020-04-06"), last_eval_sat, by = "week")

forecasts_case <- map_dfr(
  mondays, function(the_weeks) {
    load_latest_forecasts(
      last_forecast_date = the_weeks,
      forecast_date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc case"),
      source = "zoltar")
  }
) %>% filter(model %in% models_primary_secondary)


forecasts_inc1 <- map_dfr(
  mondays[1:10], function(the_weeks) {
    load_latest_forecasts(
      last_forecast_date = the_weeks,
      forecast_date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc death"),
      source = "zoltar")
  }
)

forecasts_inc2 <- map_dfr(
  mondays[11:20], function(the_weeks) {
    load_latest_forecasts(
      last_forecast_date = the_weeks,
      forecast_date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc death"),
      source = "zoltar")
  }
)

forecasts_inc3 <- map_dfr(
  mondays[21:30], function(the_weeks) {
    load_latest_forecasts(
      last_forecast_date = the_weeks,
      forecast_date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc death"),
      source = "zoltar")
  }
)

forecasts_inc4 <- map_dfr(
  mondays[31:length(mondays)], function(the_weeks) {
    load_latest_forecasts(
      last_forecast_date = the_weeks,
      forecast_date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc death"),
      source = "zoltar")
  }
)


forecasts_inc <- rbind(forecasts_inc1,forecasts_inc2,forecasts_inc3,forecasts_inc4) %>% filter(model %in% models_primary_secondary)

forecasts_case1 <- unique(forecasts_case) #used to ensure there are no duplicates
forecasts_inc1 <- unique(forecasts_inc)


#covidhub utils function to score the data

score_case <- score_forecasts(forecasts = forecasts_case1,
                              truth = truth_dat_case,
                              return_format = "long",
                              use_median_as_point = TRUE)

score_inc <- score_forecasts(forecasts = forecasts_inc1,
                             truth = truth_dat_inc,
                             return_format = "long",
                             use_median_as_point = TRUE)



# function to clean the datasets and add in columns to count the number of weeks, horizons, and locations
mutate_scores <- function(x) {
  x %>%
    group_by(model, location, horizon, score_name) %>% #Add count of weeks
    mutate(n_weeks = n(),
           n_weeks_3wksPrior = sum(target_end_date >= (last_eval_sat - 3*7) & horizon == "1"),
           n_weeks_10wksPrior =sum(target_end_date >= first_eval_sat & horizon == "1")) %>%
    ungroup() %>%
    group_by(model, location, target_end_date, score_name) %>% #Add count of horizons
    mutate(n_horizons = n()) %>%
    ungroup() %>%
    group_by(model, horizon,  target_end_date, score_name) %>% #Add count of locations
    mutate(n_locations = n()) %>%
    ungroup()  %>%
    mutate(submission_sat = as.Date(calc_target_week_end_date(forecast_date, horizon=0))) %>%
    filter(!model %in% c("CU-scenario_high","CU-scenario_mid","CU-scenario_low","CU-nochange"))
  }

score_case_all <- mutate_scores(score_case)
score_inc_all <- mutate_scores(score_inc)

# #write csv of the data 
# write.csv(truth_dat_case, "truth_dat_case.csv", row.names = FALSE)
# write.csv(truth_dat_inc, "truth_dat_inc.csv", row.names = FALSE)
# 
# 
# #write csv to save scores (this will be taken out if we use a csv pipeline)
# write.csv(score_case_edit, "score_case_edit.csv", row.names = FALSE)
# write.csv(score_inc_edit, "score_inc_edit.csv", row.names = FALSE)


#write rds of the data 
save(truth_dat_case, file = "reports/truth_dat_case.rda")
save(truth_dat_inc, file = "reports/truth_dat_inc.rda")


#write rda to save scores (this will be taken out if we use a csv pipeline)
save(score_case_all, file = "reports/score_case_all.rda")
save(score_inc_all, file = "reports/score_inc_all.rda")

