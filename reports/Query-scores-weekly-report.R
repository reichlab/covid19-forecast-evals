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
county_fips <- hub_locations %>%  #counties: 500 most populous
  filter(geo_type == "county") %>% 
  filter(abbreviation %in% datasets::state.abb) %>% 
  arrange(-population) %>% 
  filter(row_number()<=500) %>% pull(fips)
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
first_sat_history <- last_eval_sat - 7*n_weeks_history #First saturday for historical data
first_mon_history <- forecast_mon - 7*n_weeks_history #First monday for historical data
mondays <- seq(first_mon_history, last_eval_sat, by = "week")
saturdays <- seq(first_sat_history, last_eval_sat, by = "week")
eval_sat <- c(first_eval_sat, last_eval_sat) #range of dates evaluated 

# new way, but not working yet
# hub_repo_path <- "C:\\Users\\mzorn\\Documents\\GitHub\\covid19-forecast-hub"
# models_primary_secondary <- get_model_metadata(models = NULL, source = "local_hub_repo",hub_repo_path) %>% filter(designation %in% c("secondary", "primary")) %>% pull(model)

models_primary_secondary <- get_model_designations(source = "zoltar") %>% filter(designation %in% c("secondary", "primary")) %>% pull(model)

#function to load truth data for death and case targets
truth_function <- function(x,y) {
  load_truth(
    truth_source = "JHU",
    target_variable = c(x),
    truth_end_date = Sys.Date(),
    temporal_resolution = "weekly",
    locations = y)
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

#' @return forecast dataframe augmented by columns reference_date and
#' relative_horizon
align_forecasts_one_temporal_resolution <- function(
  forecasts,
  reference_dates,
  reference_weekday,
  reference_windows,
  drop_nonpos_relative_horizons
) {
  if (length(unique(forecasts$temporal_resolution)) > 1) {
    stop("standardize_forecasts_one_temporal_resolution only supports forecasts at a single temporal resolution.")
  }
  
  if (is.null(reference_windows)) {
    if (reference_weekday == "Saturday") {
      reference_windows <- -4:2
    } else if (reference_weekday == "Monday") {
      reference_windows <- -6:0
    } else {
      stop("Reference windows undefined")
    }
  }
  
  if (!is.list(reference_windows)) {
    reference_windows <- list(reference_windows)
  }
  
  if (!is.null(reference_dates)) {
    # ensure we have dates
    reference_dates <- as.Date(reference_dates)
  } else {
    # every date from that of first forecast - diameter of first window
    # to that of last forecast + diameter of last window
    all_dates <- seq(
      min(forecasts$forecast_date) - (
        max(sort(reference_windows[[1]])) -
          min(sort(reference_windows[[1]]))
      ),
      max(forecasts$forecast_date) + (
        max(sort(reference_windows[[length(reference_windows)]])) -
          min(sort(reference_windows[[length(reference_windows)]])) 
      ),
      by = 1
    )
    
    # keep the dates identified above that are the specified reference_weekday
    reference_dates <- all_dates[weekdays(all_dates) == reference_weekday]
  }
  
  # create a tibble where each row contains:
  # - a possible forecast date
  # - a reference date to which that forecast date should be assigned
  ref_df <- tibble(
    reference_date = reference_dates,
    forecast_date = purrr::map2(
      reference_date, 
      reference_windows, 
      ~.x+.y
    )
  ) %>% unnest(cols = forecast_date)
  
  # ensure that in the tibble constructed above, each forecast date is
  # associated with at most one reference date
  # this could be violated if some windows are overlapping
  reps <- ref_df %>%
    dplyr::group_by(forecast_date) %>%
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
  if (nrow(reps) > 0) {
    stop(paste0(
      "The following forecast dates are associated with multiple reference dates: ",
      paste(reps %>% dplyr::pull(forecast_date), collapse = ", ")
    ))
  }
  
  # join with the reference date lookup table above
  # and calculate the relative horizon
  forecasts <- forecasts %>% 
    dplyr::left_join(ref_df, by = "forecast_date") %>% 
    dplyr::mutate(
      ts_days = ifelse(temporal_resolution == "wk", 7, 1),
      relative_horizon = 
        ceiling(as.numeric((target_end_date - reference_date) / ts_days))
    ) %>%
    dplyr::select(-ts_days)
  
  if (drop_nonpos_relative_horizons) {
    forecasts <- forecasts %>%
      dplyr::filter(relative_horizon > 0)
  }
  
  return(forecasts)
}

###########################
# CASES
# Load truth data
state_county <- c(the_locations,county_fips)
truth_dat_case_all <- truth_function("inc case",state_county) 

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
      locations = state_county,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc case"),
      source = "zoltar")
  }
) 

#manually add in COVIDhub ensembles
forecasts_case_cte <- 
    load_forecasts(
      models = "COVIDhub-trained_ensemble",
      dates = mondays,
      date_window_size = 6,
      locations = state_county,
      types = "quantile",
      targets = paste(1:4, "wk ahead inc case"),
      source = "zoltar")

forecasts_case_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = mondays,
    date_window_size = 6,
    locations = state_county,
    types = "quantile",
    targets = paste(1:4, "wk ahead inc case"),
    source = "zoltar")

forecasts_case_all <- bind_rows(forecasts_case,forecasts_case_cte,forecasts_case_c4e)

#ensure there are no duplicates
#divide into smaller groups
forecasts_case_x1 <- forecasts_case_all %>%
  filter(forecast_date <= mondays[5])

forecasts_case_x2 <- forecasts_case_all %>%
  filter(forecast_date > mondays[5] & forecast_date <= mondays[10])

forecasts_case_x3 <- forecasts_case_all %>%
  filter(forecast_date > mondays[10] & forecast_date <= mondays[15])

forecasts_case_x4 <- forecasts_case_all %>%
  filter(forecast_date > mondays[15] & forecast_date <= mondays[20])

forecasts_case_x5 <- forecasts_case_all %>%
  filter(forecast_date > mondays[20])

#ensure there are no duplicates in each smaller group (may have to increase memory limit memory.limit(24000))
memory.limit(30000)
forecasts_case_x1_update <- unique(forecasts_case_x1)
forecasts_case_x2_update <- unique(forecasts_case_x2)
forecasts_case_x3_update <- unique(forecasts_case_x3)
forecasts_case_x4_update <- unique(forecasts_case_x4)
forecasts_case_x5_update <- unique(forecasts_case_x5)

forecasts_case_update <- rbind(forecasts_case_x1_update,forecasts_case_x2_update,forecasts_case_x3_update,forecasts_case_x4_update,forecasts_case_x5_update)
save(forecasts_case_update, file = "reports/forecasts_case_update.rda")

load(file = "reports/forecasts_case_update.rda") 
load(file = "reports/truth_dat_case.rda") 

# divide up forecasts into managable size to score (by horizon)
forecasts_case_update_x1 <- forecasts_case_update %>%
  filter(horizon == 1)
forecasts_case_update_x2 <- forecasts_case_update %>%
  filter(horizon == 2)
forecasts_case_update_x3 <- forecasts_case_update %>%
  filter(horizon == 3)
forecasts_case_update_x4 <- forecasts_case_update %>%
  filter(horizon == 4)

#covidhub utils function to score the data
score_case_x1 <- score_forecasts(forecasts = forecasts_case_update_x1,
                                 truth = truth_dat_case,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
score_case_x2 <- score_forecasts(forecasts = forecasts_case_update_x2,
                                 truth = truth_dat_case,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
score_case_x3 <- score_forecasts(forecasts = forecasts_case_update_x3,
                                 truth = truth_dat_case,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
score_case_x4 <- score_forecasts(forecasts = forecasts_case_update_x4,
                                 truth = truth_dat_case,
                                 return_format = "long",
                                 use_median_as_point = TRUE)

# clean the datasets and add in columns to count the number of weeks, horizons, and locations
score_case_x1_all <- mutate_scores(score_case_x1)
score_case_x2_all <- mutate_scores(score_case_x2)
score_case_x3_all <- mutate_scores(score_case_x3)
score_case_x4_all <- mutate_scores(score_case_x4)

save(score_case_x1_all, file = "reports/score_case_x1_all.rda")
save(score_case_x2_all, file = "reports/score_case_x2_all.rda")
save(score_case_x3_all, file = "reports/score_case_x3_all.rda")
save(score_case_x4_all, file = "reports/score_case_x4_all.rda")

load(file = "reports/score_case_x1_all.rda")
load(file = "reports/score_case_x2_all.rda")
load(file = "reports/score_case_x3_all.rda")
load(file = "reports/score_case_x4_all.rda")

score_case_all <- rbind(score_case_x1_all,score_case_x2_all,score_case_x3_all,score_case_x4_all)

#write rda to save scores and truth (this will be taken out if we use a csv pipeline)
save(score_case_all, file = "reports/score_case_all.rda")
save(truth_dat_case, file = "reports/truth_dat_case.rda")
save(truth_dat_case_all, file = "reports/truth_dat_case_all.rda")

###########################
# DEATHS
# Load truth data
truth_dat_inc_all <- truth_function("inc death",the_locations) 

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
    dates = mondays,
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:4, "wk ahead inc death"),
    source = "zoltar")

forecasts_inc_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = mondays,
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
  target_variable = c("inc hosp",the_locations),
  truth_end_date = Sys.Date(),
  temporal_resolution = "daily",
  locations = the_locations)

#filter for last 6 months
truth_dat_hosp <- truth_dat_hosp_all  %>%
  filter(target_end_date >= first_mon_history) 

#write rda to save truth
save(truth_dat_hosp, file = "reports/truth_dat_hosp.rda")
save(truth_dat_hosp_all, file = "reports/truth_dat_hosp_all.rda")

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

#manually add in COVIDhub ensembles
forecasts_hosp_cte <- 
  load_forecasts(
    models = "COVIDhub-trained_ensemble",
    dates = mondays,
    date_window_size = 1,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:28, "day ahead inc hosp"),
    source = "zoltar")

forecasts_hosp_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = mondays,
    date_window_size = 1,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:28, "day ahead inc hosp"),
    source = "zoltar")

forecasts_hosp_all <- bind_rows(forecasts_hosp,forecasts_hosp_cte,forecasts_hosp_c4e)

#align dates
forecasts_hosp_x <- align_forecasts_one_temporal_resolution(forecasts = forecasts_hosp_all,
  reference_dates = mondays,                                                          
  reference_weekday = "Monday",
  reference_windows =  -6:0,
  drop_nonpos_relative_horizons = TRUE
)


#reduce size
forecasts_hosp_all_x <- forecasts_hosp_x %>%
  select(model, reference_date, location, relative_horizon, temporal_resolution, target_variable, target_end_date, type, quantile, value) %>%
  rename(forecast_date=reference_date,horizon=relative_horizon)

#divide into smaller groups
forecasts_hosp_x1 <- forecasts_hosp_all_x %>%
  filter(forecast_date <= mondays[7])

forecasts_hosp_x2 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[7] & forecast_date <= mondays[12])

forecasts_hosp_x3 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[12] & forecast_date <= mondays[19])

forecasts_hosp_x4 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[19] & forecast_date <= mondays[24])

forecasts_hosp_x5 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[24])

#ensure there are no duplicates in each smaller group (may have to increase memory limit memory.limit(24000))
memory.limit(30000)
forecasts_hosp_x1_update <- unique(forecasts_hosp_x1)
forecasts_hosp_x2_update <- unique(forecasts_hosp_x2)
forecasts_hosp_x3_update <- unique(forecasts_hosp_x3)
forecasts_hosp_x4_update <- unique(forecasts_hosp_x4)
forecasts_hosp_x5_update <- unique(forecasts_hosp_x5)

forecasts_hosp_update <- rbind(forecasts_hosp_x1_update,forecasts_hosp_x2_update,forecasts_hosp_x3_update,forecasts_hosp_x4_update,forecasts_hosp_x5_update)
save(forecasts_hosp_update, file = "reports/forecasts_hosp_update.rda")

load(file = "reports/forecasts_hosp_update.rda")
load( file = "reports/truth_dat_hosp.rda")

#covidhub utils function to score the data
score_hosp <- score_forecasts(forecasts = forecasts_hosp_update,
                              truth = truth_dat_hosp,
                              return_format = "long",
                              use_median_as_point = TRUE)
save(score_hosp, file = "reports/score_hosp.rda")

load(file = "reports/score_hosp_all.rda") 
load( file = "reports/truth_dat_hosp.rda")
#convert scores to weekly
score_hosp_wk <- score_hosp %>%
  rename(horz_day = horizon)  %>%
  mutate(horizon = ceiling(horz_day/7)) %>%
  group_by(model,score_name,location,forecast_date,horizon,target_variable) %>%
  mutate(n_days = n()) %>% 
  dplyr::summarise(score_value_mean = mean(score_value),
           score_value_sum = sum(score_value),
           score_value_n = mean(n_days)) %>% 
  mutate(score_value = case_when(score_name %in% c("wis","abs_error","dispersion","overprediction","underprediction") ~ score_value_mean,
                                 score_name %in% c("coverage_50","coverage_95") ~ score_value_sum/score_value_n)) %>%
  filter(score_name %in% c("wis","abs_error","dispersion","overprediction","underprediction","coverage_50","coverage_95")) %>%
  mutate(temporal_resolution = "wk",
         target_end_date = forecast_date+ horizon*7)  %>%
  select(model,score_name,location,forecast_date,horizon,score_value,target_variable,temporal_resolution,target_end_date)

# clean the datasets and add in columns to count the number of weeks, horizons, and locations
score_hosp_all <- mutate_scores(score_hosp_wk)  

#write rda to save scores (this will be taken out if we use a csv pipeline)
save(truth_dat_hosp, file = "reports/truth_dat_hosp.rda")
save(truth_dat_hosp_all, file = "reports/truth_dat_hosp_all.rda")
save(score_hosp_all, file = "reports/score_hosp_all.rda")
