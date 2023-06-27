#Script to query scores
######################################################################
# USE R 4.2.1 version
# ISSUE: N(RAW SCORE) NE N(LN score), GETTING NA FOR  some RAW SCORE
######################################################################

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
library(magrittr) # pipe operator
library(data.table) 

# new libraries
library(crosstalk)
library(plotly)
library(SciViews)

source("reports/functions_weekly_evaluation.R")

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
n_weeks_history <- 52 # number of weeks for historical data

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


###########################
# HOSPITALIZATION
# Load truth data
# RESTART RSTUDIO BEFORE RUNNING CODE FOR HOSP
###########################


truth_dat_hosp_all <- load_truth(
  truth_source = "HealthData",
  target_variable = c("inc hosp",the_locations),
  truth_end_date = Sys.Date(),
  temporal_resolution = "daily",
  locations = the_locations)

# #convert truth to log scale
# truth_dat_hosp_all <- truth_dat_hosp_all  %>%
#   mutate(value_ln=ln(value+1))

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
      date_window_size = 6,
      locations = the_locations,
      types = "quantile",
      targets = paste(1:28, "day ahead inc hosp"),
      source = "zoltar")
  }
) 
save(forecasts_hosp, file = "reports/forecasts_hosp.rda")

########################
#RESTART RSTUDIO
########################
#manually add in COVIDhub ensembles
forecasts_hosp_cte <- 
  load_forecasts(
    models = "COVIDhub-trained_ensemble",
    dates = mondays,
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:28, "day ahead inc hosp"),
    source = "zoltar")
save(forecasts_hosp_cte, file = "reports/forecasts_hosp_cte.rda")

forecasts_hosp_c4e <- 
  load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = mondays,
    date_window_size = 6,
    locations = the_locations,
    types = "quantile",
    targets = paste(1:28, "day ahead inc hosp"),
    source = "zoltar")
save(forecasts_hosp_c4e, file = "reports/forecasts_hosp_c4e.rda")

load(file = "reports/forecasts_hosp.rda")
load(file = "reports/forecasts_hosp_c4e.rda")
load(file = "reports/forecasts_hosp_cte.rda")
forecasts_hosp_all <- bind_rows(forecasts_hosp,forecasts_hosp_cte,forecasts_hosp_c4e)
save(forecasts_hosp_all, file = "reports/forecasts_hosp_all.rda")
########################
#RESTART RSTUDIO
########################
#align dates
load(file = "reports/forecasts_hosp_all.rda")
forecasts_hosp_x <- align_forecasts_one_temporal_resolution(forecasts = forecasts_hosp_all,
  reference_dates = mondays,                                                          
  reference_weekday = "Monday",
  reference_windows =  -6:0,
  drop_nonpos_relative_horizons = TRUE
)
save(forecasts_hosp_x, file = "reports/forecasts_hosp_x.rda")

#reduce size
forecasts_hosp_all_x <- forecasts_hosp_x %>%
  select(model, reference_date, location, relative_horizon, temporal_resolution, target_variable, target_end_date, type, quantile, value) %>%
  rename(forecast_date=reference_date,horizon=relative_horizon)

#1. divide into smaller groups
#2. ensure there are no duplicates in each smaller group (may have to increase memory limit memory.limit(24000))
forecasts_hosp_x1 <- forecasts_hosp_all_x %>%
  filter(forecast_date <= mondays[7])
forecasts_hosp_x1_update <- unique(forecasts_hosp_x1)
save(forecasts_hosp_x1_update, file = "reports/forecasts_hosp_x1_update.rda")

forecasts_hosp_x2 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[7] & forecast_date <= mondays[12])
forecasts_hosp_x2_update <- unique(forecasts_hosp_x2)
save(forecasts_hosp_x2_update, file = "reports/forecasts_hosp_x2_update.rda")

forecasts_hosp_x3 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[12] & forecast_date <= mondays[19])
forecasts_hosp_x3_update <- unique(forecasts_hosp_x3)
save(forecasts_hosp_x3_update, file = "reports/forecasts_hosp_x3_update.rda")

forecasts_hosp_x4 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[19] & forecast_date <= mondays[24])
forecasts_hosp_x4_update <- unique(forecasts_hosp_x4)
save(forecasts_hosp_x4_update, file = "reports/forecasts_hosp_x4_update.rda")

forecasts_hosp_x5 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[24] & forecast_date <= mondays[29])
forecasts_hosp_x5_update <- unique(forecasts_hosp_x5)
save(forecasts_hosp_x5_update, file = "reports/forecasts_hosp_x5_update.rda")

forecasts_hosp_x6 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[29] & forecast_date <= mondays[34])
forecasts_hosp_x6_update <- unique(forecasts_hosp_x6)
save(forecasts_hosp_x6_update, file = "reports/forecasts_hosp_x6_update.rda")

forecasts_hosp_x7 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[34] & forecast_date <= mondays[39])
forecasts_hosp_x7_update <- unique(forecasts_hosp_x7)
save(forecasts_hosp_x7_update, file = "reports/forecasts_hosp_x7_update.rda")

forecasts_hosp_x8 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[39] & forecast_date <= mondays[44])
forecasts_hosp_x8_update <- unique(forecasts_hosp_x8)
save(forecasts_hosp_x8_update, file = "reports/forecasts_hosp_x8_update.rda")

forecasts_hosp_x9 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[44] & forecast_date <= mondays[49])
forecasts_hosp_x9_update <- unique(forecasts_hosp_x9)
save(forecasts_hosp_x9_update, file = "reports/forecasts_hosp_x9_update.rda")

forecasts_hosp_x10 <- forecasts_hosp_all_x %>%
  filter(forecast_date > mondays[49])
forecasts_hosp_x10_update <- unique(forecasts_hosp_x10)
save(forecasts_hosp_x10_update, file = "reports/forecasts_hosp_x10_update.rda")


########################
#RESTART RSTUDIO
########################
load(file = "reports/forecasts_hosp_x1_update.rda")
load(file = "reports/forecasts_hosp_x2_update.rda")
load(file = "reports/forecasts_hosp_x3_update.rda")
load(file = "reports/forecasts_hosp_x4_update.rda")
load(file = "reports/forecasts_hosp_x5_update.rda")
load(file = "reports/forecasts_hosp_x6_update.rda")
load(file = "reports/forecasts_hosp_x7_update.rda")
load(file = "reports/forecasts_hosp_x8_update.rda")
load( file = "reports/forecasts_hosp_x9_update.rda")
load(file = "reports/forecasts_hosp_x10_update.rda")


forecasts_hosp_update <- rbind(forecasts_hosp_x1_update,forecasts_hosp_x2_update,forecasts_hosp_x3_update,forecasts_hosp_x4_update,forecasts_hosp_x5_update,
                               forecasts_hosp_x6_update,forecasts_hosp_x7_update,forecasts_hosp_x8_update,forecasts_hosp_x9_update,forecasts_hosp_x10_update)

# # convert forecasts to log scale
# forecasts_hosp_update <- forecasts_hosp_update  %>%
#   mutate(value_ln=ln(value+1))

save(forecasts_hosp_update, file = "reports/forecasts_hosp_update.rda")

#score forecasts (raw scale)
########################
#RESTART RSTUDIO
########################

load(file = "reports/forecasts_hosp_update.rda")
load( file = "reports/truth_dat_hosp.rda")

# divide up forecasts into managable size to score (by horizon)
forecasts_hosp_update_x1 <- forecasts_hosp_update %>%
  filter(horizon <= 7)
forecasts_hosp_update_x2 <- forecasts_hosp_update %>%
  filter(horizon > 7 & horizon <=14)
forecasts_hosp_update_x3 <- forecasts_hosp_update %>%
  filter(horizon > 14 & horizon <=21)
forecasts_hosp_update_x4 <- forecasts_hosp_update %>%
  filter(horizon > 21)

#covidhub utils function to score the data
score_hosp_x1 <- score_forecasts(forecasts = forecasts_hosp_update_x1,
                                 truth = truth_dat_hosp,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
save(score_hosp_x1, file = "reports/score_hosp_x1.rda")

score_hosp_x2 <- score_forecasts(forecasts = forecasts_hosp_update_x2,
                                 truth = truth_dat_hosp,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
save(score_hosp_x2, file = "reports/score_hosp_x2.rda")

score_hosp_x3 <- score_forecasts(forecasts = forecasts_hosp_update_x3,
                                 truth = truth_dat_hosp,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
save(score_hosp_x3, file = "reports/score_hosp_x3.rda")

score_hosp_x4 <- score_forecasts(forecasts = forecasts_hosp_update_x4,
                                 truth = truth_dat_hosp,
                                 return_format = "long",
                                 use_median_as_point = TRUE)
save(score_hosp_x4, file = "reports/score_hosp_x4.rda")

load(file = "reports/score_hosp_x1.rda")
load(file = "reports/score_hosp_x2.rda")
score_hosp <- rbind(score_hosp_x1,score_hosp_x2,score_hosp_x3,score_hosp_x4)
save(score_hosp, file = "reports/score_hosp.rda")

#transform data (log)
########################
#RESTART RSTUDIO
########################
load(file = "reports/forecasts_hosp_update.rda")
load( file = "reports/truth_dat_hosp.rda")

#prepare data for tranform
truth_dat_hosp_1<- truth_dat_hosp %>% 
  rename("true_value"="value") %>%
  select(target_end_date,location,true_value,target_variable)

forecasts_hosp_update_1 <-  forecasts_hosp_update %>%
  rename("prediction"="value")  


transform_data<-merge(x=forecasts_hosp_update_1,y=truth_dat_hosp_1,by=c("target_end_date",
                                                                        "location","target_variable"),all.x=TRUE,all.y=TRUE)
setDT(transform_data)

#transform data
transform_data_1<- transform_data%>%
  .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
  transform_forecasts(offset = 1) 

save(transform_data_1, file = "reports/transform_data_1.rda")
load( file = "reports/transform_data_1.rda")

#merge transformed truth data
truth_dat_hosp_ln<-transform_data_1 %>%
  filter(scale=="log") %>%
  unique(c("target_end_date","location"), incomparables = FALSE, fromLast = FALSE,
         nmax = NA)   %>%
  rename("value"="true_value")  %>%
  select(location,target_end_date,value)  

truth_log<-truth_dat_hosp %>%
  select(-value) %>%
  left_join(truth_dat_hosp_ln,by=c("location","target_end_date"))  
save(truth_log, file = "reports/truth_log.rda")

# divide up forecasts into managable size and score (by horizon)
forecast_log_x1<- transform_data_1 %>%
  filter(scale=="log" & horizon <= 7 & type=="quantile")  %>%
  rename("value"="prediction") %>%
  select(model,forecast_date,target_end_date,location,target_variable,horizon,quantile,value,temporal_resolution,type)

score_hosp_x1_log <- score_forecasts(forecasts = forecast_log_x1,
                                     truth = truth_log,
                                     return_format = "long",
                                     use_median_as_point = TRUE)
save(score_hosp_x1_log, file = "reports/score_hosp_x1_log.rda") 

forecast_log_x2<- transform_data_1 %>%
  filter(scale=="log" & horizon > 7 & horizon <=14 & type=="quantile")  %>%
  rename("value"="prediction") %>%
  select(model,forecast_date,target_end_date,location,target_variable,horizon,quantile,value,temporal_resolution,type)

score_hosp_x2_log <- score_forecasts(forecasts = forecast_log_x2,
                                     truth = truth_log,
                                     return_format = "long",
                                     use_median_as_point = TRUE)
save(score_hosp_x2_log, file = "reports/score_hosp_x2_log.rda") 

forecast_log_x3<- transform_data_1 %>%
  filter(scale=="log" & horizon > 14 & horizon <=21 & type=="quantile")  %>%
  rename("value"="prediction") %>%
  select(model,forecast_date,target_end_date,location,target_variable,horizon,quantile,value,temporal_resolution,type)

score_hosp_x3_log <- score_forecasts(forecasts = forecast_log_x3,
                                     truth = truth_log,
                                     return_format = "long",
                                     use_median_as_point = TRUE)
save(score_hosp_x3_log, file = "reports/score_hosp_x3_log.rda") 

forecast_log_x4<- transform_data_1 %>%
  filter(scale=="log" & horizon > 21  & type=="quantile")  %>%
  rename("value"="prediction") %>%
  select(model,forecast_date,target_end_date,location,target_variable,horizon,quantile,value,temporal_resolution,type)

score_hosp_x4_log <- score_forecasts(forecasts = forecast_log_x4,
                                     truth = truth_log,
                                     return_format = "long",
                                     use_median_as_point = TRUE)
save(score_hosp_x4_log, file = "reports/score_hosp_x4_log.rda") 
score_hosp_log <- rbind(score_hosp_x1_log,score_hosp_x2_log,score_hosp_x3_log,score_hosp_x4_log)
save(score_hosp_log, file = "reports/score_hosp_log.rda")
##########################
# Convert scores to weekly (raw scale)
# RESTART RSTUDIO
#convert to R 4.1.12
# RUN ON REMOTE (TRANFER FILE VIA GOOGLE DRIVE)
########################
memory.limit(30000)
load(file = "reports/score_hosp.rda") 
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
save(score_hosp_all, file = "reports/score_hosp_all.rda")


##########################
# Convert scores to weekly (natural log scale)
# RESTART RSTUDIO
#
# RUN ON REMOTE (TRANFER FILE VIA GOOGLE DRIVE)
########################
memory.limit(30000)
load(file = "reports/score_hosp_log.rda") 
#convert scores to weekly
score_hosp_wk_ln <- score_hosp_log %>%
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
score_hosp_all_ln <- mutate_scores(score_hosp_wk_ln)  

#write rda to save scores (this will be taken out if we use a csv pipeline)
save(score_hosp_all_ln, file = "reports/score_hosp_all_log.rda")