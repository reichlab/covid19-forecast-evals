## determine model eligibility
library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)
library(lubridate)
source("code/unit_timezero_forecast_complete.R")

## loads in important dates about analysis including a the_timezeros_inc vector
source("code/load-global-analysis-dates.R") 

## connect to Zoltar
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

## table to help organize and choose forecasts
timezero_weeks <- tibble(
    ## every possible timezero
    forecast_date = the_timezeros_inc, 
    ## the associated target_end_date for 1-week ahead targets
    target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(forecast_date, horizon=1)))

## lots of code here to determine eligible models based on elibility criteria

# 1. subset models to "primary" and "secondary" designated models
primary_models <- models(zoltar_connection, project_url) %>%
    filter(notes %in% c("primary", "secondary"))

# 2. obtain timezeroes for remaining models, eliminate ones that don't have the right dates
date_filtered_models <- tibble(model=character(), forecast_date=Date(), target_end_date_1wk_ahead=Date())
for(i in 1:nrow(primary_models)) {

    message(paste("** starting model", primary_models[i,"model_abbr"]))
    this_model_forecasts <- forecasts(zoltar_connection, primary_models[i,"url"]) 

    ## is there a forecast for every required week?
    required_forecast_weeks <- this_model_forecasts %>% 
        right_join(timezero_weeks, by=c("timezero_date" = "forecast_date")) %>% 
        mutate(forecast_exists = !is.na(forecast_data_url)) %>%
        group_by(target_end_date_1wk_ahead) %>% 
        summarize(n_forecasts = sum(forecast_exists)) %>%
      mutate(n_forecasts = ifelse(target_end_date_1wk_ahead <= last_4wk_target_end_date, n_forecasts, NA_integer_))
    
    ## if there are no forecasts for more than MAXIMUM_MISSING_WEEKS weeks, this model is not eligible
    if(sum(required_forecast_weeks$n_forecasts== 0, na.rm = T) > MAXIMUM_MISSING_WEEKS) {
        next()
    } else {
        ## choose most recent forecast for each week
        timezeroes_to_select <- this_model_forecasts %>% 
            inner_join(timezero_weeks, by=c("timezero_date" = "forecast_date")) %>%
            group_by(target_end_date_1wk_ahead) %>%
            arrange(timezero_date) %>%
            summarize(forecast_date = last(timezero_date)) %>%
            mutate(model = primary_models[i,"model_abbr"])
    }
    ## return table with model and timezeroes that are eligible for inclusion based only on dates 
    date_filtered_models <- bind_rows(date_filtered_models, timezeroes_to_select) 
}

#######################################################################
## query potentially eligible forecasts to see if they are eligible

inc_targets <- paste(1:4, "wk ahead inc death")
#cum_targets <- paste(1:4, "wk ahead cum death")
the_targets <- c(inc_targets)#, cum_targets)

## store vectors of models to consider
date_eligible_models <- unique(date_filtered_models$model)


model_completes <- tibble(model=character(), forecast_date=Date(), target_end_date_1wk_ahead=Date(),
    target_group=character(), num_units_eligible=numeric())

for(this_model in date_eligible_models){
    
    fcasts_to_query <- filter(date_filtered_models, model==this_model)
    
    fcasts <- load_forecasts( 
        models = this_model, 
        forecast_dates = fcasts_to_query$forecast_date,
        locations = UNITS_FOR_ELIGIBILITY,
        types = c("quantile"),
        targets = inc_targets
        ) %>%
        mutate(target_group = "inc")
    
    
    if(nrow(fcasts)==0) 
        next()
    ## disqualifying omissions for a timezero
    ##  - not all 4 targets for inc or cum
    ##  - less than 25 locations forecasted for inc or cum targets
    ##  - not all the quantiles for every prediction
    
    ## for each unit-timezero pair, compute a binary "was this prediction complete"
    preds_to_eval <- fcasts %>%
        group_by(model, location, forecast_date, target_group) %>%
        summarize(complete=FALSE)
    
    for(j in 1:nrow(preds_to_eval)){
        preds_to_eval$complete[j] <- unit_timezero_forecast_complete(filter(fcasts, forecast_date==preds_to_eval$forecast_date[j]), type=preds_to_eval$target_group[j])
    }
    
    this_model_completes <- preds_to_eval %>% 
        group_by(model, forecast_date, target_group) %>%
        summarize(num_units_eligible = sum(complete)) %>%
        mutate(target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(forecast_date, horizon=1)))

    
    model_completes <- bind_rows(model_completes, this_model_completes)
}

inc_model_overall <- model_completes %>%
    filter(target_group=="inc", forecast_date %in% the_timezeros_inc) %>%
    ## calculate how many weeks had the eligible number of units
    group_by(model) %>%
    ## sum number of weeks with minimum locations and in core evaluation period 
    mutate(num_eligible_weeks = sum(num_units_eligible >= NUM_UNITS & forecast_date <= last_timezero4wk)) %>%
    ungroup() %>%
    ## filter so that we only have models with eligible number of weeks
    filter(num_eligible_weeks >= NUM_WEEKS_INC, num_units_eligible >= NUM_UNITS) %>%
    select(-num_eligible_weeks) %>%
  mutate(include_overall = TRUE)


#filter models eligible for inclusion in phases 
inc_model_completes_phases <- model_completes %>%
  mutate(seasonal_phase = case_when(forecast_date < first_target_end_date_summer ~ "spring", #set dates based on forecast date 
                                    forecast_date >= first_target_end_date_summer & forecast_date  < first_target_end_date_winter ~ "summer",
                                    forecast_date >= first_target_end_date_winter ~ "winter")) %>%
  ## calculate how many weeks had the eligible number of units in each phase 
  group_by(model,seasonal_phase) %>%
  ## sum number of weeks with minimum locations and in core evaluation period 
  mutate(num_eligible_weeks_phase = sum(num_units_eligible >= NUM_UNITS & forecast_date <= last_timezero4wk)) %>%
  ungroup() %>% group_by(seasonal_phase) %>%
  ## filter so that we only have models with eligible number of weeks
  filter(num_eligible_weeks_phase >= max(num_eligible_weeks_phase)*.66, num_units_eligible >= NUM_UNITS) %>% ungroup() %>%
  select(-num_eligible_weeks_phase)  %>%
  mutate(include_phases = TRUE)

inc_model_completes <- inc_model_overall %>% 
  full_join(inc_model_completes_phases) %>%
  mutate(seasonal_phase = case_when(forecast_date < first_target_end_date_summer ~ "spring", #set dates based on forecast date 
                                    forecast_date >= first_target_end_date_summer & forecast_date  < first_target_end_date_winter ~ "summer",
                                    forecast_date >= first_target_end_date_winter ~ "winter")) %>%
  mutate(include_overall = ifelse(is.na(include_overall),  "FALSE", "TRUE"),
         include_phases = ifelse(is.na(include_phases), "FALSE", "TRUE"))
  

## output data.frame with list of models and eligibility
write_csv(inc_model_completes, file="paper-inputs/model-eligibility-inc.csv")
