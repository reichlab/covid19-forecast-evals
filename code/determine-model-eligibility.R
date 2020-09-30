## determine model eligibility
library(zoltr)  ##devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(lubridate)
source("../../covid19-forecast-hub/code/processing-fxns/get_next_saturday.R")


## connect to Zoltar
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

## some date set-up

## All possible dates considered forecasts could have been made
the_timezeros_cum <- seq(from = as.Date("2020-04-28"), to = as.Date("2020-08-03"), by="days")
the_timezeros_inc <- seq(as.Date("2020-05-13"), to = as.Date("2020-08-03"), by="days")

## table to help organize and choose forecasts
timezero_weeks <- tibble(
    ## every possible timezero
    timezero = the_timezeros_cum, 
    ## the associated target_end_date for 1-week ahead targets
    target_end_date_1wk_ahead = get_next_saturday(timezero) + (wday(timezero)>2)*7)

## lots of code here to determine eligible models based on elibility criteria

# 1. subset models to "primary" and "secondary" designated models
primary_models <- models(zoltar_connection, project_url) %>%
    filter(notes %in% c("primary", "secondary"))

# 2. obtain timezeroes for remaining models, eliminate ones that don't have the right dates
date_filtered_models <- tibble(target_end_date_1wk_ahead=Date(), last_forecast_in_week=Date(), model=character())
for(i in 1:nrow(primary_models)) {
    message(paste("starting model", primary_models[i,"model_abbr"]))
    this_model_forecasts <- forecasts(zoltar_connection, primary_models[i,"url"]) %>%
        mutate(timezero_date=Sys.Date())
    for(j in 1:nrow(this_model_forecasts)) {
        this_model_forecasts[j,"timezero_date"] <- timezero_info(zoltar_connection, this_model_forecasts[j,"timezero_url"])$timezero_date
    }
    
    ## is there a forecast for every required week?
    required_forecast_weeks <- this_model_forecasts %>% 
        right_join(timezero_weeks, by=c("timezero_date" = "timezero")) %>% 
        mutate(forecast_exists = !is.na(forecast_data_url))%>%
        group_by(target_end_date_1wk_ahead) %>% 
        summarize(n_forecasts = sum(forecast_exists))
    
    ## if there are no forecasts for more than 3 weeks, this model is not eligible
    if(sum(required_forecast_weeks$n_forecasts==0) > 3) {
        next()
    } else {
        ## choose most recent forecast for each week
        timezeroes_to_select <- this_model_forecasts %>% 
            right_join(timezero_weeks, by=c("timezero_date" = "timezero")) %>%
            group_by(target_end_date_1wk_ahead) %>%
            arrange(timezero_date) %>%
            summarize(last_forecast_in_week = last(timezero_date)) %>%
            ungroup() %>%
            mutate(model = primary_models[i,"model_abbr"])
    }
    ## return table with model and timezeroes that are eligible for inclusion based only on dates 
    date_filtered_models <- bind_rows(date_filtered_models, timezeroes_to_select) 
}

# - query potentially eligible forecasts to see if they are eligible


## output data.frame with list of models and eligibility