## determine model eligibility
library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)
library(lubridate)
# source("../covid19-forecast-hub/code/processing-fxns/get_next_saturday.R")
source("code/unit_timezero_forecast_complete.R")

## maximum number of weeks missing that we allow before disqualifying a model
MAXIMUM_MISSING_WEEKS <- 3
UNITS_FOR_ELIGIBILITY <- read_csv("../covid19-forecast-hub/data-locations/locations.csv") %>%
    mutate(for_scoring = abbreviation %in% c("US", datasets::state.abb)) %>%
    filter(for_scoring) %>%
    pull(location)

## minimum number of weeks for eligibility
NUM_WEEKS_CUM <- 14
NUM_WEEKS_INC <- 11

## minumum number of models in a week to be considered eligible in a given week
NUM_UNITS <- 25


## All possible dates considered forecasts could have been made
## these include start/end dates for each cum/inc targets 
the_timezeros_cum <- seq(from = as.Date("2020-04-28"), to = as.Date("2020-08-24"), by="days")
the_timezeros_inc <- seq(from = as.Date("2020-05-13"), to = as.Date("2020-08-24"), by="days")


## connect to Zoltar
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

## table to help organize and choose forecasts
timezero_weeks <- tibble(
    ## every possible timezero
    timezero = the_timezeros_cum, 
    ## the associated target_end_date for 1-week ahead targets
    target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(timezero, horizon=1)))

## lots of code here to determine eligible models based on elibility criteria

# 1. subset models to "primary" and "secondary" designated models
primary_models <- models(zoltar_connection, project_url) %>%
    filter(notes %in% c("primary", "secondary"))

# 2. obtain timezeroes for remaining models, eliminate ones that don't have the right dates
date_filtered_models <- tibble(model=character(), timezero=Date(), target_end_date_1wk_ahead=Date())
for(i in 1:nrow(primary_models)) {
    message(paste("** starting model", primary_models[i,"model_abbr"]))
    this_model_forecasts <- forecasts(zoltar_connection, primary_models[i,"url"]) 

    ## is there a forecast for every required week?
    required_forecast_weeks <- this_model_forecasts %>% 
        right_join(timezero_weeks, by=c("timezero_date" = "timezero")) %>% 
        mutate(forecast_exists = !is.na(forecast_data_url)) %>%
        group_by(target_end_date_1wk_ahead) %>% 
        summarize(n_forecasts = sum(forecast_exists))
    
    ## if there are no forecasts for more than MAXIMUM_MISSING_WEEKS weeks, this model is not eligible
    if(sum(required_forecast_weeks$n_forecasts==0) > MAXIMUM_MISSING_WEEKS) {
        next()
    } else {
        ## choose most recent forecast for each week
        timezeroes_to_select <- this_model_forecasts %>% 
            inner_join(timezero_weeks, by=c("timezero_date" = "timezero")) %>%
            group_by(target_end_date_1wk_ahead) %>%
            arrange(timezero_date) %>%
            summarize(timezero = last(timezero_date)) %>%
            mutate(model = primary_models[i,"model_abbr"])
    }
    ## return table with model and timezeroes that are eligible for inclusion based only on dates 
    date_filtered_models <- bind_rows(date_filtered_models, timezeroes_to_select) 
}

#######################################################################
## query potentially eligible forecasts to see if they are eligible

inc_targets <- paste(1:4, "wk ahead inc death")
cum_targets <- paste(1:4, "wk ahead cum death")
the_targets <- c(inc_targets, cum_targets)

## store vectors of models to consider
date_eligible_models <- unique(date_filtered_models$model)


model_completes <- tibble(model=character(), timezero=Date(), target_end_date_1wk_ahead=Date(),
    target_group=character(), num_units_eligible=numeric())

for(this_model in date_eligible_models){
    
    fcasts_to_query <- filter(date_filtered_models, model==this_model)
    
    fcasts <- do_zoltar_query(zoltar_connection, 
        project_url =  project_url,
        is_forecast_query = TRUE,
        models = this_model, 
        targets = the_targets,
        units = UNITS_FOR_ELIGIBILITY,
        types = c("quantile"),
        timezeros = fcasts_to_query$timezero) %>%
        select(-c(cat, prob, sample, family, param1, param2,param3)) %>%
        mutate(target_group = ifelse(target %in% inc_targets, "inc", "cum"))
    
    if(nrow(fcasts)==0) 
        next()
    ## disqualifying omissions for a timezero
    ##  - not all 4 targets for inc or cum
    ##  - less than 25 locations forecasted for inc or cum targets
    ##  - not all the quantiles for every prediction
    
    ## for each unit-timezero pair, compute a binary "was this prediction complete"
    preds_to_eval <- fcasts %>%
        group_by(model, unit, timezero, target_group) %>%
        summarize(complete=FALSE)
    
    for(j in 1:nrow(preds_to_eval)){
        preds_to_eval$complete[j] <- unit_timezero_forecast_complete(filter(fcasts, timezero==preds_to_eval$timezero[j]), type=preds_to_eval$target_group[j])
    }
    
    this_model_completes <- preds_to_eval %>% 
        group_by(model, timezero, target_group) %>%
        summarize(num_units_eligible = sum(complete)) %>%
        mutate(target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(timezero, horizon=1)))

    
    model_completes <- bind_rows(model_completes, this_model_completes)
}

cum_model_completes <- model_completes %>%
    ## only look at cum targets
    filter(target_group=="cum", timezero %in% the_timezeros_cum) %>%
    ## calculate how many weeks had the eligible number of units
    group_by(model) %>%
    mutate(num_eligible_weeks = sum(num_units_eligible >= NUM_UNITS)) %>%
    ungroup() %>%
    ## filter so that we only have models with eligible number of weeks
    filter(num_eligible_weeks >= NUM_WEEKS_CUM, num_units_eligible >= NUM_UNITS) %>%
    select(-num_eligible_weeks) #%>%
    ## complete
    ##complete(model, target_end_date_1wk_ahead, fill = list(timezero=NA, num_units_eligible=0, target_group="cum")) %>%
    ##group_by(model) %>%

inc_model_completes <- model_completes %>%
    filter(target_group=="inc", timezero %in% the_timezeros_inc) %>%
    ## calculate how many weeks had the eligible number of units
    group_by(model) %>%
    mutate(num_eligible_weeks = sum(num_units_eligible >= NUM_UNITS)) %>%
    ungroup() %>%
    ## filter so that we only have models with eligible number of weeks
    filter(num_eligible_weeks >= NUM_WEEKS_INC, num_units_eligible >= NUM_UNITS) %>%
    select(-num_eligible_weeks)
    ## complete(model, target_end_date_1wk_ahead, fill = list(timezero=NA, num_units_eligible=0, target_group="inc"))


all_model_completes <- bind_rows(cum_model_completes, inc_model_completes)

write_csv(all_model_completes, path="paper-inputs/model-eligibility.csv")

## output data.frame with list of models and eligibility