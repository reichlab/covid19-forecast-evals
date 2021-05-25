require(covidHubUtils)
require(tidyverse)

## retrieval date for truth data used in score calculation
truth_date <- as.Date("2021-05-03")

## important dates for score calculations
first_timezero <- as.Date("2020-04-21") ## earliest forecast_date in an eligible file 

## NR: there was some distinction between these two in the first version that is not present anymore?
last_timezero4wk <- as.Date("2021-04-05") ## latest forecast_date for 4 wk ahead in an eligible file
last_timezero <- as.Date("2021-04-26") ## latest forecast_date for 1 wk ahead in an eligible file, based on 1 wk ahead. 

## range of target_week_end_dates
first_1wk_target_end_date <- as.Date(calc_target_week_end_date(first_timezero, 1))
last_1wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero, 1)) #last date for 1 week ahead forecast
last_4wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero4wk, 1)) #last end date for 4 week ahead forecast 

## All possible dates considered forecasts could have been made
## these include start/end dates for each inc targets 
the_timezeros_inc <- seq(from = first_timezero, to = last_timezero, by="days")
the_timezeros_eligibility <- seq(from = first_timezero, to = last_timezero4wk, by="days")

## maximum number of weeks missing that we allow before disqualifying a model
MAXIMUM_MISSING_WEEKS <- 46 #formerly 17 # formerly 3 
UNITS_FOR_ELIGIBILITY <-covidHubUtils::hub_locations %>% 
  filter(geo_type == "state") %>% filter(location_name != "U.S. Minor Outlying Islands") %>% #added to remove counties
  mutate(for_scoring = abbreviation %in% c("US", datasets::state.abb)) %>%
  filter(for_scoring) %>%
  pull(fips)


## max number of weeks missing for overall analysis (not stratified by phase)
MAXIMUM_MISSING_WEEKS_OVERALL <- 17 #formerly 17 # formerly 3 

num_weeks_forecasted <- length(seq.Date(first_1wk_target_end_date, last_4wk_target_end_date, by="7 days"))
NUM_WEEKS_INC <- num_weeks_forecasted - MAXIMUM_MISSING_WEEKS_OVERALL  #for inclusion overall, must have submitted this many weeks


## minumum number of locations in a week to be considered eligible in a given week
NUM_UNITS <- 25

#Important dates
#last_date_evaluated <- as.Date(calc_target_week_end_date(truth_date, -2)) #last date evaluated

#Important Dates for three phases of pandemic
first_forecast_date_spring  <- as.Date("2020-04-21") 
first_forecast_date_summer  <- as.Date("2020-06-30")
first_forecast_date_winter  <- as.Date("2020-11-17")

