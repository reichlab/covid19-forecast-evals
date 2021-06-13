require(covidHubUtils)
require(tidyverse)

## retrieval date for truth data used in score calculation
truth_date <- as.Date("2021-05-25") #previously 2021-05-03 

## spreadsheet for keeping track of dates
## https://docs.google.com/spreadsheets/d/1u6PTlQoNgIyWwE_msswftg2P3U0JOmt5pwIRciAq0Wk/edit#gid=1071484175

## important dates for score calculations
first_timezero <- as.Date("2020-04-21") ## earliest forecast_date in an eligible file 
last_timezero <- as.Date("2021-04-26") ## latest forecast_date for 1 wk ahead in an eligible file, based on 1 wk ahead. 
last_timezero4wk <- as.Date("2021-04-05") ## latest forecast_date for 4 wk ahead in an eligible file

## range of target_week_end_dates. 
first_target_end_date <- as.Date(calc_target_week_end_date(first_timezero, 1))
last_target_end_date <- as.Date(calc_target_week_end_date(last_timezero, 1)) #last date for 1 week ahead forecast


## All possible dates considered forecasts could have been made
## these include start/end dates for each inc target
the_timezeros <- seq(from = first_timezero, to = last_timezero, by="days") #53 eval weeks worth of timezeros 
the_timezeros_4wk <- seq(from = first_timezero, to = last_timezero4wk, by="days") 
the_target_end_dates <- seq(from = first_target_end_date, last_target_end_date, by = "week")

  
## maximum number of weeks missing that we allow before disqualifying a model
MAXIMUM_MISSING_WEEKS <- 45 #formerly 46. Updated b/c there are now only 10 weeks in spring period and need to include models that have submitted for 6 weeks in spring
                            #formerly 17 # formerly 3 

UNITS_FOR_ELIGIBILITY <-covidHubUtils::hub_locations %>% 
  filter(geo_type == "state") %>% filter(location_name != "U.S. Minor Outlying Islands") %>% #added to remove counties
  mutate(for_scoring = abbreviation %in% c("US", datasets::state.abb)) %>%
  filter(for_scoring) %>%
  pull(fips)


## max number of weeks missing for overall analysis (not stratified by phase)
MAXIMUM_MISSING_WEEKS_OVERALL <- 21 #changed b/c there are 53 weeks and 53*0.6 = 31.8 #formerly 17 #formerly 3 

num_weeks_forecasted <- length(the_target_end_dates)
NUM_WEEKS_INC <- num_weeks_forecasted - MAXIMUM_MISSING_WEEKS_OVERALL  #for inclusion overall, must have submitted this many weeks


## minumum number of locations in a week to be considered eligible in a given week
NUM_UNITS <- 25


#Important Dates for three phases of pandemic

# timezeros of phases used for eligibility
first_timezero_spring  <- first_timezero
first_timezero_summer  <- as.Date("2020-06-30")
first_timezero_winter  <- as.Date("2020-11-17")



#used for calculating WIS
first_target_end_date_spring  <- as.Date("2020-05-02") 
first_target_end_date_summer  <- as.Date("2020-07-11")
first_target_end_date_winter  <- as.Date("2020-11-28")

#range of target_end_dates 
range_fcast_dates <- c(
  first_target_end_date_spring,
  first_target_end_date_summer,
  first_target_end_date_winter,
  last_target_end_date)