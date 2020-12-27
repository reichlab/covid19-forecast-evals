require(covidHubUtils)

## important dates for score calculations
first_timezero <- as.Date("2020-05-12") ## earliest forecast_date in an eligible file 
last_timezero4wk <- as.Date("2020-10-26") ## latest forecast_date in an eligible file, formerly was as.Date("2020-08-24")
last_timezero <- as.Date("2020-11-16") ## latest forecast_date in an eligible file, based on 1 wk ahead 

## range of target_week_end_dates
first_1wk_target_end_date <- as.Date(calc_target_week_end_date(first_timezero, 1))
last_1wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero, 1)) #last date for 1 week ahead forecast
last_4wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero4wk, 1)) #last end date for 4 week ahead forecast 

## All possible dates considered forecasts could have been made
## these include start/end dates for each inc targets 
the_timezeros_inc <- seq(from = first_timezero, to = last_timezero, by="days")

the_timezeros_eligibility <- seq(from = first_timezero, to = last_timezero4wk, by="days")

## maximum number of weeks missing that we allow before disqualifying a model
MAXIMUM_MISSING_WEEKS <- 6 # formerly 3
UNITS_FOR_ELIGIBILITY <- covidHubUtils::hub_locations %>% 
  filter(geo_type == "state") %>% filter(location_name != "U.S. Minor Outlying Islands") %>% #added to remove counties
  mutate(for_scoring = abbreviation %in% c("US", datasets::state.abb)) %>%
  filter(for_scoring) %>%
  pull(fips)

## minimum number of weeks for eligibility
num_weeks_forecasted <- length(seq.Date(first_1wk_target_end_date, last_4wk_target_end_date, by="7 days"))
NUM_WEEKS_INC <- num_weeks_forecasted - MAXIMUM_MISSING_WEEKS

## minumum number of models in a week to be considered eligible in a given week
NUM_UNITS <- 25

#Data issue Date
version_date <- as.Date("2020-12-07")

last_date_evaluated <- as.Date(calc_target_week_end_date(version_date, -2)) #last date evaluated