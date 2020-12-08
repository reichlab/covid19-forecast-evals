require(covidHubUtils)

## important dates for score calculations
first_timezero <- as.Date("2020-05-12") ## earliest forecast_date in an eligible file 
last_timezero <- as.Date("2020-10-26") ## latest forecast_date in an eligible file, formerly was as.Date("2020-08-24")

## range of target_week_end_dates
first_1wk_target_end_date <- as.Date(calc_target_week_end_date(first_timezero, 1))
last_1wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero, 1))
last_4wk_target_end_date <- as.Date(calc_target_week_end_date(last_timezero, 4))

## All possible dates considered forecasts could have been made
## these include start/end dates for each inc targets 
the_timezeros_inc <- seq(from = first_timezero, to = last_timezero, by="days")

## maximum number of weeks missing that we allow before disqualifying a model
MAXIMUM_MISSING_WEEKS <- 6 # formerly 3
UNITS_FOR_ELIGIBILITY <- covidHubUtils::hub_locations %>%
  mutate(for_scoring = abbreviation %in% c("US", datasets::state.abb)) %>%
  filter(for_scoring) %>%
  pull(fips)

## minimum number of weeks for eligibility
num_weeks_forecasted <- length(seq.Date(first_1wk_target_end_date, last_1wk_target_end_date, by="7 days"))
NUM_WEEKS_INC <- num_weeks_forecasted - MAXIMUM_MISSING_WEEKS

## minumum number of models in a week to be considered eligible in a given week
NUM_UNITS <- 25

