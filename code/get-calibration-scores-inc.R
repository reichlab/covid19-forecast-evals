## get calibration for incident death forecasts
#Estee Cramer, Nick Reich
#Script Created 2020-10-14, updated 2020-11-09

#Load Libraries
library(lubridate)
library(zoltr)
library(tidyverse)
library(covidHubUtils) ##devtools::install_github("reichlab/covidHubUtils")

source("code/load-global-analysis-dates.R")
data(hub_locations)

model_eligibility_inc <- read.csv("paper-inputs/model-eligibility-inc.csv") %>%
  filter(target_group == "inc") %>%
  select(model, forecast_date) %>% 
  mutate(forecast_date = as.Date(forecast_date)) %>%
  group_by(model) %>%
  mutate(timezero_count = paste("timezero", row_number())) %>% #Create column of forecast_dates
  ungroup() %>%
  pivot_wider(id_cols = timezero_count, names_from =  model, values_from = forecast_date)  %>% #Create df with models column names
  column_to_rownames(var= "timezero_count") #rownames as count of timezeros


# ## locations/dates with reporting anomalies
# dates_with_issues <- read_csv("paper-inputs/anomaly-reporting-dates.csv", col_types = "nDccDnnnc") %>%
#   filter(to_remove==1)

## models to pull forecasts for
the_models_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(model) %>% unique()
n_inc_models <- length(the_models_inc)
#the_models_inc1 <- the_models_inc[1:(round(n_inc_models/2))]
#the_models_inc2 <- the_models_inc[(round(n_inc_models/2)+1):n_inc_models]

#Locations to pull forecasts for (not national and not AS)
the_locations <- hub_locations %>% filter(geo_type == "state") %>% 
  filter(location_name != "United States" & location_name != "American Samoa") %>% 
  pull(fips)
  
## the targets to pull forecasts for
the_targets_inc <- paste(1:20, "wk ahead inc death")

## Dates to extract from Zoltar
the_timezeros_inc <- model_eligibility  %>% pull(forecast_date) %>% unique()

#Query forecasts from covidhubUtils

## load scores
inc_forecasts <- map_dfr(
  1:length(model_eligibility_inc),
  function(x){
    forecasts <- load_forecasts(
      models = colnames(model_eligibility_inc)[x],
      forecast_dates = model_eligibility_inc %>% pull(x),
      locations = hub_locations %>% filter(geo_type == "state") %>% pull(fips),
      types <- c("quantile"), 
      targets = c(paste(1:20, "wk ahead inc death")))
  }
)


inc_calibration  <- inc_forecasts %>%
  filter(target_end_date <= last_date_evaluated) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)

# inc_calibration1 <- do_zoltar_query(zoltar_connection,
#   project_url =  "https://zoltardata.com/api/project/44/",
#   query_type = "forecasts",
#   models = the_models_inc1,
#   units = the_locations,
#   targets = the_targets_inc,
#   timezeros = the_timezeros_inc,
#   types = c("quantile")) %>%
#   filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)
# 
# inc_calibration2 <- do_zoltar_query(zoltar_connection,
#   project_url =  "https://zoltardata.com/api/project/44/",
#   query_type = "forecasts",
#   models = the_models_inc2,
#   units = the_locations,
#   targets = the_targets_inc,
#   timezeros = the_timezeros_inc,
#   types = c("quantile")) %>%
#  
# 
# 
# inc_calibration <- bind_rows(inc_calibration1, inc_calibration2) 


#Write csv files with zoltar data 
write_csv(inc_calibration, path = "paper-inputs/inc-calibration.csv")
