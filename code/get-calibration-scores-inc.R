## get calibration for incident death forecasts
#Estee Cramer, Nick Reich
#Script Created 2020-10-14, updated 2020-11-09

#Load Libraries
library(lubridate)
library(zoltr)
library(tidyverse)
library(covidHubUtils) ##devtools::install_github("reichlab/covidHubUtils")

data(hub_locations)


model_eligibility <- read_csv("paper-inputs/model-eligibility-inc.csv")

## locations/dates with reporting anomalies
dates_with_issues <- read_csv("paper-inputs/anomaly-reporting-dates.csv", col_types = "nDccDnnnc") %>%
  filter(to_remove==1)

## models to pull forecasts for
the_models_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(model) %>% unique()
n_inc_models <- length(the_models_inc)
the_models_inc1 <- the_models_inc[1:(round(n_inc_models/2))]
the_models_inc2 <- the_models_inc[(round(n_inc_models/2)+1):n_inc_models]

#Locations to pull forecasts for
the_locations <- c("US", "01", "02", "04", "05", "06", "08", "09", "10", "12", 
  "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
  "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", 
  "48", "49", "50", "51", "53", "54", "55", "56")

## the targets to pull forecasts for
the_targets_inc <- paste(1:20, "wk ahead inc death")

## Dates to extract from Zoltar
the_timezeros_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(timezero) %>% unique()

#Query scores from Zoltar 
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

#Query incidence forecasts 

inc_calibration1 <- do_zoltar_query(zoltar_connection,
  project_url =  "https://zoltardata.com/api/project/44/",
  is_forecast_query = TRUE,
  models = the_models_inc1,
  units = the_locations,
  targets = the_targets_inc,
  timezeros = the_timezeros_inc,
  types = c("quantile")) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)

inc_calibration2 <- do_zoltar_query(zoltar_connection,
  project_url =  "https://zoltardata.com/api/project/44/",
  is_forecast_query = TRUE,
  models = the_models_inc2,
  units = the_locations,
  targets = the_targets_inc,
  timezeros = the_timezeros_inc,
  types = c("quantile")) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)


inc_calibration <- bind_rows(inc_calibration1, inc_calibration2) 


#Write csv files with zoltar data 
write_csv(inc_calibration, path = paste0("paper-inputs/inc-calibration.csv"))
