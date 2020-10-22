#Script to query zoltar for use in calibration coverage plot
#Estee Cramer
#Script Created 2020-10-14


#Load Libraries
library(lubridate)
library(zoltr)
library(tidyverse)
library(covidHubUtils) ##devtools::install_github("reichlab/covidHubUtils")

data(hub_locations)


model_eligibility <- read_csv("paper-inputs/model-eligibility.csv")

## locations/dates with reporting anomalies
dates_with_issues <- read_csv("paper-inputs/anomaly-reporting-dates.csv", col_types = "nDccDnnnc") %>%
  filter(to_remove==1)


## models to pull forecasts for
the_models_cum <- model_eligibility %>% filter(target_group=="cum") %>% pull(model) %>% unique()
the_models_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(model) %>% unique()

#Locations to pull forecasts for
the_locations <- c("US", "01", "02", "04", "05", "06", "08", "09", "10", "12", 
                   "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                   "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                   "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", 
                   "48", "49", "50", "51", "53", "54", "55", "56")

## the targets to pull forecasts for
the_targets_cum <- paste(1:20, "wk ahead cum death")
the_targets_inc <- paste(1:20, "wk ahead inc death")

## Dates to extract from Zoltar
the_timezeros_cum <- model_eligibility %>% filter(target_group=="cum") %>% pull(timezero) %>% unique()
the_timezeros_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(timezero) %>% unique()

#Query scores from Zoltar 
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

cum_calibration1 <- do_zoltar_query(zoltar_connection,
                                    project_url =  "https://zoltardata.com/api/project/44/",
                                    is_forecast_query = TRUE,
                                    models = the_models_cum[1:7],
                                    units = the_locations,
                                    targets = the_targets_cum,
                                    timezeros = the_timezeros_cum,
                                    types = c("quantile"), verbose = FALSE) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)

cum_calibration2 <- do_zoltar_query(zoltar_connection,
                                    project_url =  "https://zoltardata.com/api/project/44/",
                                    is_forecast_query = TRUE,
                                    models = the_models_cum[8:10],
                                    units = the_locations,
                                    targets = the_targets_cum,
                                    timezeros = the_timezeros_cum,
                                    types = c("quantile")) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)

cum_calibration3 <- do_zoltar_query(zoltar_connection,
                                    project_url =  "https://zoltardata.com/api/project/44/",
                                    is_forecast_query = TRUE,
                                    models = the_models_cum[11:15],
                                    units = the_locations,
                                    targets = the_targets_cum,
                                    timezeros = the_timezeros_cum,
                                    types = c("quantile"), verbose = FALSE) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)

cum_calibration <- rbind(cum_calibration1, cum_calibration2, cum_calibration3) 



#Query incidence forecasts 

inc_calibration <- do_zoltar_query(zoltar_connection,
                                    project_url =  "https://zoltardata.com/api/project/44/",
                                    is_forecast_query = TRUE,
                                    models = the_models_inc,
                                    units = the_locations,
                                    targets = the_targets_inc,
                                    timezeros = the_timezeros_inc,
                                    types = c("quantile")) %>%
  filter(quantile == .025 | quantile == .25 | quantile == .75 | quantile == .975)




#Write csv files with zoltar data 
write_csv(cum_calibration, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-cum-calibration.csv"))
write_csv(inc_calibration, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-inc-calibration.csv"))


