library(lubridate)
library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils)
library(tidyverse)

source("code/load-global-analysis-dates.R")
data("hub_locations")

##Read in eligible data 
model_eligibility_inc <- read.csv("paper-inputs/model-eligibility-inc.csv") %>%
  filter(target_group == "inc") %>%
  select(model, forecast_date) %>% 
  mutate(forecast_date = as.Date(forecast_date)) %>%
  group_by(model) %>%
  mutate(timezero_count = paste("timezero", row_number())) %>% #Create column of forecast_dates
  ungroup() %>%
  pivot_wider(id_cols = timezero_count, names_from =  model, values_from = forecast_date)  %>% #Create df with models column names
  column_to_rownames(var= "timezero_count") #rownames as count of timezeros

#download truth data
truth <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = today(), 
  temporal_resolution = "weekly",
  locations = hub_locations %>% filter(geo_type == "state") %>% pull(fips))

## load models and corresponding timezeroes 
inc_scores_covidhub_utils <- map_dfr(
1:length(model_eligibility_inc),
  function(x){
    forecasts <- load_forecasts(
    models = colnames(model_eligibility_inc)[x],
    forecast_dates = model_eligibility_inc %>% pull(x),
    locations = hub_locations %>% filter(geo_type == "state") %>% pull(fips),
    types <- c("quantile"), 
    targets = c(paste(1:20, "wk ahead inc death"))
    )
    return(score_forecasts(forecasts, truth))
  }
)

inc_scores_covidhub_utils <- inc_scores_covidhub_utils %>%
  filter(target_end_date <= last_date_evaluated) %>%
  left_join(hub_locations %>% select(location = fips, location_name)) %>%
  mutate(target_1wk = as.Date(calc_target_week_end_date(forecast_date, 1)))

write.csv(inc_scores_covidhub_utils, "paper-inputs/inc-scores.csv", row.names = FALSE)