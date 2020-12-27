library(lubridate)
library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils)
library(tidyverse)

source("code/load-global-analysis-dates.R")
data("hub_locations")

##Read in eligible data 
model_eligibility_inc <- read.csv("paper-inputs/model-eligibility-inc.csv") %>%
  filter(target_group == "inc") %>%
  select(model, timezero) %>% 
  mutate(timezero = as.Date(timezero)) %>%
  group_by(model) %>%
  mutate(timezero_count = paste("timezero", row_number())) %>% #Create column of timezeros
  ungroup() %>%
  pivot_wider(id_cols = timezero_count, names_from =  model, values_from = timezero)  %>% #Create df with models column names
  column_to_rownames(var= "timezero_count") #rownames as count of timezeros

#download truth data
truth <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = last_4wk_target_end_date, 
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


write.csv(inc_scores_covidhub_utils, "paper-inputs/inc_scores_covidhubutils.csv", row.names = FALSE)