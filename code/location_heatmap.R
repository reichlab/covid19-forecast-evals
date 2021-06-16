## determine model eligibility
#library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)
library(lubridate)
#source("code/unit_timezero_forecast_complete.R")
source("code/load-global-analysis-dates.R")
source("code/unit_timezero_forecast_complete.R")

#1. Load all forecasts 
the_models <- get_model_designations(source = "zoltar") %>% 
  filter(designation %in% c("primary", "secondary")) %>%
  pull(model)

the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips)

the_targets_inc <- c("4 wk ahead inc death")

inc_tmp <- load_forecasts(
  forecast_dates = the_timezeros,
  locations = the_locations,
  types = "quantile",
  targets = the_targets_inc) 
  

inc_tmp_unique <-  inc_tmp %>%
  mutate(sat_fcast_week = as.Date(calc_target_week_end_date(forecast_date, horizon = 0))) %>%
  group_by(model, location, sat_fcast_week, quantile) %>%
  mutate(forecast_in_wk_quant = row_number(), 
         last_forecast_in_wk_quant = forecast_in_wk_quant == max(forecast_in_wk_quant)) %>% 
  filter(last_forecast_in_wk_quant) %>%
  ungroup() %>%
  group_by(model, sat_fcast_week, location, horizon) %>%
  mutate(n_quant = n()) %>% ungroup() %>%
  group_by(model, location, sat_fcast_week) %>%
  mutate(forecast_in_wk = row_number(), 
         last_forecast_in_wk = forecast_in_wk == max(forecast_in_wk)) %>% 
  filter(last_forecast_in_wk) %>%
  ungroup() 
  



#count number of weeks each team submitted
by_weeks <- inc_tmp_unique %>%
  group_by(model, location) %>%
  summarise(n_weeks_submit_forecast = n()) %>%
  select(-location) %>%
  distinct() %>%
  group_by(model) %>%
  filter(n_weeks_submit_forecast == max(n_weeks_submit_forecast)) %>%
  ungroup()

#Count number of locations each team submitted weekly
num_loc <- inc_tmp_unique %>%
  group_by(model, sat_fcast_week) %>%
  summarise(n_loc = n()) %>%
  right_join(by_weeks) %>%
  mutate(model = as.factor(model)) %>%
  ungroup() %>%
  left_join(inc_tmp_unique %>% select(model, sat_fcast_week, n_quant))

#Filter out teams that have fewer than 25 locations at every time point
for_loc_figure <- num_loc %>%
  group_by(model) %>%
  filter(max(n_loc) >= NUM_UNITS) %>% #remove models with fewer than 25 locations at all times
  filter(min(sat_fcast_week) <= last_target_end_date) %>% #filter models that have start date before end of scored period
  filter(!(model %in% c( "CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid"))) %>% #remove models that aren't secondary or primary 
  ungroup() 


# # 2. obtain timezeroes for remaining models, eliminate ones that don't have the right dates
# date_filtered_models <- tibble(model=character(), forecast_date=Date(), target_end_date_1wk_ahead=Date())
# for(i in 1:nrow(primary_models)) {
# 
#     message(paste("** starting model", primary_models[i,"model_abbr"]))
#     this_model_forecasts <- forecasts(zoltar_connection, primary_models[i,"url"])
# 
#     ## is there a forecast for every required week?
#     required_forecast_weeks <- this_model_forecasts %>%
#         right_join(timezero_weeks, by=c("timezero_date" = "forecast_date")) %>%
#         mutate(forecast_exists = !is.na(forecast_data_url)) %>%
#         group_by(target_end_date_1wk_ahead) %>%
#         summarize(n_forecasts = sum(forecast_exists)) %>%
#       mutate(n_forecasts = ifelse(target_end_date_1wk_ahead <= last_4wk_target_end_date, n_forecasts, NA_integer_))
# 
# 
#         ## choose most recent forecast for each week
#         timezeroes_to_select <- this_model_forecasts %>%
#             inner_join(timezero_weeks, by=c("timezero_date" = "forecast_date")) %>%
#             group_by(target_end_date_1wk_ahead) %>%
#             arrange(timezero_date) %>%
#             summarize(forecast_date = last(timezero_date)) %>%
#             mutate(model = primary_models[i,"model_abbr"])
# 
#     ## return table with model and timezeroes that are eligible for inclusion based only on dates
#     date_filtered_models <- bind_rows(date_filtered_models, timezeroes_to_select)
# }

#######################################################################
## query potentially eligible forecasts to see if they are eligible
# 
# inc_targets <- paste(1:4, "wk ahead inc death")
# #cum_targets <- paste(1:4, "wk ahead cum death")
# the_targets <- c(inc_targets)#, cum_targets)
# 
# ## store vectors of models to consider
# date_eligible_models <- unique(for_loc_figure$model)
# 
# 
# model_completes <- tibble(model=character(), forecast_date=Date(), target_end_date_1wk_ahead=Date(),
#     target_group=character(), num_units_eligible=numeric())
# 
# for(this_model in date_eligible_models){
#     
#     fcasts_to_query <- filter(date_filtered_models, model==as.factor(this_model))
#     
#     fcasts <- load_forecasts( 
#         models = this_model, 
#         forecast_dates = fcasts_to_query$forecast_date,
#         locations = hub_locations %>% filter(geo_type == "state") %>%
#         filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>% pull(fips),
#         types = c("quantile"),
#         targets = inc_targets
#         ) %>%
#         mutate(target_group = "inc")
#     
#     
#     if(nrow(fcasts)==0) 
#         next()
# 
#     ## for each unit-timezero pair, compute a binary "was this prediction complete"
#     preds_to_eval <- fcasts %>%
#         group_by(model, location, forecast_date, target_group) %>%
#         summarize(complete=FALSE)
#     
#     for(j in 1:nrow(preds_to_eval)){
#         preds_to_eval$complete[j] <- unit_timezero_forecast_complete(filter(fcasts, forecast_date==preds_to_eval$forecast_date[j]), type=preds_to_eval$target_group[j])
#     }
#     
#     this_model_completes <- preds_to_eval %>% 
#         group_by(model, forecast_date, target_group) %>%
#         summarize(num_units_eligible = sum(complete)) %>%
#         mutate(target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(forecast_date, horizon=1)))
# 
#     
#     model_completes <- bind_rows(model_completes, this_model_completes)
# }
# 
# 
# for_loc_figure <- model_completes %>%
#     filter(target_group=="inc", forecast_date %in% the_timezeros_inc) %>%
#     ## calculate how many weeks had the eligible number of units
#     group_by(model) %>%
#   mutate(sat_fcast_week = target_end_date_1wk_ahead - 7,
#          sum_units = sum(num_units_eligible)) %>% 
#   ungroup() %>%
#   filter(num_units_eligible > 0) 


write_csv(for_loc_figure, file="paper-inputs/heatmap_data.csv")
