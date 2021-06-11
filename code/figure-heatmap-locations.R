#library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)

source("code/load-global-analysis-dates.R")



# 
# #the_scores <- c("interval_50")
# the_models <- c() 
# the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips)
# the_targets_inc <- c("4 wk ahead inc death")
# 
# 
# inc_tmp <- load_forecasts(
#   forecast_dates = the_timezeros_inc,
#   locations = the_locations,
#   types = "quantile",
#   targets = the_targets_inc) %>%
#   filter(quantile == 0.5)
# 
# inc_tmp_unique <-  inc_tmp %>%
#   mutate(sat_fcast_week = as.Date(calc_target_week_end_date(forecast_date, horizon = 0))) %>%
#   group_by(model, location, sat_fcast_week) %>%
#   mutate(forecast_in_wk = row_number(), 
#          last_forecast_in_wk = forecast_in_wk == max(forecast_in_wk)) %>% 
#   filter(last_forecast_in_wk) %>% 
#   ungroup()  
# 
# 
# #count number of weeks each team submitted
# by_weeks <- inc_tmp_unique %>%
#   group_by(model, location) %>%
#   summarise(n_weeks_submit_forecast = n()) %>%
#   select(-location) %>%
#   distinct() %>%
#   group_by(model) %>%
#   filter(n_weeks_submit_forecast == max(n_weeks_submit_forecast)) %>%
#   ungroup()
# 
# #Count number of locations each team submitted weekly
# num_loc <- inc_tmp_unique %>%
#   group_by(model, sat_fcast_week) %>%
#   summarise(n_loc = n()) %>%
#   right_join(by_weeks) %>%
#   mutate(model = as.factor(model)) %>%
#   ungroup()
# 
# #Filter out teams that have fewer than 25 locations at every time point
# for_loc_figure <- num_loc %>%
#   group_by(model) %>%
#   filter(max(n_loc) >= NUM_UNITS) %>% #remove models with fewer than 25 locations at all times
#   filter(min(sat_fcast_week) <= last_1wk_target_end_date) %>% #filter models that have start date before end of scored period
#   filter(!(model %in% c( "CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid"))) %>% #remove models that aren't secondary or primary
#   mutate(sum_loc = sum(n_loc)) %>%
#   ungroup() 
# 
# 
# for_loc_figure$model <- fct_reorder(for_loc_figure$model, for_loc_figure$sum_loc) #reorder factors by number of submission weeks
# for_loc_figure$model_numeric <- as.numeric(for_loc_figure$model)  #create numeric value for model names


for_loc_figure <- read.csv("paper-inputs/heatmap_data.csv") %>%
  mutate(sat_fcast_week = as.Date(sat_fcast_week),
       model = fct_reorder(model, n_weeks_submit_forecast, max),
       model_numeric = as.numeric(model)) 

scored_models <- read_csv("paper-inputs/inc-scores.csv") %>%
                        filter(include_overall == "TRUE") %>%
                        group_by(model) %>%
                        summarise(n_forecasts = n()) %>%
                        arrange(desc(n_forecasts)) %>%
                        pull(model)


#Plot of locations each model submitted to each week
sf1 <- ggplot(for_loc_figure, aes(y=model, x=sat_fcast_week, fill= n_loc < 25)) + 
  geom_tile() +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 weeks") +
  #geom_text(aes(label = num_units_eligible), size = 3.5) +
  # geom_rect(color="#ED42FA",
  #           xmin= (first_1wk_target_end_date -7) - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
  #           xmax= (last_4wk_target_end_date -7) + 3.5 ,
  #           ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "JHU_IDD-CovidSP"]) + .5,
  #           ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "IHME-CurveFit"]) - .5,  
  #           size = .75, fill=alpha("grey",0)) +
  # geom_rect(color="#ED42FA",
  #           xmin= (first_1wk_target_end_date -7) - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
  #           xmax= (last_4wk_target_end_date -7) + 3.5 ,
  #           ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UMich-RidgeTfReg"]) + .5,
  #           ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UMich-RidgeTfReg"]) - .5,
  #           size = .75, fill=alpha("grey",0)) +
  # geom_rect(color="#340EC3",
  #           xmin= (last_4wk_target_end_date) - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
  #           xmax= (last_date_evaluated  -7) + 3.5 ,
  #           ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "JHU_IDD-CovidSP"]) + .5,
  #           ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "IHME-CurveFit"]) - .5,  
  #           size = .75, fill=alpha("grey",0)) +
  # geom_rect(color="#340EC3",
  #           xmin= (last_4wk_target_end_date) - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
  #           xmax= (last_date_evaluated  -7) + 3.5 ,
  #           ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UMich-RidgeTfReg"]) + .5,
  #           ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UMich-RidgeTfReg"]) - .5,
  #           size = .75, fill=alpha("grey",0)) +
  scale_fill_manual(name = " ", values = c( "turquoise3","lightgrey"),labels = c("Eligible","Ineligible" )) +
  xlab("Saturday of Forecast Submission Week") + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 8, colour = ifelse(levels(for_loc_figure$model) %in% scored_models, 'violetred', 'black')),
        title = element_text(size = 9)) +
  guides(size = "none", color = "none", alpha = "none") +
  scale_y_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR")) +
  geom_vline(xintercept  = range_fcast_dates, linetype = 2) 




# ggsave("../figures/inc_loc_heatmap.jpg", width=3, height=5)
# ggsave("../figures/incidence_loc_heatmap.png", width=3, height=5)

pdf(file = "figures/inc-loc-heatmap.pdf",width=11, height=6)
print(sf1)
dev.off()

jpeg(file = "figures/inc-loc-heatmap.jpg", width=11, height=6, units="in", res=300)
print(sf1)
dev.off()


#Count number of models
length(unique(for_loc_figure$model))

#Count number of submissions for scored models
submission_count <- for_loc_figure %>% filter(model %in% c("IHME-CurveFit", "YYG-ParamSearch", "PSI-DRAFT", "RobertWalraven-ESG", 
 "USACE-ERDC_SEIR", "CU-select", "NotreDame-mobility", 
"UA-EpiCovDA", "UCLA-SuEIR", "Covid19Sim-Simulator", 
"COVIDhub-ensemble", "LANL-GrowthRate", "OliverWyman-Navigator", 
"UT-Mobility", "COVIDhub-baseline", "GT-DeepCOVID", "JHU_IDD-CovidSP", 
"MOBS-GLEAM_COVID", "UMass-MechBayes"))

#Count number of forecasts
sum(submission_count$n_loc) *4







