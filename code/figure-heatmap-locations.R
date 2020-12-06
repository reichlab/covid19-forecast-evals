#library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)


#the_scores <- c("interval_50")
the_models <- c() 
the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips)
the_targets_inc <- c("4 wk ahead inc death")
the_timezeros_inc <- seq(from = as.Date("2020-05-13"), to = Sys.Date(), by="days")
                       

inc_tmp <- load_forecasts(
  forecast_dates = the_timezeros_inc,
  locations = the_locations,
  types = "quantile",
  targets = the_targets_inc) %>%
  filter(quantile == 0.5)

inc_tmp_unique <-  inc_tmp %>%
  mutate(sat_fcast_week = as.Date(calc_target_week_end_date(forecast_date, horizon = 0))) %>%
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
  ungroup()

#Filter out teams that have fewer than 25 locations at every time point
for_loc_figure <- num_loc %>%
  group_by(model) %>%
  filter(max(n_loc) >= 25) %>% #remove models with fewer than 25 locations at all times
  filter(min(sat_fcast_week) <= as.Date("2020-10-17")) %>% #filter models that have start date before end of scored period
  filter(!(model %in% c( "CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid"))) %>% #remove models that aren't secondary or primary 
  ungroup() 


for_loc_figure$model <- fct_reorder(for_loc_figure$model, for_loc_figure$n_weeks_submit_forecast) #reorder factors by number of submission weeks
for_loc_figure$model_numeric <- as.numeric(for_loc_figure$model)  #create numeric value for model names

#Plot of locations each model submitted to each week
sf1 <- ggplot(for_loc_figure, aes(y=model, x=sat_fcast_week, fill= n_loc)) + 
  geom_tile() +
  theme_bw() +
  geom_text(aes(label=n_loc), size = 3.5) +
  geom_rect(aes(color="red"),
            xmin=as.Date("2020-05-23") - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
            xmax=as.Date("2020-10-17") + 3.5 ,
            ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UMass-MechBayes"]) + .5,
            ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UCLA-SuEIR"]) - .5,  
            size = 1, fill=alpha("grey",0)) +
  geom_rect(aes(color="red"),
            xmin=as.Date("2020-05-23") - 3.5,
            xmax=as.Date("2020-10-17") + 3.5,
            ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UA-EpiCovDA"]) + .5,
            ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "USACE-ERDC_SEIR"]) - .5,
            size = 1,fill=alpha("grey",0)) +
  geom_rect(aes(color="red"),
            xmin=as.Date("2020-05-23") - 3.5,
            xmax=as.Date("2020-10-17") + 3.5,
            ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "RobertWalraven-ESG"]) + .5,
            ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "YYG-ParamSearch"]) - .5,
            size = 1,fill=alpha("grey",0)) +
  geom_rect(aes(color="red"),
            xmin=as.Date("2020-05-23") - 3.5,
            xmax=as.Date("2020-10-17") + 3.5,
            ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "IHME-CurveFit"]) + .5,
            ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "IHME-CurveFit"]) - .5,
            size = 1,fill=alpha("grey",0)) +
  scale_fill_steps(low="white", high="blue", name = "Number of Locations") +
  xlab("Saturday of Forecast Submission Week") + ylab(NULL) +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = c(for_loc_figure$sat_fcast_week)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 10)) +
  guides(fill= "none", size = "none", color = "none", alpha = "none") +
  ggtitle("Number of locations submitted for incidence death forecasts weekly")  

# ggsave("../figures/inc_loc_heatmap.jpg", width=3, height=5)
# ggsave("../figures/incidence_loc_heatmap.png", width=3, height=5)

pdf(file = "figures/inc-loc-heatmap.pdf",width=8, height=5)
print(sf1)
dev.off()

jpeg(file = "figures/inc-loc-heatmap.jpg", width=8, height=5, units="in", res=300)
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







