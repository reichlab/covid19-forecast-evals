library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)


zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

the_scores <- c("interval_50")
the_models <- c() 
the_locations <- hub_locations %>% filter(geo_type == "state") %>% pull(fips)
the_targets_inc <- c("4 wk ahead inc death")
the_timezeros_inc <- seq(as.Date("2020-04-13"), as.Date("2020-09-10"), by="days")

inc_tmp <- do_zoltar_query(zoltar_connection, project_url =  "https://zoltardata.com/api/project/44/", 
                           is_forecast_query = FALSE, 
                           models = the_models, 
                           units = the_locations, 
                           targets = the_targets_inc, 
                           timezeros = the_timezeros_inc,
                           scores = the_scores)

inc_tmp_unique <-  inc_tmp %>%
  mutate(sat_fcast_week = as.Date(calc_target_week_end_date(timezero, horizon = 0))) %>%
  group_by(model, unit, sat_fcast_week) %>%
  mutate(forecast_in_wk = row_number(), 
         last_forecast_in_wk = forecast_in_wk == max(forecast_in_wk)) %>% 
  filter(last_forecast_in_wk) %>% 
  ungroup()  


#count number of weeks each team submitted
by_weeks <- inc_tmp_unique %>%
  group_by(model, unit) %>%
  summarise(n_weeks_submit_forecast = n()) %>%
  select(-unit) %>%
  distinct() %>%
  group_by(model) %>%
  filter(n_weeks_submit_forecast == max(n_weeks_submit_forecast)) %>%
  ungroup()

#Count number of locations each team submitted weekly
num_loc <-inc_tmp_unique %>%
  group_by(model, sat_fcast_week) %>%
  summarise(n_loc = n()) %>%
  right_join(by_weeks) %>%
  mutate(model = as.factor(model)) %>%
  ungroup()

#Filter out teams that have fewer than 25 locations at every time point
for_loc_figure <- num_loc %>%
  group_by(model) %>%
  filter(max(n_loc) >= 25) %>% #remove models with fewer than 25 locations at all times
  filter(min(sat_fcast_week) <= as.Date("2020-08-29")) %>% #filter models that have start date before end of scored period
  filter(!(model %in% c( "CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid"))) %>% #remove models that aren't secondary or primary 
  ungroup() %>%
  filter(sat_fcast_week <= as.Date("2020-08-29"))


for_loc_figure$model <- fct_reorder(for_loc_figure$model, for_loc_figure$n_weeks_submit_forecast) #reorder factors by number of submission weeks
for_loc_figure$model_numeric <- as.numeric(for_loc_figure$model)  #create numeric value for model names

#Plot of locations each model submitted to each week
sf1 <- ggplot(for_loc_figure, aes(y=model, x=sat_fcast_week, fill= n_loc)) + 
  geom_tile() +
  theme_bw() +
  geom_text(aes(label=n_loc), size = 3.5) +
  geom_rect(aes(color="red"),
            xmin=as.Date("2020-05-23") - 3.5, #color of box, start date 3 days before actual date so rectangle covers entire box
            xmax=as.Date("2020-08-29") + 3.5 ,
            ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "YYG-ParamSearch"]) + .5,
            ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UCLA-SuEIR"]) - .5,  
            size = 1, fill=alpha("grey",0)) +
  # geom_rect(aes(color="red"),
  #           xmin=as.Date("2020-05-23") - 3.5,
  #           xmax=as.Date("2020-08-29") + 3.5,
  #           ymax= unique(for_loc_figure$model_numeric[for_loc_figure$model == "LANL-GrowthRate"]) + .5,
  #           ymin= unique(for_loc_figure$model_numeric[for_loc_figure$model == "UCLA-SuEIR"]) - .5,
  #           size = 1,fill=alpha("grey",0)) +
  scale_fill_steps(low="white", high="blue", name = "Number of Locations") +
  xlab("Saturday of Forecast Submission Week") + ylab(NULL) +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = c(for_loc_figure$sat_fcast_week)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 10)) +
  guides(fill=FALSE, size = FALSE, color = FALSE, alpha = FALSE) +
  ggtitle("Number of locations submitted for incidence death forecasts weekly")  
# 
# ggsave("../figures/inc_loc_heatmap.jpg", width=3, height=5)
# ggsave("../figures/incidence_loc_heatmap.png", width=3, height=5)

pdf(file = "figures/inc-loc-heatmap.pdf",width=8, height=5,res=300)
print(sf1)
dev.off()

jpeg(file = "figures/inc-loc-heatmap.jpg", width=8, height=5, units="in", res=300)
print(sf1)
dev.off()
