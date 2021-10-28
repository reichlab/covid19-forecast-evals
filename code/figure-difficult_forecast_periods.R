#Forecasting difficult forecast periods 



#locations to forecast / score:  

#Period A: 

# (a) the summer 2020 waves in the south and southwest, 
# (b) the Nov 2020 early rise in cases in some states before the major winter wave began, 
# (c) the B.1.1.7/alpha wave that lagged the major winter wave in the midwest in March/April 2021, and the 
# (d) delta wave as mentioned in point 1 above. 

#10/12/21 plan of action:
#pick states and time periods for each of these. They can be changed as needed. 

#load libraries
library(covidHubUtils)
library(tidyverse)

theme_set(theme_bw())
data("hub_locations")

source("code/load-global-analysis-dates.R")

#read in files

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") 

#Summer waves in the South and Southwest:
fcast_AL_summer2020 <-  load_forecasts(
  dates = seq.Date(as.Date("2020-05-11"),
                   as.Date("2020-08-11"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c( "01"))

# #B. Nov 2020 early rise in cases in some states before the major winter wave began
fcast_AZ_Nov2020 <-  load_forecasts(
  dates = seq.Date(as.Date("2020-10-01"),
                   as.Date("2020-12-01"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c( "04"))

#C.  B.1.1.7/alpha wave that lagged the major winter wave in the midwest in March/April 2021
fcast_MI_Mar2021 <-  load_forecasts(
  dates = seq.Date(as.Date("2021-03-01"),
                   as.Date("2021-05-01"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c("26"))

#D. Delta Wave
fcast_FL_Aug2021 <-  load_forecasts(
  dates = seq.Date(as.Date("2021-07-01"),
                   as.Date("2021-10-01"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c("12"))

all_forecasts <- rbind(fcast_AL_summer2020, fcast_AZ_Nov2020, fcast_MI_Mar2021, fcast_FL_Aug2021) 
# 
# all_forecasts$location_new <- factor(all_forecasts$location_name, levels = c("Alabama", "Arizona", "Michigan", "Florida"))


#Load truth data 
all_truth <- inc_scores %>%
  filter(model == "COVIDhub-ensemble") %>%
  filter(target_end_date > as.Date("2020-05-15") & target_end_date < as.Date("2020-10-01") & location == "01" |
           target_end_date > as.Date("2020-09-11") & target_end_date < as.Date("2021-01-11") & location == "04" |
           target_end_date > as.Date("2021-02-11") & target_end_date < as.Date("2021-06-11") & location == "26" |
           target_end_date > as.Date("2021-06-01") & target_end_date < as.Date("2021-12-11") & location == "12") %>%
  select(model, target_variable, target_end_date,location, value = truth_value, location_name) %>%
  mutate(model = "Observed Data (JHU)")

# 
# all_truth$location_new <- factor(all_truth$location_name, levels = c("Alabama", "Arizona", "Michigan", "Florida"))



p1_forecasts <- plot_forecasts(forecast_data = all_forecasts,
                     truth_data = all_truth,
                     models = c("COVIDhub-ensemble"),
                     forecast_dates = c(as.Date("2020-06-08"), as.Date("2020-07-06"), as.Date("2020-07-27"),  as.Date("2020-08-10"),
                                        as.Date("2020-10-05"), as.Date("2020-10-26"),as.Date("2020-11-02"), as.Date("2020-11-30"),
                                        as.Date("2021-03-01"), as.Date("2021-03-29"),as.Date("2021-04-12"),  as.Date("2021-04-26"),
                                        as.Date("2021-07-05"), as.Date("2021-07-26"),as.Date("2021-08-09"), as.Date("2021-08-16"), as.Date("2021-08-30"), as.Date("2021-09-27")),
                     target_variable = "inc death",
                     # truth_source = "JHU",
                     fill_transparency = 0.5,
                     show_caption = FALSE, 
                     facet = . ~ factor(location_name,levels=c("Alabama","Arizona",  "Michigan", "Florida")),
                     facet_scales ='free',
                     facet_ncol = 1,
                     fill_by_model = TRUE,
                     plot=FALSE,
                     top_layer = c("truth"),
                     subtitle = "none")  +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%y") +
  theme(legend.position='none')


horizons_difficult_periods <- inc_scores %>% 
  filter(include_overall) %>%
  filter(horizon %in% 1:4) %>%
  filter(target_end_date > as.Date("2020-05-15") & target_end_date < as.Date("2020-10-01") & location == "01" |
           target_end_date > as.Date("2020-09-11") & target_end_date < as.Date("2021-01-11") & location == "04" |
           target_end_date > as.Date("2021-02-11") & target_end_date < as.Date("2021-06-11") & location == "26" |
           target_end_date > as.Date("2021-06-01") & target_end_date < as.Date("2021-12-11") & location == "12") %>%
  group_by(model, horizon, location_name) %>% 
  mutate(n_weeks = n()) %>% ungroup() %>% filter(n_weeks > 3) %>% #have to have submitted for at least 3 weeks during eval period
  group_by(model, horizon, location_name) %>% 
  summarise(avg_90PI = mean(quantile_coverage_0.95, na.rm = T)) %>% ungroup() 


horizons_difficult_periods$location_new<- factor(horizons_difficult_periods$location_name, 
                                    levels=c("Alabama",  "Arizona", "Michigan", "Florida"))


calib_avg_select <- horizons_difficult_periods %>%
  group_by(horizon, location_new) %>%
  summarize(avg_90PI= mean(avg_90PI, na.rm = T)) 

calib_baseline_select <- horizons_difficult_periods %>%
  filter(model == "COVIDhub-baseline")

calib_ensemble_select <- horizons_difficult_periods %>%
  filter(model == "COVIDhub-ensemble")


p2_calib_graph_hardperiod <- ggplot(horizons_difficult_periods, aes(x=horizon, y=avg_90PI)) + 
  geom_line(aes(group=model), color = "darkgray", alpha = 0.5)+
  theme_bw() +
  geom_point(aes(group=model),color = "darkgray", alpha = 0.5, size = 2) + 
  facet_wrap(location_new ~ . , ncol = 1, scale = "free_x") +
   geom_line(data = calib_avg_select, aes(color = "blue")) + 
  geom_point(data = calib_avg_select, aes(color = "blue", shape = "blue"),size = 2) +
  geom_line(data = calib_baseline_select, aes(color = "green")) + 
  geom_point(data = calib_baseline_select, aes(color="green", shape = "green"), size = 2) +
  geom_line(data = calib_ensemble_select, aes(color="red")) +
  geom_point(data = calib_ensemble_select, aes(color="red", shape = "red"), size = 2) +
  xlab("Horizon") + ylab("Observed Coverage") + ggtitle("avg 95% PI during difficult forecasting period") +
  scale_color_identity(name = NULL, 
                       breaks = c("blue", "green", "red"), 
                       labels = c("Average of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
                       guide = "legend") +
  scale_shape_manual(name = NULL,
                     values = c(19, 15, 17),
                     labels = c("Average of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(group = "none") +
 #scale_x_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) +
  scale_y_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) + 
  theme(legend.position = c(0.1, 0.77), legend.justification = c(0,-.1),
        legend.background=element_blank()) 


#Fig 3 all periods 

horizons_all_periods <- inc_scores %>% 
  filter(include_overall) %>%
  filter(horizon %in% 1:4) %>%
  filter(location %in% c("01","04", "26", "12")) %>%
  group_by(model, horizon, location_name) %>%
  summarise(avg_90PI = mean(quantile_coverage_0.95, na.rm = T))

horizons_all_periods$location_new<- factor(horizons_all_periods$location_name, 
                                                 levels=c("Alabama",  "Arizona", "Michigan", "Florida"))

calib_avg <- horizons_all_periods %>%
  group_by(horizon, location_new) %>%
  summarize(avg_90PI= mean(avg_90PI, na.rm = T)) 

calib_baseline <- horizons_all_periods %>%
  filter(model == "COVIDhub-baseline")

calib_ensemble <- horizons_all_periods %>%
  filter(model == "COVIDhub-ensemble")

p3_calib_graph_all <- ggplot(horizons_all_periods, aes(x=horizon, y=avg_90PI)) + 
  geom_line(aes(group=model), color = "darkgray", alpha = 0.5) +
  theme_bw() +
  geom_point(aes(group=model),color = "darkgray", alpha = 0.5, size = 2) + 
  facet_wrap(location_new ~ . , ncol = 1, scale = "free_x") +
  geom_line(data = calib_avg, aes(color = "blue")) + 
  geom_point(data = calib_avg, aes(color = "blue", shape = "blue"),size = 2) +
  geom_line(data = calib_baseline, aes(color = "green")) + 
  geom_point(data = calib_baseline, aes(color="green", shape = "green"), size = 2) +
  geom_line(data = calib_ensemble, aes(color="red")) +
  geom_point(data = calib_ensemble, aes(color="red", shape = "red"), size = 2) +
  xlab("Horizon") + ylab("Observed Coverage") +  ggtitle("avg 95% PI across entire forecast period") +
  scale_color_identity(name = NULL,
                       breaks = c("blue", "green", "red"),
                       labels = c("Average of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
                       guide = "none") +
  scale_shape_manual(name = NULL,
                     values = c(19, 15, 17),
                     labels = c("Average of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(group = "none", color = "none", shape = "none") +
 # scale_x_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) +
  scale_y_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) 
  # theme(legend.position = c(0.0, 0.75), legend.justification = c(0,-.1),
  #       legend.background=element_blank()) 



#Merge 3 plots
pdf(file = "figures/wis-difficult_forecast_periods.pdf", width=6, height=7)
gridExtra::grid.arrange(p1_forecasts, p2_calib_graph_hardperiod, p3_calib_graph_all, layout_matrix = matrix(c(1,2,3), ncol=3))
dev.off()

# pdf(file = "figures/wis-difficult_forecast_periods_point.pdf", width=8, height=9)
# gridExtra::grid.arrange(p1_forecasts, p2_wis, p3_pi, layout_matrix = matrix(c(1,2,3), ncol=3))
# dev.off()



###### TRASH 

# 
# p2_pi <- ggplot(horizons_difficult_periods, aes(x = horizon, y = avg_90PI)) +
#  
#   geom_line(aes(group = model), color="darkgray", alpha=.5) +
#   geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
#   stat_summary(fun=mean, geom="line", aes(color="blue")) +
#   stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
#   geom_line(data=filter(horizons_all_scores, model=="COVIDhub-ensemble"), aes(group = model, color="red")) +
#   geom_point(data=filter(horizons_all_scores, model=="COVIDhub-ensemble"), aes(group = model, color="red", shape = "17")) +
#   # geom_line(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green")) +
#   # geom_point(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green", shape = "15")) +
#   scale_y_continuous(name = "Prediction Interval") +
#   # scale_color_identity(name = NULL,
#   #                      breaks = c( "blue", "green", "red"),
#   #                      labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   # scale_shape_manual(name = NULL,
#   #                    values = c(15, 17,19),
#   #                    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   #scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
#   # guides(color="none", group = "none", shape = "none") +
#   ggtitle("avg 95% PI by model") 
# # +
# #   theme(axis.text.x = element_text(vjust = 2, hjust = -0.2, angle=45))
# 


# truth_AL_summer2020 <- load_truth(
#   truth_source = "JHU",
#   target_variable = "inc death",
#   truth_end_date = "2020-09-01",
#   locations = "01", #Alabama
#   hub = c("US"))

# score_AL_summer2020 <-  score_forecasts(
#   forecasts = fcast_AL_summer2020, 
#   truth = truth_AL_summer2020)

# truth_AZ_Nov2020 <- load_truth(
#   truth_source = "JHU",
#   target_variable = "inc death",
#   truth_end_date = "2020-12-01",
#   locations = "04", #Arizona
#   hub = c("US"))

# score_AZ_Nov2020 <-  score_forecasts(
#   forecasts = fcast_AZ_Nov2020, 
#   truth = truth_AZ_Nov2020,
#   use_median_as_point = TRUE)

# truth_MI_Mar2021 <- load_truth(
#   truth_source = "JHU",
#   target_variable = "inc death",
#   truth_end_date = "2021-05-01",
#   locations = "26", #Michigan
#   hub = c("US"))

# truth_FL_Aug2021 <- load_truth(
#   truth_source = "JHU",
#   target_variable = "inc death",
#   truth_end_date = "2021-10-01",
#   locations = "12", #Florida
#   hub = c("US"))

# score_MI_Mar2021 <-  score_forecasts(
#   forecasts = fcast_MI_Mar2021, 
#   truth = truth_MI_Mar2021,
#   use_median_as_point = FALSE)


# score_FL_Aug2021 <-  score_forecasts(
#   forecasts = fcast_FL_Aug2021, 
#   truth = truth_FL_Aug2021,
#   use_median_as_point = FALSE)


#Bind dfs together 

# all_truth <- rbind(truth_AL_summer2020, truth_AZ_Nov2020, truth_MI_Mar2021, truth_FL_Aug2021) %>%

# all_scores <- rbind(score_AL_summer2020, score_AZ_Nov2020, score_MI_Mar2021, score_FL_Aug2021)
# all_scores$location_f = factor(all_scores$location, levels=c("01","04", "26", "12"))

# p1_forecasts_point <- plot_forecasts(forecast_data = all_forecasts,
#                                truth_data = all_truth,
#                                target_variable = "inc death",
#                                truth_source = "JHU",
#                                fill_transparency = 0.5,
#                                intervals = NULL,
#                                show_caption = FALSE, 
#                                facet = . ~ location_new,
#                                facet_scales ='free',
#                                facet_ncol = 1,
#                                fill_by_model = TRUE,
#                                plot=FALSE,
#                                top_layer = c("truth"),
#                                subtitle = "none")  +
#   scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
#   theme(legend.position='none')

# , axis.text.x = element_text(hjust = -0.2, angle=45)
# 
# fig2_scores <- all_scores %>%
#   group_by(horizon, location_f, target_end_date) %>%
#   summarise(mean_wis = mean(wis, na.rm = T)) %>%
#   mutate(model = "mean average") 
# 
# fig2_scores2 <- all_scores %>%
#   filter(model == "COVIDhub-ensemble") %>%
#   group_by(horizon, location_f, target_end_date) %>%
#   summarise(mean_wis = mean(wis, na.rm = T)) %>%
#   mutate(model = "COVIDhub-ensemble") 
# 
# fig2_for_graph <- rbind(fig2_scores, fig2_scores2) %>% filter(horizon %in% c("1","4"))


# 
# p2_wis <- ggplot(filter(all_scores, horizon =="1"), aes(x = target_end_date, y = scales::oob_squish(wis, range(0,500)))) +
#   facet_wrap(location_f ~ . , scales = "free", ncol = 1) +
#   geom_line(aes(group = model), color="darkgray", alpha=.5) +
#   geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
#   stat_summary(fun=mean, geom="line", aes(color="blue")) +
#   stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
#   geom_line(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red")) +
#   geom_point(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red", shape = "17")) +
#   # geom_line(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green")) +
#   # geom_point(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green", shape = "15")) +
#   scale_y_continuous(name = "WIS") +
#   scale_color_identity(name = NULL,
#                        breaks = c( "blue", "green", "red"),
#                        labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   scale_shape_manual(name = NULL,
#                      values = c(15, 17,19),
#                      labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
#   guides(color="none", group = "none", shape = "none") +
#   ggtitle("1-wk ahead WIS by model") 

# 
# 
# p2_wis <- ggplot(fig2_for_graph, aes(x = target_end_date, y = mean_wis, color = model, linetype = horizon)) +
#   facet_wrap(location_f ~ . , scales = "free", ncol = 1) +
#   geom_line() +
#   geom_point( alpha=.5, size = 2) +
#   # stat_summary(fun=mean, geom="line", aes(color="blue")) +
#   # stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
#   # geom_line(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red")) +
#   # geom_point(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red", shape = "17")) +
#   scale_y_continuous(name = "WIS") +
#   # scale_color_identity(name = NULL,
#   #                      breaks = c( "blue",  "red"),
#   #                      labels = c("Average score of all models", "COVIDhub-ensemble")) +
#   # scale_shape_manual(name = NULL,
#   #                    values = c(15,19),
#   #                    labels = c( "1-week ahead horizon", "4-week ahead horizon")) +
#   scale_x_date(name=NULL, date_breaks = "2 weeks", date_labels = "%b-%Y") +
#   #guides(color="none", group = "none", shape = "none", linetype = "none") +
#   ggtitle("1-wk and 4-wk ahead WIS") 

