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

#Summer waves in the South and Southwest:
truth_AL_summer2020 <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = "2020-09-01",
  locations = "01", #Alabama
  hub = c("US"))

fcast_AL_summer2020 <-  load_forecasts(
    dates = seq.Date(as.Date("2020-05-11"),
                              as.Date("2020-08-11"),
                              by="1 day"),
    targets = paste(1:4, "wk ahead inc death"),
    locations = c( "01"))

score_AL_summer2020 <-  score_forecasts(
  forecasts = fcast_AL_summer2020, 
  truth = truth_AL_summer2020)

#B. Nov 2020 early rise in cases in some states before the major winter wave began
truth_AZ_Nov2020 <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = "2020-12-01",
  locations = "04", #Arizona
  hub = c("US"))

fcast_AZ_Nov2020 <-  load_forecasts(
  dates = seq.Date(as.Date("2020-10-01"),
                   as.Date("2020-12-01"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c( "04"))

score_AZ_Nov2020 <-  score_forecasts(
  forecasts = fcast_AZ_Nov2020, 
  truth = truth_AZ_Nov2020,
  use_median_as_point = TRUE)


#C.  B.1.1.7/alpha wave that lagged the major winter wave in the midwest in March/April 2021
truth_MI_Mar2021 <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = "2021-05-01",
  locations = "26", #Michigan
  hub = c("US"))

fcast_MI_Mar2021 <-  load_forecasts(
  dates = seq.Date(as.Date("2021-03-01"),
                   as.Date("2021-05-01"),
                   by="1 day"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = c("26"))

score_MI_Mar2021 <-  score_forecasts(
  forecasts = fcast_MI_Mar2021, 
  truth = truth_MI_Mar2021,
  use_median_as_point = FALSE)



#D. Delta Wave
truth_FL_Aug2021 <- load_truth(
  truth_source = "JHU",
  target_variable = "inc death",
  truth_end_date = "2021-10-01",
  locations = "12", #Florida
  hub = c("US"))

fcast_FL_Aug2021 <-  load_forecasts(
  dates = seq.Date(as.Date("2021-07-01"),
                   as.Date("2021-10-01"),
                   by="1 day"),
  targets = paste(1:2, "wk ahead inc death"),
  locations = c("12"))

score_FL_Aug2021 <-  score_forecasts(
  forecasts = fcast_FL_Aug2021, 
  truth = truth_FL_Aug2021,
  use_median_as_point = FALSE)




#Bind dfs together 

all_truth <- rbind(truth_AL_summer2020, truth_AZ_Nov2020, truth_MI_Mar2021, truth_FL_Aug2021) %>%
  filter(target_end_date > as.Date("2020-05-15") & target_end_date < as.Date("2020-10-01") & location == "01" |
        target_end_date > as.Date("2020-09-11") & target_end_date < as.Date("2021-01-11") & location == "04" |
        target_end_date > as.Date("2021-02-11") & target_end_date < as.Date("2021-06-11") & location == "26" |
        target_end_date > as.Date("2021-06-01") & target_end_date < as.Date("2021-12-11") & location == "12")

all_truth$location_new = factor(all_truth$location_name, levels=c("Alabama",  "Arizona", "Michigan", "Florida"))


all_forecasts <- rbind(fcast_AL_summer2020, fcast_AZ_Nov2020, fcast_MI_Mar2021, fcast_FL_Aug2021)
all_forecasts$location_new = factor(all_forecasts$location_name, levels=c("Alabama",  "Arizona", "Michigan", "Florida"))
 
all_scores <- rbind(score_AL_summer2020, score_AZ_Nov2020, score_MI_Mar2021, score_FL_Aug2021)
all_scores$location_f = factor(all_scores$location, levels=c("01","04", "26", "12"))


p1_forecasts <- plot_forecasts(forecast_data = all_forecasts,
                     truth_data = all_truth,
                     models = c("COVIDhub-ensemble"),
                     forecast_dates = c(as.Date("2020-06-08"), as.Date("2020-07-06"), as.Date("2020-07-27"),  as.Date("2020-08-10"),
                                        as.Date("2020-10-05"), as.Date("2020-10-26"),as.Date("2020-11-02"), as.Date("2020-11-30"),
                                        as.Date("2021-03-01"), as.Date("2021-03-29"),as.Date("2021-04-12"),  as.Date("2021-04-26"),
                                        as.Date("2021-07-05"), as.Date("2021-07-26"),as.Date("2021-08-09"), as.Date("2021-08-16"), as.Date("2021-08-30"), as.Date("2021-09-27")),
                     target_variable = "inc death",
                     truth_source = "JHU",
                     fill_transparency = 0.5,
                     show_caption = FALSE, 
                     facet = . ~ location_new,
                     facet_scales ='free',
                     facet_ncol = 1,
                     fill_by_model = TRUE,
                     plot=FALSE,
                     top_layer = c("truth"),
                     subtitle = "none")  +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(legend.position='none')



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

p2_wis <- ggplot(filter(all_scores, horizon =="1"), aes(x = target_end_date, y = scales::oob_squish(wis, range(0,500)))) +
  facet_wrap(location_f ~ . , scales = "free", ncol = 1) +
  geom_line(aes(group = model), color="darkgray", alpha=.5) +
  geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
  stat_summary(fun=mean, geom="line", aes(color="blue")) +
  stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
  geom_line(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red")) +
  geom_point(data=filter(all_scores, model=="COVIDhub-ensemble",  horizon =="1"), aes(group = model, color="red", shape = "17")) +
  # geom_line(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green")) +
  # geom_point(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green", shape = "15")) +
  scale_y_continuous(name = "WIS") +
  scale_color_identity(name = NULL,
                       breaks = c( "blue", "green", "red"),
                       labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  scale_shape_manual(name = NULL,
                     values = c(15, 17,19),
                     labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
  guides(color="none", group = "none", shape = "none") +
  ggtitle("1-wk ahead WIS by model") 



# Calibration Plot (95%)

horizons_all_scores <- all_scores %>% 
  group_by(model, location_f, target_end_date) %>% 
  summarise(n_horiz = n(),
            horizon_pi_95 = mean(quantile_coverage_0.95))

p3_pi <- ggplot(horizons_all_scores, aes(x = target_end_date, y = horizon_pi_95)) +
  facet_wrap(location_f ~ . , scales = "free", ncol = 1) +
  geom_line(aes(group = model), color="darkgray", alpha=.5) +
  geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
  stat_summary(fun=mean, geom="line", aes(color="blue")) +
  stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
  geom_line(data=filter(horizons_all_scores, model=="COVIDhub-ensemble"), aes(group = model, color="red")) +
  geom_point(data=filter(horizons_all_scores, model=="COVIDhub-ensemble"), aes(group = model, color="red", shape = "17")) +
  # geom_line(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green")) +
  # geom_point(data=filter(all_scores, model=="COVIDhub-baseline", horizon =="1"), aes(group = model, color="green", shape = "15")) +
  scale_y_continuous(name = "Prediction Interval") +
  scale_color_identity(name = NULL,
                       breaks = c( "blue", "green", "red"),
                       labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  scale_shape_manual(name = NULL,
                     values = c(15, 17,19),
                     labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b-%Y") +
  guides(color="none", group = "none", shape = "none") +
  ggtitle("1-4 wk ahead avg 95% PI by model") 
# +
#   theme(axis.text.x = element_text(vjust = 2, hjust = -0.2, angle=45))



#Merge 3 plots
pdf(file = "figures/wis-difficult_forecast_periods.pdf", width=8, height=9)
gridExtra::grid.arrange(p1_forecasts, p2_wis, p3_pi, layout_matrix = matrix(c(1,2,3), ncol=3))
dev.off()

# pdf(file = "figures/wis-difficult_forecast_periods_point.pdf", width=8, height=9)
# gridExtra::grid.arrange(p1_forecasts, p2_wis, p3_pi, layout_matrix = matrix(c(1,2,3), ncol=3))
# dev.off()












