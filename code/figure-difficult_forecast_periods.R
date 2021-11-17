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

## define periods of interest
periods_of_interest <- tibble(
  wave = c("summer_2020", "fall_2020", "alpha_2021", "delta_2021"),
  location_name = c("Louisiana", "North Dakota", "Michigan", "Florida"),
  first_target_end_date = c("2020-07-04", "2020-08-29", "2021-03-20", "2021-07-17"),
  last_target_end_date = c("2020-10-10", "2021-02-06", "2021-07-03", "2021-10-30"),
  max_death_for_label = c(600, 200, 1000, 3000)
) %>%
  mutate(
    ## forecast date of 4 week ahead forecast for the last_target_end_date
    first_target_end_date = as.Date(first_target_end_date),
    last_target_end_date = as.Date(last_target_end_date),
    first_possible_forecast_date = first_target_end_date - 26,
    last_possible_forecast_date = last_target_end_date - 5 
  ) %>%
  left_join(select(hub_locations, location_name, fips))

## get forecast dates for all weeks of interest
date_seqs <- purrr::map(
  1:nrow(periods_of_interest),
  function(x)
    seq.Date(periods_of_interest$first_possible_forecast_date[x],
             periods_of_interest$last_possible_forecast_date[x],
             by="1 weeks")
)


## load forecasts
all_forecasts <- purrr::map_dfr(
  1:nrow(periods_of_interest),
  function(x) {
    load_forecasts(
      models = c("COVIDhub-ensemble"),
      dates = date_seqs[[x]],
      targets = paste(1:4, "wk ahead inc death"),
      locations = periods_of_interest$fips[x]
    )
  }
)

## load truth
all_truth <- load_truth(truth_source = "JHU", target_variable = "inc death")


## get forecast dates every 4 weeks
date_seqs_to_plot <- purrr::map(
  1:nrow(periods_of_interest),
  function(x)
    seq.Date(periods_of_interest$first_target_end_date[x] - 5,
             periods_of_interest$last_possible_forecast_date[x] + 2,
             by="4 weeks")
)

## plot all locations/weeks into a list
# p1_forecasts <- purrr::map(
#   1:nrow(periods_of_interest),
#   function(x) {
#     update_geom_defaults("point", list(size=1))
#     tmp <- plot_forecasts(forecast_data = all_forecasts,
#                    truth_data = all_truth,
#                    models = "COVIDhub-ensemble",
#                    forecast_dates = date_seqs_to_plot[[x]],  
#                    target_variable = "inc death",
#                    fill_transparency = 0.5, 
#                    locations = periods_of_interest$fips[x],
#                    intervals = 0.95,
#                    show_caption = FALSE, 
#                    fill_by_model = TRUE,
#                    plot=FALSE,
#                    title = "none",
#                    top_layer = "truth",
#                    subtitle = "none")  +
#       annotate("text", 
#                x =  as.Date(periods_of_interest$first_target_end_date[x]), 
#                y = Inf, 
#                label=periods_of_interest$location_name[x], hjust=0, vjust = 1.2) +
#       scale_x_date(name = NULL, 
#                    limits = c(periods_of_interest$first_target_end_date[x], periods_of_interest$last_target_end_date[x]),
#                    date_breaks = "1 month", date_labels = "%b '%y") +
#       theme(
#         axis.ticks.length.x = unit(0.5, "cm"),
#         axis.text.x = element_text(vjust = 7, hjust = -0.2),
#         legend.position='none')
#     return(tmp)
#   }
# )
# 
# p1_all_forecasts <- gridExtra::grid.arrange(grobs = p1_forecasts,
#   layout_matrix = matrix(1:nrow(periods_of_interest), ncol=1))

## alternate forecast plot

truth_to_plot <- all_truth %>%
  mutate(period1 = target_end_date > periods_of_interest$first_target_end_date[1] & target_end_date < periods_of_interest$last_target_end_date[1] & location == periods_of_interest$fips[1],
         period2 = target_end_date > periods_of_interest$first_target_end_date[2] & target_end_date < periods_of_interest$last_target_end_date[2] & location == periods_of_interest$fips[2],
         period3 = target_end_date > periods_of_interest$first_target_end_date[3] & target_end_date < periods_of_interest$last_target_end_date[3] & location == periods_of_interest$fips[3],
         period4 = target_end_date > periods_of_interest$first_target_end_date[4] & target_end_date < periods_of_interest$last_target_end_date[4] & location == periods_of_interest$fips[4],
         any_period = if_any(starts_with("period"))
  ) %>%
  filter(any_period) %>%
  mutate(location_name = factor(location_name, 
                                levels=unique(periods_of_interest$location_name)))

forecasts_to_plot <- all_forecasts %>%
  mutate(period1 = target_end_date > periods_of_interest$first_target_end_date[1] & target_end_date < periods_of_interest$last_target_end_date[1] & location == periods_of_interest$fips[1],
         period2 = target_end_date > periods_of_interest$first_target_end_date[2] & target_end_date < periods_of_interest$last_target_end_date[2] & location == periods_of_interest$fips[2],
         period3 = target_end_date > periods_of_interest$first_target_end_date[3] & target_end_date < periods_of_interest$last_target_end_date[3] & location == periods_of_interest$fips[3],
         period4 = target_end_date > periods_of_interest$first_target_end_date[4] & target_end_date < periods_of_interest$last_target_end_date[4] & location == periods_of_interest$fips[4],
         any_period = if_any(starts_with("period"))
  ) %>%
  filter(any_period) %>%
  filter(quantile %in% c(0.025, 0.5, 0.975)) %>%
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "q") %>%
  mutate(location_name = factor(location_name, 
                                levels=unique(periods_of_interest$location_name)))

p1_all_forecasts_alt <- 
  ggplot(truth_to_plot, 
       aes(x=target_end_date)) +
  geom_point(aes(y=value, color="obs", shape="obs")) + geom_line(aes(y=value)) +
  geom_point(data=filter(forecasts_to_plot, horizon=="1"), 
             aes(y=q0.5, target_end_date-1, color="1wk", shape="1wk")) +
  geom_point(data=filter(forecasts_to_plot, horizon=="4"), 
             aes(y=q0.5, target_end_date+1, color="4wk", shape="4wk")) +
  geom_linerange(data=filter(forecasts_to_plot, horizon=="1"), 
                 aes(ymin=q0.025, ymax=q0.975, x=target_end_date-1, color="1wk")) +
  geom_linerange(data=filter(forecasts_to_plot, horizon=="4"), 
                 aes(ymin=q0.025, ymax=q0.975, x=target_end_date+1, color="4wk")) +
  facet_wrap(location_name~., ncol = 1, scales="free") +
  ylab("Incident deaths") +
  ggtitle("Forecasts, select states & waves") +
  scale_shape_manual(name=NULL,
                     values=c(17, 17, 16),
                     labels = c("1 week ahead forecast", "4 week ahead forecast", "observed data")) +
  scale_color_manual(name=NULL,
                     labels = c("1 week ahead forecast", "4 week ahead forecast", "observed data"), 
                     values = c("obs"="black", "1wk"="#cb181d", "4wk"="#fc9272"),
                     guide = "legend") +
  scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b '%y") +
  theme(
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2),
    legend.position = c(0, 1), 
    legend.justification = c(0,1),
    legend.background=element_blank())

### plots for coverage rates
horizons_difficult_periods <- inc_scores %>% 
  filter(
    include_overall,
    horizon %in% 1:4
    ) %>%
  mutate(
    period1 = target_end_date > periods_of_interest$first_target_end_date[1] & target_end_date < periods_of_interest$last_target_end_date[1] & location == periods_of_interest$fips[1],
    period2 = target_end_date > periods_of_interest$first_target_end_date[2] & target_end_date < periods_of_interest$last_target_end_date[2] & location == periods_of_interest$fips[2],
    period3 = target_end_date > periods_of_interest$first_target_end_date[3] & target_end_date < periods_of_interest$last_target_end_date[3] & location == periods_of_interest$fips[3],
    period4 = target_end_date > periods_of_interest$first_target_end_date[4] & target_end_date < periods_of_interest$last_target_end_date[4] & location == periods_of_interest$fips[4],
    any_period = if_any(starts_with("period"))
  ) %>%
  filter(any_period) %>%
  #have to have submitted for at least 3 weeks during eval period
  group_by(model, location_name) %>%
  mutate(n_weeks = length(unique(forecast_date))) %>%
  filter(n_weeks > 3) %>% 
  group_by(model, horizon, location_name) %>% 
  summarise(avg_95PI_cov = mean(quantile_coverage_0.95, na.rm = T)) %>% 
  ungroup() %>%
  mutate(location_new = factor(location_name, 
                               levels=unique(periods_of_interest$location_name)))


calib_avg_select <- horizons_difficult_periods %>%
  filter(!(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble"))) %>%
  group_by(horizon, location_new) %>%
  summarize(avg_95PI_cov = mean(avg_95PI_cov, na.rm = T)) 

calib_baseline_select <- horizons_difficult_periods %>%
  filter(model == "COVIDhub-baseline")

calib_ensemble_select <- horizons_difficult_periods %>%
  filter(model == "COVIDhub-ensemble")


p2_calib_graph_hardperiod <- horizons_difficult_periods %>%
  filter(!(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble"))) %>%
  ggplot(aes(x=horizon, y=avg_95PI_cov)) + 
  geom_boxplot(aes(x=factor(horizon)), color = "darkgray", width = 0.3) +
  # geom_line(aes(group=model), color = "darkgray", alpha = 0.5)+
  # geom_point(aes(group=model),color = "darkgray", alpha = 0.5, size = 2) + 
  facet_wrap(location_new ~ . , ncol = 1) +
  # geom_line(data = calib_avg_select, aes(color = "blue")) + 
  # geom_point(data = calib_avg_select, aes(color = "blue", shape = "blue"),size = 2) +
  geom_line(data = calib_baseline_select, aes(color = "green")) + 
  geom_point(data = calib_baseline_select, aes(color="green", shape = "green"), size = 2) +
  geom_line(data = calib_ensemble_select, aes(color="red"), alpha=.7) +
  geom_point(data = calib_ensemble_select, aes(color="red", shape = "red"), size = 2, alpha=.7) +
  geom_hline(yintercept=0.95, linetype=2) +
  xlab("Horizon") + ylab("Observed Coverage") + 
  ggtitle("95% PI coverage, selected wave") +
  scale_color_identity(name = NULL, 
                       breaks = c("green", "red"), 
                       labels = c("COVIDhub-baseline","COVIDhub-ensemble"),
                       guide = "legend") +
  scale_shape_manual(name = NULL,
                     values = c(15, 17),
                     labels = c("COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(group = "none") +
 #scale_x_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) +
  scale_y_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) + 
  theme(legend.position = c(0.1, 0.77), legend.justification = c(0,-.1),
        legend.background=element_blank()) 


#Fig 3 all periods 

horizons_all_periods <- inc_scores %>% 
  filter(
    include_overall,
    horizon %in% 1:4
  ) %>%
  filter(location %in% periods_of_interest$fips) %>%
  group_by(model, horizon, location_name) %>%
  summarise(avg_95PI_cov = mean(quantile_coverage_0.95, na.rm = T)) %>%
  mutate(location_new = factor(location_name, 
                               levels=unique(periods_of_interest$location_name)))

calib_avg <- horizons_all_periods %>%
  filter(!(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble"))) %>%
  group_by(horizon, location_new) %>%
  summarize(avg_95PI_cov= mean(avg_95PI_cov, na.rm = T)) 

calib_baseline <- horizons_all_periods %>%
  filter(model == "COVIDhub-baseline")

calib_ensemble <- horizons_all_periods %>%
  filter(model == "COVIDhub-ensemble")

p3_calib_graph_all <- horizons_all_periods %>%
  filter(!(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble"))) %>%
  ggplot(aes(x=horizon, y=avg_95PI_cov)) + 
  # geom_line(aes(group=model), color = "darkgray", alpha = 0.5) +
  # geom_point(aes(group=model),color = "darkgray", alpha = 0.5, size = 2) + 
  geom_boxplot(aes(x=factor(horizon)), color = "darkgray", width = 0.3) +
  facet_wrap(location_new ~ . , ncol = 1) +
  # geom_line(data = calib_avg, aes(color = "blue")) + 
  # geom_point(data = calib_avg, aes(color = "blue", shape = "blue"),size = 2) +
  geom_line(data = calib_baseline, aes(color = "green")) + 
  geom_point(data = calib_baseline, aes(color="green", shape = "green"), size = 2) +
  geom_line(data = calib_ensemble, aes(color="red")) +
  geom_point(data = calib_ensemble, aes(color="red", shape = "red"), size = 2) +
  geom_hline(yintercept=0.95, linetype=2) +
  xlab("Horizon") + ylab("Observed Coverage") +  
  ggtitle("95% PI coverage, all weeks") +
  scale_color_identity(name = NULL,
                       breaks = c("green", "red"),
                       labels = c("COVIDhub-baseline","COVIDhub-ensemble"),
                       guide = "none") +
  scale_shape_manual(name = NULL,
                     values = c(15, 17),
                     labels = c("COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(group = "none", color = "none", shape = "none") +
  scale_y_continuous(limits=c(0,1), labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) 



#Merge 3 plots
pdf(file = "figures/fig-difficult_forecast_periods.pdf", width=10, height=12)
gridExtra::grid.arrange(p1_all_forecasts_alt, p2_calib_graph_hardperiod, p3_calib_graph_all, layout_matrix = matrix(c(1,2,3), ncol=3))
dev.off()

jpeg(file = "figures/fig-difficult_forecast_periods.jpg",  width=10, height=12, units="in", res=200)
gridExtra::grid.arrange(p1_all_forecasts_alt, p2_calib_graph_hardperiod, p3_calib_graph_all, layout_matrix = matrix(c(1,2,3), ncol=3))
dev.off()
