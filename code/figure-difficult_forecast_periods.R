library(tidyverse)

source("code/load-global-analysis-dates.R")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(target %in% paste(c(1,4), "wk ahead inc death"),
         target_end_date_1wk_ahead >= first_target_end_date,
         target_end_date_1wk_ahead <= last_target_end_date) 

locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")

inc_scores_overall <- inc_scores %>% filter(include_overall == "TRUE")


#Period 1: summer 2020 waves in the south and southwest
# 
# ## assemble truth data observations for US levels
# load_summer_south_and_southwest <- load_forecasts(
#   dates = seq.Date(as.Date("2020-05-11"), 
#                             as.Date("2020-07-11"), 
#                             by="3 weeks"),
#   targets = paste(1:4, "wk ahead inc death"),
#   locations = c( "06", "08", "13", "22","32",
#                 "35", "48"))
# 
# p1 <- plot_forecasts(forecast_data = load_summer_south_and_southwest,
#                      target_variable = "inc death",
#                      truth_source = "JHU",
#                      fill_by_model = TRUE,
#                      fill_transparency = 0.5,
#                      intervals = c(0.5,0.95),
#                      show_caption = FALSE, 
#                      facet = ~location,
#                      facet_scales ='free_y',
#                      plot=FALSE) 
# 
# 
# p1_updated <- p1 + 
#   scale_x_date(
#     limits=c(as.Date("2020-05-11"), as.Date("2020-07-11")), 
#     date_breaks = "2 weeks",
#     name=element_blank(), 
#     expand=c(0,0)) +
#   geom_vline(xintercept=range_fcast_dates, linetype=2) +
#   theme(axis.ticks.length.x = unit(0.5, "cm"),
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
# 
# #p1_legend <- get_legend(p1_updated)
# p1_updated_no_legend <- p1_updated + theme(legend.position='none')


#Select time frame to use 
R1_df <- data.frame(Region = rep("Region 1", 6), location_name = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                                                                 "Rhode Island", "Vermont"))
R2_df <- data.frame(Region = rep("Region 2", 4), location_name = c("New Jersey", "New York", "Puerto Rico", "Virgin Islands"))
R3_df <- data.frame(Region = rep("Region 3", 6), location_name = c("Delaware", "District of Columbia", "Maryland", "Pennsylvania", 
                                                               "Virginia", "West Virginia"))
R4_df <- data.frame(Region = rep("Region 4", 8), location_name = c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi",
                                                               "North Carolina", "South Carolina", "Tennessee"))
R5_df <- data.frame(Region = rep("Region 5", 6), location_name = c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin"))
R6_df <- data.frame(Region = rep("Region 6", 5), location_name = c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas"))
R7_df <- data.frame(Region = rep("Region 7", 4), location_name = c("Iowa", "Kansas", "Missouri", "Nebraska"))
R8_df <- data.frame(Region = rep("Region 8", 6), location_name = c("Colorado"," Montana", "North Dakota", "South Dakota"," Utah", "Wyoming"))
R9_df <- data.frame(Region = rep("Region 9", 6), location_name = c("Arizona", "California", "Hawaii", "Nevada", "American Samoa", 
                                                               "Northern Mariana Islands"))
R10_df <- data.frame(Region = rep("Region 9", 4), location_name = c("Alaska", "Idaho", "Oregon","Washington"))

US_df <- data.frame(Region = "US", location_name = c("United States"))

all_regions <- bind_rows(R1_df, R2_df, R3_df,R4_df,R5_df,R6_df,R7_df,R8_df,R9_df,R10_df, US_df)
  

plot_states_summer_2020 <- load_forecasts(
  models = "COVIDhub-ensemble",
  dates = seq.Date(as.Date("2020-05-11"), 
                   as.Date("2020-08-11"), 
                   by="3 weeks"),
  targets = paste(1:4, "wk ahead inc death")) %>% 
  left_join(all_regions) 


plot_states_2020 <- plot_states_summer_2020 %>%
  mutate(location = fct_relevel(location, levels(Region))) %>% 
  mutate(location = as.character(location))


pdf('figures/summer2020_trends.pdf')
for(i in 1:7) {
  p1 <- plot_forecasts(forecast_data = plot_states_2020,
                       target_variable = "inc death",
                       truth_source = "JHU",
                       fill_transparency = 0.5,
                       intervals = c(0.5,0.95),
                       show_caption = FALSE, 
                       facet = ~ Region + location,
                       facet_scales ='free_y',
                       plot=FALSE)  +
    ggforce::facet_wrap_paginate(~Region + location, ncol = 2, nrow = 4,  scales = "free_y", page = i) 
  print(p1)
}
dev.off()



# 
# f4a <- ggplot(obs_inc_deaths, aes(x=target_end_date, y=value)) +
#   geom_point() +
#   geom_line() + 
#   scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b") +
#   scale_y_continuous(name = "Incident deaths in US") +
#   ggtitle("A: Observed weekly COVID-19 deaths in the US") +
#   theme(axis.ticks.length.x = unit(0.5, "cm"),
#         axis.text.x = element_text(vjust = 7, hjust = -0.2))
# 
# 
# avg_scores_byweek <- avg_wis_by_model_target_week %>% 
#   ungroup() %>%
#   filter(target %in% c("1 wk ahead inc death", "4 wk ahead inc death")) %>%
#   mutate(horizon = as.numeric(str_sub(target, 1, 1)),
#          target_end_date = target_end_date_1wk_ahead + 7*(horizon-1)) %>%
#   group_by(target_end_date_1wk_ahead, target) %>%
#   mutate_at(vars("mean_wis"), funs(relative_wis = (. / .[model=="COVIDhub-baseline"]))) %>%
#   ungroup() %>%  group_by(model) %>%
#   mutate(model = factor(model)) %>%
#   mutate(label = if_else(target_end_date_1wk_ahead  == max(target_end_date_1wk_ahead), 
#                          model, factor(NA_character_, ordered = FALSE))) %>% ungroup()
# 
# avg_scores_byweek$model <- factor(as.character(avg_scores_byweek$model))
# 
# 
# f4b <- ggplot(filter(avg_scores_byweek, target=="1 wk ahead inc death"), aes(x = target_end_date, y = mean_wis)) +
#   geom_line(aes(group = model), color="darkgray", alpha=.5) +
#   geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
#   stat_summary(fun=mean, geom="line", aes(color="blue")) +
#   geom_vline(xintercept = range_fcast_dates, linetype = 2) +
#   stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
#   geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red")) +
#   geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red", shape = "17")) +
#   geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green")) +
#   geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green", shape = "15")) +
#   expand_limits(y=0) +
#   coord_cartesian(ylim=c(0,200)) +
#   scale_y_continuous(name = "Average WIS") +
#   scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
#   scale_color_identity(name = NULL, 
#                        breaks = c( "blue", "green", "red"), 
#                        labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   scale_shape_manual(name = NULL,
#                      values = c(15, 17,19),
#                      labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   guides(color=FALSE, group = FALSE, shape = FALSE) +
#   ggtitle("B: Average 1-week ahead weighted interval scores by model") +
#   theme(axis.ticks.length.x = unit(0.5, "cm"),
#         axis.text.x = element_text(vjust = 7, hjust = -0.2))
# 
# 
# f4c <- ggplot(filter(avg_scores_byweek, target=="4 wk ahead inc death"), aes(x = target_end_date, y = mean_wis)) +
#   geom_line(aes(group = model), color="darkgray", alpha=.5) +
#   geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
#   stat_summary(fun=mean, geom="line", aes(color="blue")) +
#   geom_vline(xintercept = range_fcast_dates, linetype = 2) +
#   stat_summary(fun=mean, geom="point", aes(color="blue", shape = "blue")) +
#   geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red")) +
#   geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red", shape = "red")) +
#   geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green")) +
#   geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green", shape = "green")) +
#   expand_limits(y=0) +
#   ylab("Average WIS") +
#   scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
#   scale_y_continuous(name = "Average WIS") +
#   guides(group = FALSE) +
#   scale_color_manual(name = NULL, 
#                      values = c( "blue", "green", "red"), 
#                      labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
#                      guide = "legend") +
#   scale_shape_manual(name = NULL,
#                      values = c(19, 15, 17),
#                      labels = c("Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
#   coord_cartesian(ylim=c(0,500)) +
#   theme(legend.position = c(0.05, 0.8), legend.justification = c(0,.5), 
#         axis.ticks.length.x = unit(0.5, "cm"),
#         axis.text.x = element_text(vjust = 7, hjust = -0.2)) +
#   ggtitle("C: Average 4-week ahead weighted interval scores by model")
# 
# 
# pdf(file = "figures/wis-avgs-by-week.pdf", width=8, height=10)
# gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
# dev.off()
# 
# 
# jpeg(file = "figures/wis-avgs-by-week.jpg", width=8, height=10, units="in", res=200)
# gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
# dev.off()
