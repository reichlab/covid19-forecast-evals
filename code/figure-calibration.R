library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)
library(directlabels)

theme_set(theme_bw())
data("hub_locations")

source("code/load-global-analysis-dates.R")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  mutate(target = fct_relevel(target, 
    "1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death",
    "5 wk ahead inc death",  "6 wk ahead inc death",  "7 wk ahead inc death",  "8 wk ahead inc death",
    "9 wk ahead inc death",  "10 wk ahead inc death",  "11 wk ahead inc death",  "12 wk ahead inc death",
    "13 wk ahead inc death",  "14 wk ahead inc death",  "15 wk ahead inc death",  "16 wk ahead inc death",
    "17 wk ahead inc death",  "18 wk ahead inc death")) 

## compute nice table
calibration_scores_inc <- inc_scores %>%
  filter(include_overall == "TRUE") %>%
  group_by(model, target, horizon) %>%
  summarise(percent_calib50 = mean(coverage_50, na.rm = T),
            percent_calib95 = mean(coverage_95, na.rm = T)) %>% 
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == 4, model, NA_character_)) %>%
  mutate(label = fct_recode(label,"IHME-SEIR" = "IHME-CurveFit")) 
  

calibration_scores_inc_table <- calibration_scores_inc %>%
    pivot_wider(names_from = target, values_from = c(percent_calib50, percent_calib95)) 

# ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib50, x=model)) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_hline(yintercept=.5, linetype=2) +
#   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib95, x=model)) + 
#  geom_bar(position="dodge", stat="identity") + 
#  geom_hline(yintercept=.95, linetype=2) +
#  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

calib95 <- 
  ggplot(filter(calibration_scores_inc, horizon < 5), aes(x=horizon, y=percent_calib95, color=model, group=model)) + 
  geom_line() + geom_point() + 
  #geom_label(aes(label=model)) +
  #geom_dl(aes(label=model), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
  geom_hline(yintercept=.95, linetype=5) +
  scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, .95, 1)) +
  scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
  guides(color= "none") +
  ggtitle("B: 95% prediction interval coverage rates, by model")+
  geom_text_repel(aes(label = label),
    nudge_x = 0.5,
    hjust = 0,
    size=2.5,
    box.padding = 0.1,
    direction = "y",
    segment.linetype = 2, 
    segment.alpha = 0.5, 
    # min.segment.length = Inf,
    na.rm = TRUE)

calib50 <- 
  ggplot(filter(calibration_scores_inc, horizon < 5), aes(x=horizon, y=percent_calib50, color=model, group=model)) + 
  geom_line() + geom_point() + 
  #geom_label(aes(label=model)) +
  geom_hline(yintercept=.5, linetype=5) +
  #geom_dl(aes(label=model), method = list(dl.trans(x = x), "left.polygons", cex = 0.8)) +
  scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, 1)) +
  scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
  guides(color="none") +
  ggtitle("A: 50% prediction interval coverage rates, by model") +
  geom_text_repel(aes(label = label),
    nudge_x = 0.5,
    hjust = 0,
    size=2.5,
    box.padding = 0.1,
    direction = "y",
    segment.linetype = 2, 
    segment.alpha = 0.5, 
    # min.segment.length = Inf,
    na.rm = TRUE)


pdf(file = "figures/pi-coverage.pdf", width=8, height=6)
gridExtra::grid.arrange(calib50, calib95)
dev.off()

jpeg(file = "figures/pi-coverage.jpg", width=8, height=6, units="in", res=200)
gridExtra::grid.arrange(calib50, calib95)
dev.off()


#Calibration table (table 2)
calib_table <- inc_scores %>%
  filter(include_overall == "TRUE") %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(target %in% c("1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death")) %>% 
  group_by(model) %>%
  summarise(percent_calib50 = round(mean(coverage_50, na.rm = T), 2),
            percent_calib95 = round(mean(coverage_95, na.rm = T), 2),
            n_forecasts=n()) %>% 
  select(model, n_forecasts, percent_calib50, percent_calib95) %>%
  ungroup() %>%
  arrange(-percent_calib50)


# #Compute Table by Phase
# 
# calibration_scores_inc_phase <- inc_scores %>%
#   filter(include_phases == "TRUE") %>%
#   mutate(seasonal_phase = case_when(forecast_date < first_forecast_date_summer ~ "spring",
#                                     forecast_date >= first_forecast_date_summer & forecast_date  < first_forecast_date_winter ~ "summer",
#                                     forecast_date >= first_forecast_date_winter ~ "winter")) %>%
# group_by(model, target, horizon, seasonal_phase) %>%
#   summarise(percent_calib50 = mean(coverage_50, na.rm = T),
#             percent_calib95 = mean(coverage_95, na.rm = T)) %>% 
#   ungroup() %>%
#   group_by(model, seasonal_phase) %>%
#   mutate(label = if_else(horizon == 4, model, NA_character_)) %>%
#   mutate(label = fct_recode(label,"IHME-SEIR" = "IHME-CurveFit")) 
# 
# 
# calibration_scores_inc_table_phase <- calibration_scores_inc_phase %>%
#   pivot_wider(names_from = target, values_from = c(percent_calib50, percent_calib95)) 
# 
# # ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib50, x=model)) + 
# #   geom_bar(position="dodge", stat="identity") + 
# #   geom_hline(yintercept=.5, linetype=2) +
# #   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
# 
# # ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib95, x=model)) + 
# #  geom_bar(position="dodge", stat="identity") + 
# #  geom_hline(yintercept=.95, linetype=2) +
# #  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
# 
# calib95_phase <- 
#   ggplot(filter(calibration_scores_inc_phase, horizon < 5), aes(x=horizon, y=percent_calib95, color=model, group=model)) + 
#   geom_line() + geom_point() + 
#   #geom_label(aes(label=model)) +
#   #geom_dl(aes(label=model), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
#   geom_hline(yintercept=.95, linetype=5) +
#   scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, .95, 1)) +
#   scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
#   guides(color= "none") +
#   ggtitle("B: 95% prediction interval coverage rates, by model, by phase")+
#   geom_text_repel(aes(label = label),
#                   nudge_x = 0.5,
#                   hjust = 0,
#                   size=1.4,
#                   box.padding = 0.1,
#                   direction = "y",
#                   segment.linetype = 2, 
#                   segment.alpha = 0.5, 
#                   # min.segment.length = Inf,
#                   na.rm = TRUE) +
#   facet_wrap(~seasonal_phase)
# 
# calib50_phase <- 
#   ggplot(filter(calibration_scores_inc_phase, horizon < 5), aes(x=horizon, y=percent_calib50, color=model, group=model)) + 
#   geom_line() + geom_point() + 
#   #geom_label(aes(label=model)) +
#   geom_hline(yintercept=.5, linetype=5) +
#   #geom_dl(aes(label=model), method = list(dl.trans(x = x), "left.polygons", cex = 0.8)) +
#   scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, 1)) +
#   scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
#   guides(color="none") +
#   ggtitle("A: 50% prediction interval coverage rates, by model, by phase") +
#   geom_text_repel(aes(label = label),
#                   nudge_x = 0.5,
#                   hjust = 0,
#                   size=1.4,
#                   box.padding = 0.1,
#                   direction = "y",
#                   segment.linetype = 2, 
#                   segment.alpha = 0.5, 
#                   # min.segment.length = Inf,
#                   na.rm = TRUE) +
#   facet_wrap(~seasonal_phase)
# 
# 
# pdf(file = "figures/pi-coverage_phase.pdf", width=8, height=6)
# gridExtra::grid.arrange(calib50_phase, calib95_phase)
# dev.off()
# 
# jpeg(file = "figures/pi-coverage_phase.jpg", width=8, height=6, units="in", res=200)
# gridExtra::grid.arrange(calib50_phase, calib95_phase)
# dev.off()
