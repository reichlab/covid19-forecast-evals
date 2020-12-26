## plot a long-term forecast, evaluate by WIS, PI coverage
library(tidyverse)
library(covidHubUtils)
library(ggrepel)
theme_set(theme_bw())


models <- c("COVIDhub-ensemble", "COVIDhub-baseline", "IHME-CurveFit","LANL-GrowthRate","YYG-ParamSearch", "Covid19Sim-Simulator", "UCLA-SuEIR", "USACE-ERDC_SEIR")
locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")

## plots

longterm_dat <- load_latest_forecasts(models = "IHME-CurveFit",
  last_forecast_date = as.Date("2020-06-08"), forecast_date_window_size = 6,
  locations = "US",
  types = c("quantile", "point"), 
  targets = paste(1:20, "wk ahead inc death"),
  source = "zoltar")

f1 <- plot_forecast(longterm_dat, 
  models = "IHME-CurveFit", 
  target_variable = "inc death", 
  intervals = c(.5, .95), 
  show_caption=FALSE, 
  plot = FALSE) + 
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b") + 
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2)) +
  ggtitle("A: example long-term forecast from IHME", subtitle = NULL)
  
# f2 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "YYG-ParamSearch", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f3 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "LANL-GrowthRate", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f4 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "Covid19Sim-Simulator", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f5 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "UCLA-SuEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f6 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "USACE-ERDC_SEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)

# fplots <- gridExtra::grid.arrange(f1, f2, f3, f4, f5, f6)


## score evaluation

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(model %in% models, timezero <= as.Date("2020-07-13"), !(location_name %in% locs_to_exclude)) %>%
  mutate(horizon = str_split(target, " ", simplify = TRUE),
    horizon = as.numeric(horizon[,1])) %>%
  group_by(model, horizon) %>%
  mutate(nobs=n(), nlocs = length(unique(unit)))

mae_plot <- inc_scores %>%
  group_by(model, horizon) %>%
  summarize(mean_wis = mean(wis), mae = mean(abs_error)) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == max(horizon), model, NA_character_)) %>%
  ggplot(aes(x=horizon, y=mae, color=model)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "Mean absolute error") +
  scale_x_continuous(name = NULL) +
  guides(color=FALSE) +
  ggtitle("B: Mean absolute error")+
  geom_label_repel(aes(label = label),
    #nudge_x = 0.5,
    nudge_y = -.05,
    hjust = 0,
    box.padding = 0.1,
    direction = "y",
    min.segment.length = Inf,
    na.rm = TRUE)

## coverage evaluation

inc_calibration <-  read_csv("paper-inputs/inc-calibration.csv") %>%
  left_join(hub_locations, by=c("unit" = "fips")) %>%
  filter(!(location_name %in% locs_to_exclude))

inc_scores_merge <- inc_scores %>%
  left_join(inc_calibration) %>%
  pivot_wider(names_from = "quantile", values_from = "value") %>%
  mutate(calib_95 = ifelse(truth >= `0.025` & truth <= `0.975`, 1, 0),
    calib_50 = ifelse(truth >= `0.25` & truth <= `0.75`, 1, 0))

calibration_scores_inc <- inc_scores_merge %>%
  group_by(model, target) %>%
  summarise(percent_calib50 = round(sum(calib_50)/ n(),2),
    percent_calib95 = round(sum(calib_95) / n(),2)) %>% 
  mutate(horizon = str_split(target, " ", simplify = TRUE),
    horizon = as.numeric(horizon[,1])) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == max(horizon), model, NA_character_))

calib95 <- ggplot(calibration_scores_inc, aes(x=horizon, y=percent_calib95, color=model, group=model)) + 
  geom_line() + geom_point() + 
  #geom_label(aes(label=model)) +
  #geom_dl(aes(label=model), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
  geom_hline(yintercept=.95, linetype=2) +
  scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1)) +
  scale_x_continuous(name = "Forecast horizon (weeks)") +
  guides(color=FALSE) +
  ggtitle("C: 95% prediction interval coverage rates")+
  geom_label_repel(aes(label = label),
    #nudge_x = 0.5,
    nudge_y = -.05,
    hjust = 0,
    box.padding = 0.1,
    direction = "y",
    min.segment.length = Inf,
    na.rm = TRUE)

pdf(file = "figures/long-range.pdf", width=8, height=12)
gridExtra::grid.arrange(f1, mae_plot, calib95)
dev.off()

jpeg(file = "figures/long-range.jpg", width=8, height=12, units="in", res=200)
gridExtra::grid.arrange(f1, mae_plot, calib95)
dev.off()


