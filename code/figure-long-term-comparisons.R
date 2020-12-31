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

truth_dat <- load_truth(truth_source = "JHU", target_variable = "inc death", locations = "US")

f1 <- plot_forecast(longterm_dat, 
  truth_data = truth_dat, 
  model = "IHME-CurveFit", 
  target_variable = "inc death", 
  intervals = c(.5, .95), 
  title = "A: example long-term forecast from IHME", 
  subtitle = "none",
  show_caption=FALSE, 
  plot = FALSE) +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b") + 
  theme(legend.position = c(.01,.95), 
    legend.justification = c(0,1), 
    legend.box = "horizontal", 
    legend.key.size = unit(0.15, "cm"),
    plot.margin = margin(10, 15, 10, 10),axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2)
  )

# f2 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "YYG-ParamSearch", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f3 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "LANL-GrowthRate", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f4 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "Covid19Sim-Simulator", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f5 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "UCLA-SuEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f6 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "USACE-ERDC_SEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)

# fplots <- gridExtra::grid.arrange(f1, f2, f3, f4, f5, f6)


## score evaluation

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(model %in% models, forecast_date <= as.Date("2020-07-13"), !(location_name %in% locs_to_exclude)) %>%
  group_by(model, horizon) %>%
  mutate(nobs=n(), nlocs = length(unique(location_name)))

mae_plot <- inc_scores %>%
  group_by(model, horizon) %>%
  summarize(mean_wis = mean(wis), mae = mean(abs_error)) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == max(horizon), model, NA_character_)) %>%
  #ggplot(aes(x=horizon, y=mae, color=model)) +
  ggplot(aes(x=horizon, y=mean_wis, color=model)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "Mean WIS") +
  scale_x_continuous(name = NULL) +
  guides(color=FALSE) +
  ggtitle("B: Mean weighted interval score")+
  geom_label_repel(aes(label = label),
    #nudge_x = 0.5,
    nudge_y = -.05,
    hjust = 0,
    box.padding = 0.1,
    direction = "y",
    min.segment.length = Inf,
    na.rm = TRUE)

## coverage evaluation

calibration_scores_inc <- inc_scores %>%
  group_by(model, target) %>%
  summarise(percent_calib50 = round(mean(coverage_50, na.rm = T), 2),
    percent_calib95 = round(mean(coverage_95, na.rm = T), 2)) %>% 
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


