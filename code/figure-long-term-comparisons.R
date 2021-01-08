## plot a long-term forecast, evaluate by WIS, PI coverage
library(tidyverse)
library(covidHubUtils)
library(ggrepel)
theme_set(theme_bw())

source("code/load-global-analysis-dates.R")
first_date_for_longterm_eval <- as.Date("2020-10-03")

models <- c("COVIDhub-ensemble", "COVIDhub-baseline", "IHME-CurveFit","LANL-GrowthRate", "Covid19Sim-Simulator", "UCLA-SuEIR", "USACE-ERDC_SEIR")
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
  title = "A: example long-term forecast for the US from IHME", 
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
  ) +
  geom_vline(xintercept = c(first_date_for_longterm_eval+3.5, last_1wk_target_end_date-3.5), 
    linetype=2)

# f2 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "YYG-ParamSearch", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f3 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "LANL-GrowthRate", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f4 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "Covid19Sim-Simulator", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f5 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "UCLA-SuEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)
# f6 <- plot_forecast(longterm_dat, truth_data = truth_dat, model = "USACE-ERDC_SEIR", target_variable = "inc death", intervals = c(.5, .95), title = "none", show.caption=FALSE)

# fplots <- gridExtra::grid.arrange(f1, f2, f3, f4, f5, f6)


## score evaluation

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(model %in% models, ## only include models that have been going long for a while
    !(location_name %in% locs_to_exclude), ## only include states
    !(model == "COVIDhub-baseline" & horizon>4), ## exclude 5+ horizons from baseline b/c they started late
    target_end_date >= first_date_for_longterm_eval ## when long forecasts start coming due
    ) %>%
  group_by(model, horizon) %>%
  mutate(nobs=n(), nlocs = length(unique(location_name)))

## useful for seeing which horizons available when
# inc_scores %>%
#   group_by(model, target_end_date) %>%
#   summarize(max_horizon = max(horizon)) %>%
#   ungroup() %>%
#   mutate(model = reorder(model, max_horizon, FUN = max)) %>%
#   ggplot(aes(x=factor(target_end_date), y=model, fill=max_horizon)) +
#   geom_tile()


avg_scores <- inc_scores %>%
  group_by(model, horizon) %>%
  summarize(mean_wis = mean(wis), mae = mean(abs_error), nobs=n()) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == max(horizon), model, NA_character_))
  #ggplot(aes(x=horizon, y=mae, color=model)) +

avg_scores %>%
  filter(model %in% c("IHME-CurveFit", "Covid19Sim-Simulator"), horizon %in% c(1,4,20))

mae_plot <- ggplot(avg_scores, aes(x=horizon, y=mean_wis, color=model)) +
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


