library(tidyverse)
library(covidHubUtils)
library(cowplot)
theme_set(theme_bw())

source("code/load-global-analysis-dates.R")

locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")


## panel A with example forecast
longterm_dat <- load_latest_forecasts(models = c("IHME-CurveFit", "Covid19Sim-Simulator"),
  last_forecast_date = as.Date("2020-06-08"), forecast_date_window_size = 6,
  locations = "US",
  types = c("quantile", "point"), 
  targets = paste(1:20, "wk ahead inc death"),
  source = "zoltar") %>%
  mutate(model = ifelse(model=="IHME-CurveFit", "IHME-SEIR", model))

truth_dat <- load_truth(truth_source = "JHU", target_variable = "inc death", locations = "US")

panelA <- plot_forecasts(longterm_dat, 
  truth_data = truth_dat, 
  #model = "IHME-CurveFit", 
  target_variable = "inc death", 
  fill_by_model = TRUE,
  fill_transparency = .5,
  intervals = c(.5, .95), 
  title = "A: example long-term forecasts for the US from IHME and Covid19Sim", 
  subtitle = "none",
  show_caption=FALSE, 
  plot = FALSE) +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b", limits=c(first_1wk_target_end_date, last_1wk_target_end_date)) + 
  theme(legend.position = c(.01,.95), 
    legend.justification = c(0,1), 
    legend.box = "horizontal", 
    legend.key.size = unit(0.15, "cm"),
    plot.margin = margin(10, 15, 10, 10),axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2)
  ) 


## panel B with mean_wis over time
inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_overall == "TRUE")

expected_locs <- inc_scores %>%
  filter(!(location_name %in% locs_to_exclude)) %>%
  group_by(target_end_date_1wk_ahead) %>%
  summarize(nlocs_expected = n_distinct(location_name))

horizon_levels <- c(1, 4, 8, 12, 16, 20)

avg_wis_by_model_target_week <- inc_scores %>%
  filter(!(location_name %in% locs_to_exclude),
    horizon %in% horizon_levels) %>% 
  group_by(model, target, horizon, target_end_date_1wk_ahead, target_end_date) %>%
  summarize(mae = mean(abs_error), median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), pi_cov_95 = mean(coverage_95), nlocs=n()) %>%
  left_join(expected_locs)%>%
  mutate(obs_exp_locs = nlocs == nlocs_expected,
    horizon = factor(horizon, levels=horizon_levels, ordered=TRUE),
    baseline = ifelse(model=="COVIDhub-baseline", "baseline", "other models")) %>%
  filter(obs_exp_locs)


## for results in manuscript not in table
avg_wis_by_model_target_week %>% 
  filter(model %in% c("IHME-CurveFit", "Covid19Sim-Simulator")) %>%
  group_by(model, horizon) %>%
  summarize(avg_wis = mean(mean_wis)) %>%
  group_by(model) %>%
  mutate(h1_avg_wis = min(avg_wis), rel_wis = avg_wis/h1_avg_wis)


panelB <- ggplot(filter(avg_wis_by_model_target_week, model!="COVIDhub-baseline"), 
  aes(x=target_end_date, y=mean_wis, color=horizon, linetype=baseline)) +
  geom_point(aes(group = model), alpha=.3) +
  geom_smooth(se=FALSE) +
  # stat_summary(fun=mean, geom="smooth", aes(color=factor(horizon), group=horizon)) +
  ## stat_summary(fun=mean, geom="point", aes(color=factor(horizon), group=horizon)) +
  ## add baseline
  geom_smooth(data=filter(avg_wis_by_model_target_week, model=="COVIDhub-baseline", horizon%in%c(1, 4)), se=FALSE) +
  scale_color_viridis_d(direction=-1) +
  scale_y_continuous("mean WIS (log scale)", trans = "log2") +
  scale_linetype_manual(NULL, values = c(2, 1)) +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") + 
  ggtitle("B: mean WIS across time, stratified by forecast horizon") +
  theme(legend.position = c(0.01,0.99), 
    legend.direction = "horizontal",
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.background = element_rect(alpha("white", 0.5)),
    #legend.box = "horizontal",
    legend.justification = c(0,1),
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7.5, hjust = -0.4)) +
  guides(linetype=guide_legend(keywidth = 2))

panelC <- ggplot(filter(avg_wis_by_model_target_week, model!="COVIDhub-baseline"), 
  aes(x=target_end_date, y=pi_cov_95, color=horizon, linetype=baseline)) +
  geom_point(aes(group = model), alpha=.3) +
  geom_smooth(se=FALSE) +
  # stat_summary(fun=mean, geom="smooth", aes(color=factor(horizon), group=horizon)) +
  ## stat_summary(fun=mean, geom="point", aes(color=factor(horizon), group=horizon)) +
  ## add baseline
  geom_smooth(data=filter(avg_wis_by_model_target_week, model=="COVIDhub-baseline", horizon%in%c(1, 4)), se=FALSE) +
  geom_hline(yintercept=0.95, linetype=2) +
  scale_color_viridis_d(direction=-1) +
  scale_y_continuous("95% Prediction Interval Coverage") +
  scale_linetype_manual("", values = c(2, 1)) +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") + 
  ggtitle("C: 95% prediction interval coverage across time, stratified by forecast horizon") +
  theme(legend.position = "none",
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7.5, hjust = -0.4))

nobs_by_horizon_week <- avg_wis_by_model_target_week %>%
  filter(model!="COVIDhub-baseline") %>%
  group_by(horizon, target_end_date) %>%
  summarize(nmodels = n())

panelD <- ggplot(nobs_by_horizon_week, aes(x=target_end_date, y=nmodels, color=horizon, group=horizon)) +
  geom_point()+ geom_line() +
  scale_color_viridis_d(direction=-1) +
  scale_y_continuous("# models", minor_breaks = NULL) +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") + 
  ggtitle("D: number of models with evaluated forecasts for each horizon and week") +
  theme(legend.position = "none",
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7.5, hjust = -0.4))

heights <- c(rep(2/7, 3), 1/7)
jpeg(file = "figures/fig-by-horizon-week.jpg", width=10, height=12, units="in", res=200)
ggdraw(
  plot_grid(
    panelA,
    panelB,
    panelC,
    panelD,
    rel_heights = heights, 
    ncol=1, 
    align="v"
  ))
dev.off()

heights <- c(rep(2/7, 3), 1/7)
jpeg(file = "figures/fig-by-horizon-week.jpg", width=8, height=12, units="in", res=200)
ggdraw(
  plot_grid(
    panelA,
    panelB,
    panelC,
    panelD,
    rel_heights = heights, 
    ncol=1, 
    align="v"
  ))
dev.off()

pdf(file = "figures/fig-by-horizon-week.pdf", width=8, height=12)
ggdraw(
  plot_grid(
    panelA,
    panelB,
    panelC,
    panelD,
    rel_heights = heights, 
    ncol=1, 
    align="v"
  ))
dev.off()
