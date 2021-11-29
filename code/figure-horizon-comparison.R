library(tidyverse)
library(covidHubUtils)
library(cowplot)
theme_set(theme_bw())

source("code/load-global-analysis-dates.R") 

locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")


## panel A with example forecast
longterm_dat1 <- load_latest_forecasts(models = c("IHME-CurveFit", "Covid19Sim-Simulator"),
                                       last_forecast_date = as.Date("2020-06-08"), forecast_date_window_size = 6,
                                       locations = "US",
                                       types = c("quantile", "point"), 
                                       targets = paste(1:20, "wk ahead inc death"),
                                       source = "zoltar") %>%
  mutate(model = ifelse(model=="IHME-CurveFit", "IHME-SEIR", model))

longterm_dat2 <- load_latest_forecasts(models = c("IHME-CurveFit", "Covid19Sim-Simulator"),
                                       last_forecast_date = as.Date("2020-11-23"), forecast_date_window_size = 6,
                                       locations = "US",
                                       types = c("quantile", "point"), 
                                       targets = paste(1:20, "wk ahead inc death"),
                                       source = "zoltar") %>%
  mutate(model = ifelse(model=="IHME-CurveFit", "IHME-SEIR", model))

longterm_dat3 <- load_latest_forecasts(models = c("IHME-CurveFit", "Covid19Sim-Simulator"),
                                       last_forecast_date = as.Date("2021-05-07"), forecast_date_window_size = 6,
                                       locations = "US",
                                       types = c("quantile", "point"), 
                                       targets = paste(1:20, "wk ahead inc death"),
                                       source = "zoltar") %>%
  mutate(model = ifelse(model=="IHME-CurveFit", "IHME-SEIR", model))

truth_dat <- load_truth(truth_source = "JHU", target_variable = "inc death", locations = "US")

panelA <- plot_forecasts(bind_rows(longterm_dat1, longterm_dat2, longterm_dat3), 
  truth_data = truth_dat, 
  #model = "IHME-CurveFit", 
  target_variable = "inc death", 
  fill_by_model = TRUE,
  fill_transparency = .5,
  intervals = c(.5, .95), 
  title = "A: Example long-term forecasts for the US from IHME and Covid19Sim", 
  subtitle = "none",
  show_caption=FALSE, 
  plot = FALSE) +
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b `%y", limits=c(first_target_end_date, last_target_end_date)) + 
  theme(legend.position = c(.01,.95), 
    legend.justification = c(0,1), 
    legend.box = "horizontal", 
    legend.key.size = unit(0.15, "cm"),
    legend.background=element_rect(fill = alpha("white", 0.5)),
    plot.margin = margin(10, 15, 10, 10),axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.1)
  ) 


## panel B with mean_wis over time
inc_scores <- read_csv("paper-inputs/inc-scores.csv") 

expected_locs <- inc_scores %>%
  filter(!(location_name %in% locs_to_exclude)) %>%
  ## grouping by target end date ensures that we do not expect predictions for removed observations
  group_by(target_end_date) %>% 
  summarize(nlocs_expected = n_distinct(location_name))

#horizon_levels <- c(1, 4, 8, 12, 16, 20)
horizon_levels <- 1:20
horizon_subset <- c(1, 4, 8, 12, 16, 20)

avg_wis_by_model_target_week <- inc_scores %>%
  filter(!(location_name %in% locs_to_exclude),
    horizon %in% horizon_levels) %>% 
  group_by(model, target, horizon, target_end_date_1wk_ahead, target_end_date) %>%
  summarize(mae = mean(abs_error), 
            median_wis = median(wis), 
            mean_wis = mean(wis, na.rm=TRUE), 
            pi_cov_95 = mean(coverage_95), 
            mean_sharpness = mean(dispersion),
            mean_underprediction = mean(underprediction),
            mean_overprediction = mean(overprediction),
            nlocs=n()) %>%
  left_join(expected_locs)%>%
  mutate(obs_exp_locs = nlocs == nlocs_expected,
    horizon = factor(horizon, levels=horizon_levels, ordered=TRUE),
    baseline = ifelse(model=="COVIDhub-baseline", "baseline", "other models")) %>%
  filter(obs_exp_locs)

## for results in manuscript not in table

## overall across all models
avg_wis_by_model_target_week %>% 
  group_by(horizon) %>%
  summarize(avg_wis = mean(mean_wis), mean_coverage = mean(pi_cov_95), min_coverage = min(pi_cov_95), max_coverage = max(pi_cov_95)) %>%
  mutate(h1_avg_wis = min(avg_wis), rel_wis = avg_wis/h1_avg_wis)

## for two long-forecasting models
avg_wis_by_model_target_week %>% 
  filter(model %in% c("IHME-CurveFit", "Covid19Sim-Simulator")) %>%
  group_by(model, horizon) %>%
  summarize(avg_wis = mean(mean_wis)) %>%
  group_by(model) %>%
  mutate(h1_avg_wis = min(avg_wis), rel_wis = avg_wis/h1_avg_wis) %>%
  print(n=Inf)

## rel error by model/horizon
err_by_model_horizon <- avg_wis_by_model_target_week %>% 
  group_by(model, horizon) %>%
  summarize(avg_wis = mean(mean_wis), 
            mae = mean(mae), 
            dispersion = mean(mean_sharpness),
            underprediction = mean(mean_underprediction),
            overprediction = mean(mean_overprediction),
            nobs=sum(nlocs),
            pi_cov_95 = weighted.mean(pi_cov_95, w=nlocs)) %>%
  group_by(model) %>%
  mutate(h1_avg_wis = min(avg_wis), h1_mae = min(mae), 
         rel_wis = avg_wis/h1_avg_wis, 
         rel_mae = mae/h1_mae, 
         max_horizon = max(horizon)) %>%
  ungroup() %>%
  filter(max_horizon > 8, nobs>100) %>%
  mutate(model = reorder(model, -pi_cov_95),
         `# predictions` = nobs) %>%
  filter(model != "BPagano-RtDriven") %>%
  mutate(model = fct_recode(model, "IHME-SEIR" = "IHME-CurveFit"))

model_colors <- palette.colors(n=6, palette="Set1")[c(3,4,5,1,2)]

panelC_new <- ggplot(err_by_model_horizon, 
                     aes(x=horizon, y=pi_cov_95, color=model, group=model)) +
  geom_point(aes(alpha=`# predictions`, size=`# predictions`)) + geom_line() +
  xlab("horizon (weeks)") +
  geom_hline(yintercept=0.95, linetype=2) +
  scale_y_continuous(name="95% prediction interval coverage", 
                     limits=c(0,1), 
                     breaks=c(0, 0.25, 0.5, 0.75, 0.95, 1), 
                     expand = expansion(mult=0.02)) +
  scale_color_manual(values=model_colors) +
  scale_size_continuous(trans="reciprocal", 
                        breaks=c(250, 500, 1000, 2000),
                        guide = guide_legend(override.aes = list(alpha=c(.2, .4, .6, .8)))) +
  guides(alpha="none") +
  ggtitle("C: 95% prediction interval coverage, by model and horizon")
  #scale_size_continuous(breaks=size_breaks, labels=1/size_breaks, alpha=seq(0.1, 0.9,length.out=4))

  #coord_cartesian(ylim=c(0.9,5)) 

comp_err_by_model_horizon <- err_by_model_horizon %>%
  filter(model != "BPagano-RtDriven") %>%
  select(model, horizon, dispersion, overprediction, underprediction) %>%
  group_by(model, horizon) %>%
  mutate(pct_sharpness = dispersion/(dispersion + overprediction + underprediction),
         pct_underprediction = underprediction/(dispersion + overprediction + underprediction),
         pct_overprediction = overprediction/(dispersion + overprediction + underprediction)) %>%
  mutate(model = fct_recode(model, "IHME-SEIR" = "IHME-CurveFit"))

comp_err_by_model_horizon %>% 
  pivot_longer(cols=starts_with("pct"),
               names_to = "score_name") %>%
  #filter(horizon %in% c(1, 4, 8, 12, 16, 20)) %>%
  ggplot(aes(x=horizon, y=value, fill=score_name))+
  geom_bar(position="stack", stat="identity") +
  facet_wrap(.~model)

wis_component_by_horizon <- comp_err_by_model_horizon %>% 
  pivot_longer(cols=c("dispersion", "underprediction", "overprediction"),
               names_to = "WIS component") %>%
  mutate(`WIS component` = factor(`WIS component`, levels=c("overprediction", "dispersion", "underprediction"))) %>%
  #filter(horizon %in% c(1, 4, 8, 12, 16, 20)) %>%
  ggplot(aes(x=horizon, y=value, fill=`WIS component`)) +
  scale_fill_brewer(palette="Dark2") +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(.~model)


## rel error by week
# avg_wis_by_model_target_week %>% 
#   filter(model %in% c("IHME-CurveFit", "Covid19Sim-Simulator")) %>%
#   group_by(model, horizon, target_end_date) %>%
#   summarize(avg_wis = mean(mean_wis)) %>%
#   group_by(model, target_end_date) %>%
#   mutate(h1_avg_wis = min(avg_wis), rel_wis = avg_wis/h1_avg_wis) %>%
#   mutate(model = ifelse(model=="IHME-CurveFit", "IHME-SEIR", model))
#   print(n=Inf)

## panel B: figure mean wis by date color by horizon
panelB <- ggplot(filter(avg_wis_by_model_target_week, model!="COVIDhub-baseline", horizon %in% horizon_subset), 
  aes(x=target_end_date, y=mean_wis, color=horizon, linetype=baseline)) +
  geom_point(aes(group = model), alpha=.3, size=1) +
  geom_smooth(se=FALSE) +
  # stat_summary(fun=mean, geom="smooth", aes(color=factor(horizon), group=horizon)) +
  ## stat_summary(fun=mean, geom="point", aes(color=factor(horizon), group=horizon)) +
  ## add baseline
  geom_smooth(data=filter(avg_wis_by_model_target_week, model=="COVIDhub-baseline", horizon%in%c(1, 4)), se=FALSE) +
  scale_color_viridis_d(direction=-1, end=0.9) + 
  scale_y_continuous("average WIS (log scale)", trans = "log2", n.breaks = 6) +
  scale_linetype_manual(NULL, values = c(2, 1)) +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b `%y") + 
  ggtitle("B: Average WIS across time, stratified by forecast horizon") +
  theme(legend.position = c(0.01,0.99), 
    legend.direction = "horizontal",
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.background = element_rect(alpha("white", 0.5)),
    #legend.box = "horizontal",
    legend.justification = c(0,1),
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.1)) +
  guides(linetype=guide_legend(keywidth = 2))
# 
# panelC <- ggplot(filter(avg_wis_by_model_target_week, model!="COVIDhub-baseline", horizon %in% horizon_subset), 
#   aes(x=target_end_date, y=pi_cov_95, color=horizon, linetype=baseline)) +
#   geom_point(aes(group = model), alpha=.3, size=1) +
#   geom_smooth(se=FALSE) +
#   # stat_summary(fun=mean, geom="smooth", aes(color=factor(horizon), group=horizon)) +
#   ## stat_summary(fun=mean, geom="point", aes(color=factor(horizon), group=horizon)) +
#   ## add baseline
#   geom_smooth(data=filter(avg_wis_by_model_target_week, model=="COVIDhub-baseline", horizon%in%c(1, 4)), se=FALSE) +
#   geom_hline(yintercept=0.95, linetype=2) +
#   scale_color_viridis_d(direction=-1, end=0.9) +
#   scale_y_continuous("95% Prediction Interval Coverage") +
#   scale_linetype_manual("", values = c(2, 1)) +
#   scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") + 
#   ggtitle("C: 95% prediction interval coverage across time, stratified by forecast horizon") +
#   theme(legend.position = "none",
#     axis.ticks.length.x = unit(0.5, "cm"),
#     axis.text.x = element_text(vjust = 7.5, hjust = -0.4))
# 
# nobs_by_horizon_week <- avg_wis_by_model_target_week %>%
#   filter(model!="COVIDhub-baseline", horizon %in% horizon_subset) %>%
#   group_by(horizon, target_end_date) %>%
#   summarize(nmodels = n())
# 
# panelD <- ggplot(nobs_by_horizon_week, aes(x=target_end_date, y=nmodels, color=horizon, group=horizon)) +
#   geom_point(size=1)+ geom_line() +
#   scale_color_viridis_d(direction=-1, end=0.9) +
#   scale_y_continuous("# models", minor_breaks = NULL) +
#   scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") + 
#   ggtitle("D: number of models with evaluated forecasts for each horizon and week") +
#   theme(legend.position = "none",
#     axis.ticks.length.x = unit(0.5, "cm"),
#     axis.text.x = element_text(vjust = 7.5, hjust = -0.4))

#heights <- c(rep(2/7, 3), 1/7)
jpeg(file = "figures/fig-by-horizon-week.jpg", width=10, height=10, units="in", res=200)
ggdraw(
  plot_grid(
    plot_grid(panelA, panelB, align="v", ncol=1),
    plot_grid(panelC_new, ncol = 1, align = "v"),
    ncol=1, rel_heights = c(2/3, 1/3)
    )
  )
dev.off()

pdf(file = "figures/fig-by-horizon-week.pdf", width=10, height=10)
ggdraw(
  plot_grid(
    plot_grid(panelA, panelB, align="v", ncol=1),
    plot_grid(panelC_new, ncol = 1, align = "v"),
    ncol=1, rel_heights = c(2/3, 1/3)
  )
)
dev.off()

## WIS component plot for supplemental figure
jpeg(file = "figures/fig-wis-component-horizons.jpg", width=11, height=6, units="in", res=200)
wis_component_by_horizon
dev.off()

pdf(file = "figures/fig-wis-component-horizons.pdf", width=11, height=6)
wis_component_by_horizon
dev.off()


