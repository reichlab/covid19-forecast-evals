## Figure 1
library(tidyverse)
library(covidHubUtils)
library(cowplot)

source("code/load-global-analysis-dates.R")

theme_set(theme_bw())

eval_dates <- c(
  first_1wk_target_end_date - 3.5, ## first one-week-ahead target end date 
  last_4wk_target_end_date + 3.5) ## last four week ahead target end date

start_date <- as.Date("2020-02-22")
end_date <- as.Date("2020-12-06")

data("hub_locations")


## forecast plot

fcast_data <- load_forecasts(
  models = "COVIDhub-ensemble", 
  forecast_dates = "2020-07-20",
  targets = paste(1:4, "wk ahead inc death"),
  locations = "US" 
  )

p1 <- plot_forecast(forecast_data = fcast_data,
  target_variable = "inc death", 
  truth_source = "JHU",
  intervals = c(0.2, 0.5, 0.8, 0.95, 0.98),
  show_caption = FALSE, 
  plot=FALSE) +
  ggtitle("B: ensemble forecast for incident deaths at the national level from July 20, 2020")


p1_updated <- p1 + 
  scale_x_date(
    limits=c(start_date, end_date), 
    date_breaks = "1 month",
    date_labels = "%b",
    name=element_blank(), 
    expand=c(0,0)) +
  geom_vline(xintercept=eval_dates, linetype=2) +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2))

p1_legend <- get_legend(p1_updated)
p1_updated_no_legend <- p1_updated + theme(legend.position='none')

## plot of true data by state, tiled
truth_dat <- load_truth(truth_source = "JHU",
  target_variable = "inc death",
  data_location = "local_hub_repo",
  local_repo_path = "../covid19-forecast-hub/") %>%
  filter(geo_type == "state") %>%
  mutate(death_rate_per_100k = value/population*100000,
    death_bins = cut(value, breaks = c(-Inf, 0, 1, 10, 100, 500, 1000, Inf), right = FALSE, labels=c("<0", "0", "1-9", "10-99", "100-499", "500-999", "1000+")))


p2 <- truth_dat %>%
  filter(abbreviation != "US", !(is.na(value)), target_end_date <= end_date) %>%
  mutate(abbreviation = reorder(abbreviation, X=value, FUN=function(x) max(x, na.rm=TRUE))) %>%
  ggplot(aes(y=abbreviation, x=target_end_date, fill=death_bins))+
  geom_tile() +
  geom_hline(yintercept=seq(51.5, 6.5, by=-5), color="darkgrey") +
  scale_fill_brewer(palette = "Purples", guide = guide_legend(reverse = TRUE), name="deaths") +
  scale_x_date(
    limits=c(start_date, end_date), 
    date_breaks = "1 month",
    date_labels = "%b",
    name=element_blank(), 
    expand=c(0,0)) +
  scale_y_discrete(name=NULL) +
  geom_vline(xintercept=eval_dates, linetype=2) +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
  ggtitle("A: reported number of incident weekly COVID-19 deaths by state/territory")

p2_legend <- get_legend(p2)
p2_no_legend <- p2 + theme(legend.position='none')

## plot of number of forecasts for each week
#inc_scores <- 
  
inc_scores <- read_csv("data-raw/inc-scores-from-zoltar.csv") %>%
  filter(target %in% "1 wk ahead inc death") %>%
  mutate(forecast_date = as.Date(covidHubUtils::calc_target_week_end_date(timezero, horizon=0)))

n_models_per_week <- inc_scores %>%
  group_by(forecast_date) %>%
  summarize(
    total_models = length(unique(model)),
    ensemble_week = "COVIDhub-ensemble" %in% unique(model)) 

p3 <- ggplot(n_models_per_week, aes(x=forecast_date)) +
  geom_col(aes(y=total_models)) +
  geom_point(data=filter(n_models_per_week, ensemble_week), aes(y=5), shape=8, color="red") +
  #geom_text(aes(y=.5)) +
  scale_y_continuous(name="# models")+
  scale_x_date(
    limits=c(start_date, end_date), 
    date_breaks = "1 month",
    date_labels = "%b",
    name=element_blank(), 
    expand=c(0,0)) +
  geom_vline(xintercept=eval_dates, linetype=2) +
  theme(legend.position='none',  
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
  ggtitle("C: number of models submitting forecasts of incident deaths")
  

heights <- c(5/8, 2/8, 1/8)
jpeg(file = "figures/data-and-forecast.jpg", width=8, height=12, units="in", res=200)
ggdraw(
  plot_grid(
    plot_grid(p2_no_legend, p1_updated_no_legend, p3, ncol=1, align='v', rel_heights = heights),
    plot_grid(p2_legend, p1_legend, NULL, ncol=1, rel_heights = heights),
    rel_widths=c(1, 0.3)
  ))
dev.off()

pdf(file = "figures/data-and-forecast.pdf", width=8, height=12)
ggdraw(
  plot_grid(
    plot_grid(p2_no_legend, p1_updated_no_legend, p3, ncol=1, align='v', rel_heights = heights),
    plot_grid(p2_legend, p1_legend, NULL, ncol=1, rel_heights = heights),
    rel_widths=c(1, 0.3)
  ))
dev.off()


