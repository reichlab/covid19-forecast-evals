## Figure 1
library(tidyverse)
library(covidHubUtils)
library(cowplot)
library(zoltr)

source("code/load-global-analysis-dates.R")

theme_set(theme_bw())

eval_dates <- c(
  first_target_end_date - 3.5, ## first one-week-ahead target end date 
  last_target_end_date + 3.5) ## last four week ahead target end date


start_date <- as.Date("2020-02-22")
end_date <- truth_date

data("hub_locations")


## forecast plot

fcast_data <- load_forecasts(
  models = "COVIDhub-ensemble", 
  forecast_dates = seq.Date(as.Date("2020-05-11"), 
                            as.Date("2021-05-11"), 
                            by="5 weeks"),
  targets = paste(1:4, "wk ahead inc death"),
  locations = "US" 
  )

p1 <- plot_forecasts(forecast_data = fcast_data,
  target_variable = "inc death",
  truth_source = "JHU",
  intervals = c(0.5,0.95),
  show_caption = FALSE, 
  plot=FALSE) +
  ggtitle("B: ensemble forecasts for incident deaths at the national level", 
    subtitle=element_blank())


p1_updated <- p1 + 
  scale_x_date(
    limits=c(start_date, end_date), 
    date_breaks = "1 month",
    date_labels = "%b",
    name=element_blank(), 
    expand=c(0,0)) +
  geom_vline(xintercept=range_fcast_dates, linetype=2) +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2))

p1_legend <- get_legend(p1_updated)
p1_updated_no_legend <- p1_updated + theme(legend.position='none')

## plot of true data by state, tiled
truth_dat <- load_truth(truth_source = "JHU",
  target_variable = c("inc death", "cum death"),
  as_of = truth_date) %>%
  filter(geo_type == "state") %>%
  filter(!is.na(value)) %>%
  mutate(death_rate_per_100k = value/population*100000,
    death_bins = cut(value, breaks = c(-Inf, 0, 1, 10, 100, 500, 1000, Inf), right = FALSE, labels=c("<0", "0", "1-9", "10-99", "100-499", "500-999", "1000+")))

cum_death <- truth_dat %>% filter(target_variable == "cum death",
                                  target_end_date == last_target_end_date) %>%
  select(abbreviation, cum_death = value)


p2 <- truth_dat %>%
  filter(target_variable == "inc death") %>%
  left_join(cum_death) %>%
  filter(abbreviation != "US", !(is.na(value)), target_end_date <= end_date) %>%
  mutate(abbreviation = fct_reorder(abbreviation, cum_death)) %>%
 # mutate(abbreviation = reorder(abbreviation, X=value, FUN=function(x) max(x, na.rm=TRUE))) %>%
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
  geom_vline(xintercept=range_fcast_dates, linetype=2) +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
  ggtitle("A: reported number of incident weekly COVID-19 deaths by state/territory")

p2_legend <- get_legend(p2)
p2_no_legend <- p2 + theme(legend.position='none')

## plot of number of forecasts for each week

## sequence of mondays starting with mid March near first time-zero
weekly_forecast_dates <- seq.Date(as.Date("2020-03-16"), end_date, by="7 days")

all_primary_models <- get_model_designations(source="zoltar") %>%
  filter(designation != "other") %>%
  pull(model)

n_models_per_week <- map_dfr(
  1:length(weekly_forecast_dates),
  function(x){
    fcasts <- load_latest_forecasts(
      models = all_primary_models,
      last_forecast_date = weekly_forecast_dates[x],
      forecast_date_window_size = 6,
      locations = hub_locations %>% filter(geo_type == "state") %>% pull(fips),
      types = c("point", "quantile"), 
      targets = "1 wk ahead inc death",
      source="zoltar") 
    n_models = length(unique(fcasts$model))
    
    return(tibble(target_end_date_0wk_ahead = weekly_forecast_dates[x]-2, 
      total_models = n_models, 
      ensemble_week = "COVIDhub-ensemble" %in% fcasts$model))
  }
)

p3 <- ggplot(n_models_per_week, aes(x=target_end_date_0wk_ahead)) +
  geom_col(aes(y=total_models)) +
  geom_point(data=filter(n_models_per_week, ensemble_week), aes(y=5), shape=8, color="red") +
  #geom_text(aes(y=.5)) +
  scale_y_continuous(name="# models")+
  scale_x_date(
    limits=c(start_date, end_date + 3), # +3 needed to make room for last bar
    date_breaks = "1 month",
    date_labels = "%b",
    name=element_blank(), 
    expand=c(0,0)) +
  geom_vline(xintercept=range_fcast_dates, linetype=2) +
  theme(legend.position='none',  
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
  ggtitle("C: number of models submitting forecasts of incident deaths")
  

heights <- c(5/8, 2/8, 1/8)
jpeg(file = "figures/data-and-forecast.jpg", width=10, height=11, units="in", res=200)
ggdraw(
  plot_grid(
    plot_grid(p2_no_legend, p1_updated_no_legend, p3, ncol=1, align='v', rel_heights = heights),
    plot_grid(p2_legend, p1_legend, NULL, ncol=1, rel_heights = heights),
    rel_widths=c(1, 0.3)
  ))
dev.off()

pdf(file = "figures/data-and-forecast.pdf", width=8, height=11)
ggdraw(
  plot_grid(
    plot_grid(p2_no_legend, p1_updated_no_legend, p3, ncol=1, align='v', rel_heights = heights),
    plot_grid(p2_legend, p1_legend, NULL, ncol=1, rel_heights = heights),
    rel_widths=c(1, 0.3)
  ))
dev.off()


