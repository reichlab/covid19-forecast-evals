#' Plotting forecasts for specific location
#'
#' @param fips location to be plotted
#' @param first_target_end_date a Date, the first target_end_date to plot
#' @param last_target_end_date a Date, last target_end_date to plot
#' @param score_data an inc_scores data frame
#' @param truth_data a truth data frame
#'
#' @return a ggplot object and prints a plot
#'
#' @examples
plot_wave_forecasts <- function(loc_abbr,
                                first_target_end_date,
                                last_target_end_date,
                                score_data=NULL, 
                                truth_data) {
  require(covidHubUtils)
  require(lubridate)
  require(tidyverse)
  
  first_target_end_date <- ymd(first_target_end_date)
  last_target_end_date <- ymd(last_target_end_date)
  ## check that first/last target_end_date are saturdays and in date format
  if(is.na(first_target_end_date)){
    stop("first_target_end_date can't be parsed as a date")
  } else {
    if(wday(first_target_end_date)!=7)
      stop("first_target_end_date must be a saturday")
  }

  if(is.na(last_target_end_date)){
    stop("first_target_end_date can't be parsed as a date")
  } else {
    if(wday(last_target_end_date)!=7)
      stop("first_target_end_date must be a saturday")
  }
  
  
  theme_set(theme_bw())
  data("hub_locations")
  
  ## get forecast dates needed for plotting
  first_possible_forecast_date <- first_target_end_date - 26
  last_possible_forecast_date <- last_target_end_date - 5 
  
  ## get location info
  loc_row <- hub_locations %>% 
    filter(geo_type=="state", abbreviation == loc_abbr)
  fips <- loc_row$fips
  location_name <- loc_row$location_name
  
  ## get forecast dates for all weeks of interest
  forecast_dates <- seq.Date(
    first_possible_forecast_date,
    last_possible_forecast_date,
    by="1 weeks")
  
  ## get forecasts
  ensemble_forecasts <- load_forecasts(
    models = c("COVIDhub-ensemble"),
    dates = forecast_dates,
    targets = paste(1:4, "wk ahead inc death"),
    locations = fips
  )
  
  ## filter truth data
  truth_to_plot <- truth_data %>%
    filter(
      location == fips, 
      target_end_date >= first_target_end_date,
      target_end_date <= last_target_end_date
    ) 
  
  ## filter forecast data
  forecasts_to_plot <- ensemble_forecasts %>%
    filter(
      location == fips, 
      target_end_date >= first_target_end_date,
      target_end_date <= last_target_end_date,
      quantile %in% c(0.025, 0.5, 0.975)) %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "q") 
  
  forecast_plot <- 
    ggplot(truth_to_plot, 
           aes(x=target_end_date)) +
    geom_point(aes(y=value, color="obs", shape="obs")) + geom_line(aes(y=value)) +
    geom_point(data=filter(forecasts_to_plot, horizon=="1"), 
               aes(y=q0.5, target_end_date-1, color="1wk", shape="1wk")) +
    geom_point(data=filter(forecasts_to_plot, horizon=="4"), 
               aes(y=q0.5, target_end_date+1, color="4wk", shape="4wk")) +
    geom_linerange(data=filter(forecasts_to_plot, horizon=="1"), 
                   aes(ymin=q0.025, ymax=q0.975, x=target_end_date-1, color="1wk"), 
                   alpha=.7) +
    geom_linerange(data=filter(forecasts_to_plot, horizon=="4"), 
                   aes(ymin=q0.025, ymax=q0.975, x=target_end_date+1, color="4wk"), 
                   alpha=.7) +
    ylab("Incident deaths") +
    ggtitle(location_name) +
    scale_shape_manual(name=NULL,
                       values=c(17, 17, 16),
                       labels = c("1 week ahead forecast", "4 week ahead forecast", "observed data")) +
    scale_color_manual(name=NULL,
                       labels = c("1 week ahead forecast", "4 week ahead forecast", "observed data"),
                       values = c("obs"="black", "1wk"="#cb181d", "4wk"="#fc9272"),
                       guide = "legend") +
    scale_x_date(name = NULL, date_breaks = "1 month", date_labels = "%b") +
    theme(
      axis.ticks.length.x = unit(0.5, "cm"),
      axis.text.x = element_text(vjust = 7, hjust = -0.2),
      legend.position = "none", #c(0, 1), 
      #legend.justification = c(0,1),
      legend.background=element_blank()
    )
  
  print(forecast_plot)
  return(forecast_plot)
}