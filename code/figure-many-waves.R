library(tidyverse)
library(covidHubUtils)
library(googlesheets4)
library(gridExtra)

source("code/plot_wave_forecasts.R")
data(hub_locations)

## read date info
wave_sheet_url <- "https://docs.google.com/spreadsheets/d/1Qa0yIZCcb7YnF8y8REIyU6bcGweK9ejXN6NDBbchLuE/edit#gid=0"
wave_data <- read_sheet(wave_sheet_url, col_types = "cccDDnll") %>%
  left_join(filter(hub_locations, geo_type=="state")) %>% ## need to filter to leave out DC row with county FIPS
  filter(!revised) %>% ## filters out location-waves that were revised during the wave
  mutate(wave_name = factor(wave_name, 
                            levels = c("summer_2020", "fall_2020", "alpha_2021", "delta_2021"),
                            labels = c("Summer 2020", "Fall/Winter 2020/2021", "Alpha variant 2021", "Delta variant 2021"))) %>%
  arrange(wave_name, first_target_end_date)

## load truth data
truth_data <- load_truth(
  truth_source = "JHU", 
  target_variable = "inc death")

## for each wave
wave_grobs <- purrr::map(
  unique(wave_data$wave_name),
  function(x) {
    wave_data_subset <- filter(wave_data, wave_name == x)
    ## plot every location-wave
    purrr::map(
      1:nrow(wave_data_subset),
      function(x){
        plot_wave_forecasts(
          loc_abbr = wave_data_subset$abbreviation[x],
          first_target_end_date = wave_data_subset$first_target_end_date[x],
          last_target_end_date = wave_data_subset$last_target_end_date[x],
          truth_data = truth_data)
      }
    )
  }
)

## make a 3x3 Grob for each wave
nrow <- ncol <- 3
layout_matrix <- matrix(seq_len(nrow *ncol), nrow = nrow, ncol = ncol, byrow=TRUE)
grobs_summer_2020 <- marrangeGrob(wave_grobs[[1]], nrow=3, ncol=3, top="Summer 2020", layout_matrix = layout_matrix)
grobs_fall_2020 <- marrangeGrob(wave_grobs[[2]], nrow=3, ncol=3, top="Fall/Winter 2020/2021", layout_matrix = layout_matrix)
grobs_alpha_2021 <- marrangeGrob(wave_grobs[[3]], nrow=3, ncol=3, top="Alpha variant 2021", layout_matrix = layout_matrix)
grobs_delta_2021 <- marrangeGrob(wave_grobs[[4]], nrow=3, ncol=3, top="Delta variant 2021", layout_matrix = layout_matrix)

## make final paginated plot with 1 page per wave
tmp <- marrangeGrob(grobs=c(grobs_summer_2020, 
                            grobs_fall_2020, 
                            grobs_alpha_2021, 
                            grobs_delta_2021), 
                    nrow=1, ncol=1, top=NULL)

ggsave("figures/figure-many-waves.pdf", tmp, width = 12, height=9)
