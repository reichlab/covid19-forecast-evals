library(tidyverse)
library(MMWRweek)
library(covidData)
library(covidHubUtils)

source("code/load-global-analysis-dates.R")

mondays <- c(seq(from = as.Date("2020-06-01"), to = version_date-1, by = "week"), as.Date(version_date-1)) 
#currently subtracting 1 b/c 12/07 isn't a submitted forecast revision date


#load revisions as of each monday 
load_all_weeks <- function(x)
{
  load_jhu_data(
    issue_date = x, 
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "deaths") %>%
    mutate(revision_date = x) #add column listing Monday date 
}

weekly_inc_deaths <- plyr::ldply(mondays, load_all_weeks)  #combine revisions into 1 dataframe


weekly_inc_deaths <- weekly_inc_deaths %>% 
  left_join(hub_locations %>% select(location = fips, location_name, abbreviation)) %>% #add location names
  mutate(MMWRweek = MMWRweek(weekly_inc_deaths$date)$MMWRweek) %>% #add epi week
  filter(MMWRweek <= MMWRweek(last_timezero)$MMWRweek)  #filter to only include epi weeks in evaluation

#Create PDF of revisions in all locations 

pdf('figures/data_revisions.pdf')

for(i in 1:7) {
  fig_revisions <- ggplot(data = weekly_inc_deaths, aes(x = MMWRweek, y = inc, 
                                                        color = factor(revision_date))) +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    scale_x_continuous(breaks = unique(weekly_inc_deaths$MMWRweek)[c(TRUE, FALSE)]) + 
    #geom_vline(aes(xintercept = weekly_counts$first_fcast_date_impacted), linetype = "dashed") +
    #scale_x_date(date_labels = "%Y-%m-%d", breaks = c(mondays), name = "Date") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8)) +
    ggforce::facet_wrap_paginate(~location_name, ncol = 2, nrow = 4,  scales = "free_y", page = i) +
    #facet_wrap(~abbreviation, scales = "free_y") +
    ylab("Incident deaths reported") + labs(color = "Revision Date") + xlab("Epi Week")
  print(fig_revisions)
}
dev.off()