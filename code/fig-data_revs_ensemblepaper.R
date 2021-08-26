library(tidyverse)
library(MMWRweek)
library(covidData)
library(covidHubUtils)

#Mondays assessed for revision data 
mondays <- c(seq(from = as.Date("2020-06-01"), to =as.Date("2021-07-26"), by = "week")) 


# US Deaths

#load revisions as of each monday 
load_all_weeks <- function(x)
{
  load_jhu_data(
    issue_date = x, 
    location_code = c("39", "48"), #ohio and 
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "deaths") %>%
    mutate(revision_date = x) #add column listing Monday date 
}
weekly_inc_deaths <- plyr::ldply(mondays, load_all_weeks)   %>%  #combine revisions to 1 dataframe 
  left_join(hub_locations %>% select(location = fips, location_name, abbreviation))  #Add in locations 

revis_deaths <- weekly_inc_deaths %>%
  group_by(location_name, inc, date) %>%  
 mutate(inc_new = ifelse(revision_date == max(revision_date), inc, NA)) %>% ungroup() %>%
  group_by(location_name, revision_date) %>%
 mutate(num_revis = sum(inc_new >= 1, na.rm = T)) %>% ungroup() %>%
  filter(num_revis > 0) %>%
  group_by(location_name, date) %>%
  filter(revision_date == max(revision_date) | revision_date == min(revision_date)) %>% ungroup() %>%
  mutate(revision_binary = ifelse(revision_date == max(revision_date),"Reported as of 2021-07-26", "First Reported")) %>%
  mutate(target = "Deaths") 


# US Cases

#load revisions as of each monday 
load_all_weeks_cases <- function(x)
{
  load_jhu_data(
    issue_date = x, 
    location_code = c("39","48"),
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = "cases") %>%
    mutate(revision_date = x) #add column listing Monday date 
}

weekly_inc_cases<- plyr::ldply(mondays, load_all_weeks_cases) %>% 
  left_join(hub_locations %>% select(location = fips, location_name, abbreviation)) 

revis_cases <- weekly_inc_cases %>%
  group_by(location_name, inc, date) %>%  
  mutate(inc_new = ifelse(revision_date == max(revision_date), inc, NA)) %>% ungroup() %>%
  group_by(location_name, revision_date) %>%
  mutate(num_revis = sum(inc_new >= 1, na.rm = T)) %>% ungroup() %>%
  filter(num_revis > 0) %>%
  mutate(target = "Cases") %>% 
  group_by(location_name, date) %>%
  filter(revision_date == max(revision_date) | revision_date == min(revision_date)) %>% ungroup() %>%
  mutate(revision_binary = ifelse(revision_date == max(revision_date),"Reported as of 2021-07-26", "First Reported"))


# Euro Deaths 
load_euro_death <- function(x) {
load_jhu_data(
  issue_date = x,
  location_code = c("GB", "FR"),
  temporal_resolution = "weekly",
  measure = "deaths",
  geography = c("global")) %>%
 mutate(revision_date = x) #add column listing Monday date 
}

euro_death <- plyr::ldply(mondays, load_euro_death)  #combine revisions into 1 dataframe

revis_euro_death <- euro_death %>%
  mutate(location_name = fct_recode(factor(location), 
          "United Kingdom" = "GB", "France" = "FR")) %>%
  group_by(location_name, inc, date) %>%  
  mutate(inc_new = ifelse(revision_date == max(revision_date), inc, NA)) %>% ungroup() %>%
  group_by(location_name, revision_date) %>%
  mutate(num_revis = sum(inc_new >= 1, na.rm = T)) %>% ungroup() %>%
  filter(num_revis > 0) %>%
  group_by(location_name, date) %>%
  filter(revision_date == max(revision_date) | revision_date == min(revision_date)) %>% ungroup() %>%
  mutate(revision_binary = ifelse(revision_date == max(revision_date),"Reported as of 2021-07-26", "First Reported")) %>%
  mutate(target = "Deaths") 

# Euro Cases
load_euro_case <- function(x) {
  load_jhu_data(
    issue_date = x,
    location_code = c("GB", "FR"),
    temporal_resolution = "weekly",
    measure = "cases",
    geography = c("global")) %>%
    mutate(revision_date = x) #add column listing Monday date 
}

euro_case <- plyr::ldply(mondays, load_euro_case)

revis_euro_case <- euro_case %>%
  mutate(location_name = fct_recode(factor(location), 
                                    "United Kingdom" = "GB", "France" = "FR")) %>%
  group_by(location_name, inc, date) %>%  
  mutate(inc_new = ifelse(revision_date == max(revision_date), inc, NA)) %>% ungroup() %>%
  group_by(location_name, revision_date) %>%
  mutate(num_revis = sum(inc_new >= 1, na.rm = T)) %>% ungroup() %>%
  filter(num_revis > 0) %>%
  mutate(target = "Cases") %>% 
  group_by(location_name, date) %>%
  filter(revision_date == max(revision_date) | revision_date == min(revision_date)) %>% ungroup() %>%
  mutate(revision_binary = ifelse(revision_date == max(revision_date),"Reported as of 2021-07-26", "First Reported"))
  


#Graph each target individually 

library(cowplot)
library(scales)
plot_rev_binary <- function(x,y, y_title) {
  ggplot(data = x, aes(x = date, y = inc_new, 
                                          color = factor(revision_binary))) +
    geom_line() +
    geom_point(size = 1) +
    theme_bw() +
    scale_color_manual(values = c( "blue", "black")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          legend.position = "none",
          plot.title = element_text(size = 10),
          plot.margin = unit(c(0.5, 0, 0, 0), "cm")) +
    scale_y_continuous(labels = comma) +
    facet_wrap(~ location_name, nrow = 2, scales = "free_y") +
    labs(color = "Revision Date") + ylab(y_title) + xlab("") +  ggtitle(y) 
}


fig_revis_death_US <- plot_rev_binary(x = revis_deaths, y = "Deaths", y_title = "") 
fig_revis_case_US <- plot_rev_binary(x = revis_cases, y = "Cases", y_title = "Count")
us_hub <- ggdraw(plot_grid(fig_revis_case_US, fig_revis_death_US))
                 
  
fig_revis_death_EU <- plot_rev_binary(x = revis_euro_death, y = "Deaths", y_title = "")
fig_revis_case_EU <- plot_rev_binary(x = revis_euro_case, y = "Cases", y_title = "Count")
eu_hub <- ggdraw(plot_grid(fig_revis_case_EU, fig_revis_death_EU)) 


all_graphs <-  plot_grid(us_hub, eu_hub,nrow = 2, labels = c("A. US Hub", "B. European Hub"), scale = .95,
                         label_size = 10)

legend_b <- get_legend(
  fig_revis_death_US  + 
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom"))


pdf(file = "figures/fig-loc_binary.pdf", width=7, height=10)
plot_grid(all_graphs, legend_b, ncol = 1, rel_heights = c(1, .2))
dev.off()

