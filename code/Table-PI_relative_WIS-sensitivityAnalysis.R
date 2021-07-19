#This script includes code that is also in "figure-calibration.r" and "pairwise_wis.R". It has been added here in order to create table 2 all in 1 place.

library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)
library(surveillance)
 
theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_overall == "TRUE") %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(horizon %in% c(1:4)) %>%
  filter(!is.na(wis)) 

fcast_count_full <- inc_scores %>%
  group_by(model) %>%
  summarise(n_forecasts = n())   %>%
  ungroup() %>%
  mutate(max_forecasts = max(n_forecasts)) %>%
  group_by(model) %>%
  summarise(percent_total_forecasts = round(((n_forecasts/max_forecasts)*100),2))


#Calculate pairwise WIS
# helper function
next_monday <- function(date){
  nm <- rep(NA, length(date))
  for(i in seq_along(date)){
    nm[i] <- date[i] + (0:6)[weekdays(date[i] + (0:6)) == "Monday"]
  }
  return(as.Date(nm, origin = "1970-01-01"))
}

# bring all forecast_dates to Monday:
inc_scores$forecast_date <- next_monday(inc_scores$forecast_date)

scores <- inc_scores %>% select("model", "forecast_date", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)


# function for pairwise wis 
pairwise_comparison_wis <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  
  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("forecast_date", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  
  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    
    # aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ forecast_date, data = sub, FUN = mean)
    pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                nPermutation = 999)$pVal.permut
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_wis(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}

ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]


pairwise_scores <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 


#Merge count and Pairwise 
scores_full <- merge(fcast_count_full, pairwise_scores) %>%
  rename_at(vars(-model), ~ paste0(., '.full'))

#aANALYSIS 2
##Comparison all models 2020
fcast_count_2020 <- inc_scores %>%
  filter(forecast_date <= as.Date("2020-12-31")) %>%
  group_by(model) %>%
  summarise(n_forecasts = n())   %>%
  ungroup() %>%
  mutate(max_forecasts = max(n_forecasts)) %>%
  group_by(model) %>%
  summarise(percent_total_forecasts = round(((n_forecasts /max_forecasts)*100),2))

scores <- inc_scores %>%
  filter(forecast_date <= "2020-12-31") %>%
  select("model", "forecast_date", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_wis(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}

ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]

pairwise_scores <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 


# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

#Merge count and pairwise 
scores_2020 <- merge(fcast_count_2020, pairwise_scores) %>%
  rename_at(vars(-model), ~ paste0(., '.2020all'))

##Sensitivity Analysis 3: Models from 2020 with only max 8 missing weeks
fcast_count_8missing <- inc_scores %>%
  filter(forecast_date <= as.Date("2020-12-31")) %>% 
  group_by(model, target_end_date_1wk_ahead, horizon) %>%
  mutate(first_per_week = ifelse(row_number() == 1, 1, 0)) %>% ungroup() %>%
  group_by(model) %>% 
  mutate(n_weeks = sum(horizon == 4 & first_per_week == 1 )) %>% #count number of weeks that have a horizon of 4 (if grouped by horizon there are different numbers of weeks per horizon)
  ungroup() %>%
  filter(n_weeks >= max(n_weeks -8)) %>% #remove rows in which the number of weeks is more than 8 less than the max 
  group_by(model) %>%
  summarise(n_forecasts = n()) %>%
  ungroup() %>%
  mutate(max_forecasts = max(n_forecasts)) %>%
  group_by(model) %>%
  summarise(percent_total_forecasts = round(((n_forecasts /max_forecasts)*100),2))

scores <- inc_scores %>%
  filter(forecast_date <= as.Date("2020-12-31")) %>% 
  group_by(model, target_end_date_1wk_ahead, horizon) %>%
  mutate(first_per_week = ifelse(row_number() == 1, 1, 0)) %>% ungroup() %>%
  group_by(model) %>% 
  mutate(n_weeks = sum(horizon == 4 & first_per_week == 1 )) %>% #count number of weeks that have a horizon of 4 (if grouped by horizon there are different numbers of weeks per horizon)
  ungroup() %>%
  filter(n_weeks >= max(n_weeks -8)) %>% ungroup() %>%
  select("model", "forecast_date", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_wis(scores = scores, mx = models[mx], my = models[my],
                                   permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}


ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]


tab #final column is theta^*_iB

pairwise_scores <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 

#Merge count and Pairwise 
scores_8m <- merge(fcast_count_8missing, pairwise_scores) %>%
  rename_at(vars(-model), ~ paste0(., '.2020_8m'))



#aANALYSIS 4
##Comparison all models 2021
fcast_count_2021 <- inc_scores %>%
  filter(forecast_date > as.Date("2020-12-31")) %>%
  group_by(model) %>%
  summarise(n_forecasts = n())   %>%
  ungroup() %>%
  mutate(max_forecasts = max(n_forecasts)) %>%
  group_by(model) %>%
  summarise(percent_total_forecasts = round(((n_forecasts /max_forecasts)*100),2))

scores <- inc_scores %>%
  filter(forecast_date > "2020-12-31") %>%
  select("model", "forecast_date", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_wis(scores = scores, mx = models[mx], my = models[my],
                                   permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}

ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]

pairwise_scores <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 


# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

#Merge count and pairwise 
scores_2021 <- merge(fcast_count_2021, pairwise_scores) %>%
  rename_at(vars(-model), ~ paste0(., '.2021'))



##ANALYSIS 5

##Sensitivity Analysis 3: Models from 2020 with only max 8 missing weeks
fcast_count_8missing_2021 <- inc_scores %>%
  filter(forecast_date > as.Date("2020-12-31")) %>% 
  group_by(model, target_end_date_1wk_ahead, horizon) %>%
  mutate(first_per_week = ifelse(row_number() == 1, 1, 0)) %>% ungroup() %>%
  group_by(model) %>% 
  mutate(n_weeks = sum(horizon == 4 & first_per_week == 1 )) %>% #count number of weeks that have a horizon of 4 (if grouped by horizon there are different numbers of weeks per horizon)
  ungroup() %>%
  filter(n_weeks >= max(n_weeks -8)) %>% #remove rows in which the number of weeks is more than 8 less than the max 
  group_by(model) %>%
  summarise(n_forecasts = n()) %>%
  ungroup() %>%
  mutate(max_forecasts = max(n_forecasts)) %>%
  group_by(model) %>%
  summarise(percent_total_forecasts = round(((n_forecasts /max_forecasts)*100),2))

scores <- inc_scores %>%
  filter(forecast_date > as.Date("2020-12-31")) %>% 
  group_by(model, target_end_date_1wk_ahead, horizon) %>%
  mutate(first_per_week = ifelse(row_number() == 1, 1, 0)) %>% ungroup() %>%
  group_by(model) %>% 
  mutate(n_weeks = sum(horizon == 4 & first_per_week == 1 )) %>% #count number of weeks that have a horizon of 4 (if grouped by horizon there are different numbers of weeks per horizon)
  ungroup() %>%
  filter(n_weeks >= max(n_weeks -8)) %>% ungroup() %>%
  select("model", "forecast_date", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_wis(scores = scores, mx = models[mx], my = models[my],
                                   permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}


ind_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]

tab <- data.frame(model = names(geom_mean_ratios),
                  geom_mean_ratios = geom_mean_ratios,
                  ratios_baseline = ratios_baseline,
                  ratios_baseline2 = ratios_baseline2)

tab <- tab[order(tab$ratios_baseline2), ]


tab #final column is theta^*_iB

pairwise_scores <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 

#Merge count and Pairwise 
scores_8m_2021 <- merge(fcast_count_8missing_2021, pairwise_scores) %>%
  rename_at(vars(-model), ~ paste0(., '.2021_8m'))

#MERGING ALL 5 ANALYSES 
merge_all <- scores_full %>%
  full_join(scores_2020) %>%
  full_join(scores_8m) %>%
  full_join(scores_2021) %>%
  full_join(scores_8m_2021)

write_csv(merge_all, "paper-inputs/sensitivity_table2_update2.csv")