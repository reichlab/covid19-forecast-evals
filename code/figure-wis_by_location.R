library(lubridate)
library(tidyverse)
library(covidHubUtils)
library(surveillance)
library(tidytext)

#reorder states, reorder models 

source("code/load-global-analysis-dates.R")

theme_set(theme_bw())
data("hub_locations")

model_levels <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  mutate(model = recode(model, "IHME-CurveFit" = "IHME-SEIR")) %>%
  arrange(relative_wis) %>%
  pull(model)

# theme_set(theme_bw())
# data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  mutate(model = recode(model, "IHME-CurveFit" = "IHME-SEIR")) %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  filter(horizon %in% c(1:4)) %>%
  filter(include_overall == TRUE) %>%
  filter(model %in% c(model_levels)) %>%
  filter(forecast_date <= last_timezero4wk) 

#Count number of weeks a model has submitted 
models_to_highlight <- inc_scores %>%
  group_by(model, target_end_date_1wk_ahead, horizon) %>%
  mutate(first_per_week = row_number() == 1) %>% ungroup() %>%
  group_by(model) %>% 
  mutate(n_weeks = sum(horizon == 4 & first_per_week)) %>% #count number of weeks that have a horizon of 4 (includes only core)
  ungroup() %>%
  mutate(highlight = ifelse(n_weeks > max(n_weeks - 5), "all", "missing5weeks")) %>%
  select(model, highlight) %>% unique() %>%
  filter(highlight == "missing5weeks") %>% pull(model)

###################################################################################################
# Calculate pairwise WIS

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

# select relevant columns:
scores <- inc_scores %>% select("model", "forecast_date", "location", "location_name", "horizon", "abs_error", "wis") %>% droplevels()

# the included models and locations:
models <- unique(scores$model)
locations <- unique(scores$location)
location_names <- unique(scores$location_name)

# function for pairwise comparison of models
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  
  ############## This line had been deleted, but needs to be included ##########################
  # apply subset:
  scores <- scores[subset, ]
  
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




# compute pairwise and relative WIS for each location separately:
for(i in seq_along(locations)){
  
  # select location:
  loc <- locations[i]
  loc_name <- location_names[i]
  
  # matrix to store:
  results_ratio_temp <- matrix(ncol = length(models),
                               nrow = length(models),
                               dimnames = list(models, models)) 
  
  # run pairwise comparison for chosen location:
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                                 permutation_test = FALSE, # disable permutation test to speed up things
                                 subset = scores$location == loc) # this will subset to the respective location inside the function
      results_ratio_temp[mx, my] <- pwc$ratio
      results_ratio_temp[my, mx] <- 1/pwc$ratio
    }
  }
  
  # compute the geometric means etc
  ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
  geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
  ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
  
  # summarize results:
  to_add <- data.frame(model = names(ratios_baseline2_temp),
                       location = loc,
                       location_name = loc_name,
                       relative_wis = ratios_baseline2_temp,
                       log_relative_wis = log(ratios_baseline2_temp))
  
  # append to already stored:
  if(i == 1){ # initialize at first location
    average_by_loc <- to_add
  }else{
    average_by_loc <- rbind(average_by_loc, to_add)
  }
  
  cat("Finished", loc_name, "\n")
}



## plot of true data by state, tiled
truth_dat <- load_truth(truth_source = "JHU",
                        target_variable = "inc death") %>%
  filter(geo_type == "state") %>%
  filter(!is.na(value), target_end_date <= truth_date) %>%
  select(target_end_date, value, location_name, abbreviation) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  mutate(location_name = reorder(location_name, X=value, FUN=function(x) max(x, na.rm=TRUE))) %>% 
  select(location_name) %>%
  unique() %>%
  pull(location_name)



average_by_loc_to_plot <- average_by_loc %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  mutate(location_name = fct_relevel(location_name, levels(truth_dat)),
         relative_wis_text = sprintf("%.1f", round(relative_wis, 1)),
         log_relative_wis = log2(relative_wis),
         model = fct_relevel(model, model_levels)) %>%
  filter(!is.na(relative_wis)) 


# plot:
fig_wis_loc <- ggplot(average_by_loc_to_plot, 
                      aes(x=model, y=location_name, 
                          fill= scales::oob_squish(log_relative_wis, range = c(- 2.584963, 2.584963)))) +
  geom_tile() +
  geom_text(aes(label = relative_wis_text), size = 2.5) + # I adapted the rounding
  scale_fill_gradient2(low = "steelblue", high = "red", midpoint = 0, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2), 
                       labels =c("0.25", 0.5, 1, 2, 4)) + 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        #                            color=ifelse(
        #                              levels(average_by_loc_to_plot$model) %in% models_to_highlight,
        #                              "red", "black")),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 9))


pdf(file = "figures/fig-wis-location.pdf",width=8, height=8)
print(fig_wis_loc)
dev.off()

jpeg(file = "figures/fig-wis-location.jpg", width=8, height=8, units="in", res=300)
print(fig_wis_loc)
dev.off()


#PHASE SPECIFIC

inc_scores_phase <- read_csv("paper-inputs/inc-scores.csv") %>%
  mutate(model = recode(model, "IHME-CurveFit" = "IHME-SEIR")) %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  filter(horizon %in% c(1:4)) %>%
  filter(include_phases == TRUE) %>%
  filter(forecast_date <= last_timezero4wk) 


# compute pairwise and relative WIS for each location separately:
for(i in seq_along(locations)){
  
  # select location:
  loc <- locations[i]
  loc_name <- location_names[i]
  
  # matrix to store:
  results_ratio_temp <- matrix(ncol = length(models),
                               nrow = length(models),
                               dimnames = list(models, models)) 
  
  # run pairwise comparison for chosen location:
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                                 permutation_test = FALSE, # disable permutation test to speed up things
                                 subset = scores$location == loc) # this will subset to the respective location inside the function
      results_ratio_temp[mx, my] <- pwc$ratio
      results_ratio_temp[my, mx] <- 1/pwc$ratio
    }
  }
  
  # compute the geometric means etc
  ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
  geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
  ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
  
  # summarize results:
  to_add <- data.frame(model = names(ratios_baseline2_temp),
                       location = loc,
                       location_name = loc_name,
                       relative_wis = ratios_baseline2_temp,
                       log_relative_wis = log(ratios_baseline2_temp))
  
  # append to already stored:
  if(i == 1){ # initialize at first location
    average_by_loc <- to_add
  }else{
    average_by_loc <- rbind(average_by_loc, to_add)
  }
  
  cat("Finished", loc_name, "\n")
}

average_by_loc_spring <- average_by_loc %>%
  mutate(seasonal_phase = "spring")




###phases chart
library(scoringutils)

spring_phase <- inc_scores_phase %>%
  filter(include_phases == TRUE) %>%
  filter(seasonal_phase == "spring")

scores <- spring_phase %>% 
  select("model", "forecast_date", "location", "location_name", 
         "horizon", "abs_error", "wis") %>%
  mutate(model = factor(model))

# the included models and locations:
models <- unique(scores$model)
locations <- unique(scores$location)
location_names <- unique(scores$location_name)


# compute pairwise and relative WIS for each location separately:
for(i in seq_along(locations)){
  
  # select location:
  loc <- locations[i]
  loc_name <- location_names[i]
  
  # matrix to store:
  results_ratio_temp <- matrix(ncol = length(models),
                               nrow = length(models),
                               dimnames = list(models, models)) 
  
  # run pairwise comparison for chosen location:
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                                 permutation_test = FALSE, # disable permutation test to speed up things
      subset = scores$location == loc) # this will subset to the respective location inside the function
      results_ratio_temp[mx, my] <- pwc$ratio
      results_ratio_temp[my, mx] <- 1/pwc$ratio
    }
  }
  
  # compute the geometric means etc
  ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
  geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
  ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
  
  # summarize results:
  to_add <- data.frame(model = names(ratios_baseline2_temp),
                       location = loc,
                       location_name = loc_name,
                       relative_wis = ratios_baseline2_temp,
                       log_relative_wis = log(ratios_baseline2_temp))
  
  # append to already stored:
  if(i == 1){ # initialize at first location
    average_by_loc <- to_add
  }else{
    average_by_loc <- rbind(average_by_loc, to_add)
  }
  
  cat("Finished", loc_name, "\n")
}

average_by_loc_spring <- average_by_loc %>%
  mutate(seasonal_phase = "spring")


#Summer
summer_phase <- inc_scores_phase %>% 
  filter(include_phases == TRUE) %>%
  filter(seasonal_phase == "summer")  %>%
  mutate(model = factor(model))

scores <- summer_phase %>% 
  select("model", "forecast_date", "location", "location_name", "horizon", "abs_error", "wis")

# the included models and locations:
models <- unique(scores$model)
locations <- unique(scores$location)
location_names <- unique(scores$location_name)


# compute pairwise and relative WIS for each location separately:
for(i in seq_along(locations)){
  
  # select location:
  loc <- locations[i]
  loc_name <- location_names[i]
  
  # matrix to store:
  results_ratio_temp <- matrix(ncol = length(models),
                               nrow = length(models),
                               dimnames = list(models, models)) 
  
  # run pairwise comparison for chosen location:
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                                 permutation_test = FALSE, # disable permutation test to speed up things
      subset = scores$location == loc) # this will subset to the respective location inside the function
      results_ratio_temp[mx, my] <- pwc$ratio
      results_ratio_temp[my, mx] <- 1/pwc$ratio
    }
  }
  
  # compute the geometric means etc
  ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
  geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
  ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
  
  # summarize results:
  to_add <- data.frame(model = names(ratios_baseline2_temp),
                       location = loc,
                       location_name = loc_name,
                       relative_wis = ratios_baseline2_temp,
                       log_relative_wis = log(ratios_baseline2_temp))
  
  # append to already stored:
  if(i == 1){ # initialize at first location
    average_by_loc <- to_add
  }else{
    average_by_loc <- rbind(average_by_loc, to_add)
  }
  
  cat("Finished", loc_name, "\n")
}

average_by_loc_summer <- average_by_loc %>%
  mutate(seasonal_phase = "summer")


#Winter
winter_phase <- inc_scores_phase %>%
  filter(include_phases == TRUE) %>%
  filter(seasonal_phase == "winter") 

scores <- winter_phase %>%
  select("model", "forecast_date", "location", "location_name", "horizon", "abs_error", "wis") %>%
  mutate(model = factor(model))

# the included models and locations:
models <- unique(scores$model)
locations <- unique(scores$location)
location_names <- unique(scores$location_name)


# compute pairwise and relative WIS for each location separately:
for(i in seq_along(locations)){
  
  # select location:
  loc <- locations[i]
  loc_name <- location_names[i]
  
  # matrix to store:
  results_ratio_temp <- matrix(ncol = length(models),
                               nrow = length(models),
                               dimnames = list(models, models)) 
  
  # run pairwise comparison for chosen location:
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                                 permutation_test = FALSE, # disable permutation test to speed up things
     subset = scores$location == loc) # this will subset to the respective location inside the function
      results_ratio_temp[mx, my] <- pwc$ratio
      results_ratio_temp[my, mx] <- 1/pwc$ratio
    }
  }
  
  # compute the geometric means etc
  ind_baseline <- which(rownames(results_ratio_temp) == "COVIDhub-baseline")
  geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline_temp <- results_ratio_temp[, "COVIDhub-baseline"]
  ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["COVIDhub-baseline"]
  
  # summarize results:
  to_add <- data.frame(model = names(ratios_baseline2_temp),
                       location = loc,
                       location_name = loc_name,
                       relative_wis = ratios_baseline2_temp,
                       log_relative_wis = log(ratios_baseline2_temp))
  
  # append to already stored:
  if(i == 1){ # initialize at first location
    average_by_loc <- to_add
  }else{
    average_by_loc <- rbind(average_by_loc, to_add)
  }
  
  cat("Finished", loc_name, "\n")
}

average_by_loc_winter <- average_by_loc %>%
  mutate(seasonal_phase = "winter")


average_by_loc_to_plot <- average_by_loc %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  mutate(location_name = fct_relevel(location_name, levels(truth_dat)),
         relative_wis_text = sprintf("%.1f", round(relative_wis, 1)),
         log_relative_wis = log2(relative_wis),
         model = fct_relevel(model, model_levels)) %>%
  filter(!is.na(relative_wis)) 

average_by_loc_to_plot$model <- reorder(average_by_loc_to_plot$model, average_by_loc_to_plot$relative_wis)



# 
# pairwise_df_spring <- scoringutils::pairwise_comparison(
#   scores = spring_phase, 
#   metric = "interval_score",
#   baseline = "COVIDhub-baseline",
#   summarise_by = c("model", "location_name"))  %>%
#   mutate(seasonal_phase = "spring")
# 
# pairwise_df_summer <- scoringutils::pairwise_comparison(
#   scores = inc_scores %>% filter(seasonal_phase == "summer") %>% mutate(interval_score = wis), 
#   metric = "interval_score",
#   baseline = "COVIDhub-baseline",
#   summarise_by = c("model", "location_name"))  %>%
#   mutate(seasonal_phase = "summer")
# 
# 
# pairwise_df_winter <- scoringutils::pairwise_comparison(
#   scores = inc_scores %>% filter(seasonal_phase == "winter") %>% mutate(interval_score = wis), 
#   metric = "interval_score",
#   baseline = "COVIDhub-baseline",
#   summarise_by = c("model", "location_name")) %>%
#   mutate(seasonal_phase = "winter")

model_levels_phases <- read_csv("paper-inputs/table-phase-performance.csv") %>%
  group_by(seasonal_phase) %>% arrange(seasonal_phase, relative_wis) %>% 
  select(model, seasonal_phase, order_wis = relative_wis)

to_plot_phase <- rbind(average_by_loc_spring, average_by_loc_summer,average_by_loc_winter) %>%
  left_join(model_levels_phases) %>%
  mutate(model = fct_reorder(model, order_wis)) 
    
fig_wis_loc <- ggplot(to_plot_phase, aes(x = reorder_within(model,order_wis,seasonal_phase),
                                         y=location_name, fill= scales::oob_squish(log_relative_wis, 
                                                                range = c(- 2.584963, 2.584963)))) +
  geom_tile() +
  facet_grid( cols = vars(seasonal_phase), scales = "free_x", space="free") + #, scales="free_y") +
  geom_text(aes(label = round(relative_wis,1)), size = 2) + # I adapted the rounding
  scale_fill_gradient2(low = "steelblue", high = "red", midpoint = 0, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2), 
                       labels =c("0.25", 0.5, 1, 2, 4)) + 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
                                   # color=ifelse(
                                   #   levels(to_plot_phase$model) %in% models_to_highlight,
                                   #   "red", "black")),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 9)) +
  scale_x_reordered()


pdf(file = "figures/fig-wis-location_phase.pdf", width=16, height=8)
print(fig_wis_loc)
dev.off()

jpeg(file = "figures/fig-wis-location_phase.jpg", width=16, height=8, units="in", res=300)
print(fig_wis_loc)
dev.off()

