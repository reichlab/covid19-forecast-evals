#This script includes code that is also in "figure-calibration.r" and "pairwise_wis.R". It has been added here in order to create table 2 all in 1 place.

library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)
source("code/load-global-analysis-dates.R")

theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_overall == TRUE) %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(horizon %in% c(1:4)) %>%
  filter(!is.na(wis))

scores <- inc_scores %>% select("model", "target_end_date_1wk_ahead", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

invisible(library(surveillance)) # contains permutation test

# function for pairwise comparison of models
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  
  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("target_end_date_1wk_ahead", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  
  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    
    # aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ target_end_date_1wk_ahead, data = sub, FUN = mean)
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

# set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # results_pval_fcd[mx, my] <-
    #   results_pval_fcd[my, mx] <- pwc$pval_fcd
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


###############First half of eval period 
inc_scores_first_half <- inc_scores %>%
  filter(target_end_date_1wk_ahead <= as.Date("2020-10-24"))

scores <- inc_scores_first_half %>% select("model", "target_end_date_1wk_ahead", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # results_pval_fcd[mx, my] <-
    #   results_pval_fcd[my, mx] <- pwc$pval_fcd
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

pairwise_scores_first_half <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 

#############Second half of eval-period 
inc_scores_second_half <- inc_scores %>%
  filter(target_end_date_1wk_ahead > as.Date("2020-10-24"))

scores <- inc_scores_second_half %>% select("model", "target_end_date_1wk_ahead", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # results_pval_fcd[mx, my] <-
    #   results_pval_fcd[my, mx] <- pwc$pval_fcd
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

pairwise_scores_second_half <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 


#################include only teams that have less than 3 missing during second eval period


#############Second half of eval-period 
inc_scores_second_half_3missing <- inc_scores %>%
  filter(target_end_date_1wk_ahead > as.Date("2020-10-24")) %>%
  group_by(model, location_name, horizon) %>%
  mutate(n_weeks = n()) %>%
  filter(n_weeks > 12)

scores <- inc_scores_second_half_3missing %>% select("model", "target_end_date_1wk_ahead", "location", "horizon", "abs_error", "wis")

# the included models:
models <- unique(scores$model)

# matrices to store:
results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # results_pval_fcd[mx, my] <-
    #   results_pval_fcd[my, mx] <- pwc$pval_fcd
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


pairwise_scores_second_half_3missing <- tab %>%
  mutate(relative_wis = round(ratios_baseline2, 2)) %>%
  select(model, relative_wis) 





# #write csv
# write.csv(calib_pairwise, "calib_table_714_1005.csv", row.names = FALSE)

