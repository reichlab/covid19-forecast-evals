#This script includes code that is also in "figure-calibration.r" and "pairwise_wis.R". It has been added here in order to create table 2 all in 1 place.

library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)


theme_set(theme_bw())
data("hub_locations")


scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_overall == TRUE) %>%
  mutate(model = as.factor(model)) %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(horizon %in% c(1:4)) %>%
  filter(!is.na(wis)) 

calib_table <- scores %>%
  group_by(model) %>%
  summarise(n_forecasts = n(),
            calib_95 = round(sum(coverage_95)/ n(), digits = 2),
            calib_50 = round(sum(coverage_50)/ n(), digits = 2))  


# select relevant columns:
scores <- scores %>% select("model", "target_end_date_1wk_ahead", "location", "location_name", "horizon", "abs_error", "wis") %>% droplevels()

# the included models and locations:
models <- unique(scores$model)
locations <- unique(scores$location)
location_names <- unique(scores$location_name)


# function for pairwise comparison of models
pairwise_comparison_NA <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                   permutation_test = FALSE){
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("target_end_date_1wk_ahead", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx$wis))){
    warning("Some or all wis values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby$wis))){
    warning("Some or all wis values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ target_end_date_1wk_ahead, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-value for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}


results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models))


set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_NA(scores = scores, mx = models[mx], my = models[my],
                                  permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # # results_pval_fcd[mx, my] <-
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
  select(model, relative_wis) %>%
  arrange(relative_wis)


#Calculate Relative MAE

# function for pairwise comparison of models
pairwise_comparison_NA_MAE <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                   permutation_test = FALSE){
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("target_end_date_1wk_ahead", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx$abs_error))){
    warning("Some or all abs_error values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby$abs_error))){
    warning("Some or all abs_error values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  # compute ratio:
  ratio <- sum(sub$abs_error.x) / sum(sub$abs_error.y)
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$abs_error.x, sub$abs_error.y,
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(abs_error.x, abs_error.y) ~ target_end_date_1wk_ahead, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- permutationTest(sub_fcd$abs_error.x, sub_fcd$abs_error.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-value for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}


results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                            nrow = length(models),
                                                            dimnames = list(models, models)) 

set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_NA_MAE(scores = scores, mx = models[mx], my = models[my],
                                  permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    # results_pval[mx, my] <-
    #   results_pval[my, mx] <- pwc$pval
    # # results_pval_fcd[mx, my] <-
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

pairwise_scores_MAE <- tab %>%
  mutate(relative_abs_error = round(ratios_baseline2, 2)) %>%
  select(model, relative_abs_error) 


#Merge PI and Pairwise 
calib_pairwise <- merge(calib_table, pairwise_scores)

#merge with MAE
calib_pairwise <- merge(calib_pairwise, pairwise_scores_MAE) %>% arrange(model)


#write Table
write_csv(calib_pairwise, file = "paper-inputs/table-overall-performance.csv")

