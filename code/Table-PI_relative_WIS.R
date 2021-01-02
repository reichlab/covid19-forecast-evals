#This script includes code that is also in "figure-calibration.r" and "pairwise_wis.R". It has been added here in order to create table 2 all in 1 place.

library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)


theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(horizon %in% c(1:4)) 

calib_table <- inc_scores %>%
  group_by(model) %>%
  summarise(n_forecasts = n(),
            calib_95 = round(sum(coverage_95)/ n(), digits = 2),
            calib_50 = round(sum(coverage_50)/ n(), digits = 2))  


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


invisible(library(surveillance)) # contains permutation test

# function for pairwise comparison of models
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
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

set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = TRUE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    results_pval[mx, my] <-
      results_pval[my, mx] <- pwc$pval
    results_pval_fcd[mx, my] <-
      results_pval_fcd[my, mx] <- pwc$pval_fcd
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



#Calculate Relative MAE

# function for pairwise comparison of models
pairwise_comparison <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){

  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  
  
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("forecast_date", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  
  # compute ratio:
  ratio <- sum(sub$abs_error.x) / sum(sub$abs_error.y)
  
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$abs_error.x, sub$abs_error.y,
                            nPermutation = 999)$pVal.permut
    
    # aggregate by forecast date:
    sub_fcd <- aggregate(cbind(abs_error.x, abs_error.y) ~ forecast_date, data = sub, FUN = mean)
    pval_fcd <- permutationTest(sub_fcd$abs_error.x, sub_fcd$abs_error.y,
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

set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison(scores = scores, mx = models[mx], my = models[my],
                               permutation_test = TRUE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
    results_pval[mx, my] <-
      results_pval[my, mx] <- pwc$pval
    results_pval_fcd[mx, my] <-
      results_pval_fcd[my, mx] <- pwc$pval_fcd
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

