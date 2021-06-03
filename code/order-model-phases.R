#Create order of relative WIS for the models included in phase analysis 
library(tidyverse)

#TABLES BY PHASE 

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_phases == TRUE) %>%
  filter(location_name %in% datasets::state.name) %>%
  filter(horizon %in% c(1:4)) %>%
  filter(!is.na(wis)) %>% 
  select("model", "forecast_date", "target_end_date_1wk_ahead", "location", "horizon", "abs_error", "wis")

models <- unique(inc_scores$model)

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
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ forecast_date, data = sub, FUN = mean)
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


## compute the pairwise rel WIS
results_ratio <- matrix(ncol = length(models),
                        nrow = length(models),
                        dimnames = list(models, models))

for(mx in seq_along(models)){
  for(my in 1:mx){
    pwc <- pairwise_comparison_NA(scores = inc_scores, mx = models[mx], my = models[my],
                                  permutation_test = FALSE)
    results_ratio[mx, my] <- pwc$ratio
    results_ratio[my, mx] <- 1/pwc$ratio
  }
}

## standardize relWIS relative to baseline
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
  mutate(relative_wis = ifelse(is.na(relative_wis), "1", relative_wis)) %>% 
  arrange(relative_wis)

write_csv(pairwise_scores, file = "paper-inputs/phase-performance_order.csv")
