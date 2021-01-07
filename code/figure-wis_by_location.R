library(lubridate)
library(tidyverse)
library(covidHubUtils)
library(surveillance)

theme_set(theme_bw())
data("hub_locations")

model_levels <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  arrange(desc(relative_wis)) %>%
  pull(model)

source("code/load-global-analysis-dates.R")

theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  filter(horizon %in% c(1:4)) %>%
  filter(forecast_date <= last_timezero4wk) 

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
scores <- inc_scores %>% select("model", "forecast_date", "location", "location_name", "horizon", "abs_error", "wis")

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


truth <- inc_scores %>%
  filter(model == "COVIDhub-baseline") %>%
  group_by(model, location_name) %>%  #aggregate by week of submission
  summarise(sum_truth = sum(truth_value))  %>% ungroup() %>%
  select(location_name, sum_truth)

average_by_loc <- average_by_loc %>%
  left_join(truth) %>%
  mutate(location_name = fct_reorder(location_name, sum_truth),
        relative_wis = round(relative_wis,2),
        log_relative_wis = log2(relative_wis),
        model = fct_reorder(model, desc(relative_wis))) %>%
  filter(!is.na(relative_wis)) 


# plot:
fig_wis_loc <- ggplot(average_by_loc, aes(x=model, y=location_name, 
                                          fill= scales::oob_squish(log_relative_wis, 
                                                                   range = c(- 2, 2)))) +
  geom_tile() +
  geom_text(aes(label=round(relative_wis, 1)), size = 3) + # I adapted the rounding
  scale_fill_gradient2(low = "navy", high = "red", midpoint = 0, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2), 
                       labels =c("\u2264  0.25", 0.5, 1, 2, "\u2265  4")) + 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 9))


pdf(file = "figures/fig-wis-location.pdf",width=8, height=6)
print(fig_wis_loc)
dev.off()

jpeg(file = "figures/fig-wis-location.jpg", width=8, height=8, units="in", res=300)
print(fig_wis_loc)
dev.off()





