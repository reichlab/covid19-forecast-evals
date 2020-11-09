library(tidyverse)
library(rstanarm)

CORES <- 2 ## 20 is good for nick's desktop

## get as many cores as you can
options(mc.cores = parallel::detectCores())

## read in scores with eligibility criteria
cum_scores <- read_csv("paper-inputs/20201013-cum-scores.csv") %>%
    mutate(abs_error_round = round(abs_error))

## fit stan model
stan_fit <- stan_glmer(abs_error_round ~  model*target + (1|target_end_date_1wk_ahead:location_name), 
    chains=CORES,
    ##thin=10, ## earlier runs showed substantial auto-correlation
    family=neg_binomial_2(),
    data = cum_scores)

## save the model
saveRDS(stan_fit, file=paste0("paper-inputs/", Sys.Date(), "stan-fit.rds"))
