library(rstanarm)

CORES <- 20 ## good for nick's desktop

## get as many cores as you can
options(mc.cores = parallel::detectCores())

## read in scores with eligibility criteria

## fit stan model
stan_fit <- stan_glmer(round(abs_error) ~  model_code*target + (1|first_fcast_sat:location_name), 
    chains=CORES,
    thin=10, ## earlier runs showed substantial auto-correlation
    family=neg_binomial_2(),
    data = scored_models_df_pred)

## save the model
