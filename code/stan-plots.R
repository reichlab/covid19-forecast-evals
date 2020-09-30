stan_fit_nb <- readRDS("../paper-inputs/20200927-stan-fit-scores-negbin.rds")
launch_shinystan(stan_fit_nb)

pp_samples <- posterior_predict(stan_fit_nb)
dim(pp_samples)
scored_models_df_pred$pp1 <- pp_samples[1,]
scored_models_df_pred$pp2 <- pp_samples[143,]
scored_models_df_pred$pp3 <- pp_samples[1430,]
scored_models_df_pred$pp4 <- pp_samples[1864,]
scored_models_df_pred$pp5 <- pp_samples[1111,]
scored_models_df_pred$pp6 <- pp_samples[2000,]


tmp_preds <- scored_models_df_pred %>%
    select(model, target, location_name, first_fcast_sat, truth, abs_error, abs_error_rnd, pp1, pp2, pp3, pp4, pp5, pp6) %>%
    pivot_longer(cols = c(abs_error, abs_error_rnd, pp1, pp2, pp3, pp4, pp5, pp6))

## by location
ggplot(mapping=aes(x=value, col=name)) + 
    ## posterior samples
    geom_density(data=dplyr::filter(tmp_preds, name %in% paste0("pp", 1:6)), color="grey", alpha=.5, aes(group=name)) +
    ## observed data
    geom_density(data=dplyr::filter(tmp_preds, name=="abs_error_rnd"), color="black", size=1) + 
    scale_x_sqrt() +
    facet_wrap(.~location_name, scales="free")

## by model
ggplot(mapping=aes(x=value, col=name)) + 
    ## posterior samples
    geom_density(data=dplyr::filter(tmp_preds, name %in% paste0("pp", 1:6)), color="grey", alpha=.5, aes(group=name)) +
    ## observed data
    geom_density(data=dplyr::filter(tmp_preds, name=="abs_error_rnd"), color="black", size=1) + 
    scale_x_sqrt() +
    facet_wrap(.~model, scales="free")

## by target
ggplot(mapping=aes(x=value, col=name)) + 
    ## posterior samples
    geom_density(data=dplyr::filter(tmp_preds, name %in% paste0("pp", 1:6)), color="grey", alpha=.5, aes(group=name)) +
    ## observed data
    geom_density(data=dplyr::filter(tmp_preds, name=="abs_error_rnd"), color="black", size=1) + 
    scale_x_sqrt() +
    facet_wrap(.~target, scales="free")

## by target
ggplot(mapping=aes(x=value, col=name)) + 
    ## posterior samples
    geom_density(data=dplyr::filter(tmp_preds, name %in% paste0("pp", 1:6)), color="grey", alpha=.5, aes(group=name)) +
    ## observed data
    geom_density(data=dplyr::filter(tmp_preds, name=="abs_error_rnd"), color="black", size=1) + 
    scale_x_sqrt() +
    facet_wrap(.~first_fcast_sat, scales="free")
