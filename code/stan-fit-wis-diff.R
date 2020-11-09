library(tidyverse)
library(rstanarm)

CORES <- 2 ## 20 is good for nick's desktop

## get as many cores as you can
options(mc.cores = parallel::detectCores())

## read in scores with eligibility criteria
cum_scores <- read_csv("paper-inputs/20201013-cum-scores.csv") %>%
    filter(target %in% paste(1:4, "wk ahead cum death"),
        !(location_name %in% c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")),
        !(location_name=="New Jersey" & target_end_date_1wk_ahead=="2020-07-04"))

baseline_scores <- cum_scores %>% 
    filter(model=="COVIDhub-baseline") %>%
    mutate(wis_baseline = wis) %>%
    select(target_end_date_1wk_ahead, unit, target, wis_baseline)

scores_with_baseline <- cum_scores %>%
    filter(model != "COVIDhub-baseline") %>%
    left_join(baseline_scores) %>%
    mutate(wis_diff = wis-wis_baseline)
    

## plot the scores: wis_diff
ggplot(scores_with_baseline, aes(x=wis_diff)) +
    geom_histogram(bins=500) +
    facet_wrap(.~model, scales="free")

models_scored <- unique(scores_with_baseline$model)
pdf("plots/wis_diff_by_model.pdf", 10, 10)
for(i in 1:length(models_scored)){
p <- ggplot(filter(scores_with_baseline, model==models_scored[i]), aes(y=wis_diff, color=target)) +
geom_boxplot() +
facet_wrap(.~location_name) +
ggtitle(paste("WIS difference for", models_scored[i]))
print(p)
}
dev.off()


locs_scored <- unique(scores_with_baseline$location_name)
pdf("plots/wis_diff_by_loc.pdf", 10, 10)
for(i in 1:length(locs_scored)){
    p <- ggplot(filter(scores_with_baseline, location_name==locs_scored[i]), aes(y=wis_diff, color=target)) +
        geom_boxplot() +
        geom_hline(yintercept=0, linetype=2) +
        facet_wrap(.~model) +
        ggtitle(paste("WIS difference for", locs_scored[i]))
    print(p)
}
dev.off()

ggplot(scores_with_baseline, aes(y=wis_diff, color=target)) +
    geom_boxplot() +
    geom_hline(yintercept=0, linetype=2) +
    facet_wrap(.~model)

ggplot(scores_with_baseline, aes(x=wis_diff)) +
    geom_histogram(bins=100) +
    #scale_x_log10() +
    facet_wrap(.~location_name, scales="free")

## fit stan model
stan_fit <- stan_lmer(wis_diff ~  model:target - 1 + (1|target_end_date_1wk_ahead:location_name),
    chains=CORES,
    prior=student_t(),
    ##thin=10, ## earlier runs showed substantial auto-correlation
    data = scores_with_baseline)

# quantreg_fit <- quantreg::rq(wis_diff ~ model + target + location_name, tau=.5, data = scores_with_baseline)

# stan_fit <- brm(wis_diff ~  model:target - 1 + location_name, 
#     chains=CORES,
#     family=student(),
#     ##thin=10, ## earlier runs showed substantial auto-correlation
#     data = filter(scores_with_baseline, model %in% c("COVIDhub-ensemble", "JHU_IDD-CovidSP", "YYG-ParamSearch", "UT-Mobility", "UMass-MechBayes")))


## save the model
saveRDS(stan_fit, file=paste0("paper-inputs/", Sys.Date(), "-stan-fit_wis_diff.rds"))
