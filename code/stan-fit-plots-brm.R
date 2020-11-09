## stan_fit_wis_diff <- readRDS("paper-inputs/2020-10-15-stan-fit_wis_diff.rds")

coef_ests <- tibble(data.frame(fixef(stan_fit_wis_diff, probs=c(0.05, 0.95))), tmp=rownames(fixef(stan_fit_wis_diff))) %>%
    extract(tmp, "model", "model(.*?)\\s*:", remove=FALSE) %>%
    extract(tmp, "target", ".*target(.*)")

raw_means <- stan_fit_wis_diff$data %>% 
    group_by(target, model) %>%
    summarize(mean_wis_diff = mean(wis_diff), median_wis_diff = median(wis_diff), sd_wis_diff = sd(wis_diff), n_fcasts = length(unique(target_end_date_1wk_ahead))) %>%
    mutate(target=str_replace_all(target, "\\s", ""), 
        model=str_replace_all(model, "-", "M")) %>%
    left_join(coef_ests)

ggplot(raw_means, aes(x=median_wis_diff, y=Estimate, color=factor(model))) +
    #geom_point(aes(size=n_fcasts)) +
    geom_point(aes(shape=factor(target))) +
    geom_segment(aes(y=Q5, yend=Q95, x=median_wis_diff, xend=median_wis_diff)) +
    geom_abline(color="gray") +
    geom_hline(yintercept=0, linetype=2) +
    xlab("empirical average of WIS diff") +
    ylab("estimated coefficient and 90% credible interval") +
    theme_bw()


raw_means_by_loc <- stan_fit_wis_diff$data %>% 
    group_by(target, model, location_name) %>%
    summarize(mean_wis_diff = mean(wis_diff), median_wis_diff = median(wis_diff), sd_wis_diff = sd(wis_diff), n_fcasts = length(unique(target_end_date_1wk_ahead))) %>%
    mutate(target=str_replace_all(target, "\\s", ""), 
        model=str_replace_all(model, "-", "M")) %>%
    group_by(target, model) %>%
    summarize(avg_median_wis_diff = mean(median_wis_diff))
