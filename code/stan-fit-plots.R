stan_fit_wis_diff <- readRDS("paper-inputs/2020-10-15-stan-fit_wis_diff.rds")

post_ints <- tibble(data.frame(posterior_interval(stan_fit_wis_diff))) %>%
    rename(lower5 = X5., upper95=X95.) %>%
    mutate(tmp = rownames(posterior_interval(stan_fit_wis_diff))) %>%
    filter(grepl("model", tmp))

fit_coefs <- tibble(coef = fixef(stan_fit_wis_diff), tmp = names(fixef(stan_fit_wis_diff))) %>%
    left_join(post_ints) %>%
    extract(tmp, "model", "model(.*?)\\s*:", remove=FALSE) %>%
    extract(tmp, "target", ".*target(.*)")

raw_means <- stan_fit_wis_diff$data %>% 
    group_by(target, model) %>%
    summarize(mean_wis_diff = mean(wis_diff), sd_wis_diff = sd(wis_diff), n_fcasts = length(unique(timezero))) %>%
    left_join(fit_coefs)

ggplot(raw_means, aes(x=mean_wis_diff, y=coef, color=factor(model))) +
    #geom_point(aes(size=n_fcasts)) +
    geom_point(aes(shape=factor(target))) +
    geom_segment(aes(y=lower5, yend=upper95, x=mean_wis_diff, xend=mean_wis_diff)) +
    geom_abline(color="gray") +
    geom_hline(yintercept=0, linetype=2) +
    xlab("empirical average of WIS diff") +
    ylab("estimated coefficient and 90% credible interval") +
    theme_bw()
        

