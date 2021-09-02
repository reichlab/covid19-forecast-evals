library(tidyverse)
library(covidHubUtils)

theme_set(theme_bw())

source("pairwise_comparison.R")

data("hub_locations")
US_fips <- hub_locations %>%  filter(geo_type == "state") %>% filter(abbreviation %in% datasets::state.abb) %>% pull(fips) #only 50 states 

load("score_case_all.rda")

first_target_end_date <- as.Date("2021-07-01")

case_scores_focused <- filter(score_case_all, target_end_date > first_target_end_date)

## plot of coverage
case_coverage <- case_scores_focused %>%
  filter(score_name %in% c("coverage_50", "coverage_95")) %>%
  select(model, horizon, forecast_date, location, target_end_date, score_name, score_value) %>%
  pivot_wider(names_from = score_name, values_from = score_value) %>%
  group_by(model, horizon) %>%
  summarize(obs = n(),
            mean_cov_50 = mean(coverage_50),
            mean_cov_95 = mean(coverage_95)) %>%
  filter(obs>200)

write_csv(case_coverage, file="PIcoverage-by-horizon.csv")



ggplot(case_coverage, aes(x=horizon, y=mean_cov_95, color=model, group=model)) +
  geom_point(alpha=.5) +
  geom_line(alpha=0.5) +
  scale_y_continuous(name="95% PI coverage", limits=c(0,1)) + 
  geom_hline(yintercept=0.95, linetype=2) +
  geom_point(data=filter(case_coverage, model=="COVIDhub-ensemble"), color="red", shape=3, size=3) +
  geom_point(data=filter(case_coverage, model=="COVIDhub-baseline"), color="blue", shape=4, size=3)

plotly::ggplotly()


## relative WIS computation
inc_scores <- case_scores_focused %>%
  filter(n_locations >= 25) %>%
  filter(n_weeks >= 4) %>%
  droplevels()

# bring all timezeros to Monday:
inc_scores$timezero <- inc_scores$submission_sat

# restrict to 1-4 wk ahead state-level
scores <- inc_scores %>% filter(horizon %in% paste(1:4),  location %in% US_fips) %>%
  filter(!is.na(score_value)) %>%
  select("model", "timezero", "location", "horizon", "score_name", "score_value") %>%
  filter(score_name %in% c("abs_error", "wis")) %>%
  pivot_wider(names_from = score_name, values_from = score_value) %>% filter(!is.na(wis))

# the included models:
models <- unique(scores$model)


# matrices to store:
results_ratio_hzn1 <- matrix(ncol = length(models),
                             nrow = length(models),
                             dimnames = list(models, models))
results_ratio_list <- list(results_ratio_hzn1, results_ratio_hzn1,
                           results_ratio_hzn1, results_ratio_hzn1)

set.seed(123) # set seed for permutation tests

for(mx in seq_along(models)){
  for(my in 1:mx){
    for(hzn in 1:4){
      pwc <- pairwise_comparison(scores = filter(scores, horizon==hzn), 
                                 mx = models[mx], 
                                 my = models[my],
                                 permutation_test = FALSE)
      
      results_ratio_list[[hzn]][mx, my] <- pwc$ratio
      results_ratio_list[[hzn]][my, mx] <- 1/pwc$ratio
    }
  }
}

ind_baseline <- which(rownames(results_ratio_list[[1]]) == "COVIDhub-baseline")
geom_mean_ratios_hzn1 <- exp(rowMeans(log(results_ratio_list[[1]][, -ind_baseline]), na.rm = TRUE))
geom_mean_ratios_hzn2 <- exp(rowMeans(log(results_ratio_list[[2]][, -ind_baseline]), na.rm = TRUE))
geom_mean_ratios_hzn3 <- exp(rowMeans(log(results_ratio_list[[3]][, -ind_baseline]), na.rm = TRUE))
geom_mean_ratios_hzn4 <- exp(rowMeans(log(results_ratio_list[[4]][, -ind_baseline]), na.rm = TRUE))

ratios_baseline_raw_hzn1 <- results_ratio_list[[1]][, "COVIDhub-baseline"] ## avgWIS_model / avgWIS_baseline
ratios_baseline_raw_hzn2 <- results_ratio_list[[2]][, "COVIDhub-baseline"] ## avgWIS_model / avgWIS_baseline
ratios_baseline_raw_hzn3 <- results_ratio_list[[3]][, "COVIDhub-baseline"] ## avgWIS_model / avgWIS_baseline
ratios_baseline_raw_hzn4 <- results_ratio_list[[4]][, "COVIDhub-baseline"] ## avgWIS_model / avgWIS_baseline

ratios_baseline_adj_hzn1 <- geom_mean_ratios_hzn1/geom_mean_ratios_hzn1["COVIDhub-baseline"]
ratios_baseline_adj_hzn2 <- geom_mean_ratios_hzn2/geom_mean_ratios_hzn2["COVIDhub-baseline"]
ratios_baseline_adj_hzn3 <- geom_mean_ratios_hzn3/geom_mean_ratios_hzn3["COVIDhub-baseline"]
ratios_baseline_adj_hzn4 <- geom_mean_ratios_hzn4/geom_mean_ratios_hzn4["COVIDhub-baseline"]

tab <- data.frame(model = c(names(geom_mean_ratios_hzn1), 
                            names(geom_mean_ratios_hzn2),
                            names(geom_mean_ratios_hzn3), 
                            names(geom_mean_ratios_hzn4)),
                  horizon = rep(1:4, each=length(geom_mean_ratios_hzn1)),
                  geom_mean_ratios = c(geom_mean_ratios_hzn1,
                                       geom_mean_ratios_hzn2,
                                       geom_mean_ratios_hzn3,
                                       geom_mean_ratios_hzn4),
                  ratios_baseline_raw = c(ratios_baseline_raw_hzn1,
                                          ratios_baseline_raw_hzn2,
                                          ratios_baseline_raw_hzn3,
                                          ratios_baseline_raw_hzn4),
                  ratios_baseline_adj = c(ratios_baseline_adj_hzn1,
                                          ratios_baseline_adj_hzn2,
                                          ratios_baseline_adj_hzn3,
                                          ratios_baseline_adj_hzn4)
                  )

write_csv(tab, file="relativeWIS-by-horizon.csv")

ggplot(tab, aes(x=horizon, y=ratios_baseline_adj, color=model, group=model)) +
  geom_point(alpha=.5) +
  geom_line(alpha=0.5) +
  scale_y_continuous(name="relative WIS") + 
  geom_hline(yintercept=1, linetype=2) +
  geom_point(data=filter(tab, model=="COVIDhub-ensemble"), color="red", shape=3, size=3)

plotly::ggplotly()


## some forecast plots

models_of_interest <- c("COVIDhub-ensemble", "Karlen-pypm", "IEM_MED-CovidProject", "USC-SI_kJalpha")
fdat <- load_forecasts(models = models_of_interest,
                       dates = seq.Date(as.Date("2021-07-05"), as.Date("2021-08-30"), by="7 days"),
                       date_window_size = 6,
                       targets = paste(1:4, "wk ahead inc case"), 
                       locations = c("US", "12", "22") ## US, FL, LA
                       )

truth_dat <- load_truth(truth_source="JHU",
                        target_variable="inc case", 
                        locations=c("US", "12", "22")) %>%
  filter(target_end_date > as.Date("2021-06-01"))

plot_forecasts(fdat, 
               truth_data = truth_dat,
               facet = location ~ model, 
               facet_ncol=4, 
               facet_scales = "free_y",
               fill_by_model = TRUE, 
               truth_source = "JHU")
