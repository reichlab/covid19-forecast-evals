## download scores from Zoltar
library(zoltr)  ##devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")

data(hub_locations)

model_eligibility_inc <- read_csv("paper-inputs/model-eligibility-inc.csv")
model_eligibility_cum <- read_csv("paper-inputs/model-eligibility.csv")

cum_scores <- read_csv("data-raw/cum-scores-from-zoltar.csv", col_types = "ncDcccnnnnnnnnnnnnnn") %>% select(-X1)
inc_scores <- read_csv("data-raw/inc-scores-from-zoltar.csv", col_types = "ncDcccnnnnnnnnnnnnnn") %>% select(-X1)

## calculate WIS add other useful fields
cum_scores_calc <- cum_scores %>%
    mutate(wis = (.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+
            .3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90+.5*interval_100)/12)  %>% 
    #select(-starts_with("interval")) %>%
    left_join(hub_locations, by=c("unit" = "fips"))

inc_scores_calc <- inc_scores %>%
    mutate(wis = (.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+
            .3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90+.5*interval_100)/12)  %>% 
    #select(-starts_with("interval")) %>%
    left_join(hub_locations, by=c("unit" = "fips"))

## keep only forecasts for eligible model-timezeros 
cum_scores_eligible <- cum_scores_calc %>%
  right_join(filter(model_eligibility_cum, target_group=="cum"))

inc_scores_eligible <- inc_scores_calc %>%
  right_join(filter(model_eligibility_inc, target_group=="inc"))


## add target_wk_end_date for all scores and remove second forecasts per week
timezero_end_dates <- tibble(
  timezero=unique(c(cum_scores_calc$timezero, inc_scores_calc$timezero)), 
  target_end_date_1wk_ahead = as.Date(covidHubUtils::calc_target_week_end_date(timezero, horizon=1)))

cum_scores_calc <- left_join(cum_scores_calc, timezero_end_dates) %>%
  ## only keep 1 forecast per week
  group_by(model, target_end_date_1wk_ahead) %>%
  mutate(last_timezero = max(timezero)) %>%
  filter(timezero == last_timezero) 

inc_scores_calc <- left_join(inc_scores_calc, timezero_end_dates) %>%
  ## only keep 1 forecast per week
  group_by(model, target_end_date_1wk_ahead) %>%
  mutate(last_timezero = max(timezero)) %>%
  filter(timezero == last_timezero)


## write to disk
write_csv(cum_scores_eligible, file = paste0("paper-inputs/cum-scores.csv"))
write_csv(inc_scores_eligible, file = paste0("paper-inputs/inc-scores.csv"))

write_csv(cum_scores_calc, file = paste0("paper-inputs/cum-scores_all.csv"))
write_csv(inc_scores_calc, file = paste0("paper-inputs/inc-scores_all.csv"))

