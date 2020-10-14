## download scores from Zoltar
library(zoltr)  ##devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")

data(hub_locations)

model_eligibility <- read_csv("paper-inputs/model-eligibility.csv")

## locations/dates with reporting anomalies
dates_with_issues <- read_csv("paper-inputs/anomaly-reporting-dates.csv", col_types = "nDccDnnnc") %>%
    filter(to_remove==1)

cum_scores <- read_csv("data-raw/20201013-cum-scores-from-zoltar.csv", col_types = "ncDcccnnnnnnnnnnnnnn") %>% select(-X1)
inc_scores <- read_csv("data-raw/20201013-inc-scores-from-zoltar.csv", col_types = "ncDcccnnnnnnnnnnnnnn") %>% select(-X1)

## keep only forecasts for eligible model-timezeros 
cum_scores_eligible <- cum_scores %>%
    right_join(filter(model_eligibility, target_group=="cum"))

inc_scores_eligible <- inc_scores %>%
    right_join(filter(model_eligibility, target_group=="inc"))

## remove contaminated weeks of data

## for each location with an issue, remove the scores prior to the issue
cum_scores_eligible$to_keep <- TRUE
for(j in 1:nrow(dates_with_issues)){
    tmp.idx <- which(cum_scores_eligible$timezero <= dates_with_issues$first_fcast_date_impacted[j] & cum_scores_eligible$unit == dates_with_issues$fips[j])
    cum_scores_eligible$to_keep[tmp.idx] <- FALSE
}

inc_scores_eligible$to_keep <- TRUE
for(j in 1:nrow(dates_with_issues)){
  tmp.idx <- which(inc_scores_eligible$timezero <= dates_with_issues$first_fcast_date_impacted[j] & inc_scores_eligible$unit == dates_with_issues$fips[j])
  inc_scores_eligible$to_keep[tmp.idx] <- FALSE
}

cum_scores_eligible <- cum_scores_eligible %>% 
    filter(to_keep) %>%
    select(-to_keep)

inc_scores_eligible <- inc_scores_eligible %>%
  filter(to_keep) %>%
  select(-to_keep)

## calculate WIS add other useful fields
cum_scores_calc <- cum_scores_eligible %>%
    mutate(wis = (.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+
            .3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90+.5*interval_100)/12)  %>% 
    #select(-starts_with("interval")) %>%
    left_join(hub_locations, by=c("unit" = "fips"))

inc_scores_calc <- inc_scores_eligible %>%
    mutate(wis = (.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+
            .3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90+.5*interval_100)/12)  %>% 
    #select(-starts_with("interval")) %>%
    left_join(hub_locations, by=c("unit" = "fips"))


write_csv(cum_scores_calc, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-cum-scores.csv"))
write_csv(inc_scores_calc, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-inc-scores.csv"))
