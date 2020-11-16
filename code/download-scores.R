## download scores from Zoltar
library(zoltr)  ##devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")

data(hub_locations)


model_eligibility <- read_csv("paper-inputs/model-eligibility.csv")

## locations/dates with reporting anomalies
dates_with_issues <- read_csv("paper-inputs/anomaly-reporting-dates.csv", col_types = "nDccDnnnc") %>%
    filter(to_remove==1)

### Dates to extract from Zoltar
the_timezeros_cum <- model_eligibility %>% filter(target_group=="cum") %>% pull(timezero) %>% unique()
the_timezeros_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(timezero) %>% unique()

## models to pull scores for
the_models_cum <- model_eligibility %>% filter(target_group=="cum") %>% pull(model) %>% unique()
the_models_inc <- model_eligibility %>% filter(target_group=="inc") %>% pull(model) %>% unique()

## the targets to pull scores for
the_targets_cum <- paste(1:20, "wk ahead cum death")
the_targets_inc <- paste(1:20, "wk ahead inc death")

## connect and pull scores
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

the_scores <- c("abs_error", "interval_2","interval_5","interval_10","interval_20","interval_30","interval_40",
    "interval_50","interval_60", "interval_70","interval_80","interval_90","interval_100")

cum_scores <- do_zoltar_query(zoltar_connection, 
    project_url =  project_url,
    is_forecast_query = FALSE,
    models = the_models_cum, 
    targets = the_targets_cum,
    timezeros = the_timezeros_cum, 
    scores = the_scores) 

inc_scores <- do_zoltar_query(zoltar_connection, 
    project_url =  project_url,
    is_forecast_query = FALSE,
    models = the_models_inc, 
    targets = the_targets_inc,
    timezeros = the_timezeros_inc, 
    scores = the_scores) 

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


write_csv(cum_scores_calc, file = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-cum-scores.csv"))
write_csv(inc_scores_calc, file = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-inc-scores.csv"))
