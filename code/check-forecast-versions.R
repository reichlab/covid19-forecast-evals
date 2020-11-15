library(tidyverse)

vers <- read_csv("data-raw/model-forecast-versions-filled-all-forecast-from-May.csv")

forecast_versions <- vers %>% mutate(
  v1_github_timestamp = as.POSIXct(v1_github_timestamp, format="%m/%d/%y-%X"),
  v2_github_timestamp = as.POSIXct(v2_github_timestamp, format="%m/%d/%y-%X"),
  v3_github_timestamp = as.POSIXct(v3_github_timestamp, format="%m/%d/%y-%X"),
  v2_diff_hrs = difftime(v2_github_timestamp, v1_github_timestamp, units="hours"), 
  v3_diff_hrs = difftime(v3_github_timestamp, v2_github_timestamp, units="hours"))

model_eligibility <- read_csv("paper-inputs/model-eligibility-inc.csv") %>%
  left_join(forecast_versions)

## manual check that every v1 in zoltar has no additional versions
v1_idx <- model_eligibility$in_zoltar=="v1"
# below should return TRUE
all(is.na(model_eligibility$v2_github_timestamp[v1_idx]) & is.na(model_eligibility$v3_github_timestamp[v1_idx]))

## manual check that every v2 in zoltar has no additional versions
v2_idx <- model_eligibility$in_zoltar=="v2"
# below should return TRUE
all(is.na(model_eligibility$v3_github_timestamp[v2_idx]))


