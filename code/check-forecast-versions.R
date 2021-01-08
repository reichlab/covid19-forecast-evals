library(tidyverse)

#vers <- read_csv("data-raw/model-forecast-versions-filled-all-forecast-from-May.csv")
vers <- read_csv("paper-inputs/model-forecast-versions-from-May-as-of-7-1-2021.csv")

models <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  pull(model)

forecast_versions <- vers %>% mutate(
  v1_github_timestamp = as.POSIXct(v1_github_timestamp, format="%m/%d/%y-%X"),
  v2_github_timestamp = as.POSIXct(v2_github_timestamp, format="%m/%d/%y-%X"),
  v3_github_timestamp = as.POSIXct(v3_github_timestamp, format="%m/%d/%y-%X"),
  v2_diff_hrs = difftime(v2_github_timestamp, v1_github_timestamp, units="hours"), 
  v3_diff_hrs = difftime(v3_github_timestamp, v2_github_timestamp, units="hours"))

model_eligibility <- read_csv("paper-inputs/model-eligibility-inc.csv") %>%
  left_join(forecast_versions, by=c("model" = "model", "forecast_date" = "timezero")) %>%
  filter(model %in% models) %>%
  mutate(deadline = as.POSIXct(paste(target_end_date_1wk_ahead-5, "18:00:00")),
    deadline_diff = difftime(v1_github_timestamp, deadline, units="hours"))

## manual check that every v1 in zoltar has no additional versions
v1_idx <- model_eligibility$in_zoltar=="v1"
# below should return TRUE
all(is.na(model_eligibility$v2_github_timestamp[v1_idx]) & is.na(model_eligibility$v3_github_timestamp[v1_idx]))

## manual check that every v2 in zoltar has no additional versions
v2_idx <- model_eligibility$in_zoltar=="v2"
# below should return TRUE
all(is.na(model_eligibility$v3_github_timestamp[v2_idx]))


## submissions that were resubmitted after 24 hours after original submission
sum(model_eligibility$v2_diff_hrs>24 | (model_eligibility$v2_diff_hrs+model_eligibility$v3_diff_hrs)>24, na.rm=TRUE)

nrow(model_eligibility)

## count number of individual submissions more than 24 hours after the deadline
model_eligibility %>%
  filter(model != "COVIDhub-ensemble", deadline_diff>24) %>% #print(n=Inf)
  nrow()
  

write_csv(forecast_versions, "paper-inputs/forecast-versions.csv")

