library(tidyverse)

#vers <- read_csv("data-raw/model-forecast-versions-filled-all-forecast-from-May.csv")
# vers <- read_csv("paper-inputs/model-forecast-versions-from-May-as-of-7-1-2021.csv")
# vers <- read_csv("paper-inputs/model-forecast-versions-as-of-2021-06-30.csv")
vers <- read_csv("paper-inputs/model-forecast-versions-from-2021-11-29.csv")

models <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  pull(model)

forecast_versions <- vers %>% mutate(
  v1_github_timestamp = as.POSIXct(v1_github_timestamp, format="%m/%d/%y-%X"),
  v2_github_timestamp = as.POSIXct(v2_github_timestamp, format="%m/%d/%y-%X"),
  v3_github_timestamp = as.POSIXct(v3_github_timestamp, format="%m/%d/%y-%X"),
  v2_diff_hrs = difftime(v2_github_timestamp, v1_github_timestamp, units="hours"), 
  v3_diff_hrs = difftime(v3_github_timestamp, v2_github_timestamp, units="hours"),
  last_submission_timestamp = pmax(v1_github_timestamp, v2_github_timestamp, v3_github_timestamp, na.rm=TRUE))

model_eligibility <- read_csv("paper-inputs/model-eligibility-inc.csv") %>%
  left_join(forecast_versions, by=c("model" = "model", "forecast_date" = "timezero")) %>%
  filter(model %in% models) %>%
  mutate(deadline = as.POSIXct(paste(target_end_date_1wk_ahead-5, "18:00:00")),
    deadline_diff = difftime(last_submission_timestamp, deadline, units="hours"))

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

## count number of individual submissions whose evaluated forecast was 
## submitted more than 24 hours after the deadline
model_eligibility %>%
  filter(deadline_diff>24) %>% #print(n=Inf)
  nrow()

model_eligibility %>%
  filter(!(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble")), deadline_diff>24) %>% #print(n=Inf)
  group_by(model) %>% 
  summarize(nobs=n()) %>% arrange(-nobs)

## check that all files in model_eligibility are in inc_scores
inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  group_by(model, forecast_date)%>%
  summarize(nobs=n()) %>%
  full_join(model_eligibility) %>%
  group_by(model, forecast_date) %>%
  summarize(n())

## GT-DeepCovid has 1 repeated row in the forecast versions file, with forecast_date = 2020-11-09

write_csv(forecast_versions, "paper-inputs/forecast-versions.csv")

