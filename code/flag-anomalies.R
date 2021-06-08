## add columns to inc_scores for data anomalies
library(tidyverse)
library(covidHubUtils)

## TODO: filter revisions file so we only consider revisions made on or prior to the ground-truth retrieval date

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  mutate(forecast_week_end_date = as.Date(calc_forecast_week_end_date(forecast_date)))

## read anomaly csvs
data_outliers <- read_csv("../covid19-forecast-hub/data-anomalies/outliers-inc-deaths.csv") %>%
  mutate(outlier_data = TRUE, days_since_first_obs=issue_date-date) %>%
  filter(num_reviewers_marked_oulier == 2)

outliers_marked_on_first_obs <- filter(data_outliers, days_since_first_obs < 8)  

data_revs <- read_csv("../covid19-forecast-hub/data-anomalies/revisions-inc-death.csv") %>%
  ## removes observations with < 20 absolute difference and < 50% relative difference
  filter(abs(real_diff) > 20, abs(relative_diff) >= 0.5) %>%
  ## adds information about whether an observation was initially categorized as an outlier
  left_join(select(outliers_marked_on_first_obs, location, date, outlier_data)) %>%
  mutate(outlier_on_first_obs = ifelse(is.na(outlier_data), FALSE, TRUE)) %>%
  distinct(location, location_name, date, outlier_on_first_obs)

## manually change two OH observations
## there are two dates within the major period of OH revisions that are flagged as outliers on 
## the first observation. these should still be excluded, b/c they are in the weird zone for OH.
OH_idx <- which(data_revs$location_name=="Ohio" & data_revs$date %in% c(as.Date("2021-02-13"), as.Date("2021-03-20")))
data_revs$outlier_on_first_obs[OH_idx] <- FALSE

## to identify data revisions to omit: 
##   was a location/forecast_date identified as an outlier when first released? 
##   if yes, then it can be included no matter how big its revision.
##   Otherwise, forecasts with location/forecast_week_end_date equal to the location/date of large revisions, as defined above, are removed.
data_revs$revision_to_omit <- !data_revs$outlier_on_first_obs
inc_scores_with_revs <- inc_scores %>%
  left_join(select(data_revs, -location_name), 
            by=c("location"="location", "forecast_week_end_date"="date")) %>%
  mutate(revision_to_omit = ifelse(is.na(revision_to_omit)|(!revision_to_omit), FALSE, TRUE))


## for outliers: 
##   filter data_outliers to contain only rows for observations that are outlying as of the date truth was obtained
##   remove rows in inc_scores for which (target_end_date and location) match a row in filtered data_outliers
inc_scores %>%
  left_join(data_revs)


## get date on which truth data was obtained

## filter data_outliers to contain only rows for observations that are outlying as of the date truth was obtained

