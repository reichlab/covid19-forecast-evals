# covid19-forecast-evals
Code and data for forecast evaluation research papers


## analysis pipeline

 - `paper-inputs/model-eligibility.csv` is created by `code/determine-model-eligibility.R` and stores for each model and forecast week, how many locations a given model has a valid/complete forecast.
 - `paper-inputs/anomaly-reporting-dates.csv` is created by `code/testing-states-for-date-change.Rmd` and stores info on which locations/weeks have unscorable forecasts based on backfilled truth data.
 - `YYYYMMDD-stan-fit-scores-negbin.rds` is a file containing a Stan model object fit in `code/stan-fit-abs-error.R`.
 - `YYYYMMDD-cum-scores.csv` and `YYYYMMDD-inc-scores.csv` are two files containing scores from Zoltar downloaded on the date indicated by the `download-scores.R` script.
 