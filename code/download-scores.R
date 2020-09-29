## download scores from Zoltar
library(zoltr)  ##devtools::install_github("reichlab/zoltr")
library(tidyverse)

### Dates to extract from Zoltar
the_timezeros_cum <- seq(from = as.Date("2020-04-28"), to = as.Date("2020-08-03"), by="days")
the_timezeros_inc <- seq(as.Date("2020-05-13"), to = as.Date("2020-08-03"), by="days")

## models to pull scores for
the_models <- c("CovidAnalytics-DELPHI" ,"COVIDhub-ensemble" , "CU-select", "COVIDhub-baseline",  
    "LANL-GrowthRate" ,  "MOBS-GLEAM_COVID", "UCLA-SuEIR",  "YYG-ParamSearch",
    "JHU_IDD-CovidSP", "UMass-MechBayes", "UT-Mobility", "IHME-CurveFit", 
    "IowaStateLW-STEM", "PSI-DRAFT", "USC-SI_kJalpha",  "USACE-ERDC_SEIR", 
    "GT-DeepCOVID",  "UA-EpiCovDA") 

## connect and pull scores
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

the_scores <- c("abs_error", "interval_2","interval_5","interval_10","interval_20","interval_30","interval_40",
    "interval_50","interval_60", "interval_70","interval_80","interval_90","interval_100")

the_targets_cum <- paste(1:20, "wk ahead cum death")
the_targets_inc <- paste(1:20, "wk ahead inc death")

#Cumulative dataset 
cum_scores <- do_zoltar_query(zoltar_connection, 
    project_url =  project_url,
    is_forecast_query = FALSE,
    models = the_models, 
    targets = the_targets_cum,
    timezeros = the_timezeros_cum, 
    scores = the_scores) 

inc_scores <- do_zoltar_query(zoltar_connection, 
    project_url =  project_url,
    is_forecast_query = FALSE,
    models = the_models, 
    targets = the_targets_inc,
    timezeros = the_timezeros_inc, 
    scores = the_scores) 

write_csv(cum_scores, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-cum-scores.csv"))
write_csv(inc_scores, path = paste0("paper-inputs/", format(Sys.Date(), "%Y%m%d"), "-inc-scores.csv"))
