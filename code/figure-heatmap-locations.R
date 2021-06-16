#library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)

source("code/load-global-analysis-dates.R")


for_loc_figure <- read.csv("paper-inputs/heatmap_data.csv") %>%
  mutate(sat_fcast_week = as.Date(sat_fcast_week),
       model = fct_reorder(model, n_weeks_submit_forecast, max),
       model_numeric = as.numeric(model)) 

scored_models_overall <- read_csv("paper-inputs/inc-scores.csv") %>%
                        filter(include_overall == "TRUE") %>%
                        group_by(model) %>%
                        summarise(n_forecasts = n()) %>%
                        arrange(desc(n_forecasts)) %>%
                        pull(model)

scored_models_phase <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_phases == "TRUE") %>%
  group_by(model) %>%
  summarise(n_forecasts = n()) %>%
  arrange(desc(n_forecasts)) %>%
  pull(model)

title_cols <-c("Evaluated in\nMain Analysis" = "#4B0092", "Evaluated in\n supplemental Analysis" = "#E66100","Not Evaluated" = "black")


#Plot of locations each model submitted to each week
sf1 <- ggplot(for_loc_figure, aes(y=model, x=sat_fcast_week, fill= (n_loc < 25 | n_quant < 23))) + 
  geom_tile() +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 weeks") +
  annotate(geom="text", x= as.Date("2021-05-4"), y= c(40), label="Main Analysis",
            color="#4B0092", hjust = -0.6, size = 3) +
  annotate(geom="text", x= as.Date("2021-05-4"), y=c(39), label="Supplement",
           color="#E66100", hjust = -0.6, size = 3) +
  annotate(geom="text", x= as.Date("2021-05-4"), y=c(38), label="Not Evaluated",
           color="Black", hjust = -0.6, size = 3) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(name = " ", values = c( "turquoise3","lightgrey"), labels = c("Eligible","Ineligible" )) +
  xlab("Saturday of Forecast Submission Week") + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 8, 
                                   color = ifelse(levels(for_loc_figure$model) %in% scored_models_overall,"#4B0092",
                                                  ifelse(levels(for_loc_figure$model) %in% scored_models_phase,"#E66100", "black"))), 
        title = element_text(size = 9)) +
  guides(size = "none", color = "none", alpha = "none") +
  scale_y_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR")) +
  geom_vline(xintercept  = range_fcast_dates, linetype = 2) 


pdf(file = "figures/inc-loc-heatmap.pdf",width=11, height=7)
print(sf1)
dev.off()

jpeg(file = "figures/inc-loc-heatmap.jpg", width=11, height=7, units="in", res=300)
print(sf1)
dev.off()


#Count number of models
length(unique(for_loc_figure$model))

#Count number of submissions for scored models
submission_count <- for_loc_figure %>% filter(model %in% c("IHME-CurveFit", "YYG-ParamSearch", "PSI-DRAFT", "RobertWalraven-ESG", 
 "USACE-ERDC_SEIR", "CU-select", "NotreDame-mobility", 
"UA-EpiCovDA", "UCLA-SuEIR", "Covid19Sim-Simulator", 
"COVIDhub-ensemble", "LANL-GrowthRate", "OliverWyman-Navigator", 
"UT-Mobility", "COVIDhub-baseline", "GT-DeepCOVID", "JHU_IDD-CovidSP", 
"MOBS-GLEAM_COVID", "UMass-MechBayes"))

#Count number of forecasts
sum(submission_count$n_loc) *4







