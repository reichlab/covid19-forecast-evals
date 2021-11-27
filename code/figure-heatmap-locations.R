#library(zoltr)  ## devtools::install_github("reichlab/zoltr")
library(covidHubUtils) ## devtools::install_github("reichlab/covidHubUtils")
library(tidyverse)
library(grid) 

source("code/load-global-analysis-dates.R")


for_loc_figure <- read.csv("paper-inputs/heatmap_data.csv") %>%
  mutate(sat_fcast_week = as.Date(sat_fcast_week),
       model = fct_reorder(model, n_weeks_submit_forecast, max),
       model_numeric = as.numeric(model))  %>%
  distinct()

#calculate number of submissions each week (compare to fig 1c)
n_loc_weekly <- for_loc_figure %>%
  group_by(sat_fcast_week) %>% summarise(n_models = n())

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

model_change_dates <- as.data.frame(cbind(
  model = c("JHU_IDD-CovidSP","LANL-GrowthRate",  "UA-EpiCovDA", "UMich-RidgeTfReg", "RobertWalraven-ESG", "IowaStateLW-STEM","IHME-CurveFit", "COVIDhub-ensemble"),
  sat_fcast_week = c("2020-12-14","2020-10-28","2020-07-05", "2020-11-30","2021-03-15" ,"2020-07-27","2020-06-24", "2020-07-28"),
  n_loc = c("25","25","25","25","25","25", "25"),
  n_quant = c("23","23","23","23","23","23","23"))) %>%
  mutate(sat_fcast_week = as.Date(sat_fcast_week),
         model = as.factor(model),
         change_date = TRUE)

plot_date <- as.Date("2021-11-16") #as.Date("2021-10-16") #previously as.Date("2021-05-08")
#Plot of locations each model submitted to each week
sf1 <- ggplot(for_loc_figure, aes(y=model, x=sat_fcast_week, fill= (n_loc < 25 | n_quant < 23))) +  ## something about `| n_quant < 23` was here ??
  geom_raster(hjust=0) +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m-%d", 
#               date_breaks = "2 weeks", 
               breaks=seq.Date(as.Date("2020-04-25"), last_timezero + 3, by = "2 weeks"),
               expand=expansion(mult=c(0.01, 0.01))) +
  annotation_custom(
    grob = textGrob(label="Model incl. in:", hjust=0, gp=gpar(col="Black", fontsize=10, fontface="bold")),
    ymin=16, ymax=16,
    xmin=plot_date-3, xmax=plot_date-3
  ) +
  annotation_custom(
    grob = textGrob(label="Main analysis", hjust=0, gp=gpar(col="#4B0092", fontsize=10)),
    ymin=15, ymax=15,
    xmin=plot_date, xmax=plot_date
  ) +
  annotation_custom(
    grob = textGrob(label="Supplement", hjust=0, gp=gpar(col="#E66100", fontsize=10)),
    ymin=14, ymax=14,
    xmin=plot_date, xmax=plot_date
  ) +
  annotation_custom(
    grob = textGrob(label="Neither",  hjust=0, gp=gpar(col="Black", fontsize=10)),
    ymin=13, ymax=13,
    xmin=plot_date, xmax=plot_date
  ) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(name = " ", values = c( "turquoise3","lightgrey"), labels = c("Eligible","Ineligible" )) +
  xlab("Saturday of Forecast Submission Week") + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 8, 
                                   color = ifelse(levels(for_loc_figure$model) %in% scored_models_overall,"#4B0092",
                                                  ifelse(levels(for_loc_figure$model) %in% scored_models_phase,"#E66100", "black"))), 
        title = element_text(size = 11)) +
  guides(size = "none", color = "none", alpha = "none") +
  scale_y_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR")) +
  geom_vline(xintercept  = range_fcast_dates, linetype = 2) + ## subtracting 7 so they are end of submission weeks, not target end dates
  geom_point(data= model_change_dates, aes(y = model, x = sat_fcast_week), shape=18, size=3, show.legend = FALSE) 

pdf(file = "figures/inc-loc-heatmap.pdf", width=11, height=9)
print(sf1, col = 2)
dev.off()

jpeg(file ="figures/inc-loc-heatmap.jpg", width=11, height=13, units="in", res=300)
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







