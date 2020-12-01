library(readr)
library(tidyverse)

files <- list.files(path = "../covid19-forecast-hub/ensemble-metadata", pattern = "*-inc_death-model-eligibility.csv", full.names = T) #load inc eligiblity files 

filesDATE <- as.double(sub('..',"", gsub('[^0-9]', '', files))) #keep dates from filenames

files_clean <- files[filesDATE >= 20200608 & filesDATE <= 20200831]  #filter to only files that are within the evaluation period

dat_csv <- plyr::ldply(files_clean, read_csv) #read in csv files


#print all models 
dat_csv %>%
  filter(overall_eligibility == "eligible") %>% #keep only eligible rows
  distinct(model) %>% #keep 1 row for each model
  filter(!model %in% c("Covid19Sim-Simulator", "COVIDhub-baseline", "GT-DeepCOVID", "IHME-CurveFit", "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "UCLA-SuEIR", "UMass-MechBayes", "USACE-ERDC_SEIR", "UT-Mobility", "YYG-ParamSearch")) %>% #remove models that are already included  %>%
  arrange(model) %>%
  pull(model)


#model prints 24 models, but there are 2 teams that changed their names, so in total there are 22 extra teams to add contact information for authorship