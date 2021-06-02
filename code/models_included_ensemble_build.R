library(readr)
library(tidyverse)

source("code/load-global-analysis-dates.R")

files <- list.files(path = "../covid19-forecast-hub/ensemble-metadata", pattern = "*-inc_death-model-eligibility.csv", full.names = T) #load inc eligiblity files 

filesDATE <- as.double(sub('..',"", gsub('[^0-9]', '', files))) #keep dates from filenames

files_clean <- files[filesDATE >= gsub('[^0-9]',"", first_timezero) & filesDATE <= gsub('[^0-9]',"", last_timezero)]  #filter to only files that are within the evaluation period

dat_csv <- plyr::ldply(files_clean, read_csv) #read in csv files


#print all models 
# dat_csv %>% #outdated
#   filter(overall_eligibility == "eligible") %>% #keep only eligible rows
#   distinct(model) %>% #keep 1 row for each model
#   filter(!model %in% c("Covid19Sim-Simulator", "COVIDhub-baseline", "GT-DeepCOVID", "IHME-CurveFit", "JHU_IDD-CovidSP", 
#                        "LANL-GrowthRate", "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "UCLA-SuEIR", "UMass-MechBayes", 
#                        "USACE-ERDC_SEIR", "UT-Mobility", "YYG-ParamSearch")) %>% #remove models that are already included  %>%
#   arrange(model) %>%
#   pull(model)

#model prints 24 models, but there are 2 teams that changed their names, so in total there are 22 extra teams to add contact information for authorship
#With data update, prints 31 models, including repeat of rpi ans umich teams for a total of 29 teams. 


#update for manuscript 2021-06-01

updated_csv <- dat_csv %>%
  filter(overall_eligibility == "eligible") %>% #keep only eligible rows
  distinct(model) %>% #keep 1 row for each model
  arrange(model) %>% 
  select(Team.Name = model)

#overall, 56 models included in this list. 


#Filter out teams that were included in the preprint

already_included <- read.csv("paper-inputs/All_Leads.csv")  %>% #leads of all teams already included 
  filter(Team.Name != "")

newly_added_teams <- updated_csv %>% anti_join(already_included) 

#remove teams that changed their names and were already included
newly_added_teams <- newly_added_teams %>% 
  filter(!Team.Name %in% c("RPI-UW-Mob-Collision", "UM_CFG-RidgeTfReg", 
                           "Microsoft-DeepSTIA", "USC-SI_kJalpha_RF",
                           "Wadhwani_AI-BayesOpt"))


#There are 7 remaining teams. Two I reached out to before (JHU_CSSE and CAN-SEIR_CAN)



