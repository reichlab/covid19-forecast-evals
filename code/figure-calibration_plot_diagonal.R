#Script to create calibration plots

library(covidHubUtils)
library(tidyverse)

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(include_overall) %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
  mutate(target = fct_relevel(target, 
                              "1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death",
                              "5 wk ahead inc death",  "6 wk ahead inc death",  "7 wk ahead inc death",  "8 wk ahead inc death",
                              "9 wk ahead inc death",  "10 wk ahead inc death",  "11 wk ahead inc death",  "12 wk ahead inc death",
                              "13 wk ahead inc death",  "14 wk ahead inc death",  "15 wk ahead inc death",  "16 wk ahead inc death",
                              "17 wk ahead inc death",  "18 wk ahead inc death")) %>%
  filter(target %in% c("1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death"))


calibration_data <- inc_scores %>% 
  group_by(model, target) %>%
  summarise(cov_10 = mean(coverage_10),
            cov_20 = mean(coverage_20),
            cov_30 = mean(coverage_30),
            cov_40 = mean(coverage_40),
            cov_50 = mean(coverage_50),
            cov_60 = mean(coverage_60),
            cov_70 = mean(coverage_70),
            cov_80 = mean(coverage_80),
            cov_90 = mean(coverage_90),
            cov_95 = mean(coverage_95),
            cov_98 = mean(coverage_98)) %>% ungroup()

calib_data_long <- calibration_data %>%
  pivot_longer(!c(model, target), names_to = "coverage_level", values_to = "emperical_cov") %>%
  mutate(expected_cov = as.integer(stringr::str_sub(coverage_level, start= -2)) / 100)

calib_avg <- calib_data_long %>%
  group_by(target, expected_cov) %>%
  summarize(emperical_cov = mean(emperical_cov)) 

calib_baseline <- calib_data_long %>%
  filter(model == "COVIDhub-baseline")

calib_ensemble <- calib_data_long %>%
  filter(model == "COVIDhub-ensemble")


calib_graph <- ggplot(calib_data_long, aes(x=expected_cov, y=emperical_cov)) + 
  geom_line(aes(group=model), color = "darkgray", alpha = 0.5)+
  geom_point(aes(group=model),color = "darkgray", alpha = 0.5, size = 2) + 
  facet_wrap(~target) + 
  geom_abline(slope = 1, intercept = 0, size = 0.8, linetype = "dashed") +
  geom_line(data = calib_avg, aes(color = "blue")) + 
  geom_point(data = calib_avg, aes(color = "blue"),size = 2) +
  geom_line(data = calib_baseline, aes(color = "green")) + 
  geom_point(data = calib_baseline, aes(color="green"), size = 2) +
  geom_line(data = calib_ensemble, aes(color="red")) +
  geom_point(data = calib_ensemble, aes(color="red"), size = 2) +
  xlab("Expected Coverage") + ylab("Observed Coverage") + 
  scale_color_identity(name = NULL, 
                       breaks = c("blue", "green", "red"), 
                       labels = c("Average calibration of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
                       guide = "legend") +
  guides(group = FALSE) +
  scale_x_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) +
  scale_y_continuous(labels = seq(0,1,0.2), breaks = seq(0,1,0.2)) + 
  theme(legend.position = c(0.0, 0.29), legend.justification = c(0,-.1)) +
  theme(legend.background=element_blank())
  
  
  pdf(file = "figures/calibration_plot_diagonal.pdf",width=6, height=6)
  print(calib_graph)
  dev.off()
  
  jpeg(file = "figures/calibration_plot_diagonal.jpg", width=6, height=6, units="in", res=300)
  print(calib_graph)
  dev.off()