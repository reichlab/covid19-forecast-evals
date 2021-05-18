#WIS components for presentation

wis_plot <- read_csv("paper-inputs/inc-scores.csv") %>% 
  filter(model %in% c("COVIDhub-baseline", "UT-Mobility",
                      "UMass-MechBayes","RobertWalraven-ESG", "CMU-TimeSeries")) %>%
  mutate(n_forecasts = n()) %>%
  group_by(model) %>%
  summarise(sharpness = mean(sharpness, na.rm = T),
            overprediction = mean(overprediction, na.rm = T),
            underprediction = mean(underprediction, na.rm = T))


wis_only <- read_csv("paper-inputs/inc-scores.csv") %>% 
  filter(model %in% c("COVIDhub-baseline", "UT-Mobility",
                      "UMass-MechBayes","RobertWalraven-ESG", "CMU-TimeSeries")) %>%
  mutate(n_forecasts = n()) %>%
  group_by(model) %>%
  summarise(wis= mean(wis, na.rm = T)) %>%
  arrange(desc(wis)) %>% pull(model)


wis_wide <- pivot_longer(wis_plot,
                        cols = c("overprediction", "sharpness", "underprediction"),
                        names_to = "score_name") %>%
  mutate(model = fct_relevel(model, wis_only))
  
  component_plot <- ggplot(wis_wide, aes(fill=score_name, y=value, x=model)) + 
    geom_bar(position="stack", stat="identity", width = .75) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.title = element_blank(),
          axis.title.x =  element_blank()) +
    labs(y = "WIS components")

  pdf(file = "figures/component_plot.pdf", width=6, height=4)
  component_plot 
  dev.off()
