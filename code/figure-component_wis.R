#WIS components for presentation

wis_plot <- read_csv("paper-inputs/inc-scores.csv") %>% 
  filter(include_overall == "TRUE") %>%
  filter(target %in% c(paste(1:4, "wk ahead inc death"))) %>%
  filter(location %in% (hub_locations %>%
                           filter(geo_type == "state") %>% pull(fips))) %>%
  mutate(n_forecasts = n()) %>%
  group_by(model) %>%
  summarise(sharpness = mean(sharpness, na.rm = T),
            overprediction = mean(overprediction, na.rm = T),
            underprediction = mean(underprediction, na.rm = T))


model_levels <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  arrange(relative_wis) %>%
  pull(model)


wis_wide <- pivot_longer(wis_plot,
                        cols = c("overprediction", "sharpness", "underprediction"),
                        names_to = "score_name") %>%
  mutate(model = fct_relevel(model, model_levels))
  
component_plot <- ggplot(wis_wide, aes(fill=score_name, y=value, x=model)) + 
    geom_bar(position="stack", stat="identity", width = .75) +
   labs(y = "Avg unweighted WIS") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.title.x =  element_blank(),
          plot.margin = margin(10, 10, 20, 20)) +
  scale_fill_discrete(name = "WIS Components", labels = c("Overprediction","Sharpness", "Underprediction"))
  

pdf(file = "figures/component_plot.pdf", width=8, height=6)
component_plot 
dev.off()

jpeg(file = "figures/component_plot.jpg", width=8, height=6, units="in", res=300)
print(component_plot)
dev.off()