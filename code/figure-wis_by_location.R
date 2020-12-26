library(lubridate)
library(tidyverse)
library(covidHubUtils)

theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% (hub_locations %>% filter(geo_type == "state") %>% pull(location_name))) %>%
  filter(location_name != "American Samoa") %>%
  mutate(wis = (.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+.3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90+.5*interval_100)/12)

average_by_loc <- inc_scores %>%
  group_by(model, location_name) %>%  #aggregate by week of submission
  summarise(avg_wis = round(mean(wis, na.rm = T),1),
            sum_truth = sum(truth)) %>% 
  group_by(location_name) %>%     
  mutate_at(vars(matches("avg_wis")), funs(relative_wis = (. / .[model=="COVIDhub-baseline"]))) %>% 
  ungroup() %>% 
  mutate(log_relative_wis = ifelse(relative_wis == 0, 0, log2(relative_wis)),
    log_relative_wis = ifelse(log_relative_wis > 3, 3, log_relative_wis)) ## remove visual outliers

average_by_loc$model<- reorder(average_by_loc$model, -average_by_loc$avg_wis) #sort models by WIS for plot
average_by_loc$location_name <- reorder(average_by_loc$location_name, average_by_loc$sum_truth)

## summaries by model
average_by_loc %>%
  group_by(model) %>%
  summarize(total_n = n(), n_better_than_baseline = sum(relative_wis<1), pct_better = n_better_than_baseline/total_n)%>%
  arrange(pct_better)

## largest relative for ensemble
average_by_loc %>%
  filter(model=="COVIDhub-ensemble") %>%
  arrange(relative_wis) %>%
  print(n=Inf)


fig_wis_loc <- ggplot(average_by_loc, aes(x=model, y=location_name,fill= log_relative_wis)) +
  geom_tile() +
  geom_text(aes(label=round(avg_wis)), size = 3) +
  scale_fill_gradient2(low = "navy", high = "red", midpoint = 0, na.value = "grey50", name = "Relative WIS", breaks = c(-3,-2,-1,0,1,2,3), labels =c(0.125,0.25, 0.5, 1, 2, 4, 8))+ 
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.title.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      title = element_text(size = 9),
    ) 


pdf(file = "figures/fig-wis-location.pdf",width=8, height=8)
print(fig_wis_loc)
dev.off()

jpeg(file = "figures/fig-wis-location.jpg", width=8, height=8, units="in", res=300)
print(fig_wis_loc)
dev.off()



  
  
