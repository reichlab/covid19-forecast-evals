library(covidHubUtils)
library(tidyverse)
data("hub_locations")

theme_set(theme_bw())


select_models <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(model %in% c("COVIDhub-baseline", "COVIDhub-ensemble", "YYG-ParamSearch")) %>%
  filter(target == "1 wk ahead inc death") %>%
  group_by(model, target_end_date_1wk_ahead, unit) %>% 
  summarise(sum_truth = sum(truth), 
            mean_wis = mean(wis, na.rm=TRUE)) %>% 
  arrange(model, target_end_date_1wk_ahead, unit) %>%
  ungroup() %>% group_by(model, unit) %>%
  mutate(diff_truth = sum_truth - lag(sum_truth, order_by = target_end_date_1wk_ahead),
         abs_truth = abs(diff_truth)) %>% ungroup() 

#Replace missing truth data from 6/13 for ensemble
select_practice_fill <- select_models %>% 
  filter(target_end_date_1wk_ahead != as.Date("2020-05-23")) %>% 
  group_by(target_end_date_1wk_ahead, unit) %>%
  fill(sum_truth, diff_truth, abs_truth, .direction = "downup") %>% ungroup()

#Absolute Difference in truth by week
pdf('figures/wis-weekly_change.pdf')
for(i in 1:8) {
  state_wis <- ggplot(select_practice_fill, aes(x=abs_truth, y=mean_wis, group = model, color = model)) +
    geom_point() + 
    geom_smooth(se = FALSE) +
    theme_bw()  +
    xlab("Difference in observed incidence between 2 weeks (t and t+1)") + ylab("Mean WIS for week t+1")  + ggtitle("Average WIS by change in weekly observed incidence deaths. 1 week ahead target ")+
    ggforce::facet_wrap_paginate(~unit, ncol = 2, nrow = 4,  scales = "free", page = i) 
  print(state_wis)
}
dev.off()
