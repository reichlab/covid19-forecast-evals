library(lubridate)
library(tidyverse)
library(ggrepel) # need install from github I think.
library(covidHubUtils)
library(directlabels)

theme_set(theme_bw())
data("hub_locations")

## load in inc scores
inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name %in% datasets::state.name)

inc_calibration <-  read_csv("paper-inputs/inc-calibration.csv") %>%
  left_join(hub_locations, by=c("unit" = "fips")) %>%
  filter(location_name %in% datasets::state.name)

inc_scores_merge <- inc_scores %>%
  left_join(inc_calibration) %>%
  pivot_wider(names_from = "quantile", values_from = "value") %>%
  mutate(calib_95 = ifelse(truth >= `0.025` & truth <= `0.975`, 1, 0),
         calib_50 = ifelse(truth >= `0.25` & truth <= `0.75`, 1, 0))

## compute nice table
calibration_scores_inc <- inc_scores_merge %>%
  group_by(model, target) %>%
  summarise(percent_calib50 = round(sum(calib_50)/ n(),2),
            percent_calib95 = round(sum(calib_95) / n(),2)) %>% 
  mutate(target = fct_relevel(target, 
                              "1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death",
                               "5 wk ahead inc death",  "6 wk ahead inc death",  "7 wk ahead inc death",  "8 wk ahead inc death",
                               "9 wk ahead inc death",  "10 wk ahead inc death",  "11 wk ahead inc death",  "12 wk ahead inc death",
                              "13 wk ahead inc death",  "14 wk ahead inc death",  "15 wk ahead inc death",  "16 wk ahead inc death",
                              "17 wk ahead inc death",  "18 wk ahead inc death"),
    horizon = str_split(target, " ", simplify = TRUE),
    horizon = as.numeric(horizon[,1])) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(label = if_else(horizon == 4, model, NA_character_))

calibration_scores_inc_table <- calibration_scores_inc %>%
    pivot_wider(names_from = target, values_from = c(percent_calib50, percent_calib95)) 

# ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib50, x=model)) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_hline(yintercept=.5, linetype=2) +
#   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# ggplot(calibration_scores_inc, aes(fill=target, y=percent_calib95, x=model)) + 
#  geom_bar(position="dodge", stat="identity") + 
#  geom_hline(yintercept=.95, linetype=2) +
#  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

calib95 <- 
  ggplot(filter(calibration_scores_inc, horizon < 5), aes(x=horizon, y=percent_calib95, color=model, group=model)) + 
  geom_line() + geom_point() + 
  #geom_label(aes(label=model)) +
  #geom_dl(aes(label=model), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
  geom_hline(yintercept=.95, linetype=5) +
  scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, .95, 1)) +
  scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
  guides(color=FALSE) +
  ggtitle("B: 95% prediction interval coverage rates, by model")+
  geom_text_repel(aes(label = label),
    nudge_x = 0.5,
    hjust = 0,
    size=2.5,
    box.padding = 0.1,
    direction = "y",
    segment.linetype = 2, 
    segment.alpha = 0.5, 
    # min.segment.length = Inf,
    na.rm = TRUE)

calib50 <- 
  ggplot(filter(calibration_scores_inc, horizon < 5), aes(x=horizon, y=percent_calib50, color=model, group=model)) + 
  geom_line() + geom_point() + 
  #geom_label(aes(label=model)) +
  geom_hline(yintercept=.5, linetype=5) +
  #geom_dl(aes(label=model), method = list(dl.trans(x = x), "left.polygons", cex = 0.8)) +
  scale_y_continuous(name = "Empirical prediction interval coverage", limits = c(0,1), breaks=c(0, .25, .5, .75, 1)) +
  scale_x_continuous(name = "Forecast horizon (weeks)", breaks=1:4, limits=c(1, 5)) +
  guides(color=FALSE) +
  ggtitle("A: 50% prediction interval coverage rates, by model") +
  geom_text_repel(aes(label = label),
    nudge_x = 0.5,
    hjust = 0,
    size=2.5,
    box.padding = 0.1,
    direction = "y",
    segment.linetype = 2, 
    segment.alpha = 0.5, 
    # min.segment.length = Inf,
    na.rm = TRUE)


pdf(file = "figures/pi-coverage.pdf", width=8, height=6)
gridExtra::grid.arrange(calib50, calib95)
dev.off()

jpeg(file = "figures/pi-coverage.jpg", width=8, height=6, units="in", res=200)
gridExtra::grid.arrange(calib50, calib95)
dev.off()


#Calibration table (table 2)
calib_table <- inc_scores_merge %>%
  filter(target %in% c("1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death")) %>% 
  group_by(model) %>%
  summarise(percent_calib50 = round(sum(calib_50)/ n(),2),
            percent_calib95 = round(sum(calib_95) / n(),2))

count_forecasts <- inc_scores %>%
  filter(target %in% c("1 wk ahead inc death",  "2 wk ahead inc death",  "3 wk ahead inc death",  "4 wk ahead inc death")) %>%
  group_by(model) %>%
  summarise(n_forecasts = n())

calib_table <- count_forecasts %>%
  inner_join(calib_table)










