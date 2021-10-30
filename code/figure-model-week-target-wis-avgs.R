library(tidyverse)
library(ggrepel)
library(tidytext)

source("code/load-global-analysis-dates.R")

model_levels <- read_csv("paper-inputs/table-overall-performance.csv") %>%
  arrange(relative_wis) 

model_levels_phases <- read_csv("paper-inputs/table-phase-performance.csv") %>%
  group_by(seasonal_phase) %>% arrange(seasonal_phase, relative_wis) %>% 
  select(model, seasonal_phase,relative_wis)

theme_set(theme_bw())

# simple summary calculations for paper
# tmp <- read_csv("paper-inputs/inc-scores.csv") 
# ## number of unique submissions
# tmp %>%
#   group_by(model, forecast_date) %>%
#   summarize(submitted=TRUE)
# ## number of evaluation opportunities
# nrow(tmp)

## to load in sensitivity analysis version of files
# inc_scores <- inc_scores_with_both %>%
#   filter(anomaly_omit == FALSE) %>%
inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(target %in% paste(c(1,4), "wk ahead inc death"),
        target_end_date_1wk_ahead >= first_target_end_date,
        target_end_date_1wk_ahead <= last_target_end_date) 

locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")

inc_scores_overall <- inc_scores %>% filter(include_overall == "TRUE")


## by model
avg_wis_by_model <- inc_scores_overall %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(model) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE))


## by model and target
avg_wis_by_model_target <- inc_scores_overall %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(model, target) %>%
    summarize(nweeks = length(unique(target_end_date)), median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE)) %>%
  left_join(model_levels) %>%
  mutate(model = fct_reorder(model, relative_wis))


## by model and week

expected_locs <- inc_scores_overall %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(target_end_date_1wk_ahead) %>%
    summarize(nlocs_expected = n_distinct(location_name))

# avg_wis_by_model_target_week <- inc_scores %>%
#   filter(include_overall == "TRUE") %>%
#   filter(!(location_name %in% locs_to_exclude)) %>% 
#   group_by(model, target, seasonal_phase, target_end_date_1wk_ahead) %>%
#   summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), nlocs=n()) %>%
#   left_join(expected_locs) %>%
#   mutate(obs_exp_locs = nlocs == nlocs_expected) %>%
#   left_join(model_levels) %>%
#   mutate(model = fct_reorder(model, relative_wis)) 

## by model, target, and week
avg_wis_by_model_target_week <- inc_scores_overall %>%
    filter(!(location_name %in% locs_to_exclude)) %>% 
    group_by(model, target, target_end_date_1wk_ahead) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), nlocs=n()) %>%
    left_join(expected_locs) %>%
    mutate(obs_exp_locs = nlocs == nlocs_expected)

ensemble_1wk_avg <- avg_wis_by_model_target %>%
    filter(model=="COVIDhub-ensemble", target=="1 wk ahead inc death") %>%
    .$mean_wis

baseline_avg <- avg_wis_by_model_target %>%
    filter(model=="COVIDhub-baseline") %>%
    group_by(model, target) %>%
    summarize(mean_wis = mean(mean_wis),
              median_wis = median(mean_wis))

avg_wis_by_model_target_week <-  avg_wis_by_model_target_week  %>%
  left_join(model_levels) %>%
  mutate(model = fct_reorder(model, relative_wis)) 

p_boxplot <- ggplot(avg_wis_by_model_target_week, aes(x = reorder(model, relative_wis), y= scales::oob_squish(mean_wis, range = c(0,500)))) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ target) +
  geom_hline(data=baseline_avg,
             aes(yintercept=median_wis), linetype=2, color = "red") +
  geom_point(data = avg_wis_by_model_target, aes(y = mean_wis),  color = "blue", shape=4, size = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), 
        legend.position = "none") +
  ylab("Average WIS (axis on log scale)") + xlab(NULL) +
  scale_fill_date(name="forecast date") +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR")) 

averages  <- avg_wis_by_model_target_week %>%
  group_by(model, target) %>%
  summarise(median_wis = median(mean_wis),
            mean_wis = mean(mean_wis))


averages_median_under_mean <- averages %>%
  filter(median_wis >= mean_wis)

# p <- ggplot(avg_wis_by_model_target_week, aes(x=model, y=mean_wis)) +
#     #geom_hline(yintercept=ensemble_1wk_avg, linetype=2)+
#     geom_hline(data=baseline_avg, aes(yintercept=mean_wis), linetype=2)+
#     geom_point(aes(color=obs_exp_locs), position=position_jitter(width=.2, height=0), alpha=.8) +
#     ##geom_point(aes(color=obs_exp_locs, fill=target_end_date_1wk_ahead), shape=21, position=position_jitter(width=.2, height=0), alpha=.8) +
#     geom_point(data=avg_wis_by_model_target, shape=4, color="#DC3220", size=2, stroke=1)+
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), 
#           legend.position = c(.75, .9), 
#           legend.direction = "horizontal", plot.margin = margin(10, 10, 20, 20)) +
#     facet_wrap(.~target) + #, scales="free_y") +
#     ylab("Average WIS") + xlab(NULL) +
#     #expand_limits(y=0) +
#     scale_color_manual(name="all locations predicted", values=c("#9C3689", "#A8D695")) +
#     scale_fill_date(name="forecast date") +
#     scale_y_continuous(breaks=seq(0, 400, by=50)) +
#     scale_x_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR"))

pdf(file = "figures/model-target-week-wis-avgs.pdf", width=8, height=6)
print(p_boxplot)
dev.off()

jpeg(file = "figures/model-target-week-wis-avgs.jpg", width=8, height=6, units="in", res=200)
print(p_boxplot)
dev.off()



## supplemental figure overall boxplot

# p1 <- inc_scores %>% 
#     filter(include_overall== "TRUE") %>%
#     filter(!(location_name %in% c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia"))) %>% 
#     ggplot(aes(x=model, y=wis, color=target_end_date_1wk_ahead)) +
#     geom_boxplot() +
#     scale_y_log10() +
#     theme_bw() +
#     ylab("WIS (log scale)") + xlab(NULL) +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1,
#           color=
#             ifelse(levels(inc_scores$model) %in% models_to_highlight,
#             "red", "black"))) +
#     facet_wrap(.~target)  +
#   scale_x_discrete(labels=c("IHME-CurveFit" = "IHME-SEIR"))
# 
# 
# pdf(file = "figures/overall-wis-boxplot.pdf", width=8, height=6)
# print(p1)
# dev.off()
# 
# jpeg(file = "figures/overall-wis-boxplot.jpg", width=8, height=6, units="in", res=200)
# print(p1)
# dev.off()


#FIGURE BY PHASE 

inc_scores_phase <- inc_scores %>%
  filter(include_phases == "TRUE") %>%
  filter(!is.na(wis)) %>%
  mutate(model = recode(model, "IHME-CurveFit" = "IHME-SEIR")) %>%
  filter(!(location_name %in% locs_to_exclude)) 

avg_wis_by_model_target_week_phase <- inc_scores_phase %>%
  group_by(model, target, seasonal_phase, target_end_date_1wk_ahead) %>%
  summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), nlocs=n()) %>%
  left_join(model_levels_phases %>% mutate(model = recode(model, "IHME-CurveFit" = "IHME-SEIR"))) %>%
  mutate(model = fct_reorder(model, relative_wis)) %>% ungroup() 

## by model and target
avg_wis_by_model_target_phase <- inc_scores_phase %>%
  group_by(model, target, seasonal_phase) %>%
  summarize(nweeks = length(unique(target_end_date)), median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE)) %>%
  left_join(model_levels_phases) %>%
  mutate(model = fct_reorder(model, relative_wis)) %>% droplevels()

ensemble_1wk_avg <- avg_wis_by_model_target_phase %>%
  .$mean_wis

baseline_avg <- avg_wis_by_model_target_phase %>%
  filter(model=="COVIDhub-baseline") %>%
  group_by(model, seasonal_phase, target) %>%
  summarize(mean_wis = mean(mean_wis))


# avg_wis_by_model_target_week_phase <- avg_wis_by_model_target_week_1and4wk_phase %>% 
#   filter(!is.na(model)) %>%
#   mutate(model = fct_relevel(model, model_levels_phases %>% pull(model))) 

avg_phase <- inc_scores %>%
  group_by(target, seasonal_phase, model) %>%
  summarize(median_wis = median(wis, na.rm = T), mean_wis = mean(wis, na.rm=TRUE), nlocs=n())  %>%
  filter(!is.na(model)) 


#box plot 

avg_wis_by_model_target_week_phase <- avg_wis_by_model_target_week_phase %>% ungroup() %>%
  mutate(seasonal_phase = fct_relevel(factor(seasonal_phase), levels = c("spring", "summer", "winter", "delta"))) 

p_phase_boxplot <- ggplot(avg_wis_by_model_target_week_phase, aes(x = reorder_within(model,relative_wis,seasonal_phase),
                                                                  y= scales::oob_squish(mean_wis, range = c(0,500)))) +
  facet_grid(rows = vars(target), cols = vars(factor(seasonal_phase, levels = c("spring", "summer", "winter", "delta"))), scales = "free_x", space="free") + #, scales="free_y") +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(data=baseline_avg,
             aes(yintercept=mean_wis), linetype=2, color = "red") +
  geom_point(data = avg_wis_by_model_target_phase, aes(y = mean_wis),  color = "blue", shape=4, size = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle=90, vjust=0.5, hjust=1), 
        legend.position = "none") +
  ylab("Average WIS") + xlab(NULL) +
  scale_fill_date(name="forecast date") +
  scale_y_continuous(trans = "log10") +
  scale_x_reordered()
 

pdf(file = "figures/model-target-week-wis-avgs_phase_boxplot.pdf", width=11, height=6)
print(p_phase_boxplot)
dev.off()

jpeg(file = "figures/model-target-week-wis-avgs_phase_boxplot.jpg", width=12, height=8, units="in", res=200)
print(p_phase_boxplot)
dev.off()

better_than_med_2of3 <- avg_wis_by_model_target_phase %>% 
  group_by(target, seasonal_phase) %>%
  filter(mean_wis < mean_wis[model == "COVIDhub-baseline"]) %>% ungroup() %>%
  group_by(model,target) %>%
  summarise(n_better_mean = n()) %>% filter(n_better_mean >= 2) %>% ungroup() %>%
  group_by(model) %>% summarise(n_1and4 = n()) %>% filter(n_1and4 > 1)

##Figure 4 [average WIS over time]

## assemble truth data observations for US level
obs_inc_deaths <- load_truth("JHU", "inc death", locations="US") %>%
  filter(target_end_date >= first_target_end_date, 
         target_end_date <= last_target_end_date)


f4a <- ggplot(obs_inc_deaths, aes(x=target_end_date, y=value)) +
  geom_point() +
  geom_line() + 
  scale_x_date(name=NULL, date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(name = "Incident deaths in US") +
  ggtitle("A: Observed weekly COVID-19 deaths in the US") +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2))


avg_scores_byweek <- avg_wis_by_model_target_week %>% 
  ungroup() %>%
  filter(target %in% c("1 wk ahead inc death", "4 wk ahead inc death")) %>%
  mutate(horizon = as.numeric(str_sub(target, 1, 1)),
    target_end_date = target_end_date_1wk_ahead + 7*(horizon-1)) %>%
  group_by(target_end_date_1wk_ahead, target) %>%
  mutate_at(vars("mean_wis"), funs(relative_wis = (. / .[model=="COVIDhub-baseline"]))) %>%
  ungroup() %>%  group_by(model) %>%
  mutate(model = factor(model)) %>%
  mutate(label = if_else(target_end_date_1wk_ahead  == max(target_end_date_1wk_ahead), 
                         model, factor(NA_character_, ordered = FALSE))) %>% ungroup()

avg_scores_byweek$model <- factor(as.character(avg_scores_byweek$model))


# old f4, saved for now
# f4 <- ggplot(avg_scores_byweek, aes(x = target_end_date, y= relative_wis, color = model, group = model)) +
#   #scale_x_date(date_labels = "%Y-%m-%d", breaks = c(unique(avg_scores_byweek$target_end_date_0wk_ahead)),name = "Forecast Week",
#   #             limits = c(min(avg_scores_byweek$target_end_date_0wk_ahead), max(avg_scores_byweek$target_end_date_0wk_ahead) + 20)) +
#   geom_line() + 
#   geom_point(size = 2) + 
#   expand_limits(y=0) +
#   scale_y_log10() + 
#   ylab("Average WIS relative to baseline") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) + 
#   #scale_x_continuous(name = "Forecast Week", breaks= unique(avg_scores_byweek$target_end_date_0wk_ahead)[c(TRUE,FALSE)], limits=c(1, 5)) +
#   facet_wrap(~ target, scales="free_y", ncol = 1) +
#   guides(color=FALSE,  group = FALSE) +
#   geom_text_repel(aes(label = label),
#                   box.padding = .3,
#                   direction = "y",
#                   segment.linetype = 2,
#                   nudge_x = 10,
#                   segment.alpha=.5,
#                   hjust = 0,
#                   size=2.5,
#                   na.rm = TRUE)

# relative wis
# f4 <- ggplot(avg_scores_byweek, aes(x = target_end_date, y = relative_wis)) +
#   geom_line(aes(group = model), color="darkgray", alpha=.5) +
#   geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
#   stat_summary(fun=mean, geom="line", colour="blue") +
#   stat_summary(fun=mean, geom="point", colour="blue") +
#   geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble"), aes(group = model), color="red") +
#   geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble"), aes(group = model), color="red") +
#   expand_limits(y=0) +
#   scale_y_log10() +
#   ylab("Average WIS relative to baseline") +
#   #scale_x_continuous(name = "Forecast Week", breaks= unique(avg_scores_byweek$target_end_date_0wk_ahead)[c(TRUE,FALSE)], limits=c(1, 5)) +
#   facet_wrap(~ target, scales="free_y", ncol = 1) +
#   guides(color=FALSE,  group = FALSE)


f4b <- ggplot(filter(avg_scores_byweek, target=="1 wk ahead inc death"), aes(x = target_end_date, y = mean_wis)) +
  geom_line(aes(group = model), color="darkgray", alpha=.5) +
  geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
  stat_summary(fun=mean, geom="line", aes(color="blue")) +
  #geom_vline(xintercept = range_fcast_dates, linetype = 2) +
  stat_summary(fun=mean, geom="point", aes(color="blue",  shape = "19")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red", shape = "17")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green", shape = "15")) +
  expand_limits(y=0) +
  coord_cartesian(ylim=c(0,200)) +
  scale_y_continuous(name = "Average WIS") +
  scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
  scale_color_identity(name = NULL, 
    breaks = c( "blue", "green", "red"), 
    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  scale_shape_manual(name = NULL,
    values = c(15, 17,19),
    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(color=FALSE, group = FALSE, shape = FALSE) +
  ggtitle("B: Average 1-week ahead weighted interval scores by model") +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2))
      

f4c <- ggplot(filter(avg_scores_byweek, target=="4 wk ahead inc death"), aes(x = target_end_date, y = mean_wis)) +
  geom_line(aes(group = model), color="darkgray", alpha=.5) +
  geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
  stat_summary(fun=mean, geom="line", aes(color="blue")) +
  #geom_vline(xintercept = range_fcast_dates, linetype = 2) +
  stat_summary(fun=mean, geom="point", aes(color="blue", shape = "blue")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red", shape = "red")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green", shape = "green")) +
  expand_limits(y=0) +
  ylab("Average WIS") +
  scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(name = "Average WIS") +
  guides(group = FALSE) +
  scale_color_manual(name = NULL, 
    values = c( "blue", "green", "red"), 
    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
    guide = "legend") +
  scale_shape_manual(name = NULL,
                     values = c(19, 15, 17),
                     labels = c("Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  coord_cartesian(ylim=c(0,500)) +
  theme(legend.position = c(0.05, 0.8), legend.justification = c(0,.5), 
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2)) +
  ggtitle("C: Average 4-week ahead weighted interval scores by model")


pdf(file = "figures/wis-avgs-by-week.pdf", width=8, height=10)
gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
dev.off()


jpeg(file = "figures/wis-avgs-by-week.jpg", width=8, height=10, units="in", res=200)
gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
dev.off()


#Assess difference in WIS for figure caption: 
avg_wis_by_model_target %>% filter(target == "1 wk ahead inc death") %>% arrange(mean_wis)
avg_wis_by_model_target %>% filter(target == "4 wk ahead inc death") %>% arrange(mean_wis)


obs_inc_deaths_wdiff <- load_truth("JHU", "inc death", locations="US") %>%
  select(target_end_date, value) %>%
  rename(obs_deaths_on_fcast_date = value, 
    forecast_date = target_end_date) %>%
  arrange(forecast_date) %>%
  mutate(death_diff_on_fcast_date = (lag(obs_deaths_on_fcast_date)- lag(obs_deaths_on_fcast_date,2))/lag(obs_deaths_on_fcast_date,2),
    target_end_date_1wk_ahead = forecast_date+7)

## exploration of association
# score_comparison_dat <- avg_scores_byweek %>%
#   left_join(obs_inc_deaths_wdiff) %>%
#   mutate(model = relevel(model, "COVIDhub-baseline")) 
#   
# 
# score_comparison_dat %>%
#   filter(model %in% c("COVIDhub-ensemble", "UMass-MechBayes", "YYG-ParamSearch", "OliverWyman-Navigator")) %>%
#   ggplot(aes(x=obs_deaths_on_fcast_date, y=log(relative_wis), color=model, group=model)) +
#   geom_point() +
#   geom_smooth(se=FALSE, span=1) +
#   facet_grid(.~target)
# 
# score_comparison_dat %>%
#   filter(model %in% c("COVIDhub-ensemble", "UMass-MechBayes", "YYG-ParamSearch", "OliverWyman-Navigator")) %>%
#   ggplot(aes(x=death_diff_on_fcast_date, y=log(relative_wis), color=model, group=model)) +
#   geom_point() +
#   geom_smooth(se=FALSE, span=1) +
#   facet_grid(.~target)
# 
# tmp <- lm(mean_wis ~ I(obs_deaths_on_fcast_date/sd(obs_deaths_on_fcast_date)) + I(death_diff_on_fcast_date/sd(death_diff_on_fcast_date, na.rm=TRUE)) + model + target + I(target_end_date_1wk_ahead=="2020-07-11"), data=score_comparison_dat)
# summary(tmp)
# 
# tmp1 <- mgcv::gam(mean_wis ~ s(obs_deaths_on_fcast_date, k=5) + s(death_diff_on_fcast_date, k=5) + model + target + I(target_end_date_1wk_ahead=="2020-07-11"), 
#   data=filter(score_comparison_dat, model %in% c("COVIDhub-ensemble", "UMass-MechBayes", "YYG-ParamSearch", "OliverWyman-Navigator")))
# tmp1 <- mgcv::gam(log(relative_wis) ~ s(obs_deaths_on_fcast_date, k=5) + s(death_diff_on_fcast_date, k=5) + model + target + I(target_end_date_1wk_ahead=="2020-07-11"), 
#   data=score_comparison_dat)
# 
# summary(tmp1)
 
# par(mfrow=c(1,2))
# plot(tmp1, 1)
# plot(tmp1, 2)

# library(mgcViz)
# tmp2 <- mgcv::gam(log(relative_wis) ~ te(obs_deaths_on_fcast_date, death_diff_on_fcast_date, bs="tp") + model + target + I(target_end_date_1wk_ahead=="2020-07-11"), 
#   bs="cs", k=list(2,2), data=score_comparison_dat)
# plot(sm(getViz(tmp2),1))
# 
