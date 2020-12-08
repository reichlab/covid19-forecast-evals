library(tidyverse)
library(ggrepel)

source("code/load-global-analysis-dates.R")

theme_set(theme_bw())

inc_score_models <- read_csv("paper-inputs/inc-scores.csv") %>%
    pull(model) %>%
    unique()

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
    filter(target %in% paste(1:4, "wk ahead inc death"),
        model %in% inc_score_models,
        target_end_date_1wk_ahead >= first_1wk_target_end_date,
        target_end_date_1wk_ahead <= last_1wk_target_end_date)
        #!(location_name %in% c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")),
        #!(location_name=="New Jersey" & target_end_date_1wk_ahead=="2020-07-04"))

locs_to_exclude <- c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia")



# ## figure: heat map of which models forecasted when
# 
# nforecasts_by_model_week <- inc_scores %>%
#     group_by(model, target_end_date_1wk_ahead) %>%
#     summarize(nforecasts = n_distinct(location_name)) %>%
#     group_by(model) %>%
#     mutate(total_fcasts = sum(nforecasts))
# 
# nforecasts_by_model_week$model = reorder(nforecasts_by_model_week$model, nforecasts_by_model_week$total_fcasts)
# 
# ggplot(nforecasts_by_model_week, aes(x=target_end_date_1wk_ahead, y=model)) +
#     geom_tile(aes(fill=nforecasts)) +
#     geom_text(aes(label=nforecasts)) +
#     xlab("week forecast submitted") + ylab(NULL)+
#     scale_color_brewer(name = "# locations")

## show all forecasts, with boundary around evaluated ones


## by model
avg_wis_by_model <- inc_scores %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(model) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE))


## by model and target
avg_wis_by_model_target <- inc_scores %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(model, target) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE))


## by model and week

tmp <- inc_scores %>%
    group_by(model, target_end_date_1wk_ahead) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis))
# 
# ggplot(tmp, aes(x=model, y=median_wis, color=target_end_date_1wk_ahead)) +
#     geom_point() +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
#     

tmp$model <- reorder(tmp$model, -tmp$mean_wis, FUN = function(x) mean(x, na.rm=TRUE))

# ggplot(tmp, aes(x=model, y=mean_wis, color=target_end_date_1wk_ahead)) +
#     geom_point() +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 


# wilcox.test(
#     filter(tmp, model=="COVIDhub-baseline") %>%pull(mean_wis),
#     filter(tmp, model=="COVIDhub-ensemble") %>%pull(mean_wis))

## by model and week

expected_locs <- inc_scores %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(target_end_date_1wk_ahead) %>%
    summarize(nlocs_expected = n_distinct(location_name))

# avg_wis_by_model_week <- inc_scores %>%
#     filter(!(location_name %in% c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia"))) %>% 
#     group_by(model, target_end_date_1wk_ahead) %>%
#     summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), nlocs=n()) %>%
#     left_join(expected_locs)%>%
#     mutate(obs_exp_locs = nlocs == (nlocs_expected*4))
# 
# avg_wis_by_model_week$model <- reorder(avg_wis_by_model_week$model, -avg_wis_by_model_week$mean_wis, FUN=function(x) mean(x, na.rm=TRUE))
# 
# ensemble_1wk_avg <- avg_wis_by_model %>%
#     filter(model=="COVIDhub-ensemble") %>%
#     .$mean_wis
# 
# baseline_avg <- avg_wis_by_model %>%
#     filter(model=="COVIDhub-baseline") %>%
#     pull(mean_wis)
# 
# ggplot(avg_wis_by_model_week, aes(x=model, y=mean_wis)) +
#     #geom_hline(yintercept=ensemble_1wk_avg, linetype=2)+
#     #geom_hline(yintercept=baseline_avg, linetype=2)+
#     geom_point(aes(color=obs_exp_locs), position=position_jitter(width=.2, height=0), alpha=.8) +
#     geom_point(data=avg_wis_by_model, shape=4, color="#d95f02", size=2, stroke=1)+
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = c(.6, .9), legend.direction = "horizontal") +
#     ylab("Average WIS") + xlab(NULL) +
#     expand_limits(y=0) +
#     ggtitle("Mean WIS across all forecasted locations, one point per week and model") +
#     scale_color_manual(name="all locations predicted", values=c("#7570b3", "#1b9e77")) +
#     scale_fill_date(name="forecast date") +
#     scale_y_continuous(breaks=seq(0, 400, by=50))



## by model, target, and week



avg_wis_by_model_target_week <- inc_scores %>%
    filter(!(location_name %in% locs_to_exclude)) %>% 
    group_by(model, target, target_end_date_1wk_ahead) %>%
    summarize(median_wis = median(wis), mean_wis = mean(wis, na.rm=TRUE), nlocs=n()) %>%
    left_join(expected_locs)%>%
    mutate(obs_exp_locs = nlocs == nlocs_expected)

avg_wis_by_model_target_week$model <- reorder(avg_wis_by_model_target_week$model, -avg_wis_by_model_target_week$mean_wis, FUN=function(x) mean(x, na.rm=TRUE))

ensemble_1wk_avg <- avg_wis_by_model_target %>%
    filter(model=="COVIDhub-ensemble", target=="1 wk ahead inc death") %>%
    .$mean_wis

baseline_avg <- avg_wis_by_model_target %>%
    filter(model=="COVIDhub-baseline") %>%
    group_by(model, target) %>%
    summarize(mean_wis = mean(mean_wis))

p <- ggplot(avg_wis_by_model_target_week, aes(x=model, y=mean_wis)) +
    #geom_hline(yintercept=ensemble_1wk_avg, linetype=2)+
    geom_hline(data=baseline_avg, aes(yintercept=mean_wis), linetype=2)+
    geom_point(aes(color=obs_exp_locs), position=position_jitter(width=.2, height=0), alpha=.8) +
    ##geom_point(aes(color=obs_exp_locs, fill=target_end_date_1wk_ahead), shape=21, position=position_jitter(width=.2, height=0), alpha=.8) +
    geom_point(data=avg_wis_by_model_target, shape=4, color="#d95f02", size=2, stroke=1)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = c(.75, .9), legend.direction = "horizontal") +
    facet_wrap(.~target) + #, scales="free_y") +
    ylab("Average WIS") + xlab(NULL) +
    expand_limits(y=0) +
    scale_color_manual(name="all locations predicted", values=c("#7570b3", "#1b9e77")) +
    scale_fill_date(name="forecast date") +
    scale_y_continuous(breaks=seq(0, 400, by=50))

pdf(file = "figures/model-target-week-wis-avgs.pdf", width=8, height=6)
print(p)
dev.off()

jpeg(file = "figures/model-target-week-wis-avgs.jpg", width=8, height=6, units="in", res=200)
print(p)
dev.off()



## supplemental figure overall boxplot

p1 <- inc_scores %>%
    filter(!(location_name %in% c("United States", "American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "District of Columbia"))) %>% 
    mutate(model = reorder(model, -wis, FUN=function(x) mean(x, na.rm=TRUE))) %>%
    ggplot(aes(x=model, y=wis, color=target_end_date_1wk_ahead)) +
    geom_boxplot() +
    scale_y_log10() +
    theme_bw() +
    ylab("WIS") + xlab(NULL) +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    facet_wrap(.~target)

pdf(file = "figures/overall-wis-boxplot.pdf", width=8, height=6)
print(p1)
dev.off()

jpeg(file = "figures/overall-wis-boxplot.jpg", width=8, height=6, units="in", res=200)
print(p1)
dev.off()


##Figure 4 [average WIS over time]

## assemble truth data observations for US level
obs_inc_deaths <- load_truth("JHU", "inc death", locations="US") %>%
  filter(target_end_date >= first_1wk_target_end_date, 
    target_end_date <= last_4wk_target_end_date)

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
  mutate(label = if_else(target_end_date_1wk_ahead  == max(target_end_date_1wk_ahead), model, factor(NA_character_))) %>% ungroup()

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
  stat_summary(fun=mean, geom="point", aes(color="blue")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="1 wk ahead inc death"), aes(group = model, color="red")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="1 wk ahead inc death"), aes(group = model, color="green")) +
  expand_limits(y=0) +
  scale_y_continuous(name = "Average WIS") +
  scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
  #scale_x_continuous(name = "Forecast Week", breaks= unique(avg_scores_byweek$target_end_date_0wk_ahead)[c(TRUE,FALSE)], limits=c(1, 5)) +
  # facet_grid(target~.) +
  scale_color_identity(name = NULL, 
    breaks = c( "blue", "green", "red"), 
    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble")) +
  guides(color=FALSE, group = FALSE) +
  ggtitle("B: Average 1-week ahead weighted interval scores by model") +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2))


f4c <- ggplot(filter(avg_scores_byweek, target=="4 wk ahead inc death"), aes(x = target_end_date, y = mean_wis)) +
  geom_line(aes(group = model), color="darkgray", alpha=.5) +
  geom_point(aes(group = model), color="darkgray", alpha=.5, size = 2) +
  stat_summary(fun=mean, geom="line", aes(colour="blue")) +
  stat_summary(fun=mean, geom="point", aes(colour="blue")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-ensemble",  target=="4 wk ahead inc death"), aes(group = model, color="red")) +
  geom_line(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green")) +
  geom_point(data=filter(avg_scores_byweek, model=="COVIDhub-baseline", target=="4 wk ahead inc death"), aes(group = model, color="green")) +
  expand_limits(y=0) +
  ylab("Average WIS") +
  scale_x_date(name=NULL, limits=range(obs_inc_deaths$target_end_date), date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(name = "Average WIS") +
  #scale_x_continuous(name = "Forecast Week", breaks= unique(avg_scores_byweek$target_end_date_0wk_ahead)[c(TRUE,FALSE)], limits=c(1, 5)) +
  # facet_grid(target~.) +
  guides(group = FALSE) +
  scale_color_identity(name = NULL, 
    breaks = c( "blue", "green", "red"), 
    labels = c( "Average score of all models", "COVIDhub-baseline","COVIDhub-ensemble"),
    guide = "legend") +
  theme(legend.position = c(0.05, 0.7), legend.justification = c(0,.5), 
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 7, hjust = -0.2))+
  ggtitle("C: Average 4-week ahead weighted interval scores by model")



pdf(file = "figures/wis-avgs-by-week.pdf", width=8, height=10)
gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
dev.off()


jpeg(file = "figures/wis-avgs-by-week.jpg", width=8, height=10, units="in", res=200)
gridExtra::grid.arrange(f4a, f4b, f4c, layout_matrix = matrix(c(1,2,3), ncol=1))
dev.off()
