library(tidyverse)
library(covidHubUtils)
library(viridis)

theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name != "American Samoa", target %in% paste(1:4, "wk ahead inc death")) %>%
  mutate(id = paste(target_end_date_1wk_ahead, target, location_name)) %>%
  group_by(target_end_date_1wk_ahead, target, location_name) %>%
  mutate(n_models = n()) %>%
  ##filter(n_models >= 15) %>%
  arrange(wis) %>%
  mutate(model_rank = row_number(), rank_percentile = model_rank/n_models) %>%
  ungroup() %>%
  mutate(model = reorder(model, -model_rank, FUN=function(x) mean(x, na.rm=TRUE)))

## number of unique opportunities for a prediction
inc_scores %>%
  group_by(target_end_date_1wk_ahead, location_name, target) %>%
  summarize(n()) %>%
  nrow()

table(inc_scores$n_models)

## average rank
inc_scores %>%
  group_by(model) %>%
  summarize(average_rank = mean(model_rank), total_n = n(), n_top_rank = sum(model_rank==1), pct_top = n_top_rank/total_n*100)


# ggplot(inc_scores, aes(y=id, x=model, fill=model_rank)) +
#   geom_tile() +
# #  facet_grid(location_name + target_end_date_1wk_ahead + target ~.) +
#   scale_fill_gradient(na.value="red") +
#   scale_y_discrete(labels=element_blank()) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#     axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())

p1 <- ggplot(inc_scores, aes(x=model, fill=factor(model_rank))) +
  # geom_bar(position="fill") + ## for percentages
  geom_bar() +
  scale_y_continuous(expand = expansion(mult=c(0, 0.05))) +
  scale_fill_viridis(discrete=TRUE, direction = -1, name="model rank") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL)
  

pdf(file = "figures/fig-model-ranks.pdf", width=8, height=5)
print(p1)
dev.off()

jpeg(file = "figures/fig-model-ranks.jpg", width=8, height=5, units="in", res=300)
print(p1)
dev.off()


ggplot(inc_scores, aes(y=model, x=rank_percentile)) +
  geom_boxplot()


library(ggridges)
ggplot(inc_scores, aes(y=model, x=rank_percentile, height = ..density..)) +
  geom_density_ridges(scale = 1, stat = "density", trim = TRUE) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0), limits=c(0,1)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges() +
  geom_vline(xintercept=1/17)

ggplot(inc_scores, aes(y=model, x=rank_percentile, height = ..density..)) +
  geom_density_ridges(stat = "binline", binwidth=.05, draw_baseline = F) +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0), limits=c(0,1)) +   # for both axes to remove unneeded padding
  scale_fill_viridis_d(direction = -1, name="model rank") +
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

ggplot(inc_scores, aes(y=model, x=rank_percentile, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1) + 
  #geom_ridgeline_gradient() +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0), limits=c(0,1)) +   # for both axes to remove unneeded padding
  scale_fill_viridis_c(direction = -1, name="model rank") +
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

ggplot(inc_scores, aes(y=model, x=rank_percentile, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 5, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles", direction=-1)

library(ggplot2)
library(ggridges)
library(viridis)
ggplot(iris, aes(x=Sepal.Length, y=Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 15, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quantiles")
