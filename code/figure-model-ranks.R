library(tidyverse)
library(covidHubUtils)
library(viridis)

theme_set(theme_bw())
data("hub_locations")

inc_scores <- read_csv("paper-inputs/inc-scores.csv") %>%
  filter(location_name != "American Samoa", target %in% paste(1:4, "wk ahead inc death")) %>%
  mutate(id = paste(target_end_date_1wk_ahead, target, location_name)) %>%
  group_by(target_end_date_1wk_ahead, target, location_name) %>%
  arrange(wis) %>%
  mutate(model_rank = row_number()) %>%
  ungroup() %>%
  mutate(model = reorder(model, -model_rank, FUN=function(x) mean(x, na.rm=TRUE)))

## number of unique opportunities for a prediction
inc_scores %>%
  group_by(target_end_date_1wk_ahead, unit, target) %>%
  summarize(n()) %>%
  nrow()

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

