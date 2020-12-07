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

mae_by_model_target <- inc_scores %>%
    filter(!(location_name %in% locs_to_exclude)) %>%
    group_by(model, target) %>%
    summarize(mae = mean(abs_error))

mae_by_model_target$model <- reorder(mae_by_model_target$model,
  -mae_by_model_target$mae, FUN = function(x) mean(x, na.rm=TRUE))

mae_by_model_target <- mae_by_model_target %>%
  dplyr::ungroup() %>%
  dplyr::group_by(target) %>%
  dplyr::mutate(
    mae_diff_from_baseline = mae - mae[model == "COVIDhub-baseline"],
    mae_rel_to_baseline = mae / mae[model == "COVIDhub-baseline"],
  )

p <- ggplot(data = mae_by_model_target) +
  geom_raster(mapping = aes(x = model, y = target, fill = mae_rel_to_baseline)) +
  geom_label(mapping = aes(x = model, y = target, label = round(mae, 1))) +
  scale_fill_distiller("Model MAE /\nBaseline MAE", palette = "Spectral", direction = -1) + #"  Model MAE\n      - \nBaseline MAE") + #, begin = 0.1, end = 0.9, direction = -1, option = "A") +
  ylab("Forecast Horizon") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pdf(file = "figures/model-target-week-mae.pdf", width=10, height=6)
print(p)
dev.off()

jpeg(file = "figures/model-target-week-mae.jpg", width=10, height=6, units="in", res=200)
print(p)
dev.off()
