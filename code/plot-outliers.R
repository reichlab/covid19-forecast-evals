library(tidyverse)
library(covidData)
library(here)
setwd(here())

issue_date <- "2021-05-25"

# plot death outliers
outliers <- readr::read_csv(
  paste0("data-raw/outliers-inc-deaths-2021-05-25.csv")
)

data_all <- covidData::load_data(
  as_of = issue_date,
  spatial_resolution = c("national", "state"),
  temporal_resolution = "weekly",
  measure = "deaths"
) %>%
  dplyr::mutate(as_of = issue_date)

locations <- covidData::fips_codes %>%
  dplyr::select(location, location_name) %>%
  dplyr::filter(nchar(location) == 2)

pdf(paste0("outliers-inc-deaths.pdf"), width = 14, height = 10)
for (i in seq_len(nrow(locations))) {
  message(i)
  data <- data_all %>%
    dplyr::filter(
      location == locations$location[i])
  
  outliers_to_plot <- outliers %>%
    filter(
      location == locations$location[i]
    )
  
  p <- ggplot() +
    geom_line(data = data, mapping = aes(x = date, y = inc)) +
    geom_point(data = data, mapping = aes(x = date, y = inc))
  
  if (nrow(outliers_to_plot) > 0) {
    p <- p + geom_point(
      data = outliers_to_plot %>%
        dplyr::left_join(
          data,
          by = c("location", "date")
        ),
      mapping = aes(x = date, y = inc, color = type, shape = type), size = 3) +
      scale_color_manual(values = c("christmas" = "red", "new_year" = "cornflowerblue", other = "orange")) +
      scale_shape_manual(values = c("christmas" = 15, "new_year" = 16, other = 17))
  }
  
  p <- p +
    geom_hline(yintercept = 0) +
    scale_x_date(
      breaks = data %>%
        dplyr::filter(weekdays(date) == "Saturday") %>%
        dplyr::pull(date) %>%
        unique(),
      limits = c(as.Date("2020-01-01"), as.Date(issue_date))) +
    ggtitle(paste0(
      locations$location_name[i],
      ", issue date ",
      issue_date)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      panel.grid.major.x = element_line(colour = "darkgrey")
    )
  print(p)
}
dev.off()