library(tidyverse)
library(covidcast)


#Load anomaly csv
anomaly_dates <- read.csv("paper-inputs/anomaly-reporting-dates.csv") %>%
  mutate(first_fcast_date_impacted = as.Date(first_fcast_date_impacted)) %>% #convert date to proper format
  mutate(geo_value = tolower(name_to_abbr(location_name))) %>% #convert location name to abbreviation for use in merge
  select(first_fcast_date_impacted, geo_value) #keep only date impacted and location abbrev


#Load dates from covidcast  (variation in revision dates)
data_0601 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-15",
  end_day = "2020-06-01",
  geo_type = c("state"),
  geo_values = c("MI", "OR", "MI", "DE", "NJ", "NY", "TX"),
  as_of = "2020-06-01")  %>%
  mutate(revision_date = "2020-06-01")

data_0624 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-15",
  end_day = "2020-06-24",
  geo_type = c("state"),
  geo_values = c("MI", "OR", "MI", "DE", "NJ", "NY", "TX"),
  as_of = "2020-06-24")  %>%
  mutate(revision_date = "2020-06-24")

data_0705 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-15",
  end_day = "2020-07-02",
  geo_type = c("state"),
  geo_values = c("MI", "OR", "MI", "DE", "NJ", "NY", "TX"),
  as_of = "2020-07-05") %>%
  mutate(revision_date = "2020-07-05")

data_0804 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-15",
  end_day = "2020-08-04",
  geo_type = c("state"),
  geo_values = c("MI", "OR", "MI", "DE", "NJ", "NY", "TX"),
  as_of = "2020-08-04") %>%
  mutate(revision_date = "2020-08-04")


data_1013 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-15",
  end_day = "2020-08-10",
  geo_type = c("state"),
  geo_values = c("MI", "OR", "MI", "DE", "NJ", "NY", "TX"),
  as_of = "2020-10-13") %>%
  mutate(revision_date = "2020-10-13")

#dataset with all values
all_data <- rbind(data_0601,data_0624, data_0705, data_0804, data_1013) %>% 
  mutate(weekday = lubridate::wday(time_value, label=TRUE)) %>% as.data.frame() %>%
  left_join(anomaly_dates) 


#Graph values 

mon_labels <- all_data %>% filter(weekday == "Mon") %>% pull(time_value) %>% unique() #only use monday as labels on x axis

ggplot(data = all_data, aes(x = time_value, y = value, color = revision_date, fill = revision_date)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_vline(aes(xintercept = all_data$first_fcast_date_impacted), linetype = "dashed") +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = c(mon_labels), name = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  facet_wrap(~geo_value, scales = "free") +
  ylab("Incident Deaths") + ggtitle("incident death updates")

