
#Load Libraries
library(tidyverse)
library(covidcast)


#Download data for NY and NJ from 6/24 and 7/05
data_nynj_0624 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-01",
  end_day = "2020-06-24",
  geo_type = c("state"),
  geo_values = c("NY","NJ"),
  as_of = "2020-06-24")  %>%
  mutate(as_of_date = "2020-06-24")

data_nynj_0705 <- covidcast_signal(
  data_source = "jhu-csse",
  signal = "deaths_incidence_num",
  start_day = "2020-05-01",
  end_day = "2020-07-02",
  geo_type = c("state"),
  geo_values = c("NY","NJ"),
  as_of = "2020-07-05") %>%
  mutate(as_of_date = "2020-07-05")


#Create dataframes of individual states 
all_NY <- rbind(data_nynj_0624, data_nynj_0705) %>% filter(geo_value == "ny") %>%
  mutate(weekday = wday(time_value, label=TRUE)) %>% as.data.frame()

all_NJ <- rbind(data_nynj_0624, data_nynj_0705) %>% filter(geo_value == "nj") %>%
  mutate(weekday = wday(time_value, label=TRUE)) %>% as.data.frame()


#Graph dates 
sat_labels <- all_NY %>% filter(weekday == "Fri") %>% pull(time_value) #list of all saturdays for plot 

#plot of NY data (with backfilled data)
sf2_ny <- ggplot(data = all_NY, aes(x = time_value, y = value, color = as_of_date)) +
  geom_line() +
  geom_point(size = 3) +
  labs(color = "Revision Date") +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = c(sat_labels),name = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20)) +
  ylab("Incident Deaths") + ggtitle("incident death updates for New York")

# pdf(file = "figures/NY_revision-sfig3.pdf", width=8, height=6)
# print(sf2_ny)
# dev.off()
# 
# jpeg(file ="figures/NY_revision-sfig3.jpg", width=8, height=6, units="in", res=200)
# print(sf2_ny)
# dev.off()


#plot of NJ data (large update but no backfill)
sf2_nj <- ggplot(data = all_NJ, aes(x = time_value, y = value, color = as_of_date, fill = as_of_date)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%Y-%m-%d", breaks = c(sat_labels),name = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 20)) +
  ylab("Incident Deaths") + ggtitle("incident death updates for New Jersey")

# pdf(file = "figures/NJ_revision-sfig2.pdf", width=8, height=6)
# print(sf2_nj)
# dev.off()
# 
# jpeg(file ="figures/NJ_revision-sfig2.jpg", width=8, height=6, units="in", res=200)
# print(sf2_nj)
# dev.off()




