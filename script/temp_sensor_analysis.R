
library(ggplot2)
library(dplyr)
library(tidyr)

temp_sensor_data <- read.csv("output/Temp_data_filtered.csv")

str(temp_sensor_data)

# Visualizations ----------------------------------------------------------

#ggplot(temp_sensor_data,
#       aes(x = snow_position,
#           y = temperature_c,
#           fill = urban)) +
#  geom_boxplot(alpha = 0.7) +
#  labs(
#    x = "Snow position",
#    y = "Temperature (°C)",
#    fill = "Urban environment"
#  ) +
#  theme_bw()


#temp variation below/above snow and between urban and non-urban forests

ggplot(temp_sensor_data,
       aes(x = snow_position,
           y = temperature_c,
           fill = urban)) +
  geom_violin(alpha = 0.6,
              position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.12,
               position = position_dodge(width = 0.8),
               outlier.shape = NA) +
  labs(
    x = "Snow position",
    y = "Temperature (°C)",
    fill = "Urban environment"
  ) +
  theme_bw()

#temp difference above and below snow varies depending on urban or non-urban
ggplot(temp_sensor_data,
       aes(x = snow_position,
           y = temperature_c,
           colour = urban,
           group = urban)) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               position = position_dodge(0.2)) +
  stat_summary(fun = mean,
               geom = "line",
               linewidth = 1,
               position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.15,
               position = position_dodge(0.2)) +
  facet_wrap(~park_name, ncol = 3) +
  labs(
    x = "Snow position",
    y = "Mean temperature (°C)",
    colour = "Urban environment"
  ) +
  theme_bw()

#separated by parks

ggplot(temp_sensor_data,
       aes(x = interaction(urban, snow_position),
           y = temperature_c,
           fill = snow_position)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1,
               outlier.shape = NA) +
  facet_wrap(~park_name) +
  labs(
    x = "Urbanization and snow position",
    y = "Temperature (°C)",
    fill = "Snow position"
  ) +
  theme_bw()

#variation by time

ggplot(temp_sensor_data,
       aes(x = hour,
           y = temperature_c,
           colour = snow_position)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Hour of day",
    y = "Temperature (°C)",
    colour = "Snow position"
  ) +
  theme_bw()

#variation by time, in urban vs non-urban
ggplot(temp_sensor_data,
       aes(x = hour,
           y = temperature_c,
           colour = snow_position)) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_wrap(~urban) +
  labs(
    x = "Hour of day",
    y = "Temperature (°C)",
    colour = "Snow position"
  ) +
  theme_bw()

#create a heatmap showing temp variations across survey period
temp_hourly <- temp_sensor_data %>%
  group_by(date, hour, snow_position, urban) %>%
  summarise(
    mean_temp = mean(temperature_c, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(temp_hourly,
       aes(x = hour,
           y = as.Date(date),
           fill = mean_temp)) +
  geom_tile() +
  facet_grid(urban ~ snow_position) +
  scale_fill_viridis_c(name = "Temperature (°C)") +
  labs(
    x = "Hour of day",
    y = "Date"
  ) +
  theme_bw()

#variation by time, separated by parks

ggplot(temp_sensor_data,
       aes(x = hour,
           y = temperature_c,
           colour = snow_position)) +
  geom_smooth(se = FALSE) +
  facet_grid(urban ~ park_name) +
  labs(
    x = "Hour of day",
    y = "Temperature (°C)",
    colour = "Snow position"
  ) +
  theme_bw()

#calculate the different in temp (diff = above - below) for each park and 
#urban vs non-urban

temp_diff <- temp_sensor_data %>%
  group_by(park_name, urban, date_time, snow_position) %>%
  summarise(
    temperature_c = mean(temperature_c, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = snow_position,
    values_from = temperature_c
  ) %>%
  mutate(
    temp_difference = above - below
  )

#graph the differences
ggplot(temp_diff,
       aes(x = park_name,
           y = temp_difference,
           fill = urban)) +
  geom_boxplot() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "red") +
  labs(
    x = "Park",
    y = "Temperature difference (above - below snow, °C)",
    fill = "Urban environment"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





