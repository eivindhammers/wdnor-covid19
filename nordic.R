library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)

# All countries
wdall <- bind_rows(wdnor, wdswe, wdfin, wddnk) %>%
  filter(year >= 2015,
         !(week >= 14 & year == 2020))

# Scatter with loess estimate and confidence interval
ggplot(wdall, aes(x = week, y = deaths_per100k, 
                  color = country)) +
  geom_point(aes(shape = point_type)) +
  scale_shape_manual(name = "", values = c(1, 16)) +
  scale_color_discrete(name = "") +
  geom_smooth(data = filter(wdall, point_type == "2020"), se = TRUE) + 
  geom_smooth(data = filter(wdall, point_type != "2020"), linetype = "dashed", se = FALSE) +
  theme_minimal()

# Scatter with loess estimate, no confidence interval
ggplot(wdall, aes(x = week, y = deaths_per100k, 
                  color = country, alpha = point_type)) +
  geom_point(aes(shape = point_type)) +
  scale_shape_manual(name = "Period", values = c(1, 16)) +
  scale_color_discrete(name = "Country") +
  scale_alpha_discrete(range = c(0.5, 1), guide = "none") +
  geom_smooth(data = filter(wdall, point_type == "2020"), se = FALSE) + 
  stat_smooth(geom = "line", data = filter(wdall, point_type != "2020"), 
              linetype = "dashed", se = FALSE, alpha = 0.5) +
  theme_minimal() +
  xlab("Week") +
  ylab("Deaths per 100k") +
  theme(legend.position = c(0.6, 0.8), plot.margin = margin(15, 5, 5, 5), 
        legend.box = "horizontal")

