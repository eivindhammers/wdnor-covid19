library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(thematic)

source("get_data.R", encoding = "UTF-8")

thematic_on(
  bg = "#222222", fg = "white", accent = "#0CE3AC",
  font = font_spec("Helvetica", scale = 1.25)
)

# All countries
wdall <- bind_rows(wdnor, wdswe, wdfin, wddnk) %>%
  filter(year >= 2015,
         year < 2020 | week <= isoweek(Sys.Date()) - 2)

# Check latest weeks of data for each country
wdall %>% 
  filter(year == 2020) %>% 
  xtabs(~country + week, data = .)

# Scatter with loess estimate and confidence interval
ggplot(wdall, aes(x = week, y = deaths_per100k, 
                  color = country)) +
  geom_point(aes(shape = point_type)) +
  scale_shape_manual(name = "", values = c(1, 16)) +
  scale_color_discrete(name = "") +
  geom_smooth(data = filter(wdall, point_type == "2020"), se = TRUE) + 
  geom_smooth(data = filter(wdall, point_type != "2020"), linetype = "dashed", se = FALSE) 
  
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

# Faceted plot
ggplot(wdall, aes(x = week, y = deaths_per100k, 
                  color = country)) +
  geom_point(aes(shape = point_type)) +
  guides(shape = guide_legend(override.aes = list(color = 8))) +
  scale_shape_manual(name = "", values = c(1, 16)) +
  scale_color_discrete(guide = "none") +
  geom_smooth(data = filter(wdall, point_type == "2020"), se = TRUE) + 
  geom_smooth(data = filter(wdall, point_type != "2020"), linetype = "dashed", se = FALSE) +
  facet_wrap(vars(country)) 
