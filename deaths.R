library(dplyr)
library(stringr)
library(PxWebApiData)
library(lubridate)
library(ggplot2)

palette(c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#EEC21F", "gray62"))

# Data updated Tuesdays at 8 AM
update_week <- floor_date(Sys.Date(), unit = "week", week_start = 2) %>%
  strftime(., format = "%V") %>%
  as.numeric

lockdown_week <- 12

# Read data from SSB API, format, remove future weeks and week 53
deaths <- ApiData(07995, ContentsCode = "Dode1", Kjonn = 2:3, Alder = 1:104, Uke = 1:53, Tid = as.character(2000:2020)) %>%
  `[[`(1) %>%
  rename(deaths = value) %>%
  mutate(age = as.numeric(str_remove(alder, " år")),
         week = as.numeric(substr(uke, 5, 6)),
         year = as.numeric(år),
         sex = kjønn) %>%
  select(sex, age, week, year, deaths) %>%
  filter(!(year == 2020 & week >= update_week - 1),
         week != 53)

# Sum over all ages and genders, adjust preliminary numbers for the two weeks prior
totals <- deaths %>%
  group_by(week, year) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(deaths = ifelse(year == 2020 & week == update_week - 2, deaths / 0.65, deaths),
         deaths = ifelse(year == 2020 & week == update_week - 3, deaths / 0.85, deaths),
         point_type = case_when(year == 2020 & week < update_week - 3 ~ "2020",
                                year == 2020 & week >= update_week - 3 ~ "2020 preliminary",
                                year < 2020 ~ "2000--2019"),
         year_2020 = ifelse(year == 2020, TRUE, FALSE))

# Scatter with colors
ggplot(totals, aes(x = week, y = deaths)) +
  geom_point(aes(color = point_type, shape = point_type)) +
  geom_smooth(data = filter(totals, point_type != "2000--2019"),
              color = "#df536b") +
  scale_color_manual(name = "", values = c("gray62", "#df536b", "#df536b")) +
  scale_shape_manual(name = "", values = c(1, 16, 1)) +
  geom_vline(xintercept = lockdown_week, color = "#df536b") +
  annotate("text", x = 12, y = 1250, label = "Lockdown", color = "#df536b") +
  coord_cartesian(ylim = c(550, 1200), clip = "off") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8), plot.margin = margin(t = 15))
