library(dplyr)
library(stringr)
library(PxWebApiData)
library(lubridate)
library(ggplot2)

# Data updated Tuesdays at 8 AM
update_week <- floor_date(Sys.Date(), unit = "week", week_start = 2) %>%
  strftime(., format = "%V") %>%
  as.numeric

lockdown_week <- 12

popnor <- ApiData(10211, ContentsCode  = "Personer", Alder = 0:105, 
                  Kjonn = 2:3, Tid = as.character(2000:2020)) %>%
  `[[`(1) %>%
  mutate(population = value, 
         age = as.numeric(str_remove(alder, " år")),
         year = as.numeric(år),
         sex = kjønn) %>%
  select(sex, age, year, population)

# Read data from SSB API, format, remove future weeks and week 53
wdnor <- ApiData(07995, ContentsCode = "Dode1", Kjonn = 2:3, Alder = 1:104, Uke = 1:53, Tid = as.character(2000:2020)) %>%
  `[[`(1) %>%
  mutate(deaths = value, 
         age = as.numeric(str_remove(alder, " år")),
         week = as.numeric(substr(uke, 5, 6)),
         year = as.numeric(år),
         sex = kjønn) %>%
  select(sex, age, week, year, deaths) %>%
  filter(!(year == 2020 & week >= update_week - 1),
         week != 53) %>%
  left_join(popnor, by = c("year", "age", "sex"))

# Sum over all ages and genders, adjust preliminary numbers for the two weeks prior
wdnor_totals <- wdnor %>%
  group_by(week, year) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(deaths = ifelse(year == 2020 & week == update_week - 2, deaths / 0.65, deaths),
         deaths = ifelse(year == 2020 & week == update_week - 3, deaths / 0.85, deaths),
         point_type = case_when(year == 2020 & week < update_week - 3 ~ "2020",
                                year == 2020 & week >= update_week - 3 ~ "2020 preliminary",
                                year < 2020 ~ "2000--2019"))

# Scatter with colors
ggplot(wdnor_totals, aes(x = week, y = deaths)) +
  geom_point(aes(color = point_type, shape = point_type)) +
  geom_smooth(data = filter(wdnor_totals, point_type != "2000--2019"),
              color = 2) +
  scale_color_manual(name = "", values = c(8, 2, 2)) +
  scale_shape_manual(name = "", values = c(1, 16, 1)) +
  geom_vline(xintercept = lockdown_week, color = 2) +
  annotate("text", x = 12, y = 1250, label = "Lockdown", color = 2) +
  coord_cartesian(ylim = c(550, 1200), clip = "off") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8), plot.margin = margin(t = 15))

# By age groups
wdnor_ages <- wdnor %>%
  mutate(age_group = case_when(age <= 5 ~ "0-5",
                               between(age, 6, 15) ~ "6-15",
                               between(age, 16, 30) ~ "16-30",
                               between(age, 31, 66) ~ "31-66",
                               age >= 67 ~ "67+")) %>%
  group_by(week, year, age_group) %>%
  summarize(deaths = sum(deaths),
            population = sum(population)) %>%
  mutate(deaths = ifelse(year == 2020 & week == update_week - 2, deaths / 0.65, deaths),
         deaths = ifelse(year == 2020 & week == update_week - 3, deaths / 0.85, deaths),
         point_type = case_when(year == 2020 & week < update_week - 3 ~ "2020",
                                year == 2020 & week >= update_week - 3 ~ "2020 preliminary",
                                year < 2020 ~ "2000--2019"))

ggplot(wdnor_ages, aes(x = week, y = deaths,
                       color = point_type, shape = point_type)) +
  geom_point(aes(shape = point_type)) +
  scale_shape_manual(name = "", values = c(1, 16, 1)) +
  scale_color_manual(name = "", values = c(8, 2, 2)) +
  facet_wrap(vars(age_group), scales = "free_y")