library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(httr)
library(jsonlite)
library(pxweb)
library(PxWebApiData)

# Norway ----
# Norwegian data updated Tuesdays at 8 AM
update_week <- floor_date(Sys.Date(), unit = "week", week_start = 2) %>%
  strftime(., format = "%V") %>%
  as.numeric

# Norwegian population data
popnor <- ApiData(11342, ContentsCode  = "Folkemengde", Region = "0", 
                  Tid = as.character(2015:2020)) %>% 
  `[[`(1) %>%
  mutate(year = as.integer(år)) %>%
  select(population = value, year)

# Read data from SSB API, format, remove future weeks and week 53
wdnor <- ApiData(07995, ContentsCode = "Dode1", Kjonn = 2:3, Alder = 1:104, 
                 Uke = 1:53, Tid = as.character(2000:2020)) %>%
  `[[`(1) %>%
  rename(deaths = value) %>%
  mutate(age = as.numeric(str_remove(alder, " år")),
         week = as.numeric(substr(uke, 5, 6)),
         year = as.numeric(år),
         sex = kjønn) %>%
  select(sex, age, week, year, deaths) %>%
  filter(!(year == 2020 & week >= update_week - 1), 
         week != 53,
         year >= 2015) %>%
  group_by(week, year) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(deaths = ifelse(year == 2020 & week == update_week - 2, deaths / 0.65, deaths),
         deaths = ifelse(year == 2020 & week == update_week - 3, deaths / 0.85, deaths),
         point_type = case_when(year == 2020 & week < update_week - 3 ~ "2020",
                                year == 2020 & week >= update_week - 3 ~ "2020",
                                year < 2020 ~ "2015--2019"),
         country = "Norway") %>%
  left_join(popnor, by = "year") %>%
  mutate(deaths_per100k = 100000 * deaths / population)

# Sweden ----
tmp <- tempfile(".xlsx")
urlswe <- "https://www.scb.se/en/finding-statistics/statistics-by-subject-area/population/population-composition/population-statistics/pong/tables-and-graphs/preliminary-statistics-on-deaths/"
download.file(urlswe, tmp)

wdswe <- read_xlsx(tmp, sheet = "Tabell 1", skip = 6) %>%
  select("DagMånad", as.character(2015:2020)) %>%
  filter(DagMånad != "Okänd dödsdag") %>%
  mutate(month = str_sub(str_split_fixed(DagMånad, " ", n = 2)[, 2], 1, 3),
         month = replace(month, month == "maj", "may"),
         month = replace(month, month == "okt", "oct"),
         day = str_split_fixed(DagMånad, " ", n = 2)[, 1]) %>%
  pivot_longer(cols = as.character(2015:2020), names_to = "year", values_to = "deaths") %>%
  mutate(date = as_date(paste(year, month, day, sep = "-")),
         week = as.integer(isoweek(date)),
         year = as.integer(year),
         point_type = ifelse(year == 2020, "2020", "2015--2019"),
         country = "Sweden") %>%
  filter(!is.na(date),
         !(is.na(deaths) & year == 2020),
         week < 53,
         year >= 2015) %>%
  group_by(year, week, point_type, country) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  filter(deaths != 0)

# Denmark ----
urldnk_pop <- "https://api.statbank.dk/v1/data/BEF5/CSV?valuePresentation=Value&delimiter=Tab&Tid=*"

popdnk <- GET(urldnk_pop) %>%
  content("parsed", "text/tab-separated-values") %>%
  mutate(year = as.integer(TID),
         population = INDHOLD) %>%
  filter(year >= 2015) %>%
  select(year, population)

urldnk <- "https://api.statbank.dk/v1/data/dodc2/CSV?lang=en&delimiter=Tab&TID=*"

wddnk <- GET(urldnk) %>%
  content("parsed", "text/tab-separated-values") %>%
  mutate(year = as.integer(substr(TID, 1, 4)),
         week = as.integer(substr(TID, 6, 7)),
         point_type = ifelse(year == 2020, "2020", "2015--2019"),
         country = "Denmark") %>%
  filter(year >= 2015, week != 53) %>%
  select(year, week, deaths = INDHOLD, point_type, country) %>%
  left_join(popdnk, by = "year") %>%
  mutate(deaths_per100k = 100000 * deaths / population)

#  Finland ----
urlfin_pop <- "https://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11ra.px"
reqfin_pop <- '{"query":[{"code":"Alue","selection":{"filter":"item","values":["SSS"]}},{"code":"Tiedot","selection":{"filter":"item","values":["vaesto"]}},{"code":"Vuosi","selection":{"filter":"item","values":["2014","2015","2016","2017","2018","2019"]}}],"response":{"format":"json-stat"}}'

popfin_list <- POST(urlfin_pop, body = reqfin_pop) %>%
  content("text") %>%
  fromJSON()

popfin <- cbind(unlist(popfin_list$dataset$dimension$Vuosi$category$label), 
                popfin_list$dataset$value) %>% 
  as_tibble(data.frame()) %>%
  mutate(year = as.integer(V1) + 1, # 31 dec numbers, so assign to next year
         population = as.numeric(V2)) %>%
  select(year, population)

urlfin <- "https://pxnet2.stat.fi:443/PXWeb/api/v1/en/Kokeelliset_tilastot/vamuu_koke/statfin_vamuu_pxt_12ng.px"

weeks <- crossing(year = 2015:2020, week = 1:53) %>%
  mutate(week_fmt = paste(year, str_pad(week, width = 2, pad = "0"), sep = "W")) %>%
  filter(!(week == 53 & year != 2015)) %>%
  pull(week_fmt) %>%
  paste(collapse = '","')

reqfin <- paste0('{"query":[{"code":"Alue","selection":{"filter":"item","values":["SSS"]}},{"code":"Viikko","selection":{"filter":"item","values":["',
                 weeks,
                 '"]}},{"code":"Ikä","selection":{"filter":"item","values":["SSS"]}},{"code":"Sukupuoli","selection":{"filter":"item","values":["SSS"]}}],"response":{"format":"json-stat"}}')

wdfin_list <- POST(urlfin, body = reqfin) %>%
  content("text") %>%
  fromJSON()

wdfin <- cbind(unlist(wdfin_list$dataset$dimension$Viikko$category$label), 
               wdfin_list$dataset$value) %>% 
  as_tibble(data.frame()) %>%
  mutate(year = as.integer(substr(V1, 1, 4)),
         week = as.integer(substr(V1, 6, 7)),
         deaths = as.integer(V2),
         point_type = ifelse(year == 2020, "2020", "2015--2019"),
         country = "Finland") %>%
  select(year, week, deaths, point_type, country) %>%
  left_join(popfin, by = "year") %>%
  mutate(deaths_per100k = 100000 * deaths / population) %>%
  filter(week != 53)
