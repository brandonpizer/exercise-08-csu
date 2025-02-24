#Brandon Pizer 2/23/25 Daily Exercise 08

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

df = data.frame(region = state.region, state = state.name, abb = state.abb)

joined_table <- inner_join(df, covid, by = "state")

joined_table |>
  group_by(region, date) |>
  summarize(tot_cases = sum(cases), tot_deaths = sum(deaths)) |>
  pivot_longer(cols = c(tot_cases, tot_deaths), names_to = "type", values_to = "count") |>
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y") +
  theme_light() +
  labs(x = "Date", y = "Cumulative Daily Count",
       title = "Daily Covid Counts in US Regions",
       subtitle = "ESS330",
       caption = "Data: NY-Times")
