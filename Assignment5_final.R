################################################
######## solution to Data Visualization ########
################################################

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(data.table)
library(ggthemes)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(forcats)
library(maps)
library(spdep)
library(ggmap)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

cummulative_covid_data_tbl <- covid_data_tbl %>%
  mutate(month_name = lubridate::month(month, label = TRUE, abbr = FALSE)) %>%
  select(year, month_name, cases, countriesAndTerritories) %>%
  filter(year %in% c("2020")) %>%
  filter(countriesAndTerritories == 'Germany' | 
           countriesAndTerritories == 'United_Kingdom' | 
           countriesAndTerritories == 'Spain' | 
           countriesAndTerritories == 'France' | 
           countriesAndTerritories == 'United_States_of_America') %>%
  group_by(countriesAndTerritories, month_name) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(cummulative_cases = cumsum(total_cases)) %>%
  ungroup() %>%
  rename(Countries = countriesAndTerritories)

cummulative_covid_data_tbl %>%
  # Canvas
  ggplot(aes(month_name, cummulative_cases)) +
  
  # Geoms
  geom_line(aes(x     = month_name,
                y     = cummulative_cases,
                group = Countries,
                color = Countries
  ))+
  
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6,
                                                  accuracy = 0.1,
                                                  prefix = "",
                                                  suffix = "M")) +
  labs(
    title = paste("COVID-19 confirmed cases worldwide"),
    subtitle = paste("As of 12/06/2020, USA had more cases than any other country"),
    x = "Year 2020",
    y = "Cummulative Cases"
  ) +
  
  geom_smooth() +
  theme_fivethirtyeight() +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face="bold"),
    plot.caption = element_text(face = "bold.italic")
  )

#################################################

############# SOlution to part B ################
mortality_rate_tbl <- covid_data_tbl %>%
  select(countriesAndTerritories, deaths, popData2019) %>%
  group_by(countriesAndTerritories, popData2019) %>%
  summarize(total_deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(mortality_rate = total_deaths/popData2019) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  )) %>%
  filter(!is.na(mortality_rate)) %>%
  rename(country = countriesAndTerritories) %>%
  select(country, mortality_rate)

world <- map_data("world")
mortality_rate_tbl %>%
  ggplot() +
  geom_map(map = world,
           aes(map_id = country, fill = mortality_rate)) +
  expand_limits(x=world$long, y=world$lat) +
  labs(
    title = paste("Confirmed COVID-19 deaths relative to the size of the population"),
    subtitle = paste("More than 1.2 Million confirmed COVID-19 death worldwide"),
    fill = "Mortality Rate",
    x = "",
    y = "",
    caption = "Date: 12/06/2020"
  ) +
  scale_fill_gradient(low='#FF0000',high='#800000', 
                      name='Mortalityy Rate') +
  theme(legend.position = "right",
        axis.text=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_blank())







