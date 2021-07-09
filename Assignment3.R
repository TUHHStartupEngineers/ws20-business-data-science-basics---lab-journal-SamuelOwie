########################################
##        Assignment Solution         ##
########################################
source("codes.R")
library(tidyverse)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)
library(furrr)
library(RSQLite)
library(dplyr)
library(httr)
library(keyring)
library(rstudioapi)
library(stringr)
library(purrr)
library(gsubfn)

#weather_api_url <- https://pro.openweathermap.org/data/2.5/forecast/climate?q={city name}/{country code}&appid={API key}
weather_api_url <- url("https://pro.openweathermap.org/data/2.5/forecast/climate?q=London&appid={a0d29d6b98aa51d3ae1fffc903feb7b9}")
weather_data <- weather_api_url %>%
  read_html() %>%
  #html_nodes(css = "#constituents") %>%
  #html_nodes(css = ".wikitable sortable jquery-tablesorter") %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()

weather_data

#### Spotify data

library(dplyr)
library(genius)
library(spotifyr)
library(plotly)
library(ggplot2)

id <- "8647ce1b4d46462190f98c846a7528f0"
secret <- "0bc57cdaf0564f9d8e260c58d36fdf64"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token

#playlists <- get_playlist(, authorization = access_token())
justin <- get_artist_audio_features("Davido")
justina <- justin %>%
  as_tibble()
justina


source("C:/Users/user/Documents/GitHub/ws20-business-data-science-basics---lab-journal-SamuelOwie/codes.R")
artist <- "Taylor Swift"
artist_ID <- get_artist_audio_features(artist)$artist_id[1]

artist_ID
albums <- get_artist_albums(artist_ID, include_groups = c("single"),
                            authorization = access_token()) %>%
  mutate(num = row_number())%>%
  as_tibble()

albums
##################################
url_home <- "https://www.rosebikes.com/bikes"
bikes_home <- url_home %>%
  read_html()

bike_family_tbl <- bikes_home %>%
  html_nodes(css = ".catalog-navigation__link")%>%
  html_attr('title') %>%
  discard(.p = ~stringr::str_detect(.x, "Sale"))%>%
  enframe(name = "position", value = "family_class") %>%
  mutate(family_id = str_glue("#{family_class}"))%>%
  mutate(url = glue("https://www.rosebikes.com/bikes/{family_class}"))

bike_category_url <- bike_family_tbl$url[1]
xopen(bike_category_url)

html_bike_category <- read_html(bike_category_url)
bike_url_tbl <- html_bike_category %>%
  html_nodes(css =".catalog-category-bikes__button") %>%
  html_attr("href") %>%
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "postion",value = "part_url") %>%
  mutate(url = glue("https://www.rosebikes.com{part_url}"))

bike_name_tbl <- html_bike_category %>%
  html_nodes(".catalog-category-bikes__title-text") %>%
  html_text() %>%
  enframe(name = "position", value = "Name")

bike_name_tbl
bike_desc_tbl <- html_bike_category %>%
  html_nodes(".catalog-category-bikes__subtitle") %>%
  html_text() %>%
  enframe(name = "position", value = "description")

bike_price_tbl <- html_bike_category %>%
  html_nodes(".catalog-category-bikes__price-title") %>%
  html_text() %>%
  enframe(name = "position", value = "price")


get_bike_data <- function(url){
  
  html_bike_category <- read_html(url)
  
  #urls
  bike_url_tbl <- html_bike_category %>%
    html_nodes(css =".catalog-category-bikes__button") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position",value = "part_url") %>%
    mutate(url = glue("https://www.rosebikes.com{part_url}"))
  
  #descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(".catalog-category-bikes__subtitle") %>%
    html_text() %>%
    str_remove(pattern = "\\n") %>%
    str_remove(pattern = "\"") %>%
    enframe(name = "position", value = "description")
  
  #prices
  bike_price_tbl <- html_bike_category %>%
    html_nodes(".catalog-category-bikes__price-title") %>%
    html_text() %>%
    str_remove(pattern = "\\n") %>%
    str_remove(pattern = "\"") %>%
    enframe(name = "position", value = "price") %>%
    left_join(bike_desc_tbl, by = character()) %>%
    left_join(bike_url_tbl, by = character()) %>%
    select(position, url, description, price) %>%
    as_tibble()
}

bike_category_url <- bike_family_tbl$url[2]
bike_data_tbl <- get_bike_data(url = bike_category_url) %>%
  select(position, url, description, price) %>%
  as_tibble()
bike_data_tbl

for(i in seq_along(bike_family_url$url)){
  bike_category_url <- bike_family_tbl$url[i]
  bike_data_tbl <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
  Sys.sleep(5)
  print(i)
}

saveRDS(rose_bikes_cleaned_data_tbl, "Business Data Science/00_data/rose_bikes_cleaned_data_tbl.rds")


bike_data_tbl

bike_price_tbl
bike_desc_tbl
bike_family_tbl
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
  
family_id_css
bike_family_tbl


bike_family_tbl <- bikes_home %>%
  html_nodes(css = ".main-navigation-category-with-tiles__title") %>%
  html_text()
  #as_tibble()
  #html_attr('id') %>%
  discard(.p = ~stringr::str_detect(.x, "WMN|WOMEN|GEAR|OUTLET"))%>%
  #enframe(name = "position", value = "family_class") %>%
  #mutate(family_id = str_glue("#{family_class}"))

glimpse(bike_family_tbl)

