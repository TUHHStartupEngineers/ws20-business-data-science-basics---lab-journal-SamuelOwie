#comp assignment 3
library(dplyr)
library(genius)
library(spotifyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)
library(furrr)
library(RSQLite)
library(httr)
library(keyring)
library(rstudioapi)
library(stringr)
library(purrr)

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

#bike_category_url <- bike_family_tbl$url[1]
#html_bike_category <- read_html(bike_category_url)

get_bike_data <- function(url){
  
  html_bike_category <- read_html(url)
  #bike_category_url <- bike_family_tbl$url[1]
  #html_bike_category <- read_html(bike_category_url)
  
  #urls
  bike_url_tbl <- html_bike_category %>%
    html_nodes(css =".catalog-category-bikes__button") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position",value = "part_url") %>%
    mutate(url = glue("https://www.rosebikes.com{part_url}"))
  
  #Bike Names
  bike_name_tbl <- html_bike_category %>%
    html_nodes(".catalog-category-bikes__title-text") %>%
    html_text() %>%
    str_remove(pattern = "\\n") %>%
    str_remove(pattern = "\"") %>%
    enframe(name = "position", value = "Name")
  
  #descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(".catalog-category-bikes__subtitle") %>%
    html_text() %>%
    str_remove(pattern = "\\n") %>%
    str_remove(pattern = "\"") %>%
    enframe(name = "position", value = "Description")
  
  #prices
  bike_price_tbl <- html_bike_category %>%
    html_nodes(".catalog-category-bikes__price-title") %>%
    html_text() %>%
    str_remove(pattern = "\\n") %>%
    str_remove(pattern = "\"") %>%
    enframe(name = "position", value = "Price") %>%
    left_join(bike_desc_tbl, by = character()) %>%
    left_join(bike_name_tbl, by = character()) %>%
    left_join(bike_url_tbl, by = character()) %>%
    select(Name, url, Description, Price) %>%
    as_tibble()
}
rose_bike_data_tbl <- tibble()
for(i in seq_along(bike_family_tbl$url)){
  bike_category_url <- bike_family_tbl$url[i]
  rose_bike_data_tbl <- bind_rows(rose_bike_data_tbl, get_bike_data(bike_category_url))
  Sys.sleep(5)
  print(i)
}

print(rose_bike_data_tbl, n=20)
glimpse(rose_bike_data_tbl)
saveRDS(rose_bike_data_tbl, "Business Data Science/00_data/rose_bike_data_tbl.rds")

