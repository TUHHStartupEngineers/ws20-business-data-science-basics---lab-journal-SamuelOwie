library(tidyverse)
library(readxl)


#read data

bikes_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

#examine data
bikes_tbl
glimpse(orderlines_tbl)


left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))


bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id"="bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id"="bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse
bike_orderlines_joined_tbl$category


#print all unique entries that starts with Mountains
bike_orderlines_joined_tbl %>%
  select(category) %>%
  filter(str_detect(category, "^Road")) %>%
  unique()



#wrangling
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.21", "category.2", "category.3"),
           sep = " - ")%>%
mutate(total.price = price*quantity) %>%
select(-...1, -gender)%>%
select(-ends_with(".id"))
