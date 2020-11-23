#libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library("writexl")

#import files
bikes_tbl <- read_excel(path = "Business Data Science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "Business Data Science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel(path = "Business Data Science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

bikes_tbl
orderlines_tbl

#Data Wrangling
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>%
  select(category)%>%
  filter(str_detect(category, "^Gravel")) %>%
  unique()
bike_orderlines_joined_tbl$category

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ")%>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bike_orderlines_wrangled_tbl$location

bike_orderlines_wrangled_2_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ")%>%
  select(order_id, contains("order"), contains("model"), contains("category"),
         price, quantity, total_price,
         everything())
glimpse(bike_orderlines_wrangled_2_tbl)

#Solution 1
sales_by_state_tbl <- bike_orderlines_wrangled_2_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Є"))

  sales_by_state_tbl %>%
  ggplot(aes(x=state, y=sales))+
  geom_col(fill = "#58508d")+
  geom_label(aes(label=sales_text))+
  geom_smooth(method="lm", se=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  prefix = "",
                                                  suffix = " Є"))+
  labs(
    title = "Revenue by state",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )

#Solution 2
glimpse(bike_orderlines_wrangled_tbl)

sales_by_state_year_tbl <- bike_orderlines_wrangled_2_tbl %>%
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, state) %>%
  summarize(sales = sum(total_price)) %>%
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Є"))
  sales_by_state_year_tbl %>%
  ggplot(aes(x=year, y=sales, fill = state))+
  geom_col()+
  facet_wrap(~ state)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " Є"))+
  labs(
    title = "Revenue by Year and State",
    subtitle = "Each product category has an upward trend",
    fill = "States"
  )

#Optional
bike_orderlines_wrangled_2_tbl %>%
  write_xlsx("Business Data Science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
bike_orderlines_wrangled_2_tbl %>%
  write_xlsx("Business Data Science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
bike_orderlines_wrangled_2_tbl %>%
  write_xlsx("Business Data Science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

