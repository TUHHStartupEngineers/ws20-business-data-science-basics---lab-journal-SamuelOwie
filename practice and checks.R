library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)

con <- RSQLite::dbConnect(drv = SQLite(),
                          dbname = "Business Data Science/00_data/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")

album_tbl <- tbl(con, "Album") %>% collect()
album_tbl

songs <- dbGetQuery(con, 'SELECT * FROM Artist')
songs

dbDisconnect(con)
con

name <- "Fred"
glue('my name is {name}.')

check1 <- GET("https://swapi.dev/api/people/?page=2")
check1

resp <- GET("https://swapi.dev/api/people/1/")

resp

sw_api <- function(path){
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp)
}

resp1 <- sw_api("/people/1")
resp1
resp1$content
resp11 = rawToChar(resp$content)

convert_from_json = fromJSON(resp11)
convert_from_json
convert_to_json = toJSON(convert_from_json)
convert_to_json


data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)
data_list


resp1 %>%
  .$content %>%
  rawToChar() %>%
  fromJSON()


content(resp1, as = "text")
content(resp1)
content(resp1, as = "parsed")
content(resp1)

resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp

token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response

alpha_api_url <- "https://www.alphavantage.co/query"
ticker        <- "WDI.DE"

GET(alpha_api_url, query = list('function' = "GLOBAL_QUOTE",
                                symbol = ticker,
                                apikey = Sys.getenv('TOKEN'))
    )

#################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(writexl)
library(genius)
library(spotifyr)
library(plotly)
library(ggplot2)
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
library(data.table)
#############################
#part 4
bikes_tbl <- read_excel("Business Data Science/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ") %>%
  set_names(names(.) %>%
  str_replace_all("\\.", "_"))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>%
  rename(
    Model = model,
    'Bike Family' = category_1,
    'Ride Style' = category_2,
    'Bike Category' = category_3,
    'Price in Euro' = price
    
  )

bikes_tbl %>%
  select(model, price) %>%
  filter((price >5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price >5000, model %>% str_detect('Endurance')) %>%
  View()

bikes_tbl %>%
  filter(category_1 %in%c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_1 %in%c("Hybrid / City", "E-Bikes")))

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

#last five rows
bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-2):nrow(.))

bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)
  
glimpse(bikes_tbl)


bike_orderlines_tbl <- read_excel("Business Data Science/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)
bike_orderlines_tbl %>%
  mutate(freight_costs = 2*weight)%>%
  select(price, weight, freight_costs)

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price)) %>%
  select(total_price)

bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5) %>%
  select(price_log, price_sqrt)

bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  select(total_price, price_binned, everything())

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "LOW"
  )) %>%
  select(total_price, price_binned, price_binned2, everything())

bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate"
  )) %>%
  select(bike_type, everything())

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(revenue))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg = mean(total_price),
    med  = median(total_price),
    sd = sd(total_price),
    min = min(total_price),
    max = max(total_price)
    )%>%
  ungroup() %>%
  arrange(desc(count))

bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))))

bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))/length(.)))

bike_orderlines_missing %>%
  filter(!is.na(total_price))

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))

bikeshop_revenue_tbl
glimpse(bike_orderlines_tbl$category_1)
bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl%>%
  pivot_wider(names_from = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix =  "", suffix = "\u20AC"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix =  "", suffix = "\u20AC"),
    Road = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix =  "", suffix = "\u20AC"),
    'Hybrid / City' = scales::dollar('Hybrid / City', big.mark = ".", decimal.mark = ",", prefix =  "", suffix = "\u20AC"),
    'E-Bikes' = scales::dollar('E-Bikes', big.mark = ".", decimal.mark = ",", prefix =  "", suffix = "\u20AC")
                             )
bikeshop_revenue_formatted_tbl

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_times_tbl <- bike_orderlines_tbl %>% select(1:2, 4:8)

order_dates_tbl %>%
  left_join(y = order_times_tbl, by = c("order_id" = "order_id", "order_line" = "order_line"))

################
#Data Table

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
class(covid_data_dt)
  
test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', rown.names = F)
system.time({test_df_base <- read.csv("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)
test_dt

covid_data_dt %>%
  select(geoId, countryterritoryCode, continentExp,cases) %>%
  group_by(geoId, continentExp) %>%
  #group_by(countryterritoryCode, continentExp) %>%
  summarise(Number_of_Cases = sum(cases)) %>%
  ungroup() %>%
  arrange(desc(Number_of_Cases))

covid_data_dt[i, j, by]
#Take the data.table covid_data_dt, subset/reorder rows using i, 
#then calculate j, grouped by by. Let's begin by looking at i and j first - 
#subsetting rows and operating on columns

covid_data_dt[year == 2020, sum(cases), by = geoId]
covid_data_dt[order(year, month, day, countriesAndTerritories)]

data()
data("airquality")
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]
covid_data_dt[deaths > 1000]

#create single column
covid_data_dt[, deaths_per_capita := deaths/popData2019]
covid_data_dt[, deaths_per_capita, cases]

#create multiple columns at once
covid_data_dt[, ':='(deaths_per_capita = deaths / popData2019,
                     cases_per_capita = cases / popData2019,
                     deaths_per_cases = deaths / cases)]
covid_data_dt[, deaths_per_capita, cases_per_capita, deaths_per_cases]

covid_data_dt[, deaths_per_cases := NULL]

data("mtcars")
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg >20, 'hight', 'low')] %>%
  View()

glimpse(covid_data_dt)
covid_data_dt[countriesAndTerritories =="Germany" & month == 4,
              .(m_cases = mean(cases),
              m_death = mean(deaths))]

covid_data_dt[countriesAndTerritories == "Nigeria" &
                month == 12 & deaths > 1,
              length(day)]

covid_data_dt[deaths > 1000, .I, by = countriesAndTerritories]

covid_data_dt[continentExp == "Europe", 
              .(mean(cases), mean(deaths)),
              by = .(countryterritoryCode, month, year)]

covid_data_dt[, lapply(.SD, sum), 
              by = .(countriesAndTerritories,year, month),
              .SDcols = c("cases", "deaths")]


setkey(covid_data_dt, popData2019, countriesAndTerritories)
covid_data_dt$continentExp

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
covid_data_dt$countriesAndTerritories[8989:9000]


covid_data_dt[, continentExp]

####################################################
# Challenge 4
library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

#Data Import
col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))

acquisition_data <- vroom(
  file = "Business Data Science/loan_data/Acquisition_2019Q1.txt",
  delim = "|",
  col_names = names(col_types_acq),
  col_types = col_types_acq,
  na = c("", "NA", "NULL")
)

col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "Business Data Science/loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))
View(performance_data)

#important
class(acquisition_data)
setDT(acquisition_data)
class(acquisition_data)
acquisition_data %>% glimpse()

setDT(performance_data)
performance_data %>% glimpse()


#merge data
tic()
combined_data <- merge(x = acquisition_data, y = performance_data,
                       by = "loan_id",
                       all.x = TRUE,
                       all.y = FALSE)
toc()
combined_data %>% glimpse()

setkey(combined_data, "loan_id")
key(combined_data)
?setorder()
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))

combined_data %>% dim()

keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")

combined_data <- combined_data[, ..keep_cols]
combined_data %>% dim()
combined_data %>% glimpse()

tic()
temp <- combined_data %>%
  group_by(loan_id) %>%
  mutate(gt_1mo_behind_in_3mo_dplyr = lead(current_loan_delinquency_status, n = 3) >= 1) %>%
  ungroup()  
toc()

tic()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
toc()

rm(temp)

#how many loans are in each month
tic()
combined_data[!is.na(monthly_reporting_period), .N, by = monthly_reporting_period]
toc()

combined_data[current_loan_delinquency_status >= 1, list(loan_id, monthly_reporting_period, current_loan_delinquency_status,
                                                         max(current_loan_delinquency_status), by = loan_id)][order(V4, decreasing = TRUE)]

combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][
  !is.na(current_upb)][
    order(-current_upb), .(loan_id, monthly_reporting_period, current_loan_delinquency_status,
                           seller_name, current_upb)
  ]

upb_by_company_dt <- combined_data[!is.na(current_upb), .SD[.N], by = loan_id][
  , .(sum_current_upb = sum(current_upb, na.rm = TRUE), cnt_current_upb = .N), by = seller_name][
    order(sum_current_upb, decreasing = TRUE)
  ]

upb_by_company_dt







combined_data %>% dim()
