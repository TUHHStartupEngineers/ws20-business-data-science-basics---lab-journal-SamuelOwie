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















