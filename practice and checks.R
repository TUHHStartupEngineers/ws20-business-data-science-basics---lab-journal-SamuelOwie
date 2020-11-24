library(RSQLite)
library(dplyr)

con <- RSQLite::dbConnect(drv = SQLite(),
                          dbname = "Business Data Science/00_data/02_chinook/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")
