library(tidyverse)
library(vroom)
library(data.table)
library(furrr)
library(tictoc)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)


########## Assignee ####################
assignee_col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "Business Data Science/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = assignee_col_types,
  na         = c("", "NA", "NULL")
)

assignee_clean_tbl <- assignee_tbl %>%
  as_tibble() %>%
  rename(assignee_id = id) %>%
  select(assignee_id, type, organization)


#convert to data.table
setDT(assignee_clean_tbl)
class(assignee_clean_tbl)

########Patent Assignee#######
patent_assignee_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "Business Data Science/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)


patent_assignee_clean_tbl <- patent_assignee_tbl %>%
  as_tibble() %>%
  select(assignee_id, patent_id)

#convert to data.table
setDT(patent_assignee_clean_tbl)
class(patent_assignee_clean_tbl)


combined_data_1_tbl <- merge(x = patent_assignee_clean_tbl, y = assignee_clean_tbl, 
                             by    = "assignee_id", 
                             all.x = TRUE, 
                             all.y = FALSE)


combined_data_1_cleaned_tbl <- combined_data_1_tbl %>%
  select(patent_id, type, organization)

saveRDS(combined_data_1_cleaned_tbl, "Business Data Science/02_data_wrangling/patent_1_cleaned_data_tbl.rds")



########### USPC ###########
uspc_col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
)


uspc_tbl <- vroom(
  file       = "Business Data Science/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = uspc_col_types,
  na         = c("", "NA", "NULL")
)


uspc_clean_tbl <- uspc_tbl %>%
  as_tibble() %>%
  select(patent_id, mainclass_id)

setDT(uspc_clean_tbl)
class(uspc_clean_tbl)



combined_data_2_tbl <- merge(x = uspc_clean_tbl, y = combined_data_1_cleaned_tbl, 
                             by    = "patent_id", 
                             all.x = TRUE, 
                             all.y = FALSE)

combined_data_2_cleaned_tbl <- combined_data_2_tbl %>%
  select(patent_id, type, organization,mainclass_id)

saveRDS(combined_data_2_cleaned_tbl, "Business Data Science/02_data_wrangling/patent_2_cleaned_data_tbl.rds")

####### Patent #####################
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "Business Data Science/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_clean_tbl <- patent_tbl %>%
  as_tibble() %>%
  rename(patent_id = id) %>%
  select(patent_id, date)

setDT(patent_clean_tbl)
class(patent_clean_tbl)


combined_data_3_tbl <- merge(x = patent_clean_tbl, y = combined_data_2_cleaned_tbl, 
                             by    = "patent_id", 
                             all.x = TRUE, 
                             all.y = FALSE)


combined_data_3_cleaned_tbl <- combined_data_3_tbl %>%
  select(patent_id, type, date, organization, mainclass_id)


saveRDS(combined_data_3_cleaned_tbl, "Business Data Science/02_data_wrangling/patent_3_cleaned_data_tbl.rds")


assignee_pa_p_uspc_data <- readRDS("Business Data Science/02_data_wrangling/patent_3_cleaned_data_tbl.rds")

assignee_pa_p_uspc_data_tbl <- as.data.table(assignee_pa_p_uspc_data)


clean_try <- assignee_pa_p_uspc_data[,year := lubridate::year(date)]
saveRDS(clean_try, "Business Data Science/02_data_wrangling/patent_3_cleaned_data_with_year_tbl.rds")

##################################################################

#Answer to question 1, combination of assignee and patent_assignee datasets
assignee_pa_data <- readRDS("Business Data Science/02_data_wrangling/patent_1_cleaned_data_tbl.rds")
assignee_pa_data_tbl <- as.data.table(assignee_pa_data)
assignee_pa <- assignee_pa_data_tbl[, .(COUNT = uniqueN(patent_id)), by = organization][order(-COUNT)]
assignee_pa_clean <- assignee_pa[-c(4)]
assignment_1_result <- head(assignee_pa_clean, 10)
view(assignment_1_result)
saveRDS(assignment_1_result, "Business Data Science/02_data_wrangling/assignment_1_result.rds")



#Answer to question 2, combination of assignee, patent_assignee, patent, and uspc datasets

assignee_pa_p_uspc_with_year_data <- readRDS("Business Data Science/02_data_wrangling/patent_3_cleaned_data_with_year_tbl.rds")
assignee_pa_p_uspc_data_with_year_tbl <- as.data.table(assignee_pa_p_uspc_with_year_data)
glimpse(assignee_pa_p_uspc_data_with_year_tbl)
assignee_pa_p_uspc_final_tbl <- assignee_pa_p_uspc_data_with_year_tbl[year == "2019", .(COUNT = uniqueN(patent_id)), by = organization][order(-COUNT)]
assignee_pa_p_uspc_final_tbl
assignee_pa_p_uspc_final_tbl_clean <- assignee_pa_p_uspc_final_tbl[-c(1)]
assignment_2_result <- head(assignee_pa_p_uspc_final_tbl_clean, 10)
assignment_2_result
saveRDS(assignment_2_result, "Business Data Science/02_data_wrangling/assignment_2_result.rds")



#Answer to question 3, combination of assignee, patent_assignee, and uspc datasets
assignee_pa_uspc_data <- readRDS("Business Data Science/02_data_wrangling/patent_2_cleaned_data_tbl.rds")
assignee_pa_uspc_data_tbl <- as.data.table(assignee_pa_uspc_data)
glimpse(assignee_pa_uspc_data_tbl)
assignee_pa_uspc_final_tbl <- assignee_pa_uspc_data_tbl[, .(COUNT = uniqueN(patent_id)), by = .(organization, type)][order(-COUNT)]


assignee_pa_uspc_clean <- assignee_pa_uspc_final_tbl[-c(1)]
assignment_3a_result <- head(assignee_pa_uspc_clean, 10)
assignment_3a_result
saveRDS(assignment_3a_result, "Business Data Science/02_data_wrangling/assignment_3a_result.rds")


assignee_pa_uspc_final_3b_tbl <- assignee_pa_uspc_data_tbl[, .(COUNT = .N), by = type][order(-COUNT)]
assignee_pa_uspc_3b_clean <- assignee_pa_uspc_final_3b_tbl[-c(3)]
assignment_3b_result <- head(assignee_pa_uspc_3b_clean, 5)
assignment_3b_result$patent_type <- c("US Company or Corporation", "Foreign Company or Corporation", "US  Federal Government", "US Individual", "Foreign Individual")
assignment_3b_result
saveRDS(assignment_3b_result, "Business Data Science/02_data_wrangling/assignment_3b_result.rds")


###################################################################
#View Results here
assignment_1_result<- readRDS("Business Data Science/02_data_wrangling/assignment_1_result.rds")
view(assignment_1_result)

assignment_2_result <- readRDS("Business Data Science/02_data_wrangling/assignment_2_result.rds")
view(assignment_2_result)

assignment_3a_result <- readRDS("Business Data Science/02_data_wrangling/assignment_3a_result.rds")
view(assignment_3a_result)

assignment_3b_result <- readRDS("Business Data Science/02_data_wrangling/assignment_3b_result.rds")
view(assignment_3b_result)
