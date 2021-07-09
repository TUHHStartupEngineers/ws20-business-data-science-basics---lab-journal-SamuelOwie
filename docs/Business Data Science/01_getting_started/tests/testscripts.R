library(tidyverse)
diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>% head(n=5)

diamonds3 %>%
  pivot_wider(names_from = "dimension",
               values_from = "measurement") %>%
                
  head(n=5)               

diamonds4 <- readRDS("diamonds4.rds")
diamonds4

diamonds4 %>%
  separate(col = dim,
           into = c("x","y","z"),
           sep = "/",
           convert = T)


diamonds5 <- readRDS("diamonds5.rds")
diamonds5

diamonds5 %>%
  unite(clarity, clarity_prefix, clarity_suffix, sep='')

library(ggplot2)
library(dplyr)

diamonds %>%
  filter(cut == 'Ideal' | cut == 'Premium', carat >=0.23) %>%
  head(5)

diamonds %>%
  filter(cut == 'Ideal' | cut == 'Premium', carat >=0.23) %>%
  slice(3:4)

diamonds %>%
  arrange(cut, carat, desc(price))

diamonds %>%
  select(color, clarity, x:z) %>%
  head(n=5)

diamonds %>%
  select(-(x:z)) %>%
  head(n=5)

diamonds %>%
  select(x:z, everything())%>%
  head(n=5)

glimpse((diamonds))

####dates
library(lubridate)
ymd(20101215)
mdy("4/1/17")

bday <- dmy("14/10/1979")
month(bday)
year(bday)




