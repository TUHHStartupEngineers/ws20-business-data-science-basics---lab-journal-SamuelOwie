library(genius)
library(dplyr)
library(spotifyr)

id <- "8647ce1b4d46462190f98c846a7528f0"
secret <- "0bc57cdaf0564f9d8e260c58d36fdf64"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token


