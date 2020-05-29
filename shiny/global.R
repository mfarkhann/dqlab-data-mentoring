suppressPackageStartupMessages({
  library(dplyr)
  library(shinyjs)
})

df_credentials <- readRDS('data/credentials.rds')

judul <- "Shiny Example"

update_user_session <- function(user, session) {
  credentials <- readRDS("data/credentials.rds")
  row_email <- which(credentials$email == user)
  credentials$session[row_email]=session
  saveRDS(credentials, "data/credentials.rds") 
}

source('helper.R')