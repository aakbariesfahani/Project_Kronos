# Libraries ---------------------------------------------------------------

library(DBI)
library(RSQLite)
library(tidyverse)


# Data --------------------------------------------------------------------

data_src <- read_csv("data/src/tsall.csv")


# database ------------------------------------------------------------------

db <- dbConnect(RSQLite::SQLite(), dbname = "data/prod/climate.sqlite")


# PreProcessing -----------------------------------------------------------

data_prep <- function(dbName, df, type) {
     df_temp <- df %>%
          select(contains(type)) %>%
          mutate(year = 1:2010) %>%
          filter(year > 10)
     
     dbWriteTable(db, dbName, df_temp, overwrite = TRUE)
}

data_prep("pdsi", data_src, "PDSI")
data_prep("precipitation", data_src, "P")
data_prep("temprature", data_src, "_T")

