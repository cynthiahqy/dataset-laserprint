# Cache Dropbox/handtype_writtenReview.xlsx sheets to allPrinters/

# INITIALISE SESSION ----
library(tidyverse)
library(readxl)
library(here)

wbook <- here("spreadsheets/handtype_writtenReview.xlsx")
path2cache <- here("spreadsheets/cache/allPrinters/")

# split x1, remainder x1r
x1 <- c("source_vol", "source_no", "product", "product_brand", "company", "price_list", "price_street", "engine_brand", "product_type")
path2x1 <- paste0(path2cache, "allPrinters-x1.csv")

# cache to csv variables in x1
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", x1)] %>%
  write_csv(., path2x1)

# cache remaining variables
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", setdiff(names(.), x1))] %>%
  write_csv(., paste0(path2cache, "allPrinters-x1r.csv"))

# IMPORT x1 tibble for CLEANING

df_x1 <- read_csv(path2x1, 
         col_types = cols(
           row_id = col_character(),
           source_vol = col_integer(),
           source_no = col_integer(),
           product = col_character(),
           product_brand = col_factor(levels = NULL),
           company = col_character(),
           price_list = col_double(),
           price_street = col_double(),
           engine_brand = col_factor(levels = NULL),
           product_type = col_factor(levels = NULL)
           )
         )




  