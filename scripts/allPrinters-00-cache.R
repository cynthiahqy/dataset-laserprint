# Cache Dropbox/handtype_writtenReview.xlsx sheets to allPrinters/

# INITIALISE SESSION ----
library(tidyverse)
library(readxl)

wbook <- here("spreadsheets/handtype_writtenReview.xlsx")
path2cache <- here("spreadsheets/cache/allPrinters/")

# split x1, remainder x1r
x1 <- c("source_vol", "source_no", "product", "product_brand", "company", "price_list", "price_street", "engine_brand", "product_type")

# cache to csv variables in x1
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", x1)] %>%
  write_csv(., paste0(path2cache, "allPrinters-x1.csv"))

# cache remaining variables
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", setdiff(names(.), x1))] %>%
  write_csv(., paste0(path2cache, "allPrinters-x1r.csv"))
