# Import from Dropbox/handtype_printerIndex.xlsx to .Rproj/cache

library(tidyverse)
library(readxl)
library(here)

wbook <- here("spreadsheets/handtype_printerIndex.xlsx")

# function to read, rename, and write to csv
# generate new column names
read_rename_csv <- function(sheet, path) {
  namebase <- "pIndex"
  new_cols <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all(" ","_") %>%
      str_remove_all('\\.')
  }
  path %>%
    read_excel(sheet = sheet) %>% 
    rename_all(new_cols) %>%
    write_csv(paste0(here(),"/spreadsheets/cache/", namebase, "-", sheet, "-00.csv"))
}

# pipe xlsx to map(read_rename_csv)
wbook %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_rename_csv, path = wbook)

## imported 1987, 88, 99 on 19 May, 2017 > cache-printerIndex/yyyy-printIndex-00.csv