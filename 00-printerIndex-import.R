# Import from Dropbox/handtype_printerIndex.xlsx to .Rproj/cache

library(tidyverse)
library(readxl)

wbook <- "/Users/cynthiahqy/Dropbox/RA-LaserPrinter/images/printerIndex/handtype_printerIndex.xlsx"

# generate new column names
new_cols <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all(" ","_") %>%
    str_remove_all('\\.')
}

# namebase <- wbook %>%
#   basename() %>%
#   tools::file_path_sans_ext()

namebase <- "printerIndex"

# function to read, rename, and write to csv
read_rename_csv <- function(sheet, path, namebase) {
  path %>%
    read_excel(sheet = sheet) %>% 
    rename_all(new_cols) %>%
    write_csv(paste0("cache-printerIndex/", sheet, "-", namebase, "-00.csv"))
}

# pipe xlsx to map(read_rename_csv)
wbook %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_rename_csv, path = wbook, namebase = namebase)

## imported 1987, 88, 99 on 19 May, 2017 > cache-printerIndex/yyyy-printIndex-00.csv