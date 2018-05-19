# Import, read and write to csv from handtype_printerIndex.xlsx into hand_csv

library(tidyverse)
library(readxl)

path <- "/Users/cynthiahqy/Dropbox/RA-LaserPrinter/images/printerIndex/handtype_printerIndex.xlsx"

# excel_sheets(allIndex)
# 
# read_excel(allIndex)

# path %>%
#   excel_sheets() %>%
#   set_names() %>%
#   map(read_excel, path = path)

read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0("hand_csv/",pathbase, "-", sheet, ".csv"))
}

path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_then_csv, path = path)

## imported 1987, 88, 99 on 19 May, 2017