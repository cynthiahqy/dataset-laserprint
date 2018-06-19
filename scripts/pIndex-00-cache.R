# Import from Dropbox/handtype_printerIndex.xlsx to .Rproj/cache

library(tidyverse)
library(readxl)
library(here)

wbook <- here("spreadsheets/handtype_printerIndex.xlsx")
path2cache <- here("spreadsheets/cache/pIndex/")

# function to read, rename, and write to csv
# generate new column names (z denotes hand entered data)
read_rename_csv <- function(sheet, path) {
  namebase <- "pIndex"
  new_cols <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all(" ", "_") %>%
      str_remove_all('\\.') %>%
      str_c("z.", .)
  }
  path %>%
    read_excel(sheet = sheet) %>% 
    rename_all(new_cols) %>%
    mutate(
      c.index_year = sheet,
      c.review_year = z.vol + 1981,
      z.reader_service_number = NULL,
      z.pg = NULL,
      z.note = NULL,
      z.type = NULL,
      z.company_name_note = NULL,
      z.editors_choice = NULL,
      z.index_page = NULL,
      z.review_note = NULL,
      z.replaced_by = NULL
      ) %>%
    write_csv(paste0(path2cache, namebase, "-", sheet, "-00.csv"))
}

# pipe xlsx to map(read_rename_csv)
wbook %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_rename_csv, path = wbook)

# read cached files back into R

names_csv <- list.files(path2cache, pattern="*00.csv")
l.index <- lapply(str_c(path2cache, names_csv), read_csv) %>% 
  set_names(1987:1992)

# price_type2year(l.index$`1987`)
# 
# price_type2year()
# mutate(
#   flag_original = (price_type == "z.original_price"),
#   price_date = flag_original * c.index_year
# )

# gather c.price_type (doubles length of tibble)
# add variable price_year

l.index$`1987` <- l.index$`1987` %>% 
  rename(z.company_name = z.company) %>%
  mutate(price_year = c.review_year) %>%
  gather(z.original_price, z.current_price, key = "c.price_type", value = "price")

# set price_year <- c.index_year if c.price_type == z.current_price
l.index$`1987`[l.index$`1987`$c.price_type == "z.current_price", "price_year"] <- 1987

# specify conversion from cps to ppm
cps2ppm <- tribble(
  ~z.speed_unit, ~c.conversion,
  "cps", 1/16,
  "ppm", 1
)
# create c.conversion variable
# add speed_ppm variable
full_join(l.index$`1987`, cps2ppm) %>%
  mutate(speed_ppm = z.speed * c.conversion)

list_companies <- function(x) {
  unique(x$z.company_name) %>%
    sort()
}

l.companies <- lapply(l.index, list_companies)

lapply(l.companies, View)

l.index$`1989` %>% View()



  
  



