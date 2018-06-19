# Import from Dropbox/handtype_printerIndex.xlsx to .Rproj/cache

library(tidyverse)
library(readxl)
library(here)

wbook <- here("spreadsheets/handtype_printerIndex.xlsx")
path2cache <- here("spreadsheets/cache/pIndex/")

write_csv_no <- function(.data, sheet, no) {
  namebase <- "pIndex"
  write_csv(.data, paste0(path2cache, namebase, "-", sheet, "-", no, ".csv"))
}

# READ xlsx to cache----
# function to read, rename, and write to csv
# generate new column names (z denotes hand entered data)
read_rename_csv <- function(sheet, path) {
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
    write_csv_no(sheet = sheet, no = "00")
    #write_csv(paste0(path2cache, namebase, "-", sheet, "-00.csv"))
}

# pipe xlsx to map(read_rename_csv)
wbook %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_rename_csv, path = wbook)


# IMPORT cache for TIDY----

# read cached files back into R

names_csv <- list.files(path2cache, pattern="*00.csv")

l.index0 <- lapply(str_c(path2cache, names_csv), read_csv) %>% 
  set_names(1987:1992)

l.index1 <- list()

# SPECIFY tidy targets and rules

# specify conversion from cps to ppm
cps2ppm <- tribble(
  ~z.speed_unit, ~c.conversion,
  "cps", 1/16,
  "ppm", 1
)

# order tibble for cache-01
order_index1 <- function(x) {
  select(x, c(company, product, price_year, price, speed_ppm), starts_with("c."), starts_with("z."))
}

# TIDY 1987----

# gather c.price_type (doubles length of tibble)
# add variable price_year

l.index1$`1987` <- l.index0$`1987` %>% 
  rename(z.company_name = z.company) %>%
  gather(z.original_price, z.current_price, key = "c.price_type", value = "price") %>%
  mutate(price_year = c.review_year) ## default for price_type == z.original_price

# set price_year <- c.index_year if c.price_type == z.current_price
l.index1$`1987`[l.index1$`1987`$c.price_type == "z.current_price", "price_year"] <- 1987

# create c.conversion variable
# add speed_ppm variable
l.index1$`1987` <- full_join(l.index1$`1987`, cps2ppm) %>%
  mutate(speed_ppm = z.speed * c.conversion,
         company = z.company_name,
         product = z.product) %>%
  order_index1()

l.index1$`1987` %>% 
  write_csv_no(sheet = "1987", no = "01")

# TIDY 1988----

l.index1$`1988` <- l.index0$`1988` %>%
  full_join(cps2ppm) %>%
  mutate(company = z.company_name,
         product = z.product,
         price_year = c.index_year, #default is price_type == current_price
         price = z.current_price,
         speed_ppm = z.speed * c.conversion) %>%
  order_index1()

l.index1$`1988` %>% 
  write_csv_no(sheet = "1988", no = "01")

# TIDY 1989----


l.index0$`1989` %>%
  full_join(cps2ppm) %>%
  mutate(company = z.company_name,
       product = z.product,
       price_year = c.index_year,
       price = z.price,
       speed_ppm = as_numeric(z.speed) * c.conversion) %>%
  order_index1() %>% 
  write_csv_no(sheet = "1989", no = "01")

# TIDY 1990----

l.index0$`1990` %>%
  full_join(cps2ppm) %>%
  mutate(company = z.company_name,
         product = z.product,
         price_year = c.index_year,
         price = z.price,
         speed_ppm = as.numeric(z.speed) * c.conversion) %>% #NA caused z.speed to parse as <chr>
  order_index1()

# TIDY 1991

l.index0$`1991` %>%
  full_join(cps2ppm) %>%
  mutate(company = z.company_name,
         product = z.product,
         price_year = c.index_year,
         price = z.price,
         speed_ppm = as.numeric(z.speed) * c.conversion) %>%
  order_index1()  

# TIDY 1992

l.index0$`1992` %>%
  full_join(cps2ppm) %>%
  mutate(company = z.company_name,
         product = z.product,
         price_year = c.index_year,
         price = z.price,
         speed_ppm = as.numeric(z.speed) * c.conversion) %>%
  order_index1()  

# VALIDATE
## TODO: check that index1 dimensions are 15 variables wide



## LIST UNIQUE COMPANIES----
# TODO: count no. of products / company / year

list_companies <- function(x) {
  unique(x$z.company_name) %>%
    sort()
}

l.companies <- lapply(l.index, list_companies)


lapply(l.companies, View)

l.index$`1989` %>% View()



  
  



