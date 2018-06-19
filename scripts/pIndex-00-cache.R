# Import from Dropbox/handtype_printerIndex.xlsx to .Rproj/cache

library(tidyverse)
library(readxl)
library(here)

wbook <- here("spreadsheets/handtype_printerIndex.xlsx")
path2cache <- here("spreadsheets/cache/pIndex/")

write_csv_no <- function(.data, sheet, no) {
  namebase <- "pIndex"
  write_csv(.data, paste0(path2cache, namebase, "-", no, "-", sheet, ".csv"))
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

(names_csv <- list.files(path2cache, pattern=".*pIndex-00-.*"))

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
  gather(z.original_price, z.current_price, key = "c.price_type", value = "price") %>%
  mutate(price_year = c.review_year)   ## default for price_type == z.original_price

# set price_year <- c.index_year if c.price_type == z.current_price
l.index1$`1987`[l.index1$`1987`$c.price_type == "z.current_price", "price_year"] <- 1987

# create c.conversion variable
# add speed_ppm variable
l.index1$`1987` <- inner_join(l.index1$`1987`, cps2ppm) %>%
  mutate(speed_ppm = z.speed * c.conversion,
         company = z.company,
         product = z.product) %>%
  order_index1()

# TIDY 1988----

l.index1$`1988` <- l.index0$`1988` %>%
  inner_join(cps2ppm) %>%
  mutate(company = z.company_name,
         product = z.product,
         price_year = c.index_year, #default is price_type == current_price
         price = z.current_price,
         speed_ppm = z.speed * c.conversion) %>%
  order_index1()

# TIDY 1989 to 1992----

tidy_index1 <- function(x) {
  inner_join(x, cps2ppm) %>%
    mutate(company = z.company_name,
           product = z.product,
           price_year = c.index_year,
           price = z.price,
           z.speed = as.numeric(z.speed),
           speed_ppm = z.speed * c.conversion) %>%
    order_index1()
}

l.index1[3:6] <- lapply(l.index0[3:6], tidy_index1)

bind_rows(l.index1) %>%
  unite(issue, z.vol, z.no, sep = "-") %>%
  select(c(issue, company, product, price_year, price, speed_ppm)) %>%
  write_csv(paste0(path2cache, "pIndex-02-ALL.csv"))

# Exploration----

index2 <- read_csv(paste0(path2cache, "pIndex-02-ALL.csv"))





# VALIDATE



## LIST UNIQUE COMPANIES----
# TODO: count no. of products / company / year

# list_companies <- function(x) {
#   unique(x$z.company_name) %>%
#     sort()
# }
# 
# l.companies <- lapply(l.index, list_companies)
# 
# 
# lapply(l.companies, View)
# 
# l.index$`1989` %>% View()



  
  



