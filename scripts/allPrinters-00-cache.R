# Cache Dropbox/handtype_writtenReview.xlsx sheets to allPrinters/

# INITIALISE SESSION ----
library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(stringr)

path2cache <- here("spreadsheets/cache/allPrinters/")
path2correct <- here("spreadsheets/cache/corrections/")

# READ & CACHE handtype-writtenreviews -----
## load handtype
wbook <- here("spreadsheets/handtype_writtenReview.xlsx")
## split x1, remainder x1r
x1 <- c("source_vol", "source_no", "product", "product_brand", "company", "price_list", "price_street", "engine_brand", "product_type")

## cache to csv variables in x1
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", x1)] %>%
  mutate(product_brand = str_to_upper(product_brand),
         product_type = str_to_lower(product_type),
         engine_brand = str_to_upper(engine_brand)) %>%
  write_csv(., paste0(path2cache, "allPrinters-x1.csv"))

## cache remaining variables
read_excel(wbook, sheet = 3) %>%
  .[c("row_id", setdiff(names(.), x1))] %>%
  write_csv(., paste0(path2cache, "allPrinters-x1r.csv"))

# IMPORT x1 tibble for CLEANING

# df_x1 <- read_csv(paste0(path2cache, "allPrinters-x1.csv"), 
#          col_types = cols(
#            row_id = col_character(),
#            source_vol = col_integer(),
#            source_no = col_integer(),
#            product = col_character(),
#            product_brand = col_character(),
#            company = col_character(),
#            price_list = col_double(),
#            price_street = col_double(),
#            engine_brand = col_character(),
#            product_type = col_character()
#            )
#          )


# DATA CLEANING ----
## correct01: ADD parent_co, CORRECT product_brand ----
### import latest version of allprinters_x1

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1.csv"))

### create correction table for parent_co

unique_company <- 
  unique(all_printers$company) %>%
  sort() %>%
  as.tibble() %>%

colnames(unique_company) <- "company"

unique_company <-   mutate(unique_company,
  trun_company = str_remove(company, " (Inc\\.|Corp\\.|Co\\.|,$)"),
  parent_co = str_to_upper(str_remove(company, " .+"))) 

# rm_comptype <- function(x) {
#   str_remove(x, " (Inc\\.|Corp\\.|Co\\.)") %>%
#   str_remove(., ",$") 
# }

unique_company %>% write_csv(., paste0(path2correct, "company_names.csv"))

### create correction table for brands

unique_brand <- 
  unique(all_printers$product_brand) %>%
  sort() %>%
  as.tibble() 

colnames(unique_brand) <- "x1_brand"

unique_brand %>% 
  write_csv(., paste0(path2correct, "brand_names.csv"))

### read in corrections for brands and companies, merge with all_printers and cache

correct_company <- 
  read_csv(paste0(path2correct, "company_names-v01.csv")) %>%
  select(c("company", "parent_co")) %>%
  left_join(all_printers, ., by = "company")


correct_co_brand <- 
  read_csv(paste0(path2correct, "brand_names-v01.csv")) %>%
  select(c("x1_brand", "correct_brand")) %>%
  left_join(correct_company, ., by = c("product_brand" = "x1_brand")) %>% 
  mutate(product_brand = correct_brand) %>%
  select(-correct_brand)
  # filter(product_brand == "ABATON")

write_csv(correct_co_brand, paste0(path2cache, "allPrinters-x1-correct01.csv"))

## correct02: CORRECT engine_brand ----

### import latest version of allprinters_x1

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct01.csv"))

### create correction table for engine_brand (manufacturer)
unique_engine <- 
  unique(all_printers$engine_brand) %>%
  sort() %>%
  as.tibble()

colnames(unique_engine) <- "old.engine_brand"

unique_engine <-
  mutate(unique_engine, correct.engine_brand = old.engine_brand)
  
write_csv(unique_engine, paste0(path2correct, "engine_brands.csv"))

### read and merge corrections for engine_brand

correct_engine <-
  read_csv(paste0(path2correct, "engine_brands-v01.csv")) %>%
  select(c("old.engine_brand", "correct.engine_brand")) %>%
  left_join(all_printers, ., by = c("engine_brand" = "old.engine_brand")) %>%
  mutate(engine_brand = correct.engine_brand,
         correct.engine_brand = NULL)

write_csv(correct_engine, paste0(path2cache, "allPrinters-x1-correct02.csv"))

## correct03: merge price_list and price_street, reorder variables ----

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct02.csv"))

all_printers[is.na(all_printers$price_street), ]["price_street"] <- 0

### create price_max = list price, or if no list price use street price
add_price <- 
  mutate(all_printers, 
       price_max = pmax(price_list, price_street)) %>%
  select("row_id", starts_with("source"), "product", "product_brand", "parent_co", "engine_brand", "product_type", "price_max", starts_with("price"))
  
write_csv(add_price, paste0(path2cache, "allPrinters-x1-correct03.csv"))

## correct04: correct product_name ----

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct03.csv"))

unique_product <-
  unique(all_printers$product) %>%
  sort() %>%
  as.tibble()

colnames(unique_product) <- "old.product"

unique_product <-
  mutate(unique_product,
       new.product_name = str_to_upper(old.product))

write_csv(unique_product, paste0(path2correct, "product_names.csv"))

### read in product_name corrections

correct_product <- read_csv(paste0(path2correct, "product_names-v02.csv")) %>%
  select("old.product", "new.product_name") %>%
  left_join(all_printers, ., by = c("product" = "old.product")) %>%
  mutate(product = NULL) %>%
  rename(product_name = new.product_name)

write_csv(correct_product, paste0(path2cache, "allPrinters-x1-correct04.csv"))



