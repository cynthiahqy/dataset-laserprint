# Cache Dropbox/handtype_writtenReview.xlsx sheets to allPrinters/

# INITIALISE SESSION ----
library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(stringr)

wbook <- here("spreadsheets/handtype_writtenReview.xlsx")
path2cache <- here("spreadsheets/cache/allPrinters/")
path2correct <- here("spreadsheets/cache/corrections/")

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
           product_brand = col_character(),
           company = col_character(),
           price_list = col_double(),
           price_street = col_double(),
           engine_brand = col_character(),
           product_type = col_character()
           )
         ) %>%
  mutate(product_brand = str_to_upper(product_brand),
         product_type = str_to_lower(product_type),
         engine_brand = str_to_upper(engine_brand))



## DATA CLEANING 

all_printers <- df_x1

# create parent_co

uni_company <- 
  unique(all_printers$company) %>%
  sort() %>%
  as.tibble()

colnames(uni_company) <- "company"

rm_comptype <- function(x) {
  str_remove(x, " (Inc\\.|Corp\\.|Co\\.)") %>%
  str_remove(., ",$") 
}

uni_company <- mutate(uni_company, 
                      trun_company = str_remove(company, " (Inc\\.|Corp\\.|Co\\.|,$)"),
                      parent_co = str_to_upper(str_remove(company, " .+"))) 

uni_company %>% write_csv(., paste0(path2correct, "company_names.csv"))

# read in corrections

cor_company <- read_csv(paste0(path2correct, "company_names-v01.csv"))




# group by product_brand
group_by(all_printers, company, product_brand) %>%
  summarise(n_product = n()) %>%
  summarise(n_brands = n()) %>% View()

group_by(all_printers, product_brand) %>%

all_printers %>% count(product_brand)

# check for typos

list_unique <- function(df, var) {
  col_name <- deparse(substitute(var))
  col_vals <- eval(substitute(df))[[col_name]]
  unique_names <- col_vals %>% unique() %>% sort() %>% as.tibble() 
  unique_names[[2]] <- unique_names[[1]]
  colnames(unique_names) <- c(col_name, paste0(col_name), paste0("cor.", col_name))
  l.unique[[col_name]] <<- unique_names
}

# unique product_types
uni.product_type <- df_x1[["product_type"]] %>% unique() %>% sort()
uni.product_brand <- df_x1[["product_brand"]] %>% unique() %>% sort()
list_unique(df_x1, product_brand) %>% View()

## product_brand --> input N/A, correct duplications
# generate binary var: debut_brand = 1 if source_vol = min(source_vol given product_brand)
# generate binary var: fade_brand
df_x1 %>%
  ggplot(mapping = aes(x = source_vol, y = product_brand)) +
  geom_point() +
  xlim(3, 17)

df_x1 %>%
  ggplot(mapping = aes(x = source_vol, y = company)) +
  geom_point() +
  xlim(3, 17)


## EXPERIMENTAL DATA VIZ

all_printers <- df_x1
# group by engine_brand

by_engine <- all_printers %>% 
  filter(engine_brand != "?") %>%
  group_by(engine_brand) 

# plots number of reviewed prints with engines from brand, and which issues the engine_brand was present
add_count(by_engine) %>%
  # filter(n > 10) %>%
  ggplot(data = ., mapping = aes(y = n, x = source_vol, color = engine_brand, shape = n > 5)) +
  geom_point()


# price scatterplot over time
ggplot(data = all_printers) +
  geom_point(mapping = aes(x = source_vol, y = price_list, position = "jitter"))

df_x1 %>%
  filter(price_list < 10000) %>%
  ggplot() +
  geom_smooth(mapping = aes(x = source_vol, y = price_list))

# max, min, median of price over time [ ISSUE WITH INCLUSION OF BUSINESS MACHINES ]
ggplot(data = df_x1) + 
  stat_summary(
    mapping = aes(x = source_vol, y = price_list),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# number of printers reviewed
ggplot(data = df_x1) + 
  stat_count(mapping = aes(x = source_vol)) 


ggplot(data = df_x1) +
  stat_count(mapping = aes(x = engine_brand))
  