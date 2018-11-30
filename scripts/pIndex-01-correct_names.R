# Correct hand entry mistakes, and missing information

### INITIALISE ----
# load library

library(tidyverse)
library(here)
# library(validate)
library(openxlsx)
library(readxl)

# load functions from pIndex-00-cache.R

path2cache <- here("spreadsheets/cache/pIndex/")

# read dataframe

index1 <- read_csv(paste0(path2cache, "pIndex-01-1987to1992.csv"))

### FUZZY MATCHES ----

## initialise

l.unique <- list()

# u. upper case, low. lower case, uc. unique corrections
list_unique <- function(df, var) {
  col_name <- deparse(substitute(var))
  col_vals <- eval(substitute(df))[[col_name]]
  unique_names <- col_vals %>% unique() %>% sort() %>% as.tibble() 
  unique_names[[2]] <- str_to_upper(unique_names[[1]])
  unique_names[[3]] <- str_to_lower(unique_names[[1]])
  unique_names[[4]] <- unique_names[[3]]
  colnames(unique_names) <- c(col_name, paste0("u.", col_name), paste0("low.", col_name), paste0("uc.", col_name))
  l.unique[[col_name]] <<- unique_names
}

l.fuzzy_match1 <- list()

fuzzy_match1 <- function(df, var, colno) {
  col_name <- deparse(substitute(var))
  unique_names <- eval(substitute(df))[[col_name]][[colno]]
  n_pairs <- (length(unique_names) - 1)
  v.agrep <- c()
  for (i in c(1:n_pairs)) {
    v.agrep[i] <- agrepl(unique_names[i], unique_names[i + 1])
  }
  agrep_TRUE <- c()
  for (i in which(v.agrep == TRUE)) {
    agrep_TRUE <- rbind(agrep_TRUE, c(i, str_to_lower(unique_names[i]), i + 1, str_to_lower(unique_names[i + 1])))
    # print(i:(i + 1))
    # print(unique_names[i:(i + 1)])
  }
  # l.agrep[[col_name]] <<- v.agrep
  table.matches <- agrep_TRUE[-1,1:4]
  colnames(table.matches) <- c("index1", "name1", "index2", "name2")
  l.fuzzy_match1[[col_name]] <<- as.tibble(table.matches) 
}

## List unique company, product names

list_unique(index1, company)
list_unique(index1, product)
l.unique

path2corrections <- paste0(path2cache, "corrections-pIndex01-unique.xlsx")

write.xlsx(l.unique, file = path2corrections)

## Run fuzzy match

fuzzy_match1(l.unique, company, 1)
fuzzy_match1(l.unique, product, 1)
l.fuzzy_match1

write.xlsx(l.fuzzy_match1, file = paste0(path2cache, "reference-pIndex01-fuzzy_match1.xlsx"))


### READ IN CORRECTIONS ----

wbook <- paste0(path2cache, "corrections-pIndex01-unique.xlsx")
lookup2index2 <- wbook %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = wbook)

index2_cache_allvars <- index1 %>% 
  left_join(lookup2index2$company, by = "company") %>% 
  left_join(lookup2index2$product, by = "product") 

index2_cache_allvars %>% write_csv(paste0(path2cache, "pIndex-01-allvars.csv"))

index2 <- index2_cache_allvars %>%
  mutate(company = str_to_upper(uc.company),
         product = str_to_upper(uc.product),
         product_brand = str_to_upper(brand)) %>%
  select(-one_of(c("u.company", "correction 1", "uc.company", "low.company", "u.product", "low.product", "uc.product", "brand note", "brand"))) 

index2 %>% write_csv(paste0(path2cache, "pIndex-02-1987to1992.csv"))



## TODO: check product name matches with same price year (1987), but different prices
# use unique_product && year == 1987 to subset products, then price[i] == price[i + 1] for i:length[subset -1]


