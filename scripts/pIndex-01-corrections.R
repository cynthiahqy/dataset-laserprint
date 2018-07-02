# Correct hand entry mistakes, and missing information

### INITIALISE ----
# load library

library(tidyverse)
library(here)
# library(validate)
library(openxlsx)

# load functions from pIndex-00-cache.R

path2cache <- here("spreadsheets/cache/pIndex/")

write_csv_no <- function(.data, sheet, no) {
  namebase <- "pIndex"
  write_csv(.data, paste0(path2cache, namebase, "-", no, "-", sheet, ".csv"))
}

# read dataframe

index1 <- read_csv(paste0(path2cache, "pIndex-01-1987to1992.csv"))

### FUZZY MATCHES ----

## initialise

l.fuzzy_match1 <- list()
l.unique <- list()

fuzzy_match1 <- function(df, var) {
  col_name <- deparse(substitute(var))
  col_vals <- eval(substitute(df))[[col_name]]
  unique_names <- col_vals %>% unique() %>% sort()
  n_pairs <- (length(unique_names) - 1)
  v.agrep <- c()
  for (i in c(1:n_pairs)) {
    v.agrep[i] <- agrepl(unique_names[i], unique_names[i + 1])
  }
  agrep_TRUE <- c()
  for (i in which(v.agrep == TRUE)) {
    agrep_TRUE <- rbind(agrep_TRUE, unique_names[i], unique_names[i + 1])
  }
  # table.matches <- agrep_TRUE[-1,1:2]
  str(agrep_TRUE)
  colnames(agrep_TRUE) <- c(paste0(col_name, "-index1"))
  unique_names <- as.matrix(unique_names)
  colnames(unique_names) <- c(col_name)
  l.fuzzy_match1[[col_name]] <<- as.tibble(agrep_TRUE)
  l.unique[[col_name]] <<- as.tibble(unique_names)
}

## check names company, product

fuzzy_match1(index1, company)
fuzzy_match1(index1, product)
l.fuzzy_match1

write.xlsx(l.fuzzy_match1, file = paste0(path2cache, "pIndex-01-fuzzy_match1-(", lubridate::today(), ").xlsx"))

## TODO: corrections for unique company typos

l.unique 

l.unique$company <- l.unique$company %>%
  mutate(u.company = str_to_lower(company))

l.unique$product <- l.unique$product %>%
  mutate(u.product = str_to_lower(product)) 

make_u_variable <- function(df, var) {
  eval(substitute(df))[[deparse(substitute(var))]] %>%
    mutate()
}

write.xlsx(l.unique, file = paste0(path2cache, "pIndex-01-unique-(", lubridate::today(), ").xlsx"))


## TODO: check product name matches with same price year (1987), but different prices
# use unique_product && year == 1987 to subset products, then price[i] == price[i + 1] for i:length[subset -1]


