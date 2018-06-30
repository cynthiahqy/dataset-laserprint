# Correct hand entry mistakes, and missing information

### INITIALISE ----
# load library

library(tidyverse)
library(here)
library(validate)

# load functions from pIndex-00-cache.R

path2cache <- here("spreadsheets/cache/pIndex/")

write_csv_no <- function(.data, sheet, no) {
  namebase <- "pIndex"
  write_csv(.data, paste0(path2cache, namebase, "-", no, "-", sheet, ".csv"))
}

# read dataframe

index2 <- read_csv(paste0(path2cache, "pIndex-02-ALL.csv"))

### FUZZY MATCHES

## initialise

l.unique <- list()
l.agrep <- list()
l.fuzzy_match1 <- list()

fuzzy_match1 <- function(df, var) {
  col_name <- deparse(substitute(var))
  col_vals <- eval(substitute(df))[[col_name]]
  unique_names <- col_vals %>% unique() %>% sort()
  n_pairs <- (length(unique_names) - 1)
  v.agrep <- c()
  for (i in c(1:n_pairs)) {
    v.agrep[i] <- agrepl(unique_names[i], unique_names[i + 1])
  }
  agrep_TRUE <- matrix(ncol = 4)
  for (i in which(v.agrep == TRUE)) {
    agrep_TRUE <- rbind(agrep_TRUE, c(i, unique_names[i], i + 1, unique_names[i + 1]))
    # print(i:(i + 1))
    # print(unique_names[i:(i + 1)])
  }
  # l.agrep[[col_name]] <<- v.agrep
  table.matches <- agrep_TRUE[-1,1:4]
  colnames(table.matches) <- c("index1", "name1", "index2", "name2")
  l.fuzzy_match1[[col_name]] <<- as.tibble(table.matches)
}

fuzzy_match1(index2, company)
fuzzy_match1(index2, product)
l.fuzzy_match1

fuzzy_match1(unique(index2$company) %>% sort())

test <- function(df, var) {
  eval(substitute(df))[[deparse(substitute(var))]]
}

eval(substitute(index2))[[deparse(substitute(company))]]

eval(as.name("index2"))$company

# Company

## write loop agrep(company[i], company[i + 1]) to find typos

unique_company <- unique(index2$company) %>% sort()
agrep_company <- c()


fuzzy_match <- function(v_chr) {
  
}
for (i in c(1:(length(unique_company) - 1))) {
  # print(i)
  agrep_company[i] <- agrepl(unique_company[i], unique_company[i + 1])
}

for (i in which(agrep_company == TRUE)) {
  print(i:(i + 1))
  print(unique_company[i:(i + 1)])
}

## TODO: corrections for unique company typos (store as cache?)

### test

is.null(length(agrep_company))



## TODO: check product names
unique_product <- unique(index2$product) %>% sort()

## TODO: check product name matches with same price year (1987), but different prices
# use unique_product && year == 1987 to subset products, then price[i] == price[i + 1] for i:length[subset -1]





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