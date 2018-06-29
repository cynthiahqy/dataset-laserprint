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