library(tidyverse)
library(here)

library(openxlsx)
library(readxl)

path2cache <- here("spreadsheets/cache/pIndex/")
path2corrections <- paste0(path2cache, "corrections-pIndex02-product_release.xlsx")

index2 <- read_csv(paste0(path2cache, "pIndex-02-1987to1992.csv"))

index2 %>% arrange(review_year, company, product) %>%
  # select(rowid, review, company, product_brand, product, price, price_year, review_year, pIndex_year) %>%
  # write_csv(paste0(path2cache, "pIndex-02-handcorrections.csv"))

index2 %>% mutate(
  release_year = review_year,
  release_price = price) %>% 
  select(rowid, review, company, product_brand, product, release_year, release_price, price_year, price) %>%
  arrange(company, product, release_year) %>% 
  write.xlsx(file = path2corrections)