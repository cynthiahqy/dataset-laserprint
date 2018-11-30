library(tidyverse)
library(here)

library(openxlsx)
library(readxl)

path2cache <- here("spreadsheets/cache/pIndex/")
path2handtype <- here("spreadsheets/handtype/")
path2corrections <- paste0(path2cache, "corrections-pIndex02-product_release.xlsx")
path2printerguide <- paste0(path2handtype, "handtype-pGuide.xlsx")

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

pGuide_cols <- function(x) {
  x %>%
    str_c("pI02.", .)
}

pGuide_table <- index2 %>% select(c("review", "review_year", "company", "product_brand", "product", "price_year", "price")) %>% 
  arrange(review_year) %>% 
  filter(price_year == review_year) %>%
  rename_all(pGuide_cols) %>%
  mutate(cor.release_year = pI02.review_year,
         cor.release_price = pI02.price,
         cor.product_brand = "see corrections-pIndex01-unique.xlsx")

productlist_by_reviewyear <- function(x) {
  pGuide_table[pGuide_table$pI02.review_year == x, ]
}

l.pGuide <- lapply(c(1984:1992), productlist_by_reviewyear) %>% set_names(1984:1992)
  
  
l.pGuide %>% write.xlsx(file = path2printerguide)