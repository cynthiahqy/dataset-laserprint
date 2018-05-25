library(here)
library(tidyverse)
library(validate)

# 1987

# load cached data, with restrictions from raw pages
# pIndex_spec <- spec_csv(here('cache-printerIndex','1987-printerIndex-00.csv'))

pIndex <- here('cache-printerIndex','1987-printerIndex-00.csv') %>%
  read_csv(col_types = cols(
    type = col_factor(levels = c("L")),
    speed_unit = col_factor(levels = c("ppm", "cps")),
    vol = col_factor(levels = 3:6),
    no = col_factor(levels = c(19, 23)),
    index_page = col_factor(levels = c(420, 421, 422, 427, 428))
    )) 

cols_1987 <- colnames(pIndex)

## SHOULD BE A FUNCTION! ------
# check factor cols for entry errors
select_if(pIndex, is.factor) %>% 
  #  colnames() %>%
  check_that(is.na())
lapply(c("type", "speed_unit"), !is.na())

check_factor <- function() {
  v_factor <- validator(!is.na(type), !is.na(speed_unit), !is.na(vol), !is.na(no), !is.na(index_page)) 
  confront(pIndex, v_factor) %>%
    summary()
}

# RESOLVE NOTE -------------
# check for notes
check_empty <- function(){
  v_na <- validator(is.na(pg), is.na(reader_service_number), is.na(note))
  confront(pIndex, v_na) %>%
    summary()
}

check_empty()

# view notes; correct all to NA

view_notes <- function() {
  pIndex[!(is.na(pIndex$note)),  ] %>%
    select(starts_with("company"), "product", "note")
}

view_notes()

# note 1: Canon product name LBP-8II, not LBP-811; typefont has different 1 and I
pIndex[pIndex$product == "LBP-8II", "note"] <- NA_character_

# note 2 & 3: Personal Comp. Products has double entry of "LaserImage 2000" 
## check in vol5-no19-pg282, 6-19-214
## Two distinct products -- different ppm
## leave as is.

pIndex[pIndex$product == "LaserImage 2000", "note"] <- NA_character_

# check no NA

view_notes()

# 1987-01 DROP COLUMNS -----------------
# Check pg, note, reader_service_number are NA

check_empty()

# drop empty, redundant columns

drop_csv <- function(year) {
  drop <- c("pg", "reader_service_number", "type", "note")
  pIndex_drop <- 
    pIndex %>% select(-one_of(drop))
  write_csv(pIndex_drop, here("cache-printerIndex", paste0(year, '-printerIndex-01.csv')))
}
  
drop_csv(year)

# 1988

pIndex <- here('cache-printerIndex','1988-printerIndex-00.csv') %>%
  read_csv(col_types = cols(
    type = col_factor(levels = c("L")),
    speed_unit = col_factor(levels = c("ppm", "cps")),
    vol = col_factor(levels = 3:7),
    no = col_factor(levels = c(18, 19, 23)),
    index_page = col_factor(levels = c(339, 340, 343, 344, 347, 349))
  )) 

cols_1988 <- colnames(pIndex)

check_factor()

check_empty()

# compare columns

drop <- c("pg", "reader_service_number", "type", "note")
pIndex_drop <- 
  pIndex %>% select(-one_of(drop))
