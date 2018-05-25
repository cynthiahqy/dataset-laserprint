library(here)
library(tidyverse)
library(validate)

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

# check factor counts
summary(pIndex)

# view notes; correct all to NA

view_notes <- function() {
  pIndex[!(is.na(pIndex$note)), ] %>%
    select(c(company, product, note))
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

check_that(pIndex, is.na(note)) %>%
  summary()



# drop columns
pIndex[[1]] %>%
  drop <- c("replaced_by", "pg", "reader_service_number", "type")

