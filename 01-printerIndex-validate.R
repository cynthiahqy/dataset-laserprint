library(validate)
library(here)
library(tidyverse)

# load cached data, with restrictions from raw pages
# pIndex_spec <- spec_csv(here('cache-printerIndex','1987-printerIndex-00.csv'))


pIndex <- here('cache-printerIndex','1987-printerIndex-00.csv') %>%
  read_csv(col_types = cols(
    type = col_factor(levels = c("L")),
    speed_unit = col_factor(levels = c("ppm", "cps")),
    vol = col_factor(levels = 3:6),
    no = col_factor(levels = c(19, 23)),
    index_page = col_factor(levels = c(420, 421, 422, 427, 428))
    )
  )

summary(pIndex)



