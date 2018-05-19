library(tidyverse)
library(stringr)

index_1987 <- read_csv('hand_csv/handtype_printerIndex-1987.csv')

## rename columns
new_cols <- function(x) {
  x %>%
  str_to_lower() %>%
  str_replace_all(" ","_") %>%
  str_remove_all('\\.')
}

index_1987 %>% 
  rename_all(new_cols)

new_cols(colnames(index_1987))

## Check for input errors
index_1987 %>%
  summary()

ggplot(data = index_1987) + 
  geom_point(mapping = aes(x = `speed`, y = `original-price`))

## Define
# Columns to Drop
drop <- c("Replaced By","Pg.","Reader service number", "Note", "Type") %>%
  new_cols

index_1987 %>%
  select(-one_of(drop))


## Transform
# cps to ppm

volumes <- c(3:8)
years <- c(1984:1989)

db_volumeYear <- cbind(volumes, years)


