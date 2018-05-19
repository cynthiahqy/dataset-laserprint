library(tidyverse)
library(stringr)

index_1987 <- read_csv('hand_csv/handtype_printerIndex-1987.csv')

## ------ rename columns
new_cols <- function(x) {
  x %>%
  str_to_lower() %>%
  str_replace_all(" ","_") %>%
  str_remove_all('\\.')
}

# new_cols(colnames(index_1987))

index_1987 <- index_1987 %>% 
  rename_all(new_cols) 

# colnames(index_1987)

# ---- drop columns
drop <- c("Replaced By","Pg.","Reader service number", "Note", "Type") %>%
  new_cols

index_1987 <- index_1987 %>%
  select(-one_of(drop))

## ------ Check for input errors

# number ranges
index_1987 %>%
  select_if(is.numeric) %>%
  summary()

# ggplot(index_1987, aes(x = index_page))
#   + geom_boxplot()
#   
#   
#   ggplot() +
#     geom_point(mapping = aes(x = `speed`, y = `original_price`))
# 
# 
# ggplot(data = index_1987) + 
#   geom_point(mapping = aes(x = `speed`, y = `original-price`))

# -- convert to factors

library(forcats)

# -- extract levels to check duplicates
# extract_levels <- function(x) {
#   x %>%
#     factor() %>%
#     levels()
#     }
  
unique(index_1987$vol)
unique(index_1987$no)

volumes <- extract_levels(index_1987$vol)
no <- factor(index_1987$no)

speed_unit <- unique(index_1987$speed_unit)

companies <- factor(index_1987$company)
years <- volumes + 1981

# ---- attempt at FOR loop --- # 

# colnames(index_1987) %>%
#   str_c("index_1987","$",.) %>%
#   set_names() %>%
#   map(.)


## Transform

# 1 FIX speed

# cps to ppm: BASED ON note on page 420: 123 cps = 7.4 ppm >> 1 cps = 1/16.6

index_1987 %>%
  filter(speed_unit == "cps") %>%
  mutate(n_speed = speed / 16.6) ## ADD rounding to 1 decimal place


# 2 ADD year variables

vol_to_year <- function(x) {
  x + 1981
}

current_vol <- max(index_1987$vol)

v2y_index_1987 <- index_1987 %>%
  select(-one_of(c("index_page", "no"))) %>%
  mutate(
    original_year = vol_to_year(vol),
    current_year = vol_to_year(current_vol),
  ) 

a <- v2y_index_1987 %>%
  select(company, product, starts_with("original")) %>%
  filter(original_year == 1987)

b <- v2y_index_1987 %>%
  select(company, product, starts_with("current"))

left_join(a,b,by = c("product", "company")) %>%
  filter(original_price != current_price)

# 3 ADD date variable?

index_1987 %>%
  select(-index_page, -no) %>%
  unite(vol_no, vol, no, sep = "_", remove = FALSE) %>%
  
  






