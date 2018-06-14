# RUN EVERYTIME
library(tidyverse)
library(stringr)
library(here)

index_df <- list()
cache_list <- data.frame()

# 01 IMPORT printerIndex-00 (raw VALIDATED entries)

index_df[[1]] = read_csv(here('cache-printerIndex','1987-printerIndex-00.csv'))

# 03 Drop columns 

drop <- c("replaced_by", "pg", "reader_service_number", "note", "type")

index_df[[2]] <- index_df[[1]] %>%
  select(-one_of(drop))

## CACHE: 03
cache_path = here('cache-printerIndex','1987-printerIndex-02.csv')
write_csv(index_df[[2]], path = cache_path)


# 04 Check for Errors
## IMPORT: 03 (raw less columns)

index_df[[3]] <- read_csv(cache_path)



## ------ Check for input errors

# number ranges
index_df %>%
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

cps_2_ppm <- function(x) {
  x / 16.6
}

index_df$speed_unit

speed_info <- data.frame (
  unit = c("ppm", "cps"),
  conv = c(1, 1/16.6),
  test = c("dont", "do")
)

id <- match(index_df$speed_unit, speed_info$unit)
speed_info[id, ]

index_df <- index_df %>%
  mutate(n_speed = speed)



index_df[index_df$speed_unit == "cps", "n_speed"] <- 0



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
  
  






