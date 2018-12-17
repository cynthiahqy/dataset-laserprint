# Data Viz

library(forcats)
library(tidyverse)
library(here)
library(stringr)

path2cache <- here("spreadsheets/cache/allPrinters/")

# import latest version of allPrinters

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct04.csv"))

# FILTER all_printers ----

## remove unnecessary product_type
unique(all_printers$product_type)

# PLOTS parent_co ---- 

## group data set

df.by_parent <- all_printers %>%
  group_by(parent_co) %>%
  drop_na(source_vol)

## parent_entryexit
### generate entry (status == 1), exit (status == 4) indicators, longevity value (mkt_vols)

df.parent_entryexit <- df.by_parent %>%
  summarise(min_vol = min(source_vol),
            max_vol = max(source_vol),
            mkt_vols = max_vol - min_vol + 1) %>%
  mutate(parent_co = fct_reorder(parent_co, desc(min_vol))) %>%
  rename(`1` = min_vol,
         `4` = max_vol) %>%
  gather(`1`, `4`, key = "status", value = "source_vol") %>%
  mutate(status = as.numeric(status))

### plot entry/exit of parent_co by longevity, then entry year

plot.parent_entryexit <- df.parent_entryexit %>%
  ggplot(mapping = aes(y = fct_reorder(parent_co, mkt_vols), x = source_vol + 1981)) +
  geom_point(aes(shape = status)) + scale_shape_identity() + 
  geom_line(aes(group = parent_co)) +
  labs(title = "entry/exit of parent_co by longevity, then entry year", 
       subtitle = "longevity is based on first & last appearance, entry year is first appearance",
       x = "year in PC Magazine",
       y = "Company Name")

print(plot.parent_entryexit)

## parent_status

### merge status variable into all_printers, fill in review (status == 3), fill longevity value

df.parent_status <-
  left_join(all_printers, df.parent_entryexit, by = c("parent_co", "source_vol")) %>%
  select(c(parent_co, status, mkt_vols, source_vol)) %>%
  replace_na(list(status = 3)) %>%
  fill(mkt_vols)

plot.parent_status <-
  df.parent_status %>%
  ggplot(mapping = aes(y = fct_reorder(parent_co, mkt_vols), x = source_vol + 1981)) +
  geom_point(aes(shape = status)) + scale_shape_identity() + 
  geom_line(aes(group = parent_co)) 

print(plot.parent_status)

#

# PLOTS engine_brand

## engine_entryexit
by_engine <- all_printers %>%
  group_by(engine_brand) %>%
  drop_na(source_vol) %>%
  filter(!engine_brand %in% c("UNKN", "N/A"))

df.engine_entryexit <- by_engine %>%
  summarise(min_vol = min(source_vol),
            max_vol = max(source_vol),
            mkt_vols = max_vol - min_vol + 1) %>%
  mutate(engine_brand = fct_reorder(engine_brand, desc(min_vol))) %>%
  rename(`1` = min_vol,
         `4` = max_vol) %>%
  gather(`1`, `4`, key = "status", value = "source_vol") %>%
  mutate(status = as.numeric(status))

plot.engine_entryexit <- df.engine_entryexit %>%
  ggplot(mapping = aes(y = fct_reorder(engine_brand, mkt_vols), x = source_vol + 1981)) +
  geom_point(aes(shape = status)) + scale_shape_identity() + 
  geom_line(aes(group = engine_brand, colour = mkt_vols)) +
  labs(title = "entry/exit of engine_brand by longevity, then entry year", 
       subtitle = "longevity is based on first & last appearance, entry year is first appearance",
       x = "year in PC Magazine",
       y = "Engine Manufacturer")



