# Data Viz

library(forcats)
library(tidyverse)
library(here)
library(stringr)

path2cache <- here("spreadsheets/cache/allPrinters/")

# import latest version of allPrinters

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct04.csv"))

# parent_co plots ---- 

## group data set

df.by_parent <- all_printers %>%
  group_by(parent_co) %>%
  drop_na(source_vol)

## generate entry (status == 1), exit (status == 4) indicators, longevity value (mkt_vols)

df.parent_entryexit <- df.by_parent %>%
  summarise(min_vol = min(source_vol),
            max_vol = max(source_vol),
            mkt_vols = max_vol - min_vol + 1) %>%
  mutate(parent_co = fct_reorder(parent_co, desc(min_vol))) %>%
  rename(`1` = min_vol,
         `4` = max_vol) %>%
  gather(`1`, `4`, key = "status", value = "source_vol") %>%
  mutate(status = as.numeric(status))

df.parent_status <-
  left_join(all_printers, df.parent_entryexit, by = c("parent_co", "source_vol")) %>%
  select(c(parent_co, status, mkt_vols, source_vol)) %>%
  replace_na(list(status = 3)) %>%
  fill(mkt_vols)

plot.parent_status <-
  ggplot(df.parent_status, mapping = aes(y = fct_reorder(parent_co, mkt_vols), x = source_vol)) +
  geom_point(aes(shape = status)) + scale_shape_identity() + 
  geom_line(aes(group = parent_co)) 

## by entry year

df.parent_by_entry <- 
  df.parent_yearspan %>%
    mutate(parent_co = fct_reorder(parent_co, desc(min_year))) %>%
    rename(`1` = min_year,
         `4` = max_year) %>%
    gather(`1`, `4`, key = "entry", value = "date") %>%
    mutate(entry = as.numeric(entry),
           source_vol = date - 1981)

order.parent_by_entry <-
  levels(df.parent_co_by_entry$parent_co)

plot.parent_co_by_entry <-
  df.parent_co_by_entry %>%
  ggplot(mapping = aes(y = parent_co, x = date)) +
    geom_point(aes(shape = entry)) + scale_shape_identity() + 
    geom_line(aes(group = parent_co)) 

## by longevity
plot.parent_co_by_longevity <-
  df.by_parent_co_yearspan %>%
    mutate(parent_co = fct_reorder(parent_co, mkt_years)) %>%
    rename(`1` = min_year,
           `4` = max_year) %>%
    gather(`1`, `4`, key = "entry", value = "date") %>%
    mutate(entry = as.numeric(entry)) %>%
  ggplot(mapping = aes(y = parent_co, x = date)) +
    geom_point(aes(shape = entry)) + scale_shape_identity() + 
    geom_line(aes(group = parent_co)) +
  labs(title = "Years in Market by Longevity")

print(plot.parent_co_by_entry)
print(plot.parent_co_by_longevity)

## GROUPING EXPLORATION ----

# group by product_brand
group_by(all_printers, company, product_brand) %>%
  summarise(n_product = n()) %>%
  summarise(n_brands = n()) %>% View()

group_by(all_printers, product_brand) %>%
  
  all_printers %>% count(product_brand)

# check for typos

list_unique <- function(df, var) {
  col_name <- deparse(substitute(var))
  col_vals <- eval(substitute(df))[[col_name]]
  unique_names <- col_vals %>% unique() %>% sort() %>% as.tibble() 
  unique_names[[2]] <- unique_names[[1]]
  colnames(unique_names) <- c(col_name, paste0(col_name), paste0("cor.", col_name))
  l.unique[[col_name]] <<- unique_names
}

# unique product_types
uni.product_type <- df_x1[["product_type"]] %>% unique() %>% sort()
uni.product_brand <- df_x1[["product_brand"]] %>% unique() %>% sort()
list_unique(df_x1, product_brand) %>% View()

## product_brand --> input N/A, correct duplications
# generate binary var: debut_brand = 1 if source_vol = min(source_vol given product_brand)
# generate binary var: fade_brand
df_x1 %>%
  ggplot(mapping = aes(x = source_vol, y = product_brand)) +
  geom_point() +
  xlim(3, 17)

df_x1 %>%
  ggplot(mapping = aes(x = source_vol, y = company)) +
  geom_point() +
  xlim(3, 17)


## EXPERIMENTAL DATA VIZ ----

all_printers <- df_x1
# group by engine_brand

by_engine <- all_printers %>% 
  filter(engine_brand != "?") %>%
  group_by(engine_brand) 

# plots number of reviewed prints with engines from brand, and which issues the engine_brand was present
add_count(by_engine) %>%
  # filter(n > 10) %>%
  ggplot(data = ., mapping = aes(y = n, x = source_vol, color = engine_brand, shape = n > 5)) +
  geom_point()


# price scatterplot over time
ggplot(data = all_printers) +
  geom_point(mapping = aes(x = source_vol, y = price_list, position = "jitter"))

df_x1 %>%
  filter(price_list < 10000) %>%
  ggplot() +
  geom_smooth(mapping = aes(x = source_vol, y = price_list))

# max, min, median of price over time [ ISSUE WITH INCLUSION OF BUSINESS MACHINES ]
ggplot(data = df_x1) + 
  stat_summary(
    mapping = aes(x = source_vol, y = price_list),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# number of printers reviewed
ggplot(data = df_x1) + 
  stat_count(mapping = aes(x = source_vol)) 


ggplot(data = df_x1) +
  stat_count(mapping = aes(x = engine_brand))
