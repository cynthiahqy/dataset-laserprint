# Data Viz

library(forcats)
library(tidyverse)
library(here)
library(stringr)

path2cache <- here("spreadsheets/cache/allPrinters/")

# import latest version of allPrinters

all_printers <- read_csv(paste0(path2cache, "allPrinters-x1-correct04.csv"))

by_parent_co <- 
  group_by(all_printers, parent_co) %>%
  summarise(min_year = min(source_vol) + 1981,
            max_year = max(source_vol) + 1981,
            mkt_years = max_year - min_year + 1)

by_parent_co %>%
  
  
  # factor_parent_by_minyear <- by_parent_co %>% 
  #   arrange(min_year) %>%
  #   select(parent_co) 
  # 
  # factor(by_parent_co$parent_co, factor_parent_by_minyear)
  
  
  ggplot() +
  geom_point(mapping = aes(y = parent_co, x = min_year)) +
  geom_point(mapping = aes(y = parent_co, x = max_year), colour = 'red')

correct_product %>%
  group_by(parent_co) %>%
  summarise(min_year = min(source_vol) + 1981,
            max_year = max(source_vol) + 1981,
            mkt_years = max_year - min_year + 1)

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
