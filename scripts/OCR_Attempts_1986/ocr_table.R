library(tidyverse)
library(stringr)

setwd("PrinterIndex")

ocr_txt <- "01-PrinterIndex-1987_cleaned_Page_1-psm06.txt"

ocr_table <- read_lines(ocr_txt, skip = 3)
