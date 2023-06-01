
library(tidyverse)
library(arrow)

data <- readRDS('data/CN_el_cpc.rds')
data %>% write_parquet('data/CN_el_cpc.parquet')

rm(data)

data <- readRDS('data/CN_el_patent_abstract.rds')
data %>% write_parquet('CN_el_patent_abstract.parquet')
rm(data)
