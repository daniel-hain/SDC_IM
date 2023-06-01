library(tidyverse) # General DS toolkit
library(magrittr) # For advanced piping
library(arrow)

# Databases
library(DBI) # GEneral R database interface
library(RPostgres) # PostgreSQL interface driver 
library(dbplyr) # for dplyr with databases

####### Export Parquet

data <- readRDS('data/CN_el_cpc.rds')
data %>% write_parquet('data/CN_el_cpc.parquet')

rm(data)

data <- readRDS('data/CN_el_patent_abstract.rds')
data %>% write_parquet('data/CN_el_patent_abstract.parquet')
rm(data)

data <- readRDS('data/CN_inventor.rds')
data %>% write_parquet('data/CN_inventor.parquet')
rm(data)

data <- readRDS('data/CN_el_applt.rds')
data %>% write_parquet('data/CN_el_applt.parquet')
rm(data)

data <- readRDS('data/CN_applt.rds')
data %>% write_parquet('data/CN_applt.parquet')
rm(data)
