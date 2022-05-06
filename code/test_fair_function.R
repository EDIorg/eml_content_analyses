
# testing the function

library(EDIutils)
library(xml2)
library(stringr)
library(tidyverse)

source('code/get_fair_for_eml.R')

eml_file <- read_xml("data/knb-lter-ntl.1.52.xml")

df_result <- get_fair_for_eml(eml_file)
