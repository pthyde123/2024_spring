
# This is the description from of the template format from T3-Oat

# Header:
#   The first row (header) should contain the following:
# 
#   "seedlot_name","accession_name","operator_name","amount","weight_gram","description","box_name","quality","source"
# 
#   Required fields:
# seedlot_name (must be unique)
# accession_name (must exist in the database. the accession_name is the unique identifier for the named genotype)
# operator_name (the name of the person who oversaw the inventory process. can be any name.)
# amount (number of seeds in seedlot. can be provided in conjunction with weight_gram. must provide a value for amount or weight_gram or both.)
# AND/OR
# weight_gram (weight in grams of seedlot. can be provided in conjunction with amount. must provide a value for amount or weight_gram or both.)
# box_name (the box name that the seed is located in. can be any name.)
# Optional fields:
#   description (information about why this seedlot is being added)
# quality (status of the seedlot, for example "ok", "moldy", "insect damage" etc.
#          source (an alternate source, such as a plot, subplot, or plant identifier from which the seed was collected)
# 



library(tidyverse)
library(readr)
library(readxl)


# The following script is used to convert the reweighed sub samples of the source seed for the 2023 spring oat-pea trial.
# This seedlot is a subset that was pulled for genotypeing and will be used for crossing. 


# Import the seed weights 

source  <- spring_oat_source_seed <- read_excel("data/spring_oat_source_seed.xlsx")



operator_name = "Peter Hyde"
description = "Subset of souce seed use for 2023 spring oat trial and T3-oat genotyping project USDA-Ithaca2024Jannink-O3K"
box_name = "Box 1 and 2"
quality = "good"


seedlot_header <- c("seedlot_name","accession_name","operator_name","amount","weight_gram","description","box_name","quality","source")


# format of the seedlot name, this is acquisition seed so there are no trials or plot number



# {accession_name}-acquisition-oatEntry_{oatEntry}


source %>% 
  
  mutate(seedlot_name = str_c(accession_name,"-acquisition-oatEntry_",oatEntry)) %>% 
  mutate(accession_name = accession_name) %>% 
  mutate(operator_name = operator_name) %>% 
  mutate(amount = "") %>% 
  mutate(weight_gram = weight_g) %>% 
  mutate(description = description) %>% 
  mutate(box_name = box_name) %>% 
  mutate(quality = quality) %>% 
  mutate(source = source) %>% 
  
  select(all_of(seedlot_header)) %>% 
  
  write.csv("data/2023_spring_oat_source_seedlot_t3_upload.csv", row.names = FALSE)












