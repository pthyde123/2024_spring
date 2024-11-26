
library(tidyverse)
library(readxl)

library(readr)
polyculture_exp <- read_csv("~/Intercrop JLJ/2024_spring/Patrick/polyculture_exp.csv")


polyculture_exp %>% 
  pivot_longer(crop_1:crop_5,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(name)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)




#### convert data to wide form

polyculture_data <- read_excel("~/Intercrop JLJ/2024_spring/Patrick/polyculture_data.xlsx", 
                               sheet = "biomass_longform")



polyculture_data %>% 
  select(PLOT,TRTN,name,`Dry Weight g`) %>% 
  pivot_wider(names_from = name, values_from = 'Dry Weight g',values_fn = sum) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


