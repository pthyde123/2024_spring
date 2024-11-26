
library(tidyverse)

library(readxl)
Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls")


Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")



####T1

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number) %>% 
  left_join(Diversity_Plot_2024 %>% 
  select(plot_number,plot_id, Biomass_1_sampled),join_by(plot_number)) %>% 
  mutate(biomass_1_sampled = str_sub(Biomass_1_sampled,-1)) %>% 
  mutate(biomass_1_sampled = if_else(biomass_1_sampled==subplot_number,1,0)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



####T3  Diversity

## double check that "management Factor" matches the actual sampled

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`) %>% 
  left_join(Diversity_Plot_2024 %>% 
              select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
  mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
  mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
  rowwise() %>% 
  mutate(double_check = sum(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`,biomass_3_sampled,na.rm = TRUE)) %>% 
  filter(! double_check %in% c(2,0))  ### If correct it should return NOTHING




## if there are no problems add it to the subplot data

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number) %>% 
  left_join(Diversity_Plot_2024 %>% 
              select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
  mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
  mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



####T3  Density


Density_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls")


Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")





## double check that "management Factor" matches the actual sampled

Density_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number,`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`) %>% 
  left_join(Density_Plot_2024 %>% 
              select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
  mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
  mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
  rowwise() %>% 
  mutate(double_check = sum(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`,biomass_3_sampled,na.rm = TRUE)) %>% 
  filter(! double_check %in% c(2,0))  ### If correct it should return NOTHING






## if there are no problems add it to the subplot data

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number) %>% 
  left_join(Diversity_Plot_2024 %>% 
              select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
  mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
  mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)





