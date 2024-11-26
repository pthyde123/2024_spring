


library(clipr)
library(readxl)
library(tidyverse)


#####T3 Biomass######

####diversity


Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")

####  Diversity T3 bag labels
Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`)) %>% 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)  ###figure out how to just get the list to past into t3 list for barcode printing


#### Diversity T3 field book 

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`)) %>%  #### could remove this and have a field for both t3-t4 gets pretty big and maybe harder to double check, 2025 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id,subplot_id,plot_number,subplot_number,crop) %>% 
  write.csv(, file="diverstity_biomass_longform_t3.csv", row.names = FALSE)





#####density t3  biomass bag labels


Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")

Density_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`)) %>% 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id,subplot_id,plot_number,subplot_number,) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



#### Density T3 field book 

Density_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`,`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`)) %>%  #### could remove this and have a field for both t3-t4 gets pretty big and maybe harder to double check, 2025 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id,subplot_id,plot_number,subplot_number,crop) %>% 
  write.csv(, file="density_biomass_longform_t3.csv", row.names = FALSE)







#####T4 Biomass######

####diversity


Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")

####  Diversity T4 bag labels
Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`)) %>% 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id) %>% 
  write_clip(  ,col.names=FALSE)#### works better for adding a list to T3-Oat directly


#### Diversity T4 field book 

Diversity_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`)) %>%  #### could remove this and have a field for both t3-t4 gets pretty big and maybe harder to double check, 2025 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id,subplot_id,plot_number,subplot_number,crop) %>% 
  write.csv(, file="diverstity_biomass_longform_t4.csv", row.names = FALSE)





#####density t4  biomass bag labels


Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")


###  Density T4 bag labels

Density_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T4`)) %>% 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id) %>%
  write_clip(  ,col.names=FALSE)#### works better for adding a list to T3-Oat directly
  

#### Density T4 field book 

Density_SubPlot_2024 %>% 
  select(subplot_id,plot_number,subplot_number, `ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T4`) %>% 
  mutate("oat" = "oat") %>% 
  mutate("pea" = "pea") %>%
  mutate("weed" = "weed") %>% 
  pivot_longer(oat:weed,names_to = "crop", values_to = "name") %>% 
  filter(!is.na(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T4`)) %>%  #### could remove this and have a field for both t3-t4 gets pretty big and maybe harder to double check, 2025 
  mutate(biomass_sample_id = str_c("plot_",plot_number,"_","subplot_",subplot_number,"_",crop)) %>% 
  select(biomass_sample_id,subplot_id,plot_number,subplot_number,crop) %>% 
  write.csv(, file="density_biomass_longform_t4.csv", row.names = FALSE)










