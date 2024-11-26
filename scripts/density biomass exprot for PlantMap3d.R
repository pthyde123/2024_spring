library(readxl)
library(tidyverse)


plot_data <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls")


subplot_data <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")



meta_data <- plot_data %>% 
  select(plot_id,plot_number,accession_name,Biomass_1_sampled)




### data for PlantMap3d T1

subplot_data %>% 
  select(subplot_id,'ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_t1',Biomass_1_oat_g,Biomass_1_pea_g,Biomass_1_weeds_g) %>% 
  filter(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_t1` == 1) %>% 
  mutate("Oat" = Biomass_1_oat_g ) %>% 
  mutate("Pea" = Biomass_1_pea_g ) %>%
  mutate("Other" = Biomass_1_weeds_g ) %>%
  pivot_longer(Oat:Other, names_to = "Species", values_to = "Dry Wt (g)" ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)




### data for PlantMap3d T2


### data for PlantMap3d T3

colnames(subplot_data)


subplot_data %>% 
  select(plot_number, subplot_number,subplot_id,
         'ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3',t3_oat_g,t3_pea_g,t3_weed_g) %>% 
  filter((`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3` == 1)) %>% 
  rename("Oat" = t3_oat_g ) %>% 
  rename("Pea" = t3_pea_g ) %>%
  rename("Other" = t3_weed_g ) %>%
  pivot_longer(Oat:Other, names_to = "Species", values_to = "Dry Wt (g)" ) %>%  
  mutate(Image_Name = str_c("NY-Jannink_2024_CC_TM-3_Pl-plot_", subplot_id)) %>% 
  mutate('Plot ID' = str_c("plot_", plot_number,"_subplot_",subplot_number,"_oat")) %>% 
  mutate('Sample Date' = "7/18/24") %>% 
  mutate(Timing = 3) %>% 
  select(Image_Name,'Sample Date',Timing, subplot_id, Species, `Dry Wt (g)` ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)
