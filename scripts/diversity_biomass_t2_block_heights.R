


library(readxl)
library(tidyverse)


plot_data <- Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls")


subplot_data <- Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")


meta_data <- plot_data %>% 
  select(plot_id,plot_number,accession_name)


subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2`) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2`)) %>% 
  group_by(block_number) %>% 
  summarise(median = median(t2_canopy_cm),mean = mean(t2_canopy_cm)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


hist <- subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`ManagementFactor.SpringOatPeaIntercrop_2024_NY_biomass_T2`) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T2`))


ggplot(hist, aes(x=t2_canopy_cm)) + 
  geom_histogram(binwidth=3)+
  facet_wrap(~block_number)






