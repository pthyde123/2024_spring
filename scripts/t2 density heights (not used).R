

library(readxl)
library(tidyverse)


plot_data <- Density_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls")


subplot_data <- Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")


meta_data <- plot_data %>% 
  select(plot_id,plot_number,accession_name)


subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,	`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`management Factor: biomass_t2`,crop) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`management Factor: biomass_t2`)) %>% 
  group_by(crop) %>% 
  summarise(median = median(t2_canopy_cm),mean = mean(t2_canopy_cm)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


hist <- subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,	`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`management Factor: biomass_t2`,crop) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`management Factor: biomass_t2`))


ggplot(hist, aes(x=t2_canopy_cm)) + 
  geom_histogram(binwidth=3)+
  facet_wrap(~crop)

########


hist <- subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,	`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`management Factor: biomass_t2`,crop) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`management Factor: biomass_t2`))


df <-  subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,	`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`management Factor: biomass_t2`,crop) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`management Factor: biomass_t2`))

height <- subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`oat_height_6-25-24`,	`pea_height_6-25-24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm,`management Factor: biomass_t2`,crop) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`management Factor: biomass_t2`)) %>% 
  group_by(crop) %>% 
  summarise(median = median(t2_canopy_cm),mean = mean(t2_canopy_cm))



x<-hist %>% 
  left_join(df,join_by(subplot_id)) %>% 
  mutate("crop" = crop.y) %>% 
  left_join(height,join_by(crop))
  
  
  
  write.table(x,"clipboard", sep="\t", row.names=FALSE)

