


library(readxl)
library(tidyverse)




#### diversity weight collected to longform, all subplots###
Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls")

diversity_weight_t3_longform <- read_excel("~/Intercrop JLJ/2024_spring/Data/diversity_weight_t3_longform.xlsx")


Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")



### double check with biomass sampled and management factor


df<-Diversity_SubPlot_2024 %>% 
  select(plot_number, subplot_number,subplot_id) %>% 
  left_join(  diversity_weight_t3_longform %>% 
                select(subplot_id,crop,t3_weight_g) %>% 
                pivot_wider(names_from = crop, values_from = t3_weight_g),join_by(subplot_id) ) %>% 
  select(subplot_id,oat,pea,weed) %>% 
  left_join(  Diversity_SubPlot_2024 %>% 
                select(subplot_id,plot_number,subplot_number,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`) %>% 
                left_join(Diversity_Plot_2024 %>% 
                            select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
                mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
                mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
                rowwise() %>% 
                mutate(double_check = sum(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`,biomass_3_sampled,na.rm = TRUE))   ) %>% 
  select(subplot_id,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`,biomass_3_sampled,oat,pea,weed) 
  
  
 ### should return nothing if biomass subplot sampled matches biomass weight subplot labels on bag 
df %>%   
mutate("double_check_sampled" = if_else(biomass_3_sampled == 1 & 
                                          (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_sampled == "F")
  
### should return nothing if biomass management factor matches biomass weight subplot labels on bag  

df %>%   
  mutate("double_check_MF" = if_else(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3` == 1 & 
                                       (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_MF == "F")


### double checking the double check 

# the number of rows output should equal the total subplots minus the number sampled

df %>%   
  mutate("double_check_sampled" = if_else(biomass_3_sampled == 0 & 
                                            (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_sampled == "F")


#### use it if it checks out ok

Diversity_SubPlot_2024 %>% 
  select(plot_number, subplot_number,subplot_id) %>% 
  left_join(  diversity_weight_t3_longform %>% 
                select(subplot_id,crop,t3_weight_g) %>% 
                pivot_wider(names_from = crop, values_from = t3_weight_g),join_by(subplot_id) ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)




### Density weight to wideform

density_weight_t3_longform <- read_excel("~/Intercrop JLJ/2024_spring/Data/density_weight_t3_longform.xlsx")


Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")

Density_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls")



###double check

df<-Density_SubPlot_2024 %>% 
  select(plot_number, subplot_number,subplot_id) %>% 
  left_join(  density_weight_t3_longform %>% 
                select(subplot_id,crop,t3_weight_g) %>% 
                pivot_wider(names_from = crop, values_from = t3_weight_g),join_by(subplot_id) ) %>% 
  select(subplot_id,oat,pea,weed) %>% 
  left_join(  Density_SubPlot_2024 %>% 
                select(subplot_id,plot_number,subplot_number,`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`) %>% 
                left_join(Density_Plot_2024 %>% 
                            select(plot_number,plot_id, Biomass_3_sampled),join_by(plot_number)) %>% 
                mutate(biomass_3_sampled = str_sub(Biomass_3_sampled,-1)) %>% 
                mutate(biomass_3_sampled = if_else(biomass_3_sampled==subplot_number,1,0)) %>% 
                rowwise() %>% 
                mutate(double_check = sum(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`,biomass_3_sampled,na.rm = TRUE))   ) %>% 
  select(subplot_id,`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3`,biomass_3_sampled,oat,pea,weed) 

### should return nothing if biomass subplot sampled matches biomass weight subplot labels on bag 

df %>%   
  mutate("double_check_sampled" = if_else(biomass_3_sampled == 1 & 
                                            (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_sampled == "F")



### should return nothing if biomass management factor matches biomass weight subplot labels on bag  

df %>%   
  mutate("double_check_MF" = if_else(`ManagementFactor:Cornell_OatPeaDensity_2024_Ithaca_Cornell_OatPeaDensity_2024_Ithaca_biomass_T3` == 1 & 
                                       (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_MF == "F")


### double checking the double check 
# the number of rows output should equal the total subplots minus the number sampled
df %>%   
  mutate("double_check_sampled" = if_else(biomass_3_sampled == 0 & 
                                            (is.na(df$oat) | is.na(df$pea) | is.na(df$weed) ),"F","T")) %>% 
  filter(double_check_sampled == "F")





####  use it if it checks out ok

Density_SubPlot_2024 %>% 
  select(plot_number, subplot_number,subplot_id) %>% 
  left_join(  density_weight_t3_longform %>% 
                select(subplot_id,crop,t3_weight_g) %>% 
                pivot_wider(names_from = crop, values_from = t3_weight_g),join_by(subplot_id) )%>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



###### T4 weight longform to wide form for upload  ######


###  Diversity

library(readr)

diversity_weight_t4_longform <- read.csv("~/Intercrop JLJ/2024_spring/Data/diversity_weight_t4_longform.csv")

Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls")

Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")



Diversity_SubPlot_2024 %>% 
  select(plot_number, subplot_number,subplot_id) %>% 
  left_join(  diversity_weight_t4_longform %>% 
                select(subplot_id,crop,t4_weight_g) %>% 
                pivot_wider(names_from = crop, values_from = t4_weight_g),join_by(subplot_id) ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)







####  Density


### Density weight to wideform



library(readxl)
library(tidyverse)
density_weight_t4_longform <- read_excel("~/Intercrop JLJ/2024_spring/Data/density_weight_t4_longform.xlsx")


Density_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_SubPlot_2024.xls")

Density_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls")





Density_SubPlot_2024 %>% 
  select(subplot_id, plot_number,subplot_number) %>% 
  left_join(density_weight_t4_longform  %>% 
              select(subplot_id,crop,t4_weight_g) %>% 
              pivot_wider(names_from = crop, values_from = t4_weight_g)    ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)

