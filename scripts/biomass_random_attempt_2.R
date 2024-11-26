library(tidyverse)
library(readr)
library(readxl)

#####Diversity

SpringOatPeaIntercrop_2024_NY_subplot <- read_excel("~/Intercrop JLJ/2024_spring/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
                                                    sheet = "SpringOatPeaIntercrop_2024_NY_s")




SpringOatPeaIntercrop_2024_NY_subplot_pairs <- read_excel("~/Intercrop JLJ/2024_spring/SpringOatPeaIntercrop_2024_NY_subplot.xlsx", 
                                                          sheet = "pair_id")




data <- SpringOatPeaIntercrop_2024_NY_subplot %>% 
  left_join(SpringOatPeaIntercrop_2024_NY_subplot_pairs) %>% ## add the plot pairs
  select(subplot_name,subplot_id,block_number,pair,plot_number,subplot_number) %>% 
  mutate(pair_plot = str_c(pair,"_",plot_number)) %>% # create pair_plot for sorting
  mutate(pair_subplot = str_c(pair,"_",subplot_number)) # create pair_subplot for sorting

set.seed(1234) # make sure you get the same randomization every time





biomass_1_2 

data %>%
  mutate(rand = runif(nrow(data))) %>% # add random numbers to arrange by, this isn't reproducible, make permanent here or in excel?
  filter(subplot_number != 2) %>% # remove subplot 2
  group_by(pair_subplot) %>%
  slice_head(n=1) %>% # it is grouped by pair_subplot, remove one 
  arrange(pair,rand) %>% # arrange by pair and then random
  bind_cols(biomass_sample_1_2 = rep(c( 1, 2), times = (100))) %>% # assign sample biomass sample 1 or 2
  select(pair_subplot,biomass_sample_1_2) %>% 
  print(n=100)




set.seed(1235) # make sure you get the same randomization every time

biomass_3_4 <- data %>%
  mutate(rand = runif(nrow(data))) %>% # add random numbers to arrange by, this isn't reproducible, make permanent here or in excel?
  filter(subplot_number != 2) %>% # remove subplot 2
  group_by(pair_subplot) %>%
  slice_head(n=1) %>% # it is grouped by pair_subplot, remove one 
  arrange(pair,rand) %>% # arrange by pair and then random
  bind_cols(biomass_sample_3_4 = rep(c( 3, 4), times = 100)) %>% # assign sample biomass sample 3 or 4
  select(pair_subplot,biomass_sample_3_4)


biomass_sample_management_factor <- 
  data %>% 
  left_join(biomass_1_2, join_by(pair_subplot)) %>% 
  left_join(biomass_3_4, join_by(pair_subplot)) %>% 
  mutate(biomass_1 = if_else(biomass_sample_1_2 == 1,1,NA)) %>% # reassign so each sample is different column / management factor for T3 upload
  mutate(biomass_2 = if_else(biomass_sample_1_2 == 2,1,NA)) %>%
  mutate(biomass_3 = if_else(biomass_sample_3_4 == 3,1,NA)) %>%
  mutate(biomass_4 = if_else(biomass_sample_3_4 == 4,1,NA)) %>% 
  select(subplot_name,subplot_id,block_number,plot_number,subplot_number,biomass_1,biomass_2,biomass_3,biomass_4)  ##



biomass_sample_management_factor %>% 
  print(n=600)  ### look over


####write.csv(biomass_sample_management_factor, file="biomass_sample_management_factor.csv", na="", row.names = FALSE)  ## save csv without NA's for T3/OAT

