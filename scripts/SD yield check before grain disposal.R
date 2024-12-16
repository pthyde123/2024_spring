

library(tidyverse)
library(readr)


## This is a check that the yield data from SD 2023 and 2024 is good and we can dispose of the grain.
## A quick look at box plots of the yield for each accession shows no values totally off the mark.  
## There are some outlines but nothing that makes me want to go back and reweigh.



X2023_2024_multi_location <- read_csv("data/2023_2024_multi_location.csv")


# 2023

X2023_2024_multi_location %>% 
  filter(studyYear == 2023 & studyLoc == "SD") %>% 
  select(germplasmName,peaName,oat_yield,pea_yield) %>% 
  ggplot(aes(germplasmName,oat_yield))+
  geom_boxplot()



X2023_2024_multi_location %>% 
  filter(studyYear == 2023 & studyLoc == "SD") %>% 
  select(germplasmName,peaName,oat_yield,pea_yield) %>% 
  ggplot(aes(peaName,pea_yield))+
  geom_boxplot()




# 2024

X2023_2024_multi_location %>% 
  filter(studyYear == 2024 & studyLoc == "SD") %>% 
  select(germplasmName,peaName,oat_yield,pea_yield) %>% 
  ggplot(aes(germplasmName,oat_yield))+
  geom_boxplot()


X2023_2024_multi_location %>% 
  filter(studyYear == 2024 & studyLoc == "SD") %>% 
  select(germplasmName,peaName,oat_yield,pea_yield) %>% 
  ggplot(aes(peaName,pea_yield))+
  geom_boxplot()







