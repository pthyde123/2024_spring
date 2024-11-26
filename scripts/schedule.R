
library(tidyverse)
library(readxl)


schedule <- read_excel("~/Intercrop JLJ/2024_spring/2024_work_schedule.xlsx", 
                                  sheet = "winter_schedule")


schedule %>% 
  mutate(name = fct_reorder(name, -order)) %>%
  ggplot() +
  geom_segment( aes(x=name, xend=name, y=start, yend=end,color=name),size=3) +
  coord_flip()+
  theme_classic()+
  theme(
    legend.position = "none",) +
  xlab("") +
  ylab("")+
  theme(axis.text.x=element_text(angle = 0, hjust = .5))+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))+
  theme(axis.text.x = element_text( color = "black", size = 16))+
  theme(axis.text.y = element_text( color = "black", size = 16))




  geom_point(aes(x=name, y=start),shape = 21, fill="green", color="black", size=5 ) +
    geom_point(aes(x=name, y=end), shape = 21, fill="red", color="black",size=5 ) +



all_years %>%
  filter(!genotype %in% c("TMS14F1313P0010","IITA-TMS-IBA980002","TMEB1","IBA980002","IBA30572")) %>%
  group_by(genotype,treatment,year) %>% 
  summarise(mean_year = mean(seed)) %>% 
  ungroup() %>% 
  group_by(genotype,treatment) %>% 
  summarise(mean_seed = mean(mean_year), sd_seed=sd(mean_year),se_seed=(sd(mean_year)/sqrt(n())),n=n()) %>% 
  ungroup() %>% 
  dplyr::select(genotype,treatment,mean_seed) %>% 
  pivot_wider(names_from = treatment, values_from = mean_seed) %>% 
  ungroup() %>% 
  mutate(diff = BP-C) %>%
  mutate(ave = (BP+C/2)) %>% 
  mutate(genotype = fct_reorder(genotype, ave)) %>% 
  
  ggplot() +
  geom_segment( aes(x=genotype, xend=genotype, y=C, yend=BP), color="grey",size=1) +
  geom_point( aes(x=genotype, y=C),shape = 21, fill="#1B9E77", color="black", size=5 ) +
  geom_point( aes(x=genotype, y=BP), shape = 21, fill="#D95F02", color="black",size=5 ) +
  coord_flip()+
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Seed")+
  theme(axis.text.x=element_text(angle = 0, hjust = .5))+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))+
  theme(axis.text.x = element_text( color = "black", size = 16))+
  theme(axis.text.y = element_text( color = "black", size = 16))+
  scale_y_continuous(breaks=c(2,4, 6, 8,10,12,14))






