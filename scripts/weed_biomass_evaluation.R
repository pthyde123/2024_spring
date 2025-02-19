


library(readr)
library(readxl)
library(tidyverse)
Cornell_OatPeaDensity_2024_Ithaca_phenotypes <- read_csv("data/Cornell_OatPeaDensity_2024_Ithaca_phenotypes.csv")



density_meta_2024 <- read_excel("data/density_meta_2024.xlsx")


full_data <- Cornell_OatPeaDensity_2024_Ithaca_phenotypes %>% 
          left_join(density_meta_2024, by = "observationUnitName")

colnames(full_data)




full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>% 
  
  ggplot(aes(oat_pea_percent,weed_154))+
  geom_boxplot(aes(oat_pea_percent,weed_154)) +
  geom_jitter()+
  xlab("")+
  ylab("Grams")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))








full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>% 
  
  ggplot(aes(oat_pea_percent,weed_178))+
  geom_boxplot(aes(oat_pea_percent,weed_178)) +
  geom_jitter()+
  xlab("")+
  ylab("Grams")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>% 
  
  ggplot(aes(oat_pea_percent,weed_199))+
  geom_boxplot(aes(oat_pea_percent,weed_199)) +
  geom_jitter()+
  xlab("")+
  ylab("Grams")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>% 
  
  ggplot(aes(oat_pea_percent,weed_217))+
  geom_boxplot(aes(oat_pea_percent,weed_217)) +
  geom_jitter()+
  xlab("")+
  ylab("Grams")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




#############

model_data <- full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,"pea_type",oatName,peaName,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>% 
  
  select(weed_154:oat_pea_percent,pea_type,oatName,peaName) %>% 
  
  pivot_longer(weed_154:weed_217, names_to = "date", values_to = "weed_biomass") %>% 
  
  filter(oat_pea_percent != "0_50") %>% 
  filter(oat_pea_percent != "0_100")


colnames(full_data)




lm  <- lm((weed_biomass) ~  date + oat_pea_percent
                    ,data=model_data)

anova(lm)
library(agricolae)

test <- HSD.test(lm, trt = 'oat_pea_percent')

test
############




model_data <- full_data %>% 
  select(observationUnitName,observationLevel,plot,subplot,oat_percent,pea_percent,oatName,peaName,
         "Weed above ground dry biomass - g|day 154|COMP:0000079",                                              
         "Weed above ground dry biomass - g|day 178|COMP:0000080",                                              
         "Weed above ground dry biomass - g|day 199|COMP:0000081",                                              
         "Weed above ground dry biomass - g|day 217|COMP:0000082" ) %>% 
  rename("weed_154" = "Weed above ground dry biomass - g|day 154|COMP:0000079") %>% 
  rename("weed_178" = "Weed above ground dry biomass - g|day 178|COMP:0000080") %>%
  rename("weed_199" = "Weed above ground dry biomass - g|day 199|COMP:0000081") %>%
  rename("weed_217" = "Weed above ground dry biomass - g|day 217|COMP:0000082") %>% 
  
  filter(observationLevel == "subplot") %>% 
  
  mutate(oat_pea_percent = str_c(oat_percent,"_",pea_percent)) %>%
  
  mutate(weed_biomass = if_else(!is.na(weed_199), weed_199, weed_217)) %>% 
  
  mutate(oat_pea_percent = fct_relevel(oat_pea_percent, "0_50","0_100",
                                       "50_0","100_0",
                                       "50_50",
                                       "50_100", "100_50","100_100"   
  ))
 
 
model_data %>% 
  ggplot(aes(oat_pea_percent,weed_biomass))+
  geom_boxplot(aes(oat_pea_percent,weed_biomass)) +
  geom_jitter()+
  xlab("Oat % _ Pea %")+
  ylab("Weed Biomass")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



lm  <- lm((weed_biomass) ~  oat_pea_percent
          ,data=model_data)

anova(lm)
library(agricolae)

test <- HSD.test(lm, trt = 'oat_pea_percent')

test



