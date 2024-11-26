
library(readr)
library(tidyverse)
getwd()


Sp_2024_design_w_plot_g <- read_csv("~/Intercrop JLJ/2024_spring/Data/Sp_2024_design_w_plot_g.csv") ## input plot design with pea names for each plot



####   ManagementFactor:trialName_peaName  ####  pea management factor name format
####   ManagementFactor:SpringOatPeaIntercrop_2024_NY_peaName  ####  pea management factor example


MgmtF_name <- "ManagementFactor:SpringOatPeaIntercrop_2024_NY_"  #trial name with "_" at end


df<-Sp_2024_design_w_plot_g %>% 
  select(State,PlotNo,peaName) %>% 
  filter(State == str_sub(MgmtF_name,-3,-2)) # filter to state based on MgmtF_name


PeaMgmtF <- model.matrix(~ -1 + peaName, df)%>% 
  as_tibble() %>%
  rename_with(~str_replace(.,'peaName', MgmtF_name)) %>% 
  select(!paste(MgmtF_name,"na",sep = ""))    # remove management factor "na", which are monocrop oat plots

csv<-bind_cols(df,PeaMgmtF)


write.csv(bind_cols(df,PeaMgmtF),("Management_factor_NY_pea.csv"))



####South Dakota, plot assigned by Melanie, she used 100 intercrop plots plus 5 monocrop plots.  

library(readxl)
X2024_OatPea_SD_raw_data <- read_excel("~/Intercrop JLJ/2024_spring/Data/2024_OatPea_SD_raw_data.xlsx", 
                                       sheet = "t3_upload")


MgmtF_name <- "ManagementFactor:SpringOatPeaIntercrop_2024_SD_"  #trial name with "_" at end


df<-X2024_OatPea_SD_raw_data %>% 
  select(plot_number,peaName) 


PeaMgmtF <- model.matrix(~ -1 + peaName, df)%>% 
  as_tibble() %>%
  rename_with(~str_replace(.,'peaName', MgmtF_name)) %>% 
  select(!paste(MgmtF_name,"na",sep = ""))# remove management factor "na", which are monocrop oat plots

bind_cols(df,PeaMgmtF)


write.csv(bind_cols(df,PeaMgmtF),("Management_factor_SD_pea.csv"))










