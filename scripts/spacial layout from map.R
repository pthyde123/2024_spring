

####  trial coordinates may be uploaded in tab-delimited text file format
####  (.xls or .xlsx formats are NOT supported)


####Required fields:
####  plot_name (must exist in the database for this trial)
####  row_number (needed for the display of physical layout)
####  col_number (needed for the display of physical layout)

####  plot_name	row_number	col_number


##  Notes
##  Field plots will be displayed in the physical trial layout based on the 
###   coordinates you provided in the row and column number.
##  Physical trial layout will capture serpentine, zigzag and any other planting format use.


library(tidyverse)



#### New York Diversity  ####

library(readxl)
NY_map <- read_excel("~/Intercrop JLJ/2024_spring/location meta data/multi-location maps.xlsx", 
                                  sheet = "NY")

library(readxl)
Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls") %>% 
  select(plot_name,plot_number)

library(caroline)

NY_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  left_join(Diversity_Plot_2024,join_by(plot_number)) %>% 
  select(plot_name,	row_number,col_number) %>% 
  write.delim(, file = "2024_NY_Spatial_Layout.tab", quote = FALSE, row.names = FALSE, sep = "\t")



#### New York Density  ####

library(readxl)
NY_density_map <- read_excel("~/Intercrop JLJ/2024_spring/location meta data/multi-location maps.xlsx", 
                     sheet = "NY_density")

library(readxl)
Density_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Density_Plot_2024.xls") %>% 
  select(plot_name,plot_number)

library(caroline)

NY_density_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  left_join(Density_Plot_2024,join_by(plot_number)) %>% 
  select(plot_name,	row_number,col_number) %>% 
  filter(!is.na(plot_name)) %>% 
  write.delim(, file = "2024_NY_Density_Spatial_Layout.tab", quote = FALSE, row.names = FALSE, sep = "\t")


#### Illinois  ####

library(readxl)
IL_map <- read_excel("~/Intercrop JLJ/2024_spring/location meta data/multi-location maps.xlsx", 
                     sheet = "IL")


library(readr)
X2024_IL_layout <- read_csv("~/Intercrop JLJ/2024_spring/Data/2024_IL_layout.csv")%>% 
  select(plot_name,plot_number)


library(caroline)

IL_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  left_join(X2024_IL_layout,join_by(plot_number)) %>% 
  select(plot_name,	row_number,col_number) %>% 
  write.delim(, file = "2024_IL_Spatial_Layout.tab", quote = FALSE, row.names = FALSE, sep = "\t")


#### Alabama  ####

library(readxl)
AL_map <- read_excel("~/Intercrop JLJ/2024_spring/location meta data/multi-location maps.xlsx", 
                     sheet = "AL")


library(readr)
X2024_AL_layout <- read_csv("~/Intercrop JLJ/2024_spring/Data/2024_AL_layout.csv")%>% 
  select(plot_name,plot_number)


library(caroline)

AL_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  left_join(X2024_AL_layout,join_by(plot_number)) %>% 
  select(plot_name,	row_number,col_number) %>% 
  write.delim(, file = "2024_AL_Spatial_Layout.tab", quote = FALSE, row.names = FALSE, sep = "\t")




#### South Dakota  ####
library(tidyverse)
library(readxl)
SD_map <- read_excel("~/Intercrop JLJ/2024_spring/location meta data/multi-location maps.xlsx", 
                     sheet = "SD")


library(readr)
X2024_SD_layout <- read_csv("~/Intercrop JLJ/2024_spring/Data/2024_SD_layout.csv")%>% 
  select(plot_name,plot_number)


library(caroline)

SD_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  arrange(plot_number) %>% 
  as_tibble() %>% 
  write.table("clipboard", sep="\t", row.names=TRUE, col.names=TRUE)
