# Run all of Leah_GBLR_GMA.Rmd to get the data formated and have the bivarient model

library(lme4)  # For mixed-effects models
library(ggplot2)  # For visualization

# Univarient Model : 1) Oat effect on oat yield 2) Oat effect on pea yield 

# 1) Oat effect on oat yield 
# response
y <-as.matrix(dplyr::select(grainWgt, contains("oatYield"))) 

model <- lmer(oatYield ~ K2 + (1|location) + (1|blockNumber) + (1|studyYear) + (1|management), data = grainWgt)

