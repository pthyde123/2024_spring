# Run all of Leah_GBLR_GMA.Rmd to get the data formated and have the bivarient model

Multi_tab <- oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff))

colnames(Multi_tab)[2]<- "Multi_PrEff"
colnames(Multi_tab)[3]<- "Multi_AsEff"
colnames(Multi_tab)[4]<- "Multi_SI1"
colnames(Multi_tab)[5]<- "Multi_SI2"

Multi_tab <- as.matrix(Multi_tab)

library(lme4)  # For mixed-effects models

# Univarient Model : 1) Oat effect on oat yield 2) Oat effect on pea yield 

# 1) Oat effect on oat yield 
# response
y <-as.matrix(dplyr::select(grainWgt, contains("oatYield"))) 

model <- lmer(oatYield ~ K2 + (1|location) + (1|blockNumber) + (1|studyYear) + (1|management), data = grainWgt) #fixed-effect model matrix is rank deficient so dropping 755 columns / coefficients

model <- lmer(oatYield ~ K2 + management + (1|blockNumberF), data = grainWgt) #fixed-effect model matrix is rank deficient so dropping 755 columns / coefficients

model <- lmer(oatYield ~ germplasmName + (1|blockNumberF), data = grainWgt)
summary(model)

model <- lmer(oatYield ~ germplasmName + management + (1|blockNumberF), data = grainWgt)
summary(model)

model@beta # pr effects

# first number is intercept 
preff <- model@beta[-1]
preff

# 2) Oat effect on pea yield 

model_as <- lmer(peaYield ~ germplasmName + management + (1|blockNumberF), data = grainWgt)
summary(model_as)

model_as@beta # as effects

# first number is intercept 
aseff <- model_as@beta[-1]
aseff

tab <- cbind(preff,aseff)
View(tab)
rownames(tab) <- c(accessions_47$germplasmName)

#SI <- apply(tab, 1, sum) # sum of each row (1 indicates rows)
SI1 = tab[,1]+tab[,2]
SI2 = tab[,1]-tab[,2]
tab2 <- cbind(SI1, tab)
tab2 <- cbind(SI2, tab2)

# write.csv(tab2, 'data/springOatPea_univarientSIPrAs.csv')

colnames(tab2)[1] <- "Uni_SI2"
colnames(tab2)[2] <- "Uni_SI1"
colnames(tab2)[3] <- "Uni_PrEff"
colnames(tab2)[4] <- "Uni_AsEff"

together <- cbind(Multi_tab, tab2)
together <- together[,-1] # remove first column (rownames)
together <- matrix(as.numeric(together), ncol=8)
rownames(together) <- c(accessions_47$germplasmName)
colnames(together) <- c("Multi_PrEff","Multi_AsEff","Multi_SI1", "Multi_SI2", "Uni_SI2", "Uni_SI1", "Uni_PrEff", "Uni_AsEff")

cor(together[,"Multi_PrEff"], together[,"Uni_PrEff"])
cor(together[,"Multi_AsEff"], together[,"Uni_AsEff"])

cor(together[,"Multi_PrEff"], together[,"Multi_AsEff"])
cor(together[,"Uni_PrEff"], together[,"Uni_AsEff"])
