# The Selection Indices for Oats based on updated BGLR (using effect of K matrix) is significanly different than the selections from the original BGLR (using IncOatAcc)
# make two simple univariate analyses, one for oat yield and the other for pea yield to see how an analysis that isn't fancy or making any assumptions stacks up to the PrEff and AsEff

#### Start by running Leah_GBLR_GMA.Rmd to get the data formated and have the bivarient model ####

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

#### Univarient Models : 1) Oat effect on oat yield 2) Oat effect on pea yield ####

### 1) Oat effect on oat yield 

#model_Pr <- lmer(oatYield ~ K2 + (1|location) + (1|blockNumber) + (1|studyYear) + (1|management), data = grainWgt) #fixed-effect model matrix is rank deficient so dropping 755 columns / coefficients
#model_Pr <- lmer(oatYield ~ K2 + management + (1|blockNumberF), data = grainWgt) #fixed-effect model matrix is rank deficient so dropping 755 columns / coefficients
#model_Pr <- lmer(oatYield ~ management + blockNumberF + (1|GRM3), data = grainWgt) # Error in model.frame.default(data = grainWgt, drop.unused.levels = TRUE,:variable lengths differ (found for 'GRM3')
#model_Pr <- lmer(oatYield ~ germplasmName + (1|blockNumberF), data = grainWgt)

model_Pr <- lmer(oatYield ~ germplasmName + management + (1|blockNumberF), data = grainWgt)
# oat yield ~ oat accessions (names) + management (inter/mono) + YearLocationRep
summary(model_Pr)

model_Pr@beta # Producer effects

# first number is intercept 
preff <- model_Pr@beta[-1] # make list, removing the first value
preff # list of oat producer effects 

### 2) Oat effect on pea yield 

#model_As <- lmer(peaYield ~ germplasmName + peaAcc + management + (1|blockNumberF), data = grainWgt) # fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

model_As <- lmer(peaYield ~ germplasmName + management + (1|blockNumberF), data = grainWgt)
# pea yield ~ oat accessions (names) + management (inter/mono) + YearLocationRep
summary(model_As)

model_As@beta # associative effects

# first number is intercept 
aseff <- model_As@beta[-1] # make list, removing the first value
aseff # list of oat associative effects 

tab <- cbind(preff,aseff) # combine Pr and As into one matrix 
View(tab)
rownames(tab) <- c(accessions_47$germplasmName)

#SI <- apply(tab, 1, sum) # sum of each row (1 indicates rows) # decided not to use this method
SI1 = tab[,1]+tab[,2] # Pr + As
SI2 = tab[,1]-tab[,2] # Pr - As
tab2 <- cbind(SI1, tab) # add selection index 1 to new table
tab2 <- cbind(SI2, tab2) # add selection index 2 to new table

# write.csv(tab2, 'data/springOatPea_univarientSIPrAs.csv')

colnames(tab2)[1] <- "Uni_SI2"
colnames(tab2)[2] <- "Uni_SI1"
colnames(tab2)[3] <- "Uni_PrEff"
colnames(tab2)[4] <- "Uni_AsEff"

### Compare Effects Calculated by Multivarient Analysis with Effects Calculated by Univarient Analysis

together <- cbind(Multi_tab, tab2) # make new table that has both the output of the multivariant and univariant models
together <- together[,-1] # remove first column (rownames)
together <- matrix(as.numeric(together), ncol=8) # effects need to be numeric for correlation with cor() 
rownames(together) <- c(accessions_47$germplasmName) # want to keep names with this 
colnames(together) <- c("Multi_PrEff","Multi_AsEff","Multi_SI1", "Multi_SI2", "Uni_SI2", "Uni_SI1", "Uni_PrEff", "Uni_AsEff") # changing to numeric gets rid of column names, so adding them back for order

cor(together[,"Multi_PrEff"], together[,"Uni_PrEff"]) # correlation between Producer Effects of Multi and Uni
cor(together[,"Multi_AsEff"], together[,"Uni_AsEff"]) # correlation between Associative Effects of Multi and Uni

cor(together[,"Multi_PrEff"], together[,"Multi_AsEff"]) # correlation between Producer and Associative Effects (Multi)
cor(together[,"Uni_PrEff"], together[,"Uni_AsEff"]) # correlation between Producer and Associative Effects (Uni)

#write.csv(together, 'data/springOatPea_comparingMultiUni.csv')
