
 
#The R project should define the working directory properly for both of us and we can leave it out.  We could also use the "here" package the JL and Mirza are into.
getwd()


#### Library ####

library(vcfR)  # https://grunwaldlab.github.io/Population_Genetics_in_R/reading_vcf.html
library(tidyverse)
library(genomicMateSelectR) #Cool this is Marnin's
##https://wolfemd.github.io/genomicMateSelectR/reference/index.html
library(rrBLUP)


#### Import and set data ####

# Importing Phenotype file (Leah complied in other scripts and added a copy to the shared directory)
multi_location_data <- read.csv("data/2023_2024_multi_location.csv")  ##pth the previous file path will not work on both of our computers.

#list of accessions in alphabetical order, the same as "incOatAcc" # this is not needed anymore
accessions_48 <- multi_location_data %>% 
  select(germplasmName) %>% 
  arrange(germplasmName) %>% 
  unique()

accessions_48$germplasmName


# GRM strait from t3, cool, but it looks like not all the accessions from Juan are on t3. we are missing 2 but we can move forward
GRM <- read_tsv("data/SOPF_48_GRM_t3.tsv")  



#### Leah BGLR with Locations, Blocks, OatAcc, PeaAcc, Year, Mngt(inter/mono) ####


grainWgt <- multi_location_data %>%
  mutate(oatYield = oat_yield) %>%
  mutate(peaYield = pea_yield) %>%
  mutate(peaAcc = peaName) %>%
  mutate(blockNumberF=as.factor(paste(studyYear, location, blockNumber))) %>% #block factor
  select(blockNumberF, studyYear, location, blockNumber,
         plotNumber, management, germplasmName, peaAcc, oatYield, peaYield) 

# remove rows from the data file only if they have an NA value for both oat and pea yield
# NAs were messing with the ability to calculate the covariance matrix correctly
grainWgt <- grainWgt[!(is.na(grainWgt$oatYield) & is.na(grainWgt$peaYield)), ] 




#pth: the na's for the mono pea accession were causing error 
#Error in chol.default(S) : the leading minor of order 1 is not positive
#this fixed the error, but is it correct
grainWgt <- grainWgt %>% 
  mutate(peaAcc = if_else(is.na(peaAcc), "none", peaAcc))  


grainWgt



### Create matrix for response and factors#### 

yTraits <- as.matrix(dplyr::select(grainWgt, contains("Yield")))# pulls oat yield and pea yield ; taking factors with yield in them from grain weight table and making them a matrix # could have as many y variables in the matrix as you want, will run each on their own but no limit to what matrix holds
#factors into matrices
incLocations <- model.matrix(~ -1 + location, grainWgt) # 0,1 for if plots existed in given location or not; going into grain weight table, pulling out location data, and turns it into 0,1 somehow 
incBlocks <- model.matrix(~ -1 + blockNumberF, grainWgt)
incOatAcc <- model.matrix(~ -1 + germplasmName, grainWgt)
incPeaAcc <- model.matrix(~ -1 + peaAcc, grainWgt)
incYear <- model.matrix(~ -1 + studyYear, grainWgt) #year factor
incMngt <- model.matrix(~ -1 + management, grainWgt) # monoculture/intercrop factor


#### Create GRM and K ####

GRM <- GRM %>%    #GRM is pulled from t3, 2 accessions are missing, they get neutral values, I think this is ok for this selection
  select(order(colnames(GRM))) %>% # put columns(accessions) in alpha order to match incOatAcc
  arrange(stock_uniquename) %>% # put rows(accessions) in alpha order to match incOatAcc
  select(-stock_uniquename)  # remove stock_uniquename 

GRM <- as.matrix(GRM)  # convert GRM to matrix

#  use K = Z %*% GRM %*% t(Z),  Z is incidence matrix
K = incOatAcc %*% GRM %*% t(incOatAcc)

# list of factor matrices
ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incOatAcc, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            list(X=incMngt, model="BRR"),
            list(K = K, model="RKHS")) 
ETA01 <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            list(X=incMngt, model="BRR"),
            list(K = K01, model="RKHS")) 


# the order of these is critical: nothing is labeled in matrices, so we need to know it's position to pull the right info out of results (through it's number rather than a name)


#### BGLR Model ####

# this will take a few seconds to run, ?how doe we evaluate the best number of nIter and burnIn?
# will set.seed make this reproducible?

tst2 <- BGLR::Multitrait(yTraits, ETA, intercept=TRUE,
                         resCov=list(df0=4,S0=NULL,type="UN"),
                         R2=0.5,
                         nIter=10000, burnIn=2000,
                         thin=10, saveAt="",verbose=FALSE)
tst3 <- BGLR::Multitrait(yTraits, ETA01, intercept=TRUE,
                         resCov=list(df0=4,S0=NULL,type="UN"),
                         R2=0.5,
                         nIter=10000, burnIn=2000,
                         thin=10, saveAt="",verbose=FALSE)


#### BGLR Results ####

oatEff <- tst2$ETA[[3]]$beta


plot(oatEff, xlab="PrEff", ylab="AsEff",
     cex.lab=1.3, cex.axis=1.3, pch=16)


oatEffSD <- tst2$ETA[[3]]$SD.beta


oatName<-colnames(ETA[[3]]$X) %>% 
  as_tibble() %>% 
  mutate("oatName" = str_remove(value,"germplasmName")) %>% 
  select(oatName)

colnames(oatEff) <- c("PrEff", "AsEff")
rownames(oatEff) <- c(oatName$oatName)


#### Crossing Selections ####

### top SI_1 
oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  mutate(SI_3 = (PrEff + 2*AsEff)) %>% 
  mutate(SI_4 = (PrEff - 2*AsEff)) %>% 
  arrange(-SI_1)

### top SI_2
oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  arrange(-SI_2)


#Selections, hmm lots of overlap in these groups 

oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  filter(SI_1 > 14 | SI_2 > 14) 




oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  filter(SI_1 > 14 | SI_2 > 8.31) %>% # found this number from the previous two,

  write.table("clipboard",sep="\t",row.names = F)  # planted these 12-24-24
  # a little shoot first, we have plenty of seed


