library(tidyverse)
library(readr)
library(kableExtra)
library(here)
require(BGLR)
library(genomicMateSelectR) 
library(janitor)
library(ggplot2)
library(ggrepel) 

# read in phenotype data
##multi_location_data <- read.csv("/Users/leahtreffer/GitHub/Peter_2024_spring/Peter_2024_spring/data/2023_2024_multi_location.csv")



multi_location_data <- read.csv("data/2023_2024_multi_location.csv")  ##pth the previous file path will not work on both of our computers. 
#The R project should define the working directory properly for both of us and we can leave it out.  We could also use the "here" package the JL and Mirza are into.
getwd()


library(vcfR)  # https://grunwaldlab.github.io/Population_Genetics_in_R/reading_vcf.html
library(tidyverse)
devtools::install_github("wolfemd/genomicMateSelectR", ref = 'master') 
library(genomicMateSelectR) #Cool this is Marnin's
##https://wolfemd.github.io/genomicMateSelectR/reference/index.html
library(rrBLUP)


# Genotype data

##genomic data (snp data)
###Genotype data as GRM
Mirza_WOgenotype <- read.csv("/Users/leahtreffer/GitHub/Mirza_savedCombineJL.csv") |> 
  column_to_rownames(var = "rowname") # Mirza's data as reference
WOgenotype <- 

#convert to -1,0,1
WOgenotype <- WOgenotype - 1

#find out the accessions that are in intercrop trial and genotype data
##Since the format of the accession names are different, I will modify the names first
#standardize the format to find the same accession name
interAcc <- doublePlot |> 
  select(accession) |> 
  unique() |> 
  mutate(accession_2 = toupper(accession)) |> 
  mutate(accession_2 = gsub("[ _-]", "", accession_2))

WOgenotype <- WOgenotype |> 
  rownames_to_column(var = "accession")

genotypeAcc <- WOgenotype |> 
  mutate(accession_2 = toupper(accession)) |> 
  mutate(accession_2 = gsub("[ _-]", "", accession_2)) 

#using synonym
selected_rows <- genotypeAcc |> 
  filter(accession %in% c("PI555733", "PI555736")) #PA7617-3658 and PA7617-3460

#New genotype matrix
#the same accessions
genotypeMatrix <- genotypeAcc |> 
  semi_join(interAcc, c("accession_2" = "accession_2")) |> 
  rbind(selected_rows)

#delete the last columns
genotypeMatrix <- genotypeMatrix[, -ncol(genotypeMatrix)] |> 
  column_to_rownames(var = "accession") #56 accessions and for relationship matrix

#different accessions: to get info for missing genotype data
diffAccMatrix <- interAcc |>
  anti_join(genotypeAcc, c("accession_2" = "accession_2")) |> 
  filter(accession != "PA7617-3658", accession != "PA7617-3460") #38 accessions

# write.csv(genotypeMatrix, here::here("Output","SameAccessionMatrixGenotype.csv"), col.names = T)
# write.csv(diffAccMatrix, here::here("Output","DiffAccessionMatrixGenotype.csv"), col.names = T)




# need to add genotype data to this
grainWgt <- multi_location_data %>%
  mutate(oatYield = oat_yield) %>%
  mutate(peaYield = pea_yield) %>%
  mutate(peaAcc = peaName) %>%
  mutate(blockNumberF=as.factor(paste(studyYear, location, blockNumber))) %>% #block factor
  select(blockNumberF, studyYear, location, blockNumber,
         plotNumber, management, germplasmName, peaAcc, oatYield, peaYield) 
# I'm getting an error when I run the tst2 <- BGLR:Multitrait() 
# Error in chol.default(S) : the leading minor of order 1 is not positive

# residual covariance matrix S0 which (wrongly) might not be positive definite
# S0=NULL estimates the residual covariance matrix from the data
# one possibility is that the NAs are messing with the ability to calculate the covariance matrix correcly 

grainWgt <- grainWgt[!(is.na(grainWgt$oatYield) & is.na(grainWgt$peaYield)), ] # remove rows from the data file only if they have an NA value for both oat and pea yield
# this fixed it

# this makes it look nice in R markdown and shortens the talbe window that shows, with scroll optino
# not all these columns are necessary, could be paired down
grainWgt %>% 
  kable(align = "c") %>% 
  kable_classic(full_width=F,position = "left")%>% 
  scroll_box(width = "900px", height = "300px")

### Create matrix for response and factors 
yTraits <- as.matrix(dplyr::select(grainWgt, contains("Yield")))# pulls oat yield and pea yield ; taking factors with yield in them from grain weight table and making them a matrix # could have as many y variables in teh matrix as you want, will run each on thier own but no limit to waht matrix holds
#factors into matrices
incLocations <- model.matrix(~ -1 + location, grainWgt) # 0,1 for if plots existed in given location or not; going into grain weight table, pulling out location data, and turns it into 0,1 somehow 
incBlocks <- model.matrix(~ -1 + blockNumberF, grainWgt)
incOatAcc <- model.matrix(~ -1 + germplasmName, grainWgt)
incPeaAcc <- model.matrix(~ -1 + peaAcc, grainWgt)
incYear <- model.matrix(~ -1 + studyYear, grainWgt) #year factor
incMngt <- model.matrix(~ -1 + management, grainWgt) # monoculture/intercrop factor

# list of factor matrices
ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incOatAcc, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            list(X=incMngt, model="BRR")) 
# the order of these is critical: nothing is labeled in matrices, so we need to know it's position to pull the right info out of results (through it's number rather than a name)

# then run BGLR::Multitrait


#Selection index for oat:
##Pr + As = good oats + good on pea
##Pr - As = good oat + not good pea




##################### 01/22/2025 #######################################
## JL had some suggestions: Oat Accession and the K matrix were both originally used, but they are both for the independent effect of oat. K has the additional info from the GRM so it makes sense to use just that instead of both
## Will also concider general mixing ability vs Producer/Associative effects

# This part stays the same: 

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



### BGLR set up####


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

# Remove 'germplasmName' from the beginning of each column name in incOatAcc
#colnames(incOatAcc) <- gsub("^germplasmName", "", colnames(incOatAcc))

#### Create GRM and K ####

GRM <- GRM %>%    #GRM is pulled from t3, 2 accessions are missing, they get neutral values, I think this is ok for this selection
  select(order(colnames(GRM))) %>% # put columns(accessions) in alpha order to match incOatAcc
  arrange(stock_uniquename) %>% # put rows(accessions) in alpha order to match incOatAcc
  select(-stock_uniquename)  # remove stock_uniquename 

GRM <- as.matrix(GRM)  # convert GRM to matrix

#  use K = Z %*% GRM %*% t(Z),  Z is incidence matrix
K = incOatAcc %*% GRM %*% t(incOatAcc)

# *** either here or later down, I need to get the oat names related to this matrix.
# this will let me add them to the tibble table so the effect values are shown with the oat name


# new

# list of factor matrices
ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            list(X=incMngt, model="BRR"),
            list(K = K, model="RKHS")) 


#### BGLR Model ####

tst2 <- BGLR::Multitrait(yTraits, ETA, intercept=TRUE,
                         resCov=list(df0=4,S0=NULL,type="UN"),
                         R2=0.5,
                         nIter=10000, burnIn=2000,
                         thin=10, saveAt="",verbose=FALSE)

#### Pull out effect scores####

oatEff <- tst2$ETA[[6]]$beta # look at effect of K (accession with GRM)

plot(oatEff, xlab="PrEff", ylab="AsEff",
     cex.lab=1.3, cex.axis=1.3, pch=16)

oatEffSD <- tst2$ETA[[6]]$SD.beta

# map names back to K matrix
incOatAcc_names <- colnames(incOatAcc) # Extract the column names of incOatAcc
# K's row and column indices are based on the rows of incOatAcc during the multiplication
K_names <- incOatAcc_names  # Use these names in the order they correspond to K
# Save the names in the same order as they occur in K
names_in_K_order <- K_names

# add oat names to identify rows
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



# try taking OatAcc out of ETA
# needs to be variation in Pr and As effects
# if tightly correlated, there isnt much varation in diagonal (general mixing ability)

# fit the general mixing ability (total plot yield) form and PR/AS form of model








