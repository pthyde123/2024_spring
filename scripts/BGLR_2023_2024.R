#### Importing Phenotype file (Leah complied in other scripts and added a copy to the shared directory)


multi_location_data <- read.csv("data/2023_2024_multi_location.csv")  ##pth the previous file path will not work on both of our computers. 
#The R project should define the working directory properly for both of us and we can leave it out.  We could also use the "here" package the JL and Mirza are into.
getwd()


#### Importing Genotype data and creating Relationship matrix

library(vcfR)  # https://grunwaldlab.github.io/Population_Genetics_in_R/reading_vcf.html
library(tidyverse)
library(genomicMateSelectR) #Cool this is Marnin's
##https://wolfemd.github.io/genomicMateSelectR/reference/index.html
library(rrBLUP)

# on my scratch pad I usually use df and clean it up later
# load in the 34 genotypes 
vcf_34 <- read.vcfR("data/SOPF_48_genotypes.vcf")  ## this still only 34 genotypes

head(getFIX(vcf_34))

marker_id <-  getFIX(vcf_34)[,3]  # get a list of the marker names

df_34 <- as.data.frame(vcf_34@gt) %>% # save as a dataframe
  select(-FORMAT) # remove the FORMAT column

row.names(df_34) <- marker_id # set the column names as marker names

# load in the other genotypes 
vcf_14 <- read.vcfR("data/")  ## this will be the file with the other 14 genotypes

head(getFIX(vcf_14))

marker_id <-  getFIX(vcf_14)[,3]  # get a list of the marker names

df_14 <- as.data.frame(vcf_14@gt) %>% # save as a dataframe
  select(-FORMAT) # remove the FORMAT column

row.names(df_14) <- marker_id # set the column names as marker names

# combine using an outer join to keep all rows even if NA
df <- merge(df_34, df_14, by=0, all=TRUE) # 'by=0' should merge based on rowname

# convert to  -1,0,1 format
df[df == "0/0"] <- -1   # rename calls
df[df == "0/1"] <- 0
df[df == "1/1"] <- 1

df <- df %>%  mutate_if(is.character, as.numeric) # set all columns to numeric


df <- t(df) # transpose so genotypes are row names

# more clean up might be needed 
  # remove samples with to many NA's
  # remove samples with to many heterozygous markers

GRM <- A.mat(df) # calculate relationship matrix

GRM

#next step
###use K = Z %*% GRM %*% t(Z),  Z is incidence matrix


#### Leah BGLR with Locations, Blocks, OatAcc, PeaAcc, Year, Mngt(inter/mono) ####

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


grainWgt <- grainWgt %>% 
  mutate(peaAcc = if_else(is.na(peaAcc), "none", peaAcc))  #pth: the na's for the mono pea accession were causing error 
                  #Error in chol.default(S) : the leading minor of order 1 is not positive
                  #this fixed the error, but is it correct
grainWgt



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


tst2 <- BGLR::Multitrait(yTraits, ETA, intercept=TRUE,
                         resCov=list(df0=4,S0=NULL,type="UN"),
                         R2=0.5,
                         nIter=1000, burnIn=200,
                         thin=10, saveAt="",verbose=FALSE)


oatEff <- tst2$ETA[[3]]$beta

oatEffSD <- tst2$ETA[[3]]$SD.beta


oatName<-colnames(ETA[[3]]$X) %>% 
  as_tibble() %>% 
  mutate("oatName" = str_remove(value,"germplasmName")) %>% 
  select(oatName)


colnames(oatEff) <- c("PrEff", "AsEff")
rownames(oatEff) <- c(oatName$oatName)


oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  arrange(-SI_1)

oatEff %>% 
  as_tibble(rownames = NA) %>% 
  rownames_to_column() %>% 
  mutate(SI_1 = (PrEff + AsEff)) %>% 
  mutate(SI_2 = (PrEff - AsEff)) %>% 
  arrange(-SI_2)







#### Making GRM with the VCF file Mirza created in using GenomeStudio ####
# The upload of this data to T3 is proving tricky due to call formats
# Should be able to merge this with the genotype data from T3 and move forward

# Next step, select the accessions that are not on T3, (there are duplicates)
# merge them into a df that has 48 accessions 



library(vcfR)  # https://grunwaldlab.github.io/Population_Genetics_in_R/reading_vcf.html
library(tidyverse)
library(genomicMateSelectR) #Cool this is Marnin's
##https://wolfemd.github.io/genomicMateSelectR/reference/index.html
library(rrBLUP)

# on my scratch pad I usually use df and clean it up later
vcf <- read.vcfR("data/JL01.vcf")  ##

library(readxl)
Jl01_vcf_accession <- read_excel("data/Jl01_vcf_sample_name_to_accession_name.xlsx")

accession <- Jl01_vcf_accession$accession


head(getFIX(vcf))

marker_id <-  getFIX(vcf)[,3]  # get a list of the marker names

df <- as.data.frame(vcf@gt) # save as a dataframe

row.names(df) <- marker_id # set the column names as marker names  

df[df == "AA"] <- 1
df[df == "AB"] <- 0
df[df == "BB"] <- -1
df[df == "NULL"] <- NA
df[df == "NC"] <- NA

df <- df %>%  mutate_if(is.character, as.numeric) # set all columns to numeric

colnames(df) <- accession 

df <- df %>% 
  select("A25":"WI_X10710-7", "A10")

df <- t(df)

df

GRM <- A.mat(df) # calculate relationship matrix

GRM




