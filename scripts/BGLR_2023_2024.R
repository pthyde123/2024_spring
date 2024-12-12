#### Importing Phenotype file (Leah complied in other scripts and added a copy to the shared directory)
multi_location_data <- read.csv("/Users/leahtreffer/GitHub/Peter_2024_spring/Peter_2024_spring/data/2023_2024_multi_location.csv")


#### Importing Genotype data and creating Relationship matrix

library(vcfR)  # https://grunwaldlab.github.io/Population_Genetics_in_R/reading_vcf.html
library(tidyverse)
library(genomicMateSelectR) #Cool this is Marnin's
##https://wolfemd.github.io/genomicMateSelectR/reference/index.html
library(rrBLUP)

# on my scratch pad I usually use df and clean it up later
vcf <- read.vcfR("data/SOPF_48_genotypes.vcf")  ## this still isnt all of the genotypes

head(getFIX(vcf))

marker_id <-  getFIX(vcf)[,3]  # get a list of the marker names

df <- as.data.frame(vcf@gt) %>% # save as a dataframe
  select(-FORMAT) # remove the FORMAT column

row.names(df) <- marker_id # set the column names as marker names

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


####




yTraits <- as.matrix(dplyr::select(grainWgt, contains("Yield")))
incLocations <- model.matrix(~ -1 + locationName, grainWgt)
incBlocks <- model.matrix(~ -1 + blockNumberF, grainWgt)
incOatAcc <- model.matrix(~ -1 + germplasmName, grainWgt)
incPeaAcc <- model.matrix(~ -1 + peaAcc, grainWgt)


ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incOatAcc, model="BRR"),
            list(X=incPeaAcc, model="BRR"))
