# read in phenotype data
multi_location_data <- read.csv("/Users/leahtreffer/GitHub/Peter_2024_spring/Peter_2024_spring/data/2023_2024_multi_location.csv")



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





