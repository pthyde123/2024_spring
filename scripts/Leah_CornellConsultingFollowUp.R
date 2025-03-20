# try taking out one of the IL17-7334 and IL17-7339, instead of combining
# changing the balance, this might matter

multi_location_data <- read.csv("data/2023_2024_multi_location.csv")

#list of accessions in alphabetical order
accessions_48 <- multi_location_data %>% 
  dplyr::select(germplasmName) %>% 
  arrange(germplasmName) %>% 
  unique()

multi_location_data2 <- multi_location_data[multi_location_data$germplasmName != "IL17-7339", ]

#list of accessions in alphabetical order
accessions_47 <- multi_location_data2 %>% 
  dplyr::select(germplasmName) %>% 
  arrange(germplasmName) %>% 
  unique()



GRM <- read_tsv("data/SOPF_48_GRM_t3.tsv")  
GRM2 <- as.data.frame(GRM)
rownames(GRM2) <- GRM2[, 1] # Convert first column to row names
GRM2 <- GRM2[, -1] # Remove the first column
GRM2$LEGGETT <- NA
GRM2$NEWBURG <- NA
# Set all values in LEGGET and NEWBERG rows to NA
GRM2["LEGGETT", ] <- NA
GRM2["NEWBURG", ] <- NA

# remove IL17-7339
GRM2 <- GRM2[!(rownames(GRM2) %in% c("IL17-7334")),  # Remove those two accessions from GRM rows
             !(colnames(GRM2) %in% c("IL17-7334"))]  # Remove those two accessions from GRM columns

# order
GRM2 <- GRM2 %>%
  dplyr::select(order(colnames(GRM2))) %>% # put columns(accessions) in alpha order to match incOatAcc
  arrange(rownames(GRM2)) # put rows(accessions) in alpha order to match incOatAcc

GRM2 <- as.matrix(GRM2)  # convert GRM to matrix

# Get average of the diagonal
# Extract diagonal values from the data frame and calculate their average
diagonal_values <- diag(GRM2)  # Extract diagonal values
average_diagonal <- mean(diagonal_values, na.rm = TRUE)  # average of the diagonal

# Replace the LEGGETT and NEWBURG value(1) with average_diagonal
# Set all values in LEGGET and NEWBERG columns to 0
GRM2[,"LEGGETT" ] <- 0
GRM2[,"NEWBURG" ] <- 0
# Set all values in LEGGET and NEWBERG rows to 0
GRM2["LEGGETT", ] <- 0
GRM2["NEWBURG", ] <- 0

# replace cell values on diagonal
GRM2["LEGGETT", "LEGGETT"] <- average_diagonal
GRM2["NEWBURG", "NEWBURG"] <- average_diagonal

GRM3 <- GRM2
diag(GRM3) <- diag(GRM3) + 0.0001

grainWgt <- multi_location_data2 %>%
  mutate(oatYield = oat_yield) %>%
  mutate(peaYield = pea_yield) %>%
  mutate(total_grain = oat_yield + coalesce(pea_yield, 0))%>%
  mutate(peaAcc = peaName) %>%
  mutate(blockNumberF=as.factor(paste(studyYear, location, blockNumber))) %>% #block factor
  dplyr::select(blockNumberF, studyYear, location, blockNumber,
                plotNumber, management, germplasmName, peaAcc, oatYield, peaYield, total_grain) 

# remove rows from the data file only if they have an NA value for both oat and pea yield
# NAs were messing with the ability to calculate the covariance matrix correctly
grainWgt <- grainWgt[!(is.na(grainWgt$oatYield) & is.na(grainWgt$peaYield)), ] 

#pth: the na's for the mono pea accession were causing error 
#Error in chol.default(S) : the leading minor of order 1 is not positive
#this fixed the error, but is it correct
grainWgt <- grainWgt %>% 
  mutate(peaAcc = if_else(is.na(peaAcc), "none", peaAcc))  

grainWgt$studyYear <- as.factor(grainWgt$studyYear)
grainWgt$location <- as.factor(grainWgt$location)
grainWgt$blockNumber <- as.factor(grainWgt$blockNumber)

# try without monoculture plots
grainWgt2 <- grainWgt[grainWgt$management != "monoculture", ]

yTraits <- as.matrix(dplyr::select(grainWgt, contains("Yield")))# pulls oat yield and pea yield ; taking factors with yield in them from grain weight table and making them a matrix # could have as many y variables in the matrix as you want, will run each on their own but no limit to what matrix holds
#factors into matrices
incLocations <- model.matrix(~ -1 + location, grainWgt) # 0,1 for if plots existed in given location or not; going into grain weight table, pulling out location data, and turns it into 0,1 somehow 
incBlocks <- model.matrix(~ -1 + blockNumberF, grainWgt)
incOatAcc <- model.matrix(~ -1 + germplasmName, grainWgt)
incPeaAcc <- model.matrix(~ -1 + peaAcc, grainWgt)
incYear <- model.matrix(~ -1 + studyYear, grainWgt) #year factor
incMngt <- model.matrix(~ -1 + management, grainWgt) # monoculture/intercrop factor

# re-do without monoculture plots
yTraits2 <- as.matrix(dplyr::select(grainWgt2, contains("Yield")))
incLocations2 <- model.matrix(~ -1 + location, grainWgt2) 
incBlocks2 <- model.matrix(~ -1 + blockNumberF, grainWgt2)
incOatAcc2 <- model.matrix(~ -1 + germplasmName, grainWgt2)
incPeaAcc2 <- model.matrix(~ -1 + peaAcc, grainWgt2)
incYear2 <- model.matrix(~ -1 + studyYear, grainWgt2)

K = incOatAcc %*% GRM2 %*% t(incOatAcc)
# try with the matrix with 0.001 added to the diagonal 
K2 = incOatAcc %*% GRM3 %*% t(incOatAcc)

ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            list(X=incMngt, model="BRR"),
            list(K = K, model="RKHS")) 

bglr_model <- BGLR::Multitrait(yTraits, ETA, intercept=TRUE,
                               resCov=list(df0=4,S0=NULL,type="UN"),
                               R2=0.5,
                               nIter=10000, burnIn=2000,
                               thin=10, saveAt="",verbose=FALSE)

oatEff <- bglr_model$ETA[[6]]$beta # BLUP for accessions
oatEff

# add oat names to effect results
colnames(oatEff) <- c("PrEff", "AsEff")
rownames(oatEff) <- c(accessions_47$germplasmName)

write.csv(oatEff, '/Users/leahtreffer/Desktop/oatEff.csv')
# really make sure row order is staying the same
