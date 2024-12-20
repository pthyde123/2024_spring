
yTraits <- as.matrix(dplyr::select(grainWgt, contains("Yield")))# pulls oat yield and pea yield ; taking factors with yield in them from grain weight table and making them a matrix # could have as many y variables in the matrix as you want, will run each on their own but no limit to what matrix holds
#factors into matrices
incLocations <- model.matrix(~ -1 + location, grainWgt) # 0,1 for if plots existed in given location or not; going into grain weight table, pulling out location data, and turns it into 0,1 somehow 
incBlocks <- model.matrix(~ -1 + blockNumberF, grainWgt)
incOatAcc <- model.matrix(~ -1 + germplasmName, grainWgt)
incPeaAcc <- model.matrix(~ -1 + peaAcc, grainWgt)
incYear <- model.matrix(~ -1 + studyYear, grainWgt) #year factor
#incMngt <- model.matrix(~ -1 + management, grainWgt) # monoculture/intercrop factor
# list of factor matrices
ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incOatAcc, model="BRR"),
            list(X=incPeaAcc, model="BRR"),
            list(X=incYear, model="BRR"),
            #list(X=incMngt, model="BRR"),
            list(K = K, model="RKHS")) 

# iteratively test different values of burnIn and nIter to find a good fit for your BGLR model
# use convergence diagnostics and effective sample size (ESS) as evaluation criteria

library(BGLR)
library(coda)

# Define the function to test different combinations of nIter and burnIn
test_model <- function(yTraits, ETA, nIter_values, burnIn_values, thin = 10) {
  results <- data.frame(nIter = numeric(), burnIn = numeric(), ESS = numeric(), Converged = logical())
  
  for (nIter in nIter_values) {
    for (burnIn in burnIn_values) {
      cat("Testing nIter =", nIter, "and burnIn =", burnIn, "\n")
      
      # Fit the model
      model <- BGLR::Multitrait(yTraits, ETA,
                                resCov = list(df0 = 4, S0 = NULL, type = "UN"),
                                nIter = nIter, burnIn = burnIn, thin = thin, verbose = FALSE)
      
      # Extract posterior samples
      posterior_samples <- as.mcmc(model$ETA[[1]]$b)  # Example: first ETA effect
      
      # Calculate Effective Sample Size (ESS)
      ess <- effectiveSize(posterior_samples)
      
      # Check convergence using Gelman-Rubin diagnostic (requires at least 2 chains)
      converged <- all(gelman.diag(as.mcmc.list(list(posterior_samples, posterior_samples)))$mpsrf < 1.1)
      
      # Store results
      results <- rbind(results, data.frame(nIter = nIter, burnIn = burnIn, ESS = min(ess), Converged = converged))
    }
  }
  
  return(results)
}


# Define ranges for nIter and burnIn to test
nIter_values <- seq(5000, 20000, 5000)  # e.g., 5,000 to 20,000
burnIn_values <- seq(1000, 5000, 1000)  # e.g., 1,000 to 5,000

# Run the testing function
results <- test_model(yTraits, ETA, nIter_values, burnIn_values)

# Inspect results
print(results)


# Effective Sample Size (ESS): Ensures sufficient independent samples are collected 
## normal to have at least ESS=200

#Convergence: Assessed using the Gelman-Rubin diagnostic

test_model <- function(yTraits, ETA, nIter_values, burnIn_values) {
  results <- list()
  for (nIter in nIter_values) {
    for (burnIn in burnIn_values) {
      cat("Testing nIter =", nIter, ", burnIn =", burnIn, "\n")
      result <- tryCatch({
        BGLR::Multitrait(y = yTraits, ETA = ETA,
                         resCov = list(df0 = 4, S0 = diag(ncol(yTraits)), type = "UN"),
                         nIter = nIter, burnIn = burnIn, thin = 10, verbose = FALSE)
      }, error = function(e) e)
      results[[paste(nIter, burnIn, sep = "_")]] <- result
    }
  }
  return(results)
}

nIter_values <- c(5000, 10000, 20000)
burnIn_values <- c(1000, 2000, 5000)
results <- test_model(yTraits, ETA, nIter_values, burnIn_values)
print(results)








library(BGLR)
library(coda)

# Define a function to test combinations of nIter and burnIn
test_model <- function(yTraits, ETA, nIter_values, burnIn_values) {
  results <- data.frame(nIter = integer(), burnIn = integer(), ESS = numeric(), Converged = logical())
  
  for (nIter in nIter_values) {
    for (burnIn in burnIn_values) {
      cat("Testing nIter =", nIter, ", burnIn =", burnIn, "\n")
      
      # Run the model
      result <- tryCatch({
        model <- BGLR::Multitrait(
          y = yTraits,
          ETA = ETA,
          resCov = list(df0 = 4, S0 = diag(ncol(yTraits)), type = "UN"),
          nIter = nIter,
          burnIn = burnIn,
          thin = 10,
          verbose = FALSE
        )
        
        # Extract posterior samples for ESS and convergence
        posterior_samples <- as.mcmc(model$ETA[[1]]$b)
        ess <- effectiveSize(posterior_samples)
        mean_ess <- mean(ess, na.rm = TRUE)
        gelman <- tryCatch(gelman.diag(as.mcmc.list(list(posterior_samples)), autoburnin = FALSE), error = function(e) NULL)
        
        # Check convergence (Gelman diagnostic R-hat < 1.1)
        converged <- if (!is.null(gelman)) all(gelman$psrf[, "Point est."] < 1.1, na.rm = TRUE) else FALSE
        
        # Store results if ESS > 200 and converged
        if (converged){
        #if (mean_ess > 200 && converged) {
          results <- rbind(results, data.frame(nIter = nIter, burnIn = burnIn, ESS = mean_ess, Converged = converged))
        }
      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
      })
    }
  }
  
  # Return filtered results
  return(results)
}

# Define the parameter ranges
nIter_values <- c(5000, 10000, 20000, 30000, 50000, 55000, 100000)
burnIn_values <- c(1000, 2000, 5000, 6000, 10000, 12000, 20000)

# Run the function
results <- test_model(yTraits, ETA, nIter_values, burnIn_values)

# Print the results
print(results)



