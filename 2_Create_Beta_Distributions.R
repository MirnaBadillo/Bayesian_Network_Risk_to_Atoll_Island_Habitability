
#------------------------------------------
# Code to create the Beta distributions
#----------------------------------------
rm(list=ls())

# Paths to import modules
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # For RStudio users

source("1_Path_config.R")

source(paste0(base_path, "Module_Optimization_functions.R"))

# --------------
# PARAMS 
#---------------
x_values <- seq(0, 1, by = 1/6) # Support of the Beta distibution
interval <- 1:6

for (i in interval) {
  
    #-----------------------------------
    # SHAPE PARAMETERS ALPHA AND BETA
    #-------------------------------------
    
    # This optimization function looks for the first combination of optimized params that meets the standard deviation condition
    optimal_first_params_CL1 <- optim(par = params_first(i, 1), fn = fr_first, CL = 1, method = "BFGS")$par
    optimal_first_params_CL2 <- optim(par = params_first(i, 2), fn = fr_first, CL = 2, method = "BFGS")$par
    optimal_first_params_CL3 <- optim(par = params_first(i, 3), fn = fr_first, CL = 3, method = "BFGS")$par
    optimal_first_params_CL4 <- optim(par = params_first(i, 4), fn = fr_first, CL = 4, method = "BFGS")$par

    # The second optimization function looks for the second combination of optimized params that meets the probability N value 
    optimal_second_params_CL1 <- optim(par = optimal_first_params_CL1, fn = fr_second,x=x_values, CL = 1, interval = i, method = "BFGS")$par
    optimal_second_params_CL2 <- optim(par = optimal_first_params_CL2, fn = fr_second,x=x_values, CL = 2, interval = i, method = "BFGS")$par
    optimal_second_params_CL3 <- optim(par = optimal_first_params_CL3, fn = fr_second,x=x_values, CL = 3, interval = i, method = "BFGS")$par
    optimal_second_params_CL4 <- optim(par = optimal_first_params_CL4, fn = fr_second,x=x_values, CL = 4, interval = i, method = "BFGS")$par
    
    # Cumulative distribution function for each Confidence level
    cdf_CL1 <- pbeta(x_values, optimal_second_params_CL1[1], optimal_second_params_CL1[2]) 
    cdf_CL2 <- pbeta(x_values, optimal_second_params_CL2[1], optimal_second_params_CL2[2])    
    cdf_CL3 <- pbeta(x_values, optimal_second_params_CL3[1], optimal_second_params_CL3[2])
    cdf_CL4 <- pbeta(x_values, optimal_second_params_CL4[1], optimal_second_params_CL4[2])
  
    # The probability of each interval for each Confidence level is calculated
    CDF_prob_int_CL1 <- diff(cdf_CL1)
    CDF_prob_int_CL2 <- diff(cdf_CL2)
    CDF_prob_int_CL3 <- diff(cdf_CL3)
    CDF_prob_int_CL4 <- diff(cdf_CL4)
    
    CL1_prob_interval <- Int_prob_save(CDF_prob_int_CL1, i, 1)
    CL2_prob_interval <- Int_prob_save(CDF_prob_int_CL2, i, 2)
    CL3_prob_interval <- Int_prob_save(CDF_prob_int_CL3, i, 3)
    CL4_prob_interval <- Int_prob_save(CDF_prob_int_CL4, i, 4)
    CL5_prob <- CL5_prob_interval(i)
      
    i = i+1
}

