#------------------------------------------------------------------------------
# The first optimization function finds the combination of α and β that best 
# satisfies the standard deviation condition : SD = 0.25, 0.20, 0.15, 0.10 
#------------------------------------------------------------------------------

fr_first <- function(params, CL) {
  alpha <- params[1]
  beta <- params[2]
  
  if (CL == 1){
    
    Var <- (alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1))
    SD <- sqrt(Var)
    abs(SD - 0.25)
    
  } else 
    if (CL == 2){
      
      Var <- (alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1))
      SD <- sqrt(Var)
      abs(SD - 0.20)
      
    } else if (CL == 3){
      Var <- (alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1))
      SD <- sqrt(Var)
      abs(SD - 0.15)
      
    } else if (CL == 4){
      Var <- (alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1))
      SD <- sqrt(Var)
      abs(SD - 0.10)
    }
}


#--------------------------------------------------------------------------
# Initial parameters used in the first optimization function.
# A combination of alpha and beta parameters where selected for each interval and 
# each confidence level 
# ------------------------------------------------------------------------

# List of initial params
params_list_C1 <- list(
  list(c(1, 4.8), c(1, 6), c(1, 8), c(1, 16)),
  list(c(1.5, 3.2), c(3, 8), c(3, 15), c(3, 16)),
  list(c(2, 3), c(2, 3), c(4, 6), c(8, 12)),
  list(c(3, 2), c(3, 2), c(6, 4), c(12, 8)),
  list(c(3.2, 1.5), c(8, 3), c(15, 3), c(16, 3)),
  list(c(4.8, 1), c(6, 1), c(8, 1), c(16, 1))
)

params_first <- function(interval, CL) {
 
  params_list_C1[[interval]][[CL]]
}

#-----------------------------------------------------------------------------
# The second function uses the α and β values found earlier as the initial 
# parameters to find a new combination of α and β hat best satisfies two conditions:
# 1) Probability weight condition: CL 1 = 0.3, CL 2 = 0.4, CL 3 = 0.6, CL 4 = 0.8.
# 2) Asymmetry correction
#-----------------------------------------------------------------------------

fr_second <- function(params, x, CL, interval) {
  alpha <- params[1]
  beta <- params[2]
  
  # Condition 1: Probability weight condition: CL 1 = 0.3, CL 2 = 0.4, CL 3 = 0.6, CL 4 = 0.8
  thresholds_CL <- c(0.30,0.40, 0.60,0.80)
  
  if (CL >= 1 & CL <= 4 & interval >= 1 & interval <= 6) {
    CDF <- pbeta(x, alpha, beta, ncp = 0, lower.tail = TRUE, log.p = FALSE)
      
      condition1 <- abs((CDF[interval + 1] - CDF[interval]) - thresholds_CL[CL])
      
      if (interval == 2 || interval == 3 || interval == 4 || interval == 5) {
        # Condition 2: Neighboring intervals have equal values
        int_moins1 <- abs(CDF[interval] - CDF[interval - 1])
        int_plus1 <- abs(CDF[interval + 2] - CDF[interval + 1])
        condition2 <- abs(int_plus1 - int_moins1)
        
        return(condition1 + condition1 + condition2 + condition1 + condition1)}
      
      if (interval == 1 || interval == 6) {
        
        return(condition1)}
  
  }
}

#----------------------------------------------------------------------------
# Function to save the vector containing the probabilities of the interval
# for a specific confidence level
#-----------------------------------------------------------------------------

Int_prob_save <- function(CDF_prob_interval,interval, CL){

  filename <- paste0("Prob_Risk_level_", interval-1, "_CL", CL,".csv")
  
  filepath <- file.path(paste0(base_path), filename)
  write.csv(CDF_prob_interval, file = filepath, row.names = FALSE)
}

#----------------------------------------------------------------------------
# Function to save the vector containing the probabilities of the fifth 
# confidence level
#-----------------------------------------------------------------------------

CL5_prob_interval <- function(interval) {
  
  CL5_prob <- rep(0, 6)
  CL5_prob <- ifelse(seq_along(CL5_prob) == interval, 1, 0)
  filename <- paste0("Prob_Risk_level_", interval-1, "_CL5.csv")

  filepath <- file.path(paste0(base_path), filename)
  write.csv(CL5_prob, file = filepath, row.names = FALSE)
}
