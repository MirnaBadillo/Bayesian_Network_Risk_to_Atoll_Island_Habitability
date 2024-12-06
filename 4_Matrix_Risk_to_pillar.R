
#-----------------------------
# RISK TO LAND MATRIX
#-----------------------------

CP_Risk_to_land_array <- function(Risk_to_Land_levels, RC_levels){
  
  dim_array <- c(length(Risk_to_Land_levels),length(RC_levels),length(RC_levels))
  
  Risk_to_land_array <- array(0,dim = dim_array)
  
  
  for (i in 1:length(Risk_to_Land_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
        
        Risk_to_Land_value <- i - 1
        Flooding_value <- j - 1 
        Coastal_erosion_value <- k  - 1
        
        if (Coastal_erosion_value + (Flooding_value * 2) == Risk_to_Land_value) {
          Risk_to_land_array[i, j, k] <- 1.00
        }
      } 
    }
  }
  
  return(Risk_to_land_array)
}


#-----------------------------------
# RISK TO FRESHWATER SUPPLY MATRIX
#-----------------------------------

CP_Risk_to_freshwater_array <- function(Risk_to_freshwater_levels, RC_levels){
  
  dim_array <- c(length(Risk_to_freshwater_levels),length(RC_levels),length(RC_levels), length(RC_levels))
  
  Risk_to_freshwater_array <- array(0,dim = dim_array)
  
  
  for (i in 1:length(Risk_to_freshwater_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
        for (l in 1:length(RC_levels)) {
          
          Risk_to_freshwater_value <- i - 1
          Groundwater_value <- j - 1 
          Rainwater_value <- k  - 1
          Desalinisation_value <- l  - 1
          
          if (Groundwater_value + Rainwater_value + Desalinisation_value == Risk_to_freshwater_value) {
            Risk_to_freshwater_array[i, j, k,l] <- 1.00
          }
        }
      } 
    }
  }
  
  return(Risk_to_freshwater_array)
}


#-----------------------------
# RISK TO FOOD SUPPLY MATRIX
#-----------------------------

CP_Risk_to_Foodsupply_array <- function(Risk_to_Foodsupply_levels, RC_levels){
  
  dim_array <- c(length(Risk_to_Foodsupply_levels),length(RC_levels),length(RC_levels), length(RC_levels))
  
  Risk_to_Foodsupply_array <- array(0,dim = dim_array)
  
  
  for (i in 1:length(Risk_to_Foodsupply_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
        for (l in 1:length(RC_levels)) {
          
          Risk_to_Foodsupply_value <- i - 1
          Reef_fish_value <- j - 1 
          Tuna_value <- k  - 1
          Crops_value <- l  - 1
          
          if (Reef_fish_value + (Tuna_value * 2) + Crops_value == Risk_to_Foodsupply_value) {
            Risk_to_Foodsupply_array[i, j, k,l] <- 1.00
          }
        }
      } 
    }
  }
  
  return(Risk_to_Foodsupply_array)
}



CP_Risk_to_Foodsupply_array_test <- function(Risk_to_Foodsupply_levels, RC_levels) {
  
  dim_array <- c(length(Risk_to_Foodsupply_levels), length(RC_levels), length(RC_levels), length(RC_levels))
  
  Risk_to_Foodsupply_array <- array(0, dim = dim_array)
  
  for (i in 1:length(Risk_to_Foodsupply_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
        for (l in 1:length(RC_levels)) {
          
          Risk_to_Foodsupply_value <- i - 1
          Reef_fish_value <- j - 1 
          Tuna_value <- k - 1
          Crops_value <- l - 1
          
          # Calculate the sum of the influencing factors
          total_influence <- Reef_fish_value + (Tuna_value * 2) + Crops_value
          
          # Set a probability that is highest when Risk_to_Foodsupply matches total_influence
          if (Risk_to_Foodsupply_value == total_influence) {
            Risk_to_Foodsupply_array[i, j, k, l] <- 0.8 # Highest probability when they match
          } else if (abs(Risk_to_Foodsupply_value - total_influence) == 1) {
            Risk_to_Foodsupply_array[i, j, k, l] <- 0.15 # Moderate probability for near matches
          } else {
            Risk_to_Foodsupply_array[i, j, k, l] <- 0.05 # Lower probability for farther matches
          }
          
        }
      } 
    }
  }
  
  # Normalize the probabilities to ensure they sum to 1 for each combination of Reef_fish, Tuna, and Crops
  for (j in 1:length(RC_levels)) {
    for (k in 1:length(RC_levels)) {
      for (l in 1:length(RC_levels)) {
        Risk_to_Foodsupply_array[, j, k, l] <- Risk_to_Foodsupply_array[, j, k, l] / sum(Risk_to_Foodsupply_array[, j, k, l])
      }
    }
  }
  
  return(Risk_to_Foodsupply_array)
}


#------------------------------------------------
# RISK TO SETTLEMENTS AND INFRASTRUCTURE MATRIX
#---------------------------------------------------

CP_Risk_to_Settlement_array <- function(Risk_to_Settlement_levels, RC_levels){
  
  dim_array <- c(length(Risk_to_Settlement_levels),length(RC_levels),length(RC_levels), length(RC_levels))
  
  Risk_to_Settlement_array <- array(0,dim = dim_array)
  
  
  for (i in 1:length(Risk_to_Settlement_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
        for (l in 1:length(RC_levels)) {
          
          Risk_to_Settlement_value <- i - 1
          Loss_sett_value <- j - 1 
          Loss_crit_inf_value <- k  - 1
          Loss_transp_value <- l  - 1
          
          if (Loss_sett_value + Loss_crit_inf_value + (Loss_transp_value * 2) == Risk_to_Settlement_value) {
            Risk_to_Settlement_array[i, j, k,l] <- 1.00
          }
        }
      } 
    }
  }
  
  return(Risk_to_Settlement_array)
}


#------------------------------------------------
# RISK TO ECONOMIC OPPORTUNITIES MATRIX
#---------------------------------------------------
CP_Risk_to_Economic_opportunities_array <- function(Risk_to_Economic_opportunities_levels, RC_levels){

  dim_array <- c(length(Risk_to_Economic_opportunities_levels),length(RC_levels), length(RC_levels))
  
  Risk_to_Economic_opportunities_array <- array(0,dim = dim_array)
  
  
  for (i in 1:length(Risk_to_Economic_opportunities_levels)) {
    for (j in 1:length(RC_levels)) {
      for (k in 1:length(RC_levels)) {
       
          Risk_to_Economic_opportunities_value <- i - 1
          TR_value <- j - 1 
          Tourism_rev_value <- k  - 1
          
          if (TR_value + Tourism_rev_value == Risk_to_Economic_opportunities_value) {
            Risk_to_Economic_opportunities_array[i, j, k] <- 1.00
          }
        }
      } 
  }
  
  return(Risk_to_Economic_opportunities_array)
}






