library(bnlearn)
library(bnviewer)
library(ggplot2)

Risk_to_Land_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Risk_to_freshwater_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Risk_to_Foodsupply_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Risk_to_Settlement_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Risk_to_Economic_opportunities_levels <- c(0,1,2,3,4,5,6,7,8,9,10)
Risk_to_MALE_Habitability_levels <- c(0:100)

Risk_to_MALE_Habitability_array <- function(Risk_to_MALE_Habitability_levels, Risk_to_Land_levels, Risk_to_freshwater_levels, Risk_to_Foodsupply_levels, Risk_to_Settlement_levels, Risk_to_Economic_opportunities_levels){
  
  dim_array <- c(length(Risk_to_MALE_Habitability_levels), length(Risk_to_Land_levels), length(Risk_to_freshwater_levels),length(Risk_to_Foodsupply_levels),length(Risk_to_Settlement_levels), length(Risk_to_Economic_opportunities_levels))
  
  Risk_to_MALE_Habitability_array <- array(0,dim = dim_array)
  
for (i in 1:length(Risk_to_MALE_Habitability_levels)) {
  for (j in 1:length(Risk_to_Land_levels)) {
    for (k in 1:length(Risk_to_freshwater_levels)) {
        for (l in 1:length(Risk_to_Foodsupply_levels)) {
          for (m in 1:length(Risk_to_Settlement_levels)) {
            for (n in 1:length(Risk_to_Economic_opportunities_levels)) {
              
              Risk_to_MALE_Habitability_levels <- i - 1
              Risk_to_land_value <- j - 1
              Risk_to_freshwater_value <- k - 1
              Risk_to_Foodsupply_value <- l - 1 
              Risk_to_Settlement_value <- m  - 1
              Risk_to_Economic_opportunities_value <- n  - 1

              #Sum_Normalized_and_weighted_risk_levels <- ((((Risk_to_land_value * 100)/15)*2) + ((Risk_to_freshwater_value * 100)/15) + ((Risk_to_Foodsupply_value*100)/20) + ((Risk_to_Settlement_value*100)/20) + ((Risk_to_Economic_opportunities_value*100)/10))
              #Rescaled_Sum_pillars_risks <- round(((Sum_Normalized_and_weighted_risk_levels*100)/600))
              
              Sum_Normalized_and_weighted_risk_levels <- ((((Risk_to_land_value * 100)/15)*2) + ((Risk_to_freshwater_value * 100)/15) + ((Risk_to_Foodsupply_value*100)/20) + ((Risk_to_Settlement_value*100)/20) + ((Risk_to_Economic_opportunities_value*100)/10))
              Rescaled_Sum_pillars_risks <- round(((Sum_Normalized_and_weighted_risk_levels*100)/600))
              
                if (Rescaled_Sum_pillars_risks == Risk_to_MALE_Habitability_levels) {
                  Risk_to_MALE_Habitability_array[i, j, k,l,m, n] <- 1.00
            }
          }
        }
      }
    }
  }
 }
  
  return(Risk_to_MALE_Habitability_array)
}
