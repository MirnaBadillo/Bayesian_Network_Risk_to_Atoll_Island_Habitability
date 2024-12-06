
rm(list=ls())

source("1_Path_config.R")

source(paste0(base_path, "Module_Optimization_functions.R"))

  # Function to read CSV files and write matrices
  read_and_write_matrices <- function(Risk, CL, risk_type,case) {
    # File path
      file_paths <- paste(paste0(base_path, "Prob_Risk_level_", Risk, "_CL", CL, ".csv", sep=""))

      file_paths <- gsub(" ", "", file_paths)
    
      # Read CSV files
      probabilities <- lapply(file_paths, function(path) {
        read.csv(path, header = FALSE)[[1]][-1]
    })
    
    # Combine probabilities
    matrix_data <- do.call(c, probabilities)

    # Write matrix to file
    filename <- paste0("Matrix_", risk_type, "_MALE.csv")
    filepath <- file.path(paste0(base_path, filename))

    write.csv(matrix_data, file = filepath, row.names = FALSE)
  }
  
  
Supp_materials <- read.csv(paste0(base_path, "wcc700.csv"), skip = 10, row.names = NULL, sep = ";")
#------------------------
# RISK TO PILLAR 1 LAND
#------------------------
  
# Import beta distributions for Coastal erosion

Risk_CE <- as.numeric(Supp_materials[3:6,4])
CL_CE <- as.numeric(Supp_materials[3:6,5])
read_and_write_matrices(Risk_CE, CL_CE, "Coastal_Erosion",1)
read_and_write_matrices(Risk_CE, CL_CE, "Coastal_Erosion",2)
  
# Import beta distributions for Flooding
Risk_FL <- as.numeric(Supp_materials[3:6,6])
CL_FL <- as.numeric(Supp_materials[3:6,7])
read_and_write_matrices(Risk_FL, CL_FL, "Flooding",1)
read_and_write_matrices(Risk_FL, CL_FL, "Flooding",2)
  
#-------------------------------------
# RISK TO PILLAR 2 Freshwater supply
#-------------------------------------

# Import beta distributions for Groundwater

Risk_GR <- as.numeric(Supp_materials[3:6,10])
CL_GR <- as.numeric(Supp_materials[3:6,11])
read_and_write_matrices(Risk_GR, CL_GR, "Groundwater",1)
read_and_write_matrices(Risk_GR, CL_GR, "Groundwater",2)

# Import beta distributions for Rainwater
Risk_RW <- as.numeric(Supp_materials[3:6,12])
CL_RW <- as.numeric(Supp_materials[3:6,13])
read_and_write_matrices(Risk_RW, CL_RW, "Rainwater", 1)
read_and_write_matrices(Risk_RW, CL_RW, "Rainwater", 2)

# Import beta distributions for Desalinisation
Risk_DES <- as.numeric(Supp_materials[3:6,14])
CL_DES <- as.numeric(Supp_materials[3:6,15])
read_and_write_matrices(Risk_DES, CL_DES, "Desalinisation", 1)
read_and_write_matrices(Risk_DES, CL_DES, "Desalinisation", 2)

#-------------------------------
# RISK TO PILLAR 3 FOOD SUPPLY
#------------------------------

# Import beta distributions for reduced reef fisheries production
Risk_RF <- as.numeric(Supp_materials[3:6,18])
CL_RF <- as.numeric(Supp_materials[3:6,19])
read_and_write_matrices(Risk_RF, CL_RF, "Reef_fish", 1)
read_and_write_matrices(Risk_RF, CL_RF, "Reef_fish", 2)
  
# Import beta distributions for redistribution of tuna
Risk_Tuna <- as.numeric(Supp_materials[3:6,20])
CL_Tuna <- as.numeric(Supp_materials[3:6,21])
read_and_write_matrices(Risk_Tuna, CL_Tuna, "Tuna", 1)
read_and_write_matrices(Risk_Tuna, CL_Tuna, "Tuna", 2)

# Import beta distributions for reduced production of crops and livestocks 
Risk_Crops <- as.numeric(Supp_materials[3:6,22])
CL_Crops <- as.numeric(Supp_materials[3:6,23])
read_and_write_matrices(Risk_Crops, CL_Crops, "Crops", 1)
read_and_write_matrices(Risk_Crops, CL_Crops, "Crops", 2)

#------------------------------------------------
# RISK TO PILLAR 4 SETTLEMENT & INFRASTRUCTURES
#------------------------------------------------

# Import beta distributions for loss of settlements

Risk_LS <- as.numeric(Supp_materials[3:6,26])
CL_LS <- as.numeric(Supp_materials[3:6,27])
read_and_write_matrices(Risk_LS, CL_LS, "Loss_sett", 1)
read_and_write_matrices(Risk_LS, CL_LS, "Loss_sett", 2)
# Import beta distributions for loss of critical infrastructure

Risk_LCI <- as.numeric(Supp_materials[3:6,28])
CL_LCI <- as.numeric(Supp_materials[3:6,29])
read_and_write_matrices(Risk_LCI, CL_LCI, "Loss_crit_inf", 1)
read_and_write_matrices(Risk_LCI, CL_LCI, "Loss_crit_inf", 2)
# Import beta distributions for loss of transport connectivity

Risk_LT <- as.numeric(Supp_materials[3:6,30])
CL_LT <- as.numeric(Supp_materials[3:6,31])
read_and_write_matrices(Risk_LT, CL_LT, "Loss_trans", 1)
read_and_write_matrices(Risk_LT, CL_LT, "Loss_trans", 2)

#------------------------------------------------
# RISK TO PILLAR 5 ECONOMIC OPPORTUNITIES
#------------------------------------------------

# Import beta distributions for reduction in tuna fisheries revenue

Risk_TFR <- as.numeric(Supp_materials[3:6,34])
CL_TFR <- as.numeric(Supp_materials[3:6,35])
read_and_write_matrices(Risk_TFR, CL_TFR, "Tuna_rev", 1)
read_and_write_matrices(Risk_TFR, CL_TFR, "Tuna_rev", 2)

# Import beta distributions  for reduction in tourism revenue

Risk_Tourism <- as.numeric(Supp_materials[3:6,36])
CL_Tourism <- as.numeric(Supp_materials[3:6,37])
read_and_write_matrices(Risk_Tourism, CL_Tourism, "Tourism_rev", 1)
read_and_write_matrices(Risk_Tourism, CL_Tourism, "Tourism_rev", 2)

# Import beta distributions for reduction in other revenue streams (aquaculture, etc.)

# NA

