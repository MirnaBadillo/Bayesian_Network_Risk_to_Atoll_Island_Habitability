
# Bayesian Network development and analysis. This BN model is applied to Male' atoll island. 
# The example queries provided in this script allow for risk assessment and identification of risk conditions (inverse analysis).  


library(bnlearn)
library(bnviewer)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(patchwork)


rm(list=ls())


source("1_Path_config.R")
source(paste0(base_path, "Usefull_functions_BN_fusion.R"))
source(paste0(base_path, "4_Matrix_Risk_to_pillar.R"))
source(paste0(base_path, "5_Matrix_Risk_to_Male_Habitability.R"))
source(paste0(base_path, "Queries_LW_method.R"))

#----INPUT DATA---------------------------------------------------------------------------------

# Vectors indicating the discretization of the variables in the Bayesian Network
RC_levels <- c(0,1,2,3,4,5)
Risk_to_Land_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Risk_to_freshwater_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Risk_to_Foodsupply_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Risk_to_Settlement_levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Risk_to_Economic_opportunities_levels <- c(0,1,2,3,4,5,6,7,8,9,10)
Risk_to_Habitability_levels <- c(0:100)


# Function to import the Conditional Probability Tables corresponding to the Risk Criteria nodes

matrix_list_30 <- Read_matrices(paste0(base_path, "/"))

# RISK TO PILLAR 1 LAND
Matrix_Coastal_erosion <- matrix_list_30$Matrix_Coastal_Erosion_MALE
Matrix_Flooding <- matrix_list_30$Matrix_Flooding_MALE

# RISK TO PILLAR 2 FRESHWATER
Matrix_Groundwater <- matrix_list_30$Matrix_Groundwater_MALE
Matrix_Desalinisation <- matrix_list_30$Matrix_Desalinisation_MALE
Matrix_Rainwater <- matrix_list_30$Matrix_Rainwater_MALE

# RISK TO PILLAR 3 FOOD SUPPLY
Matrix_Reef_fish <- matrix_list_30$Matrix_Reef_fish_MALE
Matrix_Tuna <- matrix_list_30$Matrix_Tuna_MALE
Matrix_Crops <- matrix_list_30$Matrix_Crops_MALE

# RISK TO PILLAR 4 SETTLEMENTS & INFRASTUCTURE
Matrix_Loss_sett <- matrix_list_30$Matrix_Loss_sett_MALE
Matrix_Loss_crit_inf <- matrix_list_30$Matrix_Loss_crit_inf_MALE
Matrix_Loss_transp <- matrix_list_30$Matrix_Loss_trans_MALE

# RISK TO PILLAR 5 ECONOMIC APPORTUNITIES
Matrix_Tuna_fish_rev <- matrix_list_30$Matrix_Tuna_rev_MALE
Matrix_Tourism_rev <-  matrix_list_30$Matrix_Tourism_rev_MALE


# Function to create the Conditional Probability Tables corresponding to the Habitability Pillars
# and the Risk to Male' Habitability nodes

CP_risk_to_land <- CP_Risk_to_land_array(Risk_to_Land_levels, RC_levels)
CP_Risk_to_freshwater <- CP_Risk_to_freshwater_array(Risk_to_freshwater_levels, RC_levels)
CP_Risk_to_Foodsupply <- CP_Risk_to_Foodsupply_array(Risk_to_Foodsupply_levels, RC_levels)
CP_Risk_to_Settlement <- CP_Risk_to_Settlement_array(Risk_to_Settlement_levels, RC_levels)
CP_risk_to_Economic_opportunities <- CP_Risk_to_Economic_opportunities_array(Risk_to_Economic_opportunities_levels, RC_levels)
CP_Risk_to_MALE_Habitability <- Risk_to_MALE_Habitability_array(Risk_to_MALE_Habitability_levels, Risk_to_Land_levels, Risk_to_freshwater_levels, Risk_to_Foodsupply_levels, Risk_to_Settlement_levels, Risk_to_Economic_opportunities_levels) 


#----BAYESIAN NETWORK DEVELOPMENT-----------------------------------------------------------------

# Bayesian Network structure

net = model2network("[RCP][Time][Coastal_erosion|RCP:Time][Flooding|RCP:Time][Groundwater|RCP:Time][Rainwater|RCP:Time][Desalinisation|RCP:Time][Reef_fish|RCP:Time][Tuna|RCP:Time][Crops|RCP:Time][Loss_sett|RCP:Time][Loss_crit_inf|RCP:Time][Loss_transp|RCP:Time][Tuna_fish_rev|RCP:Time][Tourism_rev|RCP:Time][Risk_to_land|Coastal_erosion:Flooding][Risk_to_freshwater|Groundwater:Desalinisation:Rainwater][Risk_to_Foodsupply|Reef_fish:Tuna:Crops][Risk_to_Settlement|Loss_sett:Loss_crit_inf:Loss_transp][Risk_to_Economic_opportunities|Tuna_fish_rev:Tourism_rev][Risk_to_MALE_Habitability|Risk_to_land:Risk_to_freshwater:Risk_to_Foodsupply:Risk_to_Settlement:Risk_to_Economic_opportunities]")

# Interactive network
viewer(net,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title = "<br> Discrete Bayesian Network to assess risks for future atoll habitability",
       bayesianNetwork.subtitle = "Malé - Maldives",
       node.colors = list(background = "white",
                          border = "black"),
       node.font = list(color = "black", face="Arial"),
       clusters.legend.title = list(text = "<b>Legend</b> <br> Variable Categories",
                                    style = "font-size:18px;
                                             font-family:Arial;
                                             color:black;
                                             text-align:center;"),
       clusters.legend.options = list(
         list(label = "Risk criteria",
              shape = "icon",
              icon = list(code = "f10c",
                          size = 50,
                          color = "#e91e63")),
         list(label = "Habitability pillars",
              shape = "icon",
              icon = list(code = "f10c",
                          size = 50,
                          color = "#ffc107"))
       ),
         
         clusters = list(
           list(label = "Risk criteria",
                shape = "icon",
                icon = list(code = "f10c", color = "black", background = "lightblue"),
                nodes = list("Coastal_erosion","Flooding","Rainwater","Groundwater","Desalinisation", "Reef_fish","Tuna", "Crops", "Loss_sett", "Loss_crit_inf", "Loss_transp", "Tuna_fish_rev", "Tourism_rev")),
           
           list(label = "Habitability pillars",
                shape = "icon",
                icon = list(code = "f10c", color = "#ffc107"),
                nodes = list("Risk_to_land","Risk_to_freshwater","Risk_to_Foodsupply", "Risk_to_Settlement","Risk_to_Economic_opportunities")))
           
       #bayesianNetwork.enabled.interactive.mode = TRUE
      
)
                                                  
# Definition of the Conditional Probability Tables

# Node RCP
cptRCP = matrix(c(0.50,0.50), ncol = 1, dimnames = list(RCP= c(2.6,8.5)))

# Node Time
cptTime <- matrix(c(0.50, 0.50), ncol = 1, byrow = TRUE, dimnames = list(Time = c(2050,2090)))

# Node Coastal erosion conditional on RCP and Time
cptCoastal_erosion <- array(as.numeric(Matrix_Coastal_erosion[[1]][-1]),
                            dim = c(6,2,2),
                            dimnames = list(Coastal_erosion = c(0,1,2,3,4,5),
                                            RCP = c(2.6,8.5),
                                            Time = c(2050,2090)))

# Node Flooding conditional on RCP and Time
cptFlooding <- array(as.numeric(Matrix_Flooding[[1]][-1]),
                     dim = c(6,2,2),
                     dimnames = list(Flooding = c(0,1,2,3,4,5),
                                     RCP = c(2.6,8.5),
                                     Time = c(2050,2090)))

# Node Risk to land conditional on Coastal erosion and Flooding (Flooding * 2)
cptRisk_to_land <- array(c(CP_risk_to_land),
                         dim = c(16,6,6),
                         dimnames = list(Risk_to_land = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                         Flooding = c(0,1,2,3,4,5),
                                         Coastal_erosion = c(0, 1,2,3,4,5)))


# Node Fresh groundwater salinization/loss conditional on RCP and Time
cptGroundwater <- array(as.numeric(Matrix_Groundwater[[1]][-1]),
                        dim = c(6,2,2),
                        dimnames = list(Groundwater = c(0,1,2,3,4,5),
                                        RCP = c(2.6,8.5),
                                        Time = c(2050,2090)))

# Node Decrease in rainwater harvesting conditional on RCP and Time
cptRainwater <- array(as.numeric(Matrix_Rainwater[[1]][-1]),
                      dim = c(6,2,2),
                      dimnames = list(Rainwater = c(0,1,2,3,4,5),
                                      RCP = c(2.6,8.5),
                                      Time = c(2050,2090)))

# Node Decrease in desalination conditional on RCP and Time
cptDesalinisation <- array(as.numeric(Matrix_Desalinisation[[1]][-1]),
                           dim = c(6,2,2),
                           dimnames = list(Desalinisation = c(0,1,2,3,4,5),
                                           RCP = c(2.6,8.5),
                                           Time = c(2050,2090)))

# Node Risk to Freshwater conditional on Fresh groundwater salinization/loss, Decrease in rainwater harvesting and Decrease in desalination
cptRisk_to_freshwater <- array(c(CP_Risk_to_freshwater),
                               dim = c(16,6,6,6),
                               dimnames = list(Risk_to_freshwater = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                               Groundwater = c(0,1,2,3,4,5),
                                               Rainwater = c(0,1,2,3,4,5),
                                               Desalinisation = c(0, 1,2,3,4,5)))

# Node Reduced reef fisheries production conditional on RCP and time
cptReef_fish <- array(as.numeric(Matrix_Reef_fish[[1]][-1]),
                      dim = c(6,2,2),
                      dimnames = list(Reef_fish = c(0,1,2,3,4,5),
                                      RCP = c(2.6,8.5),
                                      Time = c(2050,2090)))

# Node Redistribution of tuna conditional on RCP and Time
cptTuna <- array(as.numeric(Matrix_Tuna[[1]][-1]),
                 dim = c(6,2,2),
                 dimnames = list(Tuna = c(0,1,2,3,4,5),
                                 RCP = c(2.6,8.5),
                                 Time = c(2050,2090)))

# Node Reduced production of crops and livestocks conditional on RCP and Time
cptCrops <- array(as.numeric(Matrix_Crops[[1]][-1]),
                  dim = c(6,2,2),
                  dimnames = list(Crops = c(0,1,2,3,4,5),
                                  RCP = c(2.6,8.5),
                                  Time = c(2050,2090)))

# Node Risk to Food supply conditional on Reduced reef fisheries production, Redistribution of tuna and Reduced production of crops and livestocks
cptRisk_to_Foodsupply <- array(c(CP_Risk_to_Foodsupply),
                               dim = c(21,6,6,6),
                               dimnames = list(Risk_to_Foodsupply = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                               Reef_fish = c(0,1,2,3,4,5),
                                               Tuna = c(0,1,2,3,4,5),
                                               Crops = c(0, 1,2,3,4,5)))

# Node Loss of settlements conditional on RCP and Time
cptLoss_sett <- array(as.numeric(Matrix_Loss_sett[[1]][-1]),
                      dim = c(6,2,2),
                      dimnames = list(Loss_sett = c(0,1,2,3,4,5),
                                      RCP = c(2.6,8.5),
                                      Time = c(2050,2090)))

# Node Loss of critical infrastucture conditional on RCP and Time
cptLoss_crit_inf <- array(as.numeric(Matrix_Loss_crit_inf[[1]][-1]),
                          dim = c(6,2,2),
                          dimnames = list(Loss_crit_inf = c(0,1,2,3,4,5),
                                          RCP = c(2.6,8.5),
                                          Time = c(2050,2090)))

# Node Loss of transport connectivity conditional on RCP and Time
cptLoss_transp <- array(as.numeric(Matrix_Loss_transp[[1]][-1]),
                        dim = c(6,2,2),
                        dimnames = list(Loss_transp = c(0,1,2,3,4,5),
                                        RCP = c(2.6,8.5),
                                        Time = c(2050,2090)))

# Node Risk to settlement and infrastruture conditional on Loss of settlements, Loss of critical infrastructure and Loss of transport connectivity

cptRisk_to_Settlement <- array(c(CP_Risk_to_Settlement),
                               dim = c(21,6,6,6),
                               dimnames = list(Risk_to_Settlement = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                               Loss_sett = c(0,1,2,3,4,5),
                                               Loss_crit_inf = c(0,1,2,3,4,5),
                                               Loss_transp = c(0, 1,2,3,4,5)))

# Node Reduction in tuna fisheries revenue conditional on RCP and Time
cptTuna_fish_rev <- array(as.numeric(Matrix_Tuna_fish_rev[[1]][-1]),
                          dim = c(6,2,2),
                          dimnames = list(Tuna_fish_rev = c(0,1,2,3,4,5),
                                          RCP = c(2.6,8.5),
                                          Time = c(2050,2090)))

# Node Reduction in tourism revenue conditional on RCP and Time
cptTourism_rev <- array(as.numeric(Matrix_Tourism_rev[[1]][-1]),
                        dim = c(6,2,2),
                        dimnames = list(Tourism_rev = c(0,1,2,3,4,5),
                                        RCP = c(2.6,8.5),
                                        Time = c(2050,2090)))

# Node Risk to Economic opportunities conditional on eduction in tuna fisheries revenue and Reduction in tourism revenue
cptRisk_to_Economic_opportunities <- array(c(CP_risk_to_Economic_opportunities),
                                           dim = c(11,6,6),
                                           dimnames = list(Risk_to_Economic_opportunities = c(0,1,2,3,4,5,6,7,8,9,10),
                                                           Tuna_fish_rev = c(0,1,2,3,4,5),
                                                           Tourism_rev = c(0,1,2,3,4,5)
                                                           ))

# Node Risk to Male' Habitability conditional on Risk to land, freshwater, food supply, settlements & infrastructure and economic opportunities
cptRisk_to_MALE_Habitability <- array(c(CP_Risk_to_MALE_Habitability),
                                      dim = c(101,16,16,21,21,11),
                                      dimnames = list(Risk_to_MALE_Habitability = c(0:100), 
                                                      Risk_to_land = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                                      Risk_to_freshwater = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                                      Risk_to_Foodsupply = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                      Risk_to_Settlement = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                      Risk_to_Economic_opportunities = c(0,1,2,3,4,5,6,7,8,9,10)))
                                                      
# BN CUSTOM
dfit = custom.fit(net, dist = list(RCP = cptRCP,
                                   Time = cptTime,
                                   Flooding = cptFlooding,
                                   Coastal_erosion = cptCoastal_erosion,
                                   Risk_to_land = cptRisk_to_land,
                                   Groundwater = cptGroundwater,
                                   Rainwater = cptRainwater,
                                   Desalinisation = cptDesalinisation,
                                   Risk_to_freshwater = cptRisk_to_freshwater,
                                   Reef_fish = cptReef_fish,
                                   Tuna = cptTuna,
                                   Crops = cptCrops,
                                   Risk_to_Foodsupply = cptRisk_to_Foodsupply,
                                   Loss_sett = cptLoss_sett,
                                   Loss_crit_inf = cptLoss_crit_inf,
                                   Loss_transp = cptLoss_transp,
                                   Risk_to_Settlement = cptRisk_to_Settlement,
                                   Tuna_fish_rev = cptTuna_fish_rev,
                                   Tourism_rev = cptTourism_rev,
                                   Risk_to_Economic_opportunities = cptRisk_to_Economic_opportunities,
                                   Risk_to_MALE_Habitability = cptRisk_to_MALE_Habitability))


#-----BAYESIAN NETWORK ANALYSIS-----------------------------------------------------------------

# QUERY 1) Risk assessment. 
# In this experiment, we interrogate the BN model about the probability of risk to Male' habitability 
# given an RCP scenario and a time horizon

Query_Risk_to_habitability_2050_RCP26_scenario_LW <- Query_function_scenario_LW("Risk_to_MALE_Habitability", "MALE",2050, 2.6,  dfit)
Query_Risk_to_habitability_2050_RCP85_scenario_LW <- Query_function_scenario_LW("Risk_to_MALE_Habitability", "MALE", 2050, 8.5,  dfit)


# QUERY 2) Identification of risk conditions.
# We explore the conditions that lead to high risk to Male' habitability, by calculating the probability of each Risk Criteria 
# level when the risk to habitability is high.

# RISK CRITERIA GIVEN TIME AND RCP AND RISK TO HABITABILITY LEVEL

Query_CE_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Coastal_erosion", 2090, 8.5, "High", dfit)
Query_FL_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Flooding", 2090, 8.5, "High", dfit)
Query_RW_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Rainwater", 2090, 8.5, "High", dfit)
Query_GW_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Groundwater", 2090, 8.5, "High", dfit)
Query_DE_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Desalinisation", 2090, 8.5, "High", dfit)
Query_RF_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Reef_fish", 2090, 8.5, "High", dfit)
Query_TU_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Tuna", 2090, 8.5, "High", dfit)
Query_CR_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Crops", 2090, 8.5, "High", dfit)
Query_LS_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Loss_sett", 2090, 8.5, "High", dfit)
Query_LCI_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Loss_crit_inf", 2090, 8.5, "High", dfit)
Query_LT_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Loss_transp", 2090, 8.5, "High", dfit)
Query_TFR_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Tuna_fish_rev", 2090, 8.5, "High", dfit)
Query_TR_2090_85_RTH_H_Lw <- Query_function_Risk_criteria_Time_RCP_RTH_OR_LW("Risk_to_MALE_Habitability", "MALE", "Tourism_rev", 2090, 8.5, "High", dfit)


#------PRE-PLOT------------------------------------------------------------------------------------------

# QUERY 1
Probability_2050_M <- c(as.numeric(unlist(Query_Risk_to_habitability_2050_RCP26_scenario_LW[1:101,2])), 
                        as.numeric(unlist(Query_Risk_to_habitability_2050_RCP85_scenario_LW[1:101,2])))

Scenarios_2050_df_M <- create_df_ggplot(Probability_2050_M)

# Data frame for density function
density_data_2050_26_M <- data.frame(Risk_level = rep(Risk_to_MALE_Habitability_levels, Scenarios_2050_df_M$Probability[1:101] * 5000))
density_data_2050_85_M <- data.frame(Risk_level = rep(Risk_to_MALE_Habitability_levels, Scenarios_2050_df_M$Probability[102:202] * 5000))

# QUERY 2
RC_level <- as.vector(rep(0:5, times=12))

Probability_RC_RTH_H <- as.vector(cbind(Query_CE_2090_85_RTH_H_Lw[1:6,2],
                                        Query_FL_2090_85_RTH_H_Lw[1:6,2],
                                        Query_RW_2090_85_RTH_H_Lw[1:6,2],
                                        Query_DE_2090_85_RTH_H_Lw[1:6,2],
                                        Query_RF_2090_85_RTH_H_Lw[1:6,2],
                                        Query_TU_2090_85_RTH_H_Lw[1:6,2],
                                        Query_CR_2090_85_RTH_H_Lw[1:6,2],
                                        Query_LS_2090_85_RTH_H_Lw[1:6,2],
                                        Query_LCI_2090_85_RTH_H_Lw[1:6,2],
                                        Query_LT_2090_85_RTH_H_Lw[1:6,2],
                                        Query_TFR_2090_85_RTH_H_Lw[1:6,2],
                                        Query_TR_2090_85_RTH_H_Lw[1:6,2]))


RC <- c("Coastal erosion", 
        "Flooding", 
        "Fresh groundwater sal/loss = Decr.rainwater harvesting", 
        "Decrease in desalinisation",
        "Reduced reef fisheries production", 
        "Redistribution of tuna", 
        "Reduced production of crops & livestocks", 
        "Loss of settlements", 
        "Loss of critical infrastructure", 
        "Loss of transport connectivity", 
        "Reduction in tuna fisheries revenue", 
        "Reduction in tourism revenue")

RC_names <- rep(RC, each=6) 

Query_RC_RCP_TIME_RTH_HIGH_df <- data.frame(RC_level = factor(RC_level,levels = 0:5),
                                            Probability_RC = Probability_RC_RTH_H,
                                            RC = factor(RC_names, levels=RC))

#--- --PLOTS---------------------------------------------------------------------------

# QUERY 1 Plot
plot_2050_M <- function_ggplot(Scenarios_2050_df_M, 2050, "Male'", density_data_2050_26_M, density_data_2050_85_M)

# QUERY 2 Plot
plot_RC_RCP85_2090_RTH_High_MALE<- plot_RC_all_levels(8.5,2090,Query_RC_RCP_TIME_RTH_HIGH_df,"Male'","High")


