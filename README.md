# Bayesian_Network_Risk_to_Atoll_Island_Habitability
The objective of this project is to share a program that builds a Bayesian Network (BN) model based on expert judgments.  This program also allows for Bayesian inference using the Likelihood Weighting method.

 This project consists of a series of scripts that should be executed as follows:

- Configure files paths: Run [1_Path_config](./1_Path_config.R) to set up the file pathways.

- Convert expert judgments into probabilities: Run [2_Create_Beta_distributions](./2_Create_Beta_distributions.R) to convert the risk assessment database ([wcc700](./wcc700.csv) from Duvat et al. 2021) into Beta distributions. For each combination of risk level and confidence level a Beta distribution is generated. These distributions serve as the input data for the BN model.

- Create Conditional Probability tables (CPTs) for Risk Criteria nodes: Run [3_Conditional_Probability_Tables_MALE](./3_Conditional_Probability_Tables_MALE.R) to generate the CPTs for each Risk Criteria node. These CPTs are filled using the Beta distributions.
  
- Create CPTs for Habitability Pillar nodes: Run [4_Matrix_Risk_to_pillar](./4_Matrix_Risk_to_pillar.R) to create the CPTs for each Habitability Pillar node.
  
- Create CPTs for Risk to Male' Habitability node: Run [5_Matrix_Risk_to_Male_Habitability](./5_Matrix_Risk_to_Male_Habitability.R) to generate the CPTs for the Risk to Male' Habitability node.

The CPTs for the Habitability Pillars and Risk to Male Habitability nodes are filled in a deterministic way. A probability of one is assigned to the assessed risk level (Duvat et al. 2021) and a probability of zero with the other levels.

- Build the BN model and perform inference: Run [BN_Risk_to_Habitability](./BN_Risk_to_Habitability.R) to built the Bayesian Network model and perform inference using the Likelihood Weighting method.
   
This BN model is applied to Male' atoll island. The example queries provided in this project allow for risk assessment and identification of risk conditions (inverse analysis).  

The functions used to perform queries are in the [Queries_LW_method](./Queries_LW_method.R) script.

Necessary packages:
- Bayesian Network development [bnlearn], [bnviewer]
- Plots [ggplot2], [RColorBrewer], [cowplot], [patchwork]
