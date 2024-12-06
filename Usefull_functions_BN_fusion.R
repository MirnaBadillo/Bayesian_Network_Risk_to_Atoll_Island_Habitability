
# This function allows to read the Conditional Probability Tables
Read_matrices <- function(directory_path) {
  file_names <- c("Matrix_Coastal_Erosion_MALE.csv", "Matrix_Flooding_MALE.csv",
                  "Matrix_Groundwater_MALE.csv", "Matrix_Desalinisation_MALE.csv",
                  "Matrix_Rainwater_MALE.csv", "Matrix_Reef_fish_MALE.csv",
                  "Matrix_Tuna_MALE.csv", "Matrix_Crops_MALE.csv",
                  "Matrix_Loss_sett_MALE.csv", "Matrix_Loss_crit_inf_MALE.csv",
                  "Matrix_Loss_trans_MALE.csv", "Matrix_Tuna_rev_MALE.csv",
                  "Matrix_Tourism_rev_MALE.csv")
  
  matrix_list <- list()
  
  for (file_name in file_names) {
    matrix_name <- gsub(".csv", "", file_name)
    matrix_path <- paste0(directory_path, file_name)
    matrix_list[[matrix_name]] <- read.csv(matrix_path, header = FALSE)
  }
  
  return(matrix_list)
}


# This function allows to create a data frame useful to plot the results using GGPLOT
create_df_ggplot <- function(probability) {
  
  Risk_level <- as.vector(cbind(Risk_to_Habitability_levels,Risk_to_Habitability_levels))
  RCP <- cbind(rep("RCP 2.6", times = 101), rep("RCP 8.5", times = 101))
  sce_time <- cbind(rep("RCP 2.6", times = 101), rep("RCP 8.5", times = 101))
  
  data.frame(
    Risk_level = Risk_level,
    Probability = probability,
    RCP = factor(RCP, levels = c("RCP 2.6", "RCP 8.5")),
    Scenario = factor(sce_time, levels = c("RCP 2.6", "RCP 8.5"))
  )
}


# Function to plot the results of the query 1
function_ggplot <- function(Scenarios_df_M, year, atoll, density_data_RCP26, density_data_RCP85){
  
  plot <- ggplot(Scenarios_df_M, aes(x = Risk_level, y = Probability, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge", width = 1.8, color = "black",alpha=0.8) +
    scale_fill_manual(values = c(rgb(0, 52/255, 102/255, alpha = 0.1), rgb(153/255, 0, 2/255))) + 
    labs(
      title = paste(atoll," - ", year),
      x = "Risk to Male' habitability levels",
      y = "Probability",
      fill = "Scenario"
    ) +

    theme_classic() +
    theme(legend.position = "right",
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 20), 
          plot.subtitle =element_text(size = 18), 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16))+ 
    xlim(0, 100) +
    ylim(0, 0.15)
  
  plot_density <- plot +
    geom_density(data = density_data_RCP26, stat= "density",aes(x = Risk_level, y = ..scaled.. * max(Scenarios_df_M$Probability[1:101])), 
                 color = "black", fill = "blue", alpha = 0.01,size=0.6, adjust = 1.5) +
    geom_density(data = density_data_RCP85,stat= "density", aes(x = Risk_level, y = ..scaled.. * max(Scenarios_df_M$Probability[102:202])), 
                 color = "black", fill = "red", alpha = 0.01, size=0.6, adjust = 1.5) 
  
  print(plot_density)
}


# Function to plot the results of the query 2
plot_RC_all_levels <- function(RCP, time, Query_RC_RCP_TIME_RTH_interval_df, atoll, habitability_interval) {
  

  plot <- ggplot() +

    geom_bar(
      data = Query_RC_RCP_TIME_RTH_interval_df, 
      aes(x = RC_level, y = Probability_RC, fill = "P(RISK CRITERIA | RCP = 8.5 & TIME = 2090 & RISK TO HABITABILITY = HIGH)"), 
      alpha = 0.3, color = "black", stat = "identity", position = position_dodge(width = 0.9)
    ) +

    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_discrete(limits = as.character(0:5)) +
    scale_fill_manual(
      values = c("P(RISK CRITERIA | RCP = 8.5 & TIME = 2090 & RISK TO HABITABILITY = HIGH)" = rgb(153/255, 0, 2/255)),
      labels = c("P(RISK CRITERIA | RCP = 8.5 & TIME = 2090 & RISK TO HABITABILITY = HIGH)")
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 20),
      strip.text = element_text(size = 10),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.position = "top"
    ) +

    labs(
      x = "Risk level",
      y = "Probability",
      fill = "Legend",
      color = "Legend",
      title = "Bayesian Network inverse analysis"
    )
  
  print(plot)
}
