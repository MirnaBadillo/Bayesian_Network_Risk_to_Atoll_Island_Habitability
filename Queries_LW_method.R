
# QUERY 1) Risk assessment. 
# This function allows to interrogate the BN model about the probability of risk to Male' habitability 
# given an RCP scenario and a time horizon using the Likelihood Weighting method

Query_function_scenario_LW <- function(Risk_to_Habitability, ile, Time, RCP, dfit){
  
  evidences <- paste0('list(Time = "', Time, '", RCP = "', RCP, '")')
  
  result_LW <- data.frame(Risk_to_Habitability = character(), Probability = numeric())
  
  for (j in seq_along(Risk_to_Habitability_levels)) {
    
    events <- paste0("(",Risk_to_Habitability,"=='",Risk_to_Habitability_levels[j], "')")
    
    query_LW <- eval(parse(text = paste('cpquery(fitted = dfit, event =',events, ', evidence =', evidences , ', method = "lw", n=10^5)'))) 
    result_LW <- rbind(result_LW, data.frame(Risk_to_Habitability = Risk_to_Habitability_levels[j], Probability = query_LW))
  }
  
  return(result_LW)
}


# QUERY 2) Identification of risk conditions.
# This function allows to explore the conditions that lead to high risk to Male' habitability, by calculating the probability of each Risk Criteria 
# level when the risk to habitability is high. 

#Query: P(Risk Criteria | RCP = 8.5 & Time = 2090 & Risk to Male' habitability = High). The interval HIGH correspond to the risk to habitability between 61 and 80.


Query_function_Risk_criteria_Time_RCP_RTH_OR_LW <- function(Risk_to_Habitability, ile, Risk_criteria, Time, RCP, Risk_to_habitability_int, dfit){
  
  results_LW <- data.frame(Risk_to_RC = character(),Probability = numeric())
  
  RC_levels <- 0:5
 
  if (Risk_to_habitability_int == "Very_high"){
    
    evidences <- paste0('(list( Time = "',Time, '" , RCP = "',RCP,'"', ',', Risk_to_Habitability, ' = c("81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100")))')
  }
 
  else if (Risk_to_habitability_int == "High"){
    
    evidences <- paste0('(list( Time = "',Time, '" , RCP = "',RCP,'"', ',', Risk_to_Habitability, ' = c("61", "62", "63", "64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80")))')
  }

  else if (Risk_to_habitability_int == "Moderate"){
    
    evidences <- paste0('(list( Time = "',Time, '" , RCP = "',RCP,'"', ',', Risk_to_Habitability, ' = c("41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60")))')
  }

  else if (Risk_to_habitability_int == "Low"){
    
    evidences <- paste0('(list( Time = "',Time, '" , RCP = "',RCP,'"', ',', Risk_to_Habitability, ' = c("21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40")))')
  }

  else{
    evidences <- paste0('(list( Time = "',Time, '" , RCP = "',RCP,'"', ',', Risk_to_Habitability, ' = c("0","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")))')
  }
  
  for (j in seq_along(RC_levels)){
    
    events <- paste0("(", Risk_criteria," =='",RC_levels[j], "')")
    
    query_result_prob <- eval(parse(text = paste('cpquery(fitted = dfit, event =',events, ', evidence =', evidences , ', method = "lw", n=10^5)'))) 
    query_result_prob[is.nan(query_result_prob)] <- 0
    results_LW <- rbind(results_LW, data.frame(Risk_to_RC = RC_levels[j], Probability = query_result_prob))
  }
  
  return(results_LW)
}





