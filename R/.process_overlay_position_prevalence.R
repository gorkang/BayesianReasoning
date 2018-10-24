.process_overlay_position_prevalence <- function(overlay_position_Prevalence = "868 out of 1000") {


  # DEBUG -------------------------------------------------------------------
  library(dplyr)
  # *************************************************************************
    
  if (exists("overlay_position_Prevalence") == TRUE) {
  
    overlay_prevalence = regmatches(overlay_position_Prevalence, gregexpr("[[:digit:]]+", overlay_position_Prevalence)) %>% 
      unlist() %>% 
      as.numeric()
  
    if (length(overlay_prevalence) != 2) {
      stop("***The parameter 'overlay_position_Prevalence' should be x out of y (e.g. '2 out of 100'). Now is: ", overlay_position_Prevalence)
    }
  
    overlay_prevalence_1 <<- overlay_prevalence[1]
    overlay_prevalence_2 <<- overlay_prevalence[2]
  }
  
}