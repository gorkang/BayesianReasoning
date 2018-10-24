.process_overlay_position_prevalence <- function(overlay_position_Prevalence = "868 out of 1000") {

  # DEBUG -------------------------------------------------------------------
    # library(dplyr)
    # overlay_position_Prevalence = "86.8%"
  
  # *************************************************************************
    
  if (exists("overlay_position_Prevalence") == TRUE) {

    if (grepl("out of", overlay_position_Prevalence) == TRUE) {
      
      overlay_prevalence = regmatches(overlay_position_Prevalence, gregexpr("[[:digit:]]+", overlay_position_Prevalence)) %>% 
        unlist() %>% 
        as.numeric()
      
    } else if (grepl("%", overlay_position_Prevalence)) {

      # If there is a ",", replace by "." (, is the decimal separator in some regions)
      if (grepl(",", overlay_position_Prevalence)) {
        overlay_position_Prevalence = gsub(",", ".", overlay_position_Prevalence) 
        
      }
 
      # Get rid of "%" and convert to number
      overlay_prevalence_temp = gsub("%| %", "", overlay_position_Prevalence) %>% as.numeric(.)
      
      overlay_prevalence = Min_Prevalence
      overlay_prevalence[2] = round(Min_Prevalence / (overlay_prevalence_temp / 100), 0)
    }
  
    
    if (length(overlay_prevalence) != 2) {
      stop("***The parameter 'overlay_position_Prevalence' should be x out of y (e.g. '2 out of 100') or x% (e.g. 2%). Now is: ", overlay_position_Prevalence)
    }
  
    overlay_prevalence_1 <<- overlay_prevalence[1]
    overlay_prevalence_2 <<- overlay_prevalence[2]
  }
  
}