.process_overlay_position_prevalence <- function(overlay_position_Prevalence = "868 out of 1000") {

  # DEBUG -------------------------------------------------------------------
    # library(dplyr)
    # overlay_position_Prevalence = 1
    # overlay_position_Prevalence = "86.8%"
    # overlay_position_Prevalence = "400 out of 200"
  # *************************************************************************
    
  if (exists("overlay_position_Prevalence") == TRUE) {
  

    #  x OUT OF y -------------------------------------------------------------
      
      if (grepl("out of|in", overlay_position_Prevalence) == TRUE) {
        
          overlay_prevalence = regmatches(
            overlay_position_Prevalence,
            gregexpr("[[:digit:]]+", overlay_position_Prevalence)) %>%
            unlist() %>%
            as.numeric()
          
    
          # Checks x OUT OF y -------------------------------------------------------------
          if (overlay_prevalence[2] < overlay_prevalence[1]) { stop("Impossible overlay_position_Prevalence value: ", overlay_position_Prevalence, ". The first value (", overlay_prevalence[1], ") should be a subset of the second value (", overlay_prevalence[2], ").")}
          # *******************************************************************************
          
    #  x % ----------------------------------------------------------------------
      } else if (grepl("%", overlay_position_Prevalence)) {
  
        # If there is a ",", replace by "." (, is the decimal separator in some regions)
        if (grepl(",", overlay_position_Prevalence)) {
          overlay_position_Prevalence = gsub(",", ".", overlay_position_Prevalence) 
          
        }
   
        # Get rid of "%" and convert to number
        overlay_prevalence_temp = gsub("%| %", "", overlay_position_Prevalence) %>% as.numeric(.)
        
        overlay_prevalence = Min_Prevalence
        overlay_prevalence[2] = round(Min_Prevalence / (overlay_prevalence_temp / 100), 0)
        
        # Checks x%  --------------------------------------------------------------------
        if (overlay_prevalence_temp > 100 | overlay_prevalence_temp < 0) { stop("Impossible overlay_position_Prevalence value: ", overlay_position_Prevalence, ". It should be a number between 0% and 100%")}
        # *******************************************************************************
        
      } else if (is.numeric(overlay_position_Prevalence) == TRUE) {
    
        overlay_prevalence = Min_Prevalence
        overlay_prevalence[2] = round(Min_Prevalence / (overlay_position_Prevalence / 100), 0)
        
      }
      

    # General check -----------------------------------------------------------
      if (length(overlay_prevalence) != 2) {
        stop("***The parameter 'overlay_position_Prevalence' should be x out of y (e.g. '2 out of 100'), x in y (e.g. '2 in 100'), x% (e.g. 2%) or x (assumes x = x%). Now is: ", overlay_position_Prevalence)
      }
    # *************************************************************************
    

    # Output vars -------------------------------------------------------------

      overlay_prevalence_1 <<- overlay_prevalence[1]
      overlay_prevalence_2 <<- overlay_prevalence[2]
      
  }
  
}