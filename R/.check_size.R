# If one of the overlay values is bigger than the test/disease values, we use the bigger value
.check_size <- function(overlay, Max_Prevalence, overlay_position_Prevalence, modifier_text_overlay_position, Max_FP, overlay_position_FP, Min_Prevalence, modifier_overlay_position_x, modifier_overlay_position_y) {
  
  # IMPORTANT: Figure out if modifications should be made to local or global variables ####
  
    # if (overlay != "no") {
      

    # X AXIS LIMITS -----------------------------------------------------------

      if (PPV_NPV == "PPV") {
        
          # If overlay_position_FP + modifier is more than Max_FP, we use that as the new max for x axis
          if (Max_FP <= max(overlay_position_FP) + modifier_overlay_position_x) {
            
              # Round to 2 decimals (should be 1?)
              Max_FP = round((max(overlay_position_FP) + modifier_overlay_position_x), 2)
              warning("\n\n  * One of the overlay_position_FP values is bigger than Max_FP. We use the max(overlay_position_FP) value to plot. That is Max_FP = ", Max_FP)
            
          }
        
        
      } else if (PPV_NPV == "NPV") { 
      
        # SHOULD CHANGE THIS IN MAIN SCRIPT...
        # (100 - Sensitivity) == Max_FN
        
        # If overlay_position_FP + modifier is more than Max_FN, we use that as the new max for x axis
        if ((100 - Sensitivity) <= max(overlay_position_FP) + modifier_overlay_position_x) {
          
          # Round to 2 decimals (should be 1?)
          Sensitivity = (100 - round((max(overlay_position_FP) + modifier_overlay_position_x), 2))
          warning("\n\n  * One of the overlay_position_FP values is bigger than (100 - Sensitivity) We use the max(overlay_position_FP) value to plot. That is (100 - Sensitivity) or Max_FN = ", (100 - Sensitivity))
          
        }
      }
      

    # Y AXIS ------------------------------------------------------------------

  
        # DEBUG -------------------------------------------------------------------
        if (exists("DEBUG") == FALSE) {DEBUG = 0}
  
        if(DEBUG == 1) {
          message("\n*** .check_size() *** ")
          message("Max_Prevalence: ", Max_Prevalence)
          message("overlay_prevalence_1: ", overlay_prevalence_1)
          message("overlay_prevalence_2: ", overlay_prevalence_2)
          message("Min_Prevalence: ", Min_Prevalence)
          message("modifier_overlay_position_y: ", modifier_overlay_position_y)
          message("point_Prevalence: ", point_Prevalence)
          message("\n*** END *** ")
          
        }
        # **************************************************************************
 
          # if (Max_Prevalence <= max(overlay_position_Prevalence) + abs(modifier_text_overlay_position)) {
        if (Max_Prevalence < max(point_Prevalence) +  abs(modifier_overlay_position_y)) {
          
            # Round to nearest 10'
          # Max_Prevalence <<- round((max(overlay_position_Prevalence) + (abs(modifier_text_overlay_position) + 10)), -1)
          Max_Prevalence = round(max(point_Prevalence) +  abs(modifier_overlay_position_y), 1)
          warning("\n\n  * One of the overlay_position_Prevalence is bigger than Max_Prevalence. We use the max(overlay_position_Prevalence) value to plot. NEW Max_Prevalence: ", Max_Prevalence)
  
        }
        
        # if (Min_Prevalence >= min(overlay_position_Prevalence) - abs(modifier_overlay_position_y)) {
            # overlay_prevalence_2 SHOULD BE point_Prevalence 
        if (Min_Prevalence > min(overlay_prevalence_2) - abs(modifier_overlay_position_y)) {
            
          # Min_Prevalence = round((min(overlay_position_Prevalence) - (abs(modifier_overlay_position_y) + 10)), -1)
              # overlay_prevalence_2 SHOULD BE point_Prevalence ??
          Min_Prevalence = round((min(overlay_prevalence_2) - (abs(modifier_overlay_position_y) + 10)), -1)
          warning("\n\n  * CHECK 1. Min_Prevalence = ", Min_Prevalence)
          
            if (Min_Prevalence < 1) {
              
              # Parece que al usar <<- no esta funcionando. Se deberia usar =. 
              Min_Prevalence = 1
              warning("\n\n  * CHECK 2. Min_Prevalence = ", Min_Prevalence)
              
            }
          
          warning("\n\n  * One of the overlay_position_Prevalence is smaller than Min_Prevalence We use the min(overlay_position_Prevalence) value to plot. NEW Min_Prevalence: ", Min_Prevalence)
  
        }
        
  
  
  # DEBUG -------------------------------------------------------------------
  if (exists("DEBUG") == FALSE) {DEBUG = 0}
  
  if(DEBUG == 1) {
    message("\n*** .check_size() POST *** ")
    message("Max_Prevalence: ", Max_Prevalence)
    message("overlay_prevalence_1: ", overlay_prevalence_1)
    message("overlay_prevalence_2: ", overlay_prevalence_2)
    message("Min_Prevalence: ", Min_Prevalence)
    message("modifier_overlay_position_y: ", modifier_overlay_position_y)
    message("point_Prevalence: ", point_Prevalence)
    message("\n*** END *** ")
    
  }
  # **************************************************************************
  
  
  # Max_FP <<- Max_FP
  # Sensitivity <<- Sensitivity
  # Max_Prevalence <<- Max_Prevalence
  # Min_Prevalence <<- Min_Prevalence
  


  # Recalculate PPV matrix and plot -----------------------------------------

    # TODO: ONLY RECALCULATE IF WE CHANGE ANYTHING ABOVE ####
  
    # # If overlay outside old matrix, we need to do this
    # warning("\n\n  *Recalculate PPVMatrix: ", Min_Prevalence, " ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
    # PPV_melted = .createPPVmatrix(Min_Prevalence = Min_Prevalence, Max_Prevalence = Max_Prevalence, Sensitivity = Sensitivity, Max_FP = Max_FP)
    # 
    # .plot_creation(PPV_melted)    
    # 
    # .calculate_area_overlay_coordinates(uncertainty_prevalence, 
    #                                     Min_Prevalence, Max_Prevalence, Sensitivity, Max_FP,
    #                                     overlay_position_Prevalence, overlay_position_FP = overlay_position_FP)
    
    
    # Output vars
    Min_Prevalence <<- Min_Prevalence
    Max_Prevalence <<- Max_Prevalence
    Sensitivity <<- Sensitivity
    Max_FP <<- Max_FP
    
    # }
}
