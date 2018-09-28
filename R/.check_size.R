# If one of the overlay values is bigger than the test/disease values, we use the bigger value
.check_size <- function(overlay, Max_Prevalence, overlay_position_Prevalence, modifier_text_overlay_position, Max_FP, overlay_position_FP) {
  
    if (overlay != "no") {
      
      if (Max_FP < max(overlay_position_FP)) {
        Max_FP <<- max(overlay_position_FP)
        warning("\n\n  * One of the overlay_position_FP values is bigger than Max_FP. We use the max(overlay_position_FP) value to plot.")
      }
      
      if (Max_Prevalence <= max(overlay_position_Prevalence) + modifier_text_overlay_position) {
        
        Max_Prevalence <<- (max(overlay_position_Prevalence) +  (modifier_text_overlay_position))
        
        warning("\n\n  * One of the overlay_position_Prevalence is bigger than Max_Prevalence. We use the max(overlay_position_Prevalence) value to plot. NEW Max_Prevalence: ", Max_Prevalence)
        
      }
    }
}
