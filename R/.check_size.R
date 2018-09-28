# If one of the overlay values is bigger than the test/disease values, we use the bigger value
.check_size <- function(overlay, Max_Prevalence, overlay_position_Prevalence, modifier_text_overlay_position, Max_FP, overlay_position_FP) {
  
    if (overlay != "no") {
      
      if (Max_FP <= max(overlay_position_FP) + modifier_overlay_position_x) {
        
          # Round to 2 decimals (should be 1?)
          Max_FP <<- round((max(overlay_position_FP) + modifier_overlay_position_x), 2)
          warning("\n\n  * One of the overlay_position_FP values is bigger than Max_FP. We use the max(overlay_position_FP) value to plot.")
        
        }
      
      # DEBUG:
        # message(paste0("Max_Prevalence ", Max_Prevalence, ". max(overlay_position_Prevalence) ", max(overlay_position_Prevalence), ". modifier_text_overlay_position ", modifier_text_overlay_position))
      if (Max_Prevalence <= max(overlay_position_Prevalence) + abs(modifier_text_overlay_position)) {
        
          # Round to nearest 10'
          Max_Prevalence <<- round((max(overlay_position_Prevalence) + (abs(modifier_text_overlay_position) + 10)), -1)
          warning("\n\n  * One of the overlay_position_Prevalence is bigger than Max_Prevalence. We use the max(overlay_position_Prevalence) value to plot. NEW Max_Prevalence: ", Max_Prevalence)

      }
    }
}
