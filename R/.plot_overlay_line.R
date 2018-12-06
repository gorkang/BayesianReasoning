#' Title
#'
#' @param PPV_melted 
#' @param Max_Prevalence 
#' @param overlay_position_Prevalence 
#' @param overlay_position_FP_FN 
#' @param overlay_labels 
#'
#' @return
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
.plot_overlay_line <- function(PPV_melted, Max_Prevalence, overlay_position_Prevalence, overlay_position_FP_FN, overlay_labels) {
  
    # We made the modifiers proportional to the parameters (Max_Prevalence, Max_FP)
    if (exists("size_uncertainty_area") == FALSE) {size_uncertainty_area = 0}
  
    if (abs(Max_Prevalence - max(overlay_position_Prevalence)) > 10) {
      
      modifier_text_overlay_position = (Max_Prevalence * size_uncertainty_area  + 1)
      
    } else {
      
      modifier_text_overlay_position = -(Max_Prevalence * size_uncertainty_area + 1)
      
    }
  
    # overlay_labels = c("80", "70", "60", "50", "40", "30 y.o.")
    # overlay_position_FP_FN = c(7, 8, 9, 12, 14)
    # overlay_position_Prevalence = c(26, 29, 44, 69, 227)
    
    overlay_position_x_end = c(overlay_position_FP_FN[1], overlay_position_FP_FN[-length(overlay_position_FP_FN)])
    overlay_position_y_end = c(overlay_position_Prevalence[1], overlay_position_Prevalence[-length(overlay_position_Prevalence)])
    
    
    # Create plot after adjusting overlay dimensions
      # Should re-create ppv/npv matrix first?
    .plot_creation(PPV_melted)    
    
    
    # Plot Overlay ------------------------------------------------------------
    p <<- p + ggplot2::annotate("segment", x = overlay_position_FP_FN, xend = overlay_position_x_end, 
                     y = overlay_position_Prevalence, yend = overlay_position_y_end,
                     color = "red", alpha = .1, size = 3) +
      ggplot2::annotate("text", x = overlay_position_FP_FN, y = overlay_position_Prevalence, label = overlay_labels, size = 4) 

}