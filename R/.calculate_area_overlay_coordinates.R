#' Calculate area overlay coordinates
#'
#' @param uncertainty_prevalence 
#' @param Min_Prevalence 
#' @param Max_Prevalence 
#' @param Sensitivity 
#' @param Max_FP 
#' @param overlay_position_Prevalence 
#' @param overlay_position_FP_FN 
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr sample_n filter pull
#'
#' @examples
.calculate_area_overlay_coordinates <-
  function(PPV_melted,
           uncertainty_prevalence = "high",
           Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Max_FP,
           overlay_position_Prevalence,
           overlay_position_FP_FN) {
    
  # DEBUG -------------------------------------------------------------------
  if (exists("DEBUG") == FALSE) {DEBUG = 0}
  
  if (DEBUG == 1) {
    message("\n*** .calculate_area_overlay_coordinates() *** ")
    message("uncertainty_prevalence: ", uncertainty_prevalence)
    message("Min_Prevalence: ", Min_Prevalence)
    message("Max_Prevalence: ", Max_Prevalence)
    message("Sensitivity: ", Sensitivity)
    message("Max_FP: ", Max_FP)
    # message("overlay_position_Prevalence: ", overlay_position_Prevalence)
    message("overlay_prevalence_1: ", overlay_prevalence_1)
    message("overlay_prevalence_2: ", overlay_prevalence_2)
    message("\n*** END *** ")
    
  }
  # **************************************************************************
  
    # We made the modifiers proportional to the parameters (Max_Prevalence, Max_FP)
    if (exists("uncertainty_prevalence") == FALSE) {uncertainty_prevalence = "high"}
    
    # Uncertainty - how big the square should be
    if (uncertainty_prevalence == "high") {
      size_uncertainty_area = .05
    } else if (uncertainty_prevalence == "low") {
      size_uncertainty_area = .02
    }
    
    # Width of X axis
    if (PPV_NPV == "PPV") {
      modifier_overlay_position_x <<- round((Max_FP * size_uncertainty_area)/2, 2)
    } else if (PPV_NPV == "NPV") {
      modifier_overlay_position_x <<- round(((100 - Sensitivity) * size_uncertainty_area)/2, 2)
    }
    

    # Calculates y as in Min_Prevalence out of y
    point_Prevalence_temp = Min_Prevalence / (overlay_prevalence_1 / overlay_prevalence_2)
    
    # Looks for closer value in the Prevalence column
    point_Prevalence <<-  PPV_melted %>%
      dplyr::filter(
        # Closest value to overlay_position_Prevalence
        abs(Prevalence - point_Prevalence_temp) == min(abs(Prevalence - point_Prevalence_temp))) %>% 
      dplyr::sample_n(1) %>% 
      dplyr::pull(Prevalence) 
    
    modifier_overlay_position_y <<- (1 - (Min_Prevalence/Max_Prevalence)) * (size_uncertainty_area * Max_Prevalence)
    

  # Final modifiers ---------------------------------------------------------
    
    xmin_overlay <<- overlay_position_FP_FN - modifier_overlay_position_x
    xmax_overlay <<- overlay_position_FP_FN + modifier_overlay_position_x
    ymin_overlay <<- point_Prevalence - modifier_overlay_position_y
    ymax_overlay <<- point_Prevalence + modifier_overlay_position_y
    # ymin_overlay <<- overlay_position_Prevalence - modifier_overlay_position_y
    # ymax_overlay <<- overlay_position_Prevalence + modifier_overlay_position_y
    
    ymax_text_overlay <<- point_Prevalence + modifier_overlay_position_y #+ modifier_overlay_position_y/2
    ymin_text_overlay <<- point_Prevalence - modifier_overlay_position_y #- modifier_overlay_position_y/2
    # ymax_text_overlay <<- overlay_position_Prevalence + modifier_overlay_position_y + modifier_overlay_position_y/2
    # ymin_text_overlay <<- overlay_position_Prevalence - modifier_overlay_position_y - modifier_overlay_position_y/2
    
    # Checks
    if (ymin_overlay < 1) {ymin_overlay <<- 1}
    if (ymin_overlay < Min_Prevalence) {ymin_overlay <<- Min_Prevalence}
    
    if (ymin_text_overlay < 1) {ymin_text_overlay <<- 1}
    if (xmin_overlay < 0) {xmin_overlay <<- 0}
    
    
    
    # DEBUG -------------------------------------------------------------------
    if (exists("DEBUG") == FALSE) {DEBUG = 0}
    
    if (DEBUG == 1) {
      message("\n*** .calculate_area_overlay_coordinates() OUTPUTS *** ")
      message("xmin_overlay: ", xmin_overlay)
      message("xmax_overlay: ", xmax_overlay)
      message("ymin_overlay: ", ymin_overlay)
      message("ymax_overlay: ", ymax_overlay)
      message("ymax_text_overlay: ", ymax_text_overlay)
      message("ymin_text_overlay: ", ymin_text_overlay)
      message("\n*** END *** ")
    }
    # **************************************************************************
    
}