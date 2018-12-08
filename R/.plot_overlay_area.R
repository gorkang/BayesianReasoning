#' Plot overlay area
#'
#' @param PPV_melted 
#' @param uncertainty_prevalence 
#' @param overlay_labels 
#' @param PPV_NPV 
#' @param overlay_position_FP_FN 
#'
#' @return
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
.plot_overlay_area <-
  function(PPV_melted,
           uncertainty_prevalence = "low",
           Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Max_FP,
           overlay_labels = "",
           PPV_NPV = "PPV",
           overlay_position_Prevalence,
           overlay_position_FP_FN,
           decimals_x,
           decimals_y,
           prevalence_label) {
    
  # DEBUG -------------------------------------------------------------------
  # uncertainty_prevalence = "high"
  # overlay_labels = ""
  
  
  # Calculate Overlay Coordinates x & y -------------------------------------.
  
  .calculate_area_overlay_coordinates(
    PPV_melted = PPV_melted,
    uncertainty_prevalence = uncertainty_prevalence,
    Min_Prevalence = Min_Prevalence,
    Max_Prevalence = Max_Prevalence,
    Sensitivity= Sensitivity,
    Max_FP = Max_FP,
    overlay_position_Prevalence = overlay_position_Prevalence,
    overlay_position_FP_FN = overlay_position_FP_FN)
  
    
  # CHECKS ------------------------------------------------------------------
      
      # Checks if overlay is outside the PPV matrix range, and changes values
          # Also, it re-creates PPV matrix and plot
    .check_size(
      overlay,
      Max_Prevalence,
      overlay_position_Prevalence,
      modifier_text_overlay_position,
      Sensitivity,
      Max_FP,
      overlay_position_FP_FN,
      Min_Prevalence,
      modifier_overlay_position_x,
      modifier_overlay_position_y
    )
    
    
    # Get PPV or NPV value ----------------------------------------------------
    
    # SHOULD THIS GO INSIDE .calculate_area_overlay_coordinates()???? ####
    .get_point_ppv_npv(PPV_melted, PPV_NPV = PPV_NPV, overlay_position_FP_FN = overlay_position_FP_FN) #, overlay_labels = overlay_labels, decimals_x = decimals_x, prevalence_label = prevalence_label
    


  # Color of text -----------------------------------------------------------
     
      if (point_PPV_NPV > 65) {
        Details_point_PPV_NPV_color = "white"
      } else {
        Details_point_PPV_NPV_color = "black"
      }

      

  # Position of text --------------------------------------------------------
    
      # X axis
        units_FP_FN = Max_FP/100 # Scale of the x axis
        n_pixels_per_character = units_FP_FN / 2 # How many pixels is a character (?)
        normalized_size_overlay_text_x = size_overlay_text * n_pixels_per_character
        
        # If the overlay area is too close to the limits of the plot, we put the text in the other side
        if (xmin_overlay - normalized_size_overlay_text_x < Min_FP | xmax_overlay + normalized_size_overlay_text_x < Max_FP) {
              Details_point_PPV_NPV_position_x = xmax_overlay + normalized_size_overlay_text_x
        } else {
            Details_point_PPV_NPV_position_x = xmin_overlay - normalized_size_overlay_text_x
        }
    
        
      # Y axis
        units_Prevalence = Max_Prevalence/100 # Scale of the x axis
        n_pixels_per_character = units_Prevalence * .5  # How many pixels should we move up or down? (?)
        normalized_size_overlay_text_y = n_pixels_per_character * 10
        
        if (ymin_overlay - n_pixels_per_character < Min_Prevalence) {
          Details_point_PPV_NPV_position_y = point_Prevalence + normalized_size_overlay_text_y
        } else {
          if (uncertainty_prevalence == "low") {
            Details_point_PPV_NPV_position_y = ymax_overlay - normalized_size_overlay_text_y / 2
          } else {
            Details_point_PPV_NPV_position_y = ymin_overlay + normalized_size_overlay_text_y
          }
          
        }
    
      
  # Add overlay -------------------------------------------------------------

    # If overlay outside old matrix, we need to do this
    if (DEBUG == 1) warning("\n\n  *Recalculate PPVMatrix: ", Min_Prevalence, " ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
    
    PPV_melted <- .createPPVmatrix(
      Min_Prevalence = .GlobalEnv$Min_Prevalence,
      Max_Prevalence = .GlobalEnv$Max_Prevalence,
      Sensitivity = .GlobalEnv$Sensitivity,
      Max_FP = .GlobalEnv$Max_FP
    )
    
    .plot_creation(
      PPV_melted = PPV_melted,
      Max_FP = Max_FP,
      Step_size_FP = Step_size_FP,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      prevalence_label = prevalence_label
    )  
    
    p = p +
      ggplot2::annotate("rect", color = "red", alpha = .1,
               xmin = xmin_overlay,
               xmax = xmax_overlay,
               ymin = ymin_overlay,
               ymax = ymax_overlay) +
      # Overlay center
      ggplot2::annotate("point", color = "red", alpha = .5, size = 1,
               x = overlay_position_FP_FN,
               # y = overlay_position_Prevalence) +
               y = point_Prevalence) +
      # INFORMATION
      ggplot2::annotate("text", size = 4, color = Details_point_PPV_NPV_color,
               x = Details_point_PPV_NPV_position_x,
               y = Details_point_PPV_NPV_position_y,
               label = paste0(Details_point_PPV_NPV))
      

  # Output vars -------------------------------------------------------------
    
    p <<- p
    
}
