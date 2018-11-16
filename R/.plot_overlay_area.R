#' Title
#'
#' @param PPV_melted 
#' @param uncertainty_prevalence 
#' @param overlay_labels 
#' @param PPV_NPV 
#' @param overlay_position_FP 
#'
#' @return
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
.plot_overlay_area <-
  function(PPV_melted,
           uncertainty_prevalence = "high",
           Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Max_FP,
           overlay_labels = "",
           PPV_NPV = "PPV",
           overlay_position_Prevalence,
           overlay_position_FP,
           decimals_x,
           decimals_y,
           prevalence_label) {
    
  # DEBUG -------------------------------------------------------------------
  # uncertainty_prevalence = "high"
  # overlay_labels = ""
  
  
  # Calculate Overlay Coordinates x & y -------------------------------------.
  
  # source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_BayesianReasoning/BayesianReasoning/R/.calculate_area_overlay_coordinates.R", local = FALSE)
  # source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_BayesianReasoning/BayesianReasoning/R/.get_point_ppv_npv.R", local = FALSE)
  
  .calculate_area_overlay_coordinates(
    PPV_melted = PPV_melted,
    uncertainty_prevalence = uncertainty_prevalence,
    Min_Prevalence = Min_Prevalence,
    Max_Prevalence = Max_Prevalence,
    Sensitivity= Sensitivity,
    Max_FP = Max_FP,
    overlay_position_Prevalence = overlay_position_Prevalence,
    overlay_position_FP = overlay_position_FP)
  
    
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
      overlay_position_FP,
      Min_Prevalence,
      modifier_overlay_position_x,
      modifier_overlay_position_y
    )
    
    
    # Get PPV or NPV value ----------------------------------------------------
    
    # SHOULD THIS GO INSIDE .calculate_area_overlay_coordinates()???? ####
    .get_point_ppv_npv(PPV_melted, PPV_NPV = PPV_NPV, overlay_position_FP = overlay_position_FP, overlay_labels = overlay_labels, decimals_x = decimals_x, prevalence_label = prevalence_label)
    


  # Color of text -----------------------------------------------------------
     
      if (point_PPV_NPV > 40) {
        Details_point_PPV_NPV_color = "white"
      } else {
        Details_point_PPV_NPV_color = "black"
      }

      

  # Position of text --------------------------------------------------------

      # TODO: improve this. Should be relative to size of overlay & range of x axis
      if (xmin_overlay - Min_FP < 2) {
        
        Details_point_PPV_NPV_position_x = xmax_overlay + (xmax_overlay - overlay_position_FP ) + Max_FP/75
        
      } else {
        
          Details_point_PPV_NPV_position_x = xmin_overlay - (xmax_overlay - overlay_position_FP)  - Max_FP/75#(1 + overlay_position_FP)/20
          
      }
      
      
  # Add overlay -------------------------------------------------------------

    # If overlay outside old matrix, we need to do this
    warning("\n\n  *Recalculate PPVMatrix: ", Min_Prevalence, " ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
    
    PPV_melted = .createPPVmatrix(
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
               x = overlay_position_FP, 
               # y = overlay_position_Prevalence) +
               y = point_Prevalence) +
      # INFORMATION
      ggplot2::annotate("text", size = 4, color = Details_point_PPV_NPV_color,
               x = Details_point_PPV_NPV_position_x, 
               y = point_Prevalence + modifier_overlay_position_y/2, 
               label = paste0(Details_point_PPV_NPV))
      

  # Output vars -------------------------------------------------------------
    
    p <<- p
    
}
