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
.plot_overlay_area <- function(PPV_melted, uncertainty_prevalence = "high", overlay_labels = "", PPV_NPV = "PPV", overlay_position_FP) {
  
  
  # DEBUG -------------------------------------------------------------------
  # uncertainty_prevalence = "high"
  # overlay_labels = ""
  
  
  # Calculate Overlay Coordinates x & y -------------------------------------.
  
  source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.calculate_area_overlay_coordinates.R", local = TRUE)
  source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.get_point_ppv_npv.R", local = FALSE)
  
    .calculate_area_overlay_coordinates(uncertainty_prevalence, 
                    Min_Prevalence, Max_Prevalence, Sensitivity, Max_FP,
                    overlay_position_Prevalence, overlay_position_FP = overlay_position_FP)
    
    
  # CHECKS ------------------------------------------------------------------
      
      # Checks if overlay is outside the PPV matrix range, and changes values
          # Also, it re-creates PPV matrix and plot
      .check_size(overlay, Max_Prevalence, overlay_position_Prevalence, modifier_text_overlay_position, Max_FP, overlay_position_FP, Min_Prevalence, modifier_overlay_position_x, modifier_overlay_position_y)
      
    
    # Get PPV or NPV value ----------------------------------------------------
    
    # SHOULD THIS GO INSIDE .calculate_area_overlay_coordinates()???? ####
    .get_point_ppv_npv(PPV_melted, PPV_NPV = PPV_NPV, overlay_position_FP = overlay_position_FP, overlay_labels, prevalence_label, decimals_x)
    


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
    # 
    # # DEBUG -------------------------------------------------------------------
    # if (exists("DEBUG") == FALSE) {DEBUG = 0}
    # 
    # if(DEBUG == 1) {
    #   message("\n*** .check_size() POST *** ")
    #   message("Max_Prevalence: ", Max_Prevalence)
    #   message("overlay_prevalence_1: ", overlay_prevalence_1)
    #   message("overlay_prevalence_2: ", overlay_prevalence_2)
    #   message("Min_Prevalence: ", Min_Prevalence)
    #   message("modifier_overlay_position_y: ", modifier_overlay_position_y)
    #   message("point_Prevalence: ", point_Prevalence)
    #   message("\n*** END *** ")
    #   
    # }
    # # **************************************************************************
    
    # If overlay outside old matrix, we need to do this
    warning("\n\n  *Recalculate PPVMatrix: ", Min_Prevalence, " ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
    PPV_melted = .createPPVmatrix(Min_Prevalence = Min_Prevalence, Max_Prevalence = Max_Prevalence, Sensitivity = Sensitivity, Max_FP = Max_FP)
    
    .plot_creation(PPV_melted)    
    
    
    p <<- p + 
      annotate("rect", color = "red", alpha = .1,
               xmin = xmin_overlay, 
               xmax = xmax_overlay, 
               ymin = ymin_overlay, 
               ymax = ymax_overlay) +
      # Overlay center  
      annotate("point", color = "red", alpha = .5, size = 1,
               x = overlay_position_FP, 
               # y = overlay_position_Prevalence) +
               y = point_Prevalence) +
      # INFORMATION
      annotate("text", size = 4, color = Details_point_PPV_NPV_color,
               x = Details_point_PPV_NPV_position_x, 
               y = point_Prevalence + modifier_overlay_position_y/2, 
               label = paste0(Details_point_PPV_NPV))
      
}
