#' Plot PPV heatmaps
#' 
#' Plot PPV heatmaps Plot heatmaps with PPV values for a given specificity and a range of Prevalences and FP
#' 
#' @param Max_Prevalence Maximum prevalence to show in plot (y-axis): 1-Inf
#' @param Sensitivity Sensitivity of the test: 0-100
#' @param Max_FP Maximum False Positives ratio to show in plot (x-axis): 1-100
#' @param overlay Show overlay: TRUE / FALSE
#' @param overlay_labels Lables for each point in the overlay. For example: c("80", "70", "60", "50", "40", "30", "20  y.o.")
#' @param overlay_position_FP FP value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param uncertainty_prevalence How much certainty we have about the prevalence ["high"/"low"]
#' @param overlay_position_Prevalence Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: "sp" / "en"
#' @param save_plot Should save the plot as .png or just show it?  [TRUE/FALSE]
#' @param PPV_NPV Should show PPV or NPV [PPV/NPV]
#'
#' @return A .png plot in the /output folder, or shows a plot
#' @export
# @importFrom BayesianReasoning .createPPVmatrix
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom reshape2 melt
#'
#' @examples
#' 
#' # Simple plot
#' PPV_heatmap(Max_Prevalence = 500, Sensitivity = 90, Max_FP = 15)
#' 
#' # Show a plot with an overlay
#' PPV_heatmap(Max_Prevalence = 1800, Sensitivity = 90, Max_FP = 15,
#'             label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
#'             save_plot = FALSE, Language = "en",
#'             overlay = "line",
#'             overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o."),
#'             overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14),
#'             overlay_position_Prevalence = c(22, 26, 29, 44, 69, 227, 1667))
#'
#' # Another plot with an overlay
#' PPV_heatmap(Max_Prevalence = 1200, Sensitivity = 81, Max_FP = 5,
#'             label_subtitle = "Prenatal screening for Down Syndrome by Age",
#'             save_plot = FALSE, Language = "en",
#'             overlay = "line",
#'             overlay_labels = c("40 y.o.", "35 y.o.", "30 y.o.", "25 y.o.", "20 y.o."),
#'             overlay_position_FP = c(4.8, 4.8, 4.8, 4.8, 4.8),
#'             overlay_position_Prevalence = c(68, 249, 626, 946, 1068))

#' # A plot with a point overlay
#' PPV_heatmap(Max_Prevalence = 1200, Sensitivity = 81, Max_FP = 5,
#'             label_subtitle = "Prenatal screening for Down Syndrome by Age",
#'             overlay = "area",
#'             overlay_labels = "40 y.o.",
#'             overlay_position_FP = 4.8,
#'             overlay_position_Prevalence = 68
PPV_heatmap <- function(Max_Prevalence, Sensitivity, Max_FP,
                            overlay = "no", overlay_labels, overlay_position_FP, overlay_position_Prevalence, uncertainty_prevalence = "high",
                            label_title = "", label_subtitle = "",
                            Language = "en", save_plot = FALSE,
                            PPV_NPV = "PPV") {
  
  # Libraries ---------------------------------------------------------------

  # message(getwd())
  # cat(message(getwd()))
  # print(message(getwd()))
  # stop()
  
  # Absolute paths
  source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.createPPVmatrix.R", local = TRUE)
  source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.check_size.R", local = TRUE)
  source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.plot_creation.R", local = TRUE)
  
  # Relative paths
  # source("R/.createPPVmatrix.R", local = TRUE)
  # source("R/.check_size.R", local = TRUE)
  # source("R/.plot_creation.R", local = TRUE)
  
  # DEBUG -------------------------------------------------
    
    # overlay = TRUE # TRUE / FALSE
    # Language = "en" # "sp" / "en"
    # Max_Prevalence = 4 # Prevalence (1 out of X): [1-Inf?]
    # Sensitivity = 90 # [0-100]
    # Max_FP = 5 # FP (1-Specificity): [0-100]
    # label_subtitle = "PPV of Mammogram for Breast Cancer by Age"
    # overlay = TRUE
    # overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o.")
    # overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14)
    # overlay_position_Prevalence = c(22, 26, 29, 44, 69, 227, 1667)


  # SYSTEM parameters -------------------------------------------------------
      
      #GRAPHIC Parameters *************

      # modifier_text_overlay_position = (Max_Prevalence/75)
  
      #Labels 
      if (Language == "sp") {
        
        label_caption = paste("Sensibilidad =", Sensitivity, "%")
        x_axis_label = "Tasa de falsos positivos"
        y_axis_label = "Prevalencia"
        prevalence_label = "1 de cada"
        legend_label = "VPP (%)\n"
        
      } else {
        
        label_caption = paste("Sensitivity =", Sensitivity, "%")
        x_axis_label = "False Positive rate"
        y_axis_label = "Prevalence"
        prevalence_label = "1 out of"
        legend_label = "PPV (%)\n"
        
      }
  
      if (overlay != "no") {
        filename_overlay = "_overlay"
      } else {
        filename_overlay = ""
      }
      
      

  # Create PPV matrix -------------------------------------------------------
    .createPPVmatrix(Max_Prevalence, Sensitivity, Max_FP)
  
  
  # PLOT --------------------------------------------------------------------
  
    # Heatmap
  
      .plot_creation(PPV_melted)
 
  
    # Overlay
  
      if (overlay == "line") {
        
          # We made the modifiers proportional to the parameters (Max_Prevalence, Max_FP)
          if (exists("size_uncertainty_area") == FALSE) {size_uncertainty_area = 0}
          if (abs(Max_Prevalence - max(overlay_position_Prevalence)) > 10) {
            modifier_text_overlay_position = (Max_Prevalence * size_uncertainty_area  + 1)
          } else {
            modifier_text_overlay_position = -(Max_Prevalence * size_uncertainty_area + 1)
          }
            # overlay_labels = c("80", "70", "60", "50", "40", "30 y.o.")
            # overlay_position_FP = c(7, 8, 9, 12, 14)
            # overlay_position_Prevalence = c(26, 29, 44, 69, 227)
          
            overlay_position_x_end = c(overlay_position_FP[1], overlay_position_FP[-length(overlay_position_FP)])
            overlay_position_y_end = c(overlay_position_Prevalence[1], overlay_position_Prevalence[-length(overlay_position_Prevalence)])
            
            # Add overlay
            p = p + annotate("segment", x = overlay_position_FP, xend = overlay_position_x_end, 
                         y = overlay_position_Prevalence, yend = overlay_position_y_end,
                         color = "red", alpha = .1, size = 3) +
                    annotate("text", x = overlay_position_FP, y = overlay_position_Prevalence, label = overlay_labels, size = 4) 
            

      } else if (overlay == "area") {
  
        
          # Uncertainty - how big the square should be
        
            if (uncertainty_prevalence == "high") {
              size_uncertainty_area = .10
              modifier_overlay_position_x = (Max_FP * size_uncertainty_area)
            } else if (uncertainty_prevalence == "low") {
              size_uncertainty_area = .05
              modifier_overlay_position_x = (Max_FP * size_uncertainty_area)
            } else {
              size_uncertainty_area = .075
              modifier_overlay_position_x = (Max_FP * size_uncertainty_area)
            }
              
            modifier_overlay_position_x = round(modifier_overlay_position_x, 1)
        
            
          # Position of text label
        
            # We made the modifiers proportional to the parameters (Max_Prevalence, Max_FP)
            if (exists("size_uncertainty_area") == FALSE) {size_uncertainty_area = 0}
            
            if (abs(Max_Prevalence - overlay_position_Prevalence) > 10) {
              # modifier_text_overlay_position = (Max_Prevalence * size_uncertainty_area  + 1)
              modifier_overlay_position_y = (Max_Prevalence * size_uncertainty_area  + 1)
              modifier_text_overlay_position = modifier_overlay_position_y + Max_Prevalence/50
              
              
            } else {
              # modifier_text_overlay_position = -(Max_Prevalence * size_uncertainty_area + 1)
              modifier_overlay_position_y = -(Max_Prevalence * size_uncertainty_area + 1)
              modifier_text_overlay_position = modifier_overlay_position_y + Max_Prevalence/50
              
            }
        
          
          # CHECKS ------------------------------------------------------------------
          # Checks if overlay is outside the PPV matrix range, and changes values
            .check_size(overlay, Max_Prevalence, overlay_position_Prevalence, modifier_text_overlay_position, Max_FP, overlay_position_FP)
            
            # Recalculate PPV Matrix (if overlay outside old matrix, we need to do this)
            warning("\n\n  *Recalculate PPVMatrix: ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
            .createPPVmatrix(Max_Prevalence, Sensitivity, Max_FP)
            
              
          # Create plot again after potential changes in .check_size
            .plot_creation(PPV_melted)
  
          # Check if ymin is > 1
          if ((overlay_position_Prevalence - modifier_overlay_position_y) < 1) {
            ymin_overlay = 0
          } else {
              ymin_overlay = overlay_position_Prevalence - modifier_overlay_position_y
            }
            
          # Add overlay
            p = p + 
              annotate("rect", color = "red", alpha = .1,
                       xmin = overlay_position_FP - modifier_overlay_position_x, 
                       xmax = overlay_position_FP + modifier_overlay_position_x, 
                       ymin = ymin_overlay, 
                       ymax = overlay_position_Prevalence + modifier_overlay_position_y) +
              annotate("point", color = "red", alpha = .5, size = 1,
                       x = overlay_position_FP, 
                       y = overlay_position_Prevalence) +
              annotate("text", size = 4,
                       x = overlay_position_FP, 
                       y = overlay_position_Prevalence + modifier_text_overlay_position, 
                       label = overlay_labels) 
          
      }
  
  
    # Save plot ---------------------------------------------------------------

      if (save_plot == TRUE) {
        
        print(p)
        plot_name = paste0("outputs/PPV_heatmap/", Max_Prevalence, "_", Sensitivity, "_", Max_FP, "_", Language, filename_overlay, ".png")
        ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
        cat("\n Plot created in: ", plot_name, "\n")
        
      } else {
        
        print(p)
        
      }
      
}
