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
#' @param overlay_position_Prevalence Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: "sp" / "en"
#' @param save_plot Should save the plot as .png or just show it?
#'
#' @return A .png plot in the /output folder, or shows a plot
#' @export
#'
#' @examples
#' 
#' # Simple plot
#' PPV_heatmap(Max_Prevalence = 500, Sensitivity = 90, Max_FP = 15, Language = "en", overlay = FALSE)
#' 
#' # Show a plot with an overlay
#' PPV_heatmap(Max_Prevalence = 1800, Sensitivity = 90, Max_FP = 15, 
#'                label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
#'                save_plot = TRUE, Language = "en", 
#'                 overlay = TRUE, 
#'                 overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o."),
#'                 overlay_position_FP = c(7, 8, 9, 12, 14, 14),
#'                 overlay_position_Prevalence = c(26, 29, 44, 69, 227, 1667))
PPV_heatmap <- function(Max_Prevalence, Sensitivity, Max_FP, 
                            overlay = FALSE, overlay_labels, overlay_position_FP, overlay_position_Prevalence, 
                            label_title = "", label_subtitle = "",
                            Language = "en", save_plot = TRUE) {
  
  # Libraries ---------------------------------------------------------------
  
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(tidyverse, reshape2)
  

  # DEBUG -------------------------------------------------
    
    # overlay = TRUE # TRUE / FALSE
    # Language = "en" # "sp" / "en"
    # Sensitivity = 99 # [0-100]
    # Max_FP = 10 # FP (1-Specificity): [0-100]
    # Max_Prevalence = 1667.5 # Prevalence (1 out of X): [1-Inf?]
    # label_subtitle = "PPV of Mammogram for Breast Cancer by Age"
    # overlay = TRUE
    # overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o.")
    # overlay_position_FP = c(7, 8, 9, 12, 14, 14)
    # overlay_position_Prevalence = c(26, 29, 44, 69, 227, 1667)


  # SYSTEM parameters -------------------------------------------------------
      
      #GRAPHIC Parameters *************
      
      modifier_text_overlay_position = 10
  
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
  
      if (overlay == TRUE) {
        filename_overlay = "_overlay"
      } else {
        filename_overlay = ""
      }
      
      
      # CHECKS ------------------------------------------------------------------
      
      # If one of the overlay values is bigger than the test/disease values, we use the bigger value
      if (overlay == TRUE) {
        
        if (Max_FP < max(overlay_position_FP)) {
          Max_FP = max(overlay_position_FP)
          warning("\n\n  * One of the overlay_position_FP values is bigger than Max_FP. We use the max(overlay_position_FP) value to plot.")
        }
        
        if (Max_Prevalence < max(overlay_position_Prevalence)) {
          Max_Prevalence = (max(overlay_position_Prevalence) +  (modifier_text_overlay_position + 10))
          warning("\n\n  * One of the overlay_position_Prevalence is bigger than Max_Prevalence. We use the max(overlay_position_Prevalence) value to plot.")
        }
      }
  
  
      #TEST Parameters **************
      
          # False Positives (x axis) 
          Steps_FP = 100
          Step_size_FP = Max_FP/Steps_FP
          Min_FP = 0 
          FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
          
      #CONDITION Parameters ***********
          
          #Prevalence_y - x out of y
          Prevalence_x = 1
          Min_Prevalence = 1
          Steps_Prevalence = 100
          Step_size_Prevalence = Max_Prevalence/Steps_Prevalence
          Prevalence = seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence) #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
          

  # Calculation -------------------------------------------------------------
  
      # We calculate the 100x100 PPV matrix
      PPV = (Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP) )
      # NPV = (Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP) )
      
      #Label columns and rows of matrix
      colnames(PPV) = FP
      rownames(PPV) = Prevalence
      
      # Long format para ggplot Heatmap
      PPV_melted = melt(PPV)
      
      # Give names to variables
      names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV") 
      
      
  
  # PLOT --------------------------------------------------------------------
  
      #HEATMAP
      Paleta_DV = c( "white", "grey", "gray30", "yellowgreen", "chartreuse4")
      breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
      labels_DV = c(0, 25, 50, 75, 100)
        
      breaks_x = seq(0, Max_FP, Step_size_FP * 10)
      labels_x = paste0(seq(Min_FP, Max_FP, Step_size_FP * 10), "%")
      breaks_y = seq(0, Max_Prevalence, Step_size_Prevalence * 10)
      labels_y = paste(prevalence_label, round(seq(Min_Prevalence - 1, Max_Prevalence, Step_size_Prevalence * 10), 0))[-1]
      labels_y = c(paste(prevalence_label, "1"), labels_y) #We want the legend to start on 1 out of 1
      
      
      # PLOT
      p <<- ggplot(PPV_melted, aes(melted_FP, melted_Prevalence)) + 
        geom_tile(aes(fill = melted_PPV), colour = "white") +
        scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) + 
        scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) +
        scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
        theme(text = element_text(size = 20), axis.title.y = element_text(margin = margin(0,10,0,0)), axis.title.x = element_text(margin = margin(10,0,0,0))) +
        labs(title = label_title,
             subtitle = label_subtitle, 
             caption = label_caption,
             x = x_axis_label, 
             y = y_axis_label) 
        
      
      if (overlay == TRUE) {
        
          # overlay_labels = c("80", "70", "60", "50", "40", "30 y.o.")
          # overlay_position_FP = c(7, 8, 9, 12, 14)
          # overlay_position_Prevalence = c(26, 29, 44, 69, 227)
        
          overlay_position_FP = c(overlay_position_FP[1] - 0.5, overlay_position_FP)
          overlay_position_x_end = c(overlay_position_FP[1], overlay_position_FP[-length(overlay_position_FP)])
          
          overlay_position_Prevalence = c(overlay_position_Prevalence[1] - 0.5, overlay_position_Prevalence)
          overlay_position_y_end = c(overlay_position_Prevalence[1], overlay_position_Prevalence[-length(overlay_position_Prevalence)])
          
          p = p + annotate("segment", x = overlay_position_FP, xend = overlay_position_x_end, 
                       y = overlay_position_Prevalence, yend = overlay_position_y_end,
                       color = "red", alpha = .1, size = 3) +
                  annotate("text", x = overlay_position_FP, y = overlay_position_Prevalence + modifier_text_overlay_position, label = overlay_labels, size = 4) 
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

# No overlay example
# PPV_heatmap(Max_Prevalence = 500, Sensitivity = 90, Max_FP = 15, Language = "en", overlay = FALSE) 


# Overlay example
# PPV_heatmap(Max_Prevalence = 1800, Sensitivity = 90, Max_FP = 15, 
#                 label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
#                 save_plot = TRUE, Language = "en", 
#                 overlay = TRUE, 
#                 overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o."),
#                 overlay_position_FP = c(7, 8, 9, 12, 14, 14),
#                 overlay_position_Prevalence = c(26, 29, 44, 69, 227, 1667))