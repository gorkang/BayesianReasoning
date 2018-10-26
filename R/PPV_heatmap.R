  #' Plot PPV heatmaps
  #' 
  #' Plot PPV heatmaps Plot heatmaps with PPV values for a given specificity and a range of Prevalences and FP
  #' 
  #' @param Min_Prevalence x in the "x out of y" prevalence (y-axis): 1-Inf
  #' @param Max_Prevalence y in the "x out of y" prevalence (y-axis): 1-Inf
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
  #' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
  #' @importFrom reshape2 melt
  #' @importFrom dplyr mutate filter pull
  #' @importFrom magrittr %>%
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
  #'             overlay_position_Prevalence = "1 out of 68")
  PPV_heatmap <- function(Min_Prevalence = 1, Max_Prevalence, Sensitivity, Max_FP,
                              overlay = "no", overlay_labels = "", overlay_position_FP, overlay_position_Prevalence, uncertainty_prevalence = "high",
                              label_title = "", label_subtitle = "",
                              Language = "en", save_plot = FALSE,
                              PPV_NPV = "PPV") {
    
  
    # DEBUG -------------------------------------------------
      
      # overlay = FALSE # TRUE / FALSE
      # Language = "en" # "sp" / "en"
      # Min_Prevalence = 1
      # Max_Prevalence = 800 # Prevalence (1 out of X): [1-Inf?]
      # Sensitivity = 90 # [0-100]
      # Max_FP = 5 # FP (1-Specificity): [0-100]
      # label_subtitle = "PPV of Mammogram for Breast Cancer by Age"
      # overlay = TRUE
      # overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o.")
      # overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14)
      # overlay_position_Prevalence = "888 out of 1000"#c(22, 26, 29, 44, 69, 227, 1667)
      # label_title = ""
      # label_subtitle = ""
      # PPV_NPV = "NPV"
      # library(dplyr)
      # library(ggplot2)
    
    # Min_Prevalence = 1
    # Max_Prevalence = 1000
    # Sensitivity = 90
    # Max_FP = 3
    # Language = "en"
    # PPV_NPV = "NPV"
    # overlay = "area"
    # overlay_position_FP = 10
    # overlay_position_Prevalence = "868 out of 1000"
    # uncertainty_prevalence = "high"
    # library(dplyr)
    # library(ggplot2)
  
    # Libraries ---------------------------------------------------------------
    
    # Absolute paths
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.createPPVmatrix.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.check_size.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.plot_creation.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.plot_overlay_line.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.plot_overlay_area.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.calculate_area_overlay_coordinates.R", local = TRUE)
    source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_PPV_Plot/R/.process_overlay_position_prevalence.R", local = TRUE)
    
    # Relative paths
    # source("R/.createPPVmatrix.R", local = TRUE)
    # source("R/.check_size.R", local = TRUE)
    # source("R/.plot_creation.R", local = TRUE)
    
    
    
    # Preprocessing -----------------------------------------------------------
  
    if (exists("overlay_position_Prevalence") == TRUE & overlay == "area") {
        # CHANGE overlay_position_Prevalence = "868 out of 1000" to overlay_prevalence_1 = 868; overlay_prevalence_2 = 1000
        .process_overlay_position_prevalence(overlay_position_Prevalence)
        
            #### REVIEW: CHANGE INSTANCES OF overlay_position_Prevalence to overlay_prevalence_2 in all scripts? #####
            overlay_position_Prevalence <<- overlay_prevalence_2
    }
    
    PPV_NPV <<- PPV_NPV
    Max_FP <<- Max_FP
    Max_Prevalence <<- Max_Prevalence
    Min_Prevalence <<- Min_Prevalence
    Language <<- Language
    if (exists("overlay") == FALSE) overlay = "no"
    # **************************************************************************
    
 
    # SYSTEM parameters -------------------------------------------------------
        
        #GRAPHIC Parameters *************
  
        # modifier_text_overlay_position = (Max_Prevalence/75)
        if (overlay != "no") {
          filename_overlay = paste0("_", overlay)
        } else {
          filename_overlay = ""
        }
        
        
  
    # Create PPV matrix -------------------------------------------------------
      PPV_melted = .createPPVmatrix(Min_Prevalence = Min_Prevalence, Max_Prevalence = Max_Prevalence, Sensitivity = Sensitivity, Max_FP = Max_FP)
    
      # .createNPVmatrix(Max_Prevalence, Sensitivity, Max_FP)
    
    # PLOT --------------------------------------------------------------------
    
        # Choose function depending on the type of overlay

        if (overlay == "no") {
          
           .plot_creation(PPV_melted)
          
        } else if (overlay == "line") {
          
          # .plot_creation(PPV_melted)
          .plot_overlay_line(PPV_melted)
              
  
        } else if (overlay == "area") {
    
          # .plot_creation(PPV_melted)
          .plot_overlay_area(PPV_melted, overlay_labels = overlay_labels, PPV_NPV = PPV_NPV, overlay_position_FP = overlay_position_FP)
            
        }
    
    
      # Save plot ---------------------------------------------------------------
  
        if (save_plot == TRUE) {
          
          print(p)
          plot_name = paste0("outputs/PPV_heatmap/", PPV_NPV, "_", Min_Prevalence, "_", Max_Prevalence, "_", Sensitivity, "_", Max_FP, filename_overlay, "_", Language, ".png")
          ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
          cat("\n Plot created in: ", plot_name, "\n")
          
        } else {
          
          print(p)
          
        }
        
  }
