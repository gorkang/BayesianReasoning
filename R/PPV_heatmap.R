#' Plot PPV and NPV heatmaps
#' 
#' Plot heatmaps showing the PPV for a given Sensitivity and a range of Prevalences and False Positive values or NPV values for a given Specificity and a range of Prevalences and True Positive values
#' 
#' @param Min_Prevalence x in the "x out of y" prevalence (y-axis): 1-Inf
#' @param Max_Prevalence y in the "x out of y" prevalence (y-axis): 1-Inf
#' @param Sensitivity Sensitivity of the test: 0-100
#' @param Min_FP Minimum False Positives ratio to show in plot (x-axis): 1-100
#' @param Max_FP Maximum False Positives ratio to show in plot (x-axis): 1-100
#' @param overlay Show overlay: TRUE / FALSE
#' @param overlay_labels Labels for each point in the overlay. For example: c("80", "70", "60", "50", "40", "30", "20  y.o.")
#' @param overlay_position_FP FP value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param overlay_position_FN FN value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param uncertainty_prevalence How much certainty we have about the prevalence ["high"/"low"]
#' @param overlay_prevalence_1 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(1, 1, 1, 2, 1, 1)
#' @param overlay_prevalence_2 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: "sp" / "en"
#' @param PPV_NPV Should show PPV or NPV [PPV/NPV]
#' @param DEBUG Shows debug warnings [0/1]
#' @param folder Where to save the plot (the filename would be automatically created using the plot parameters)
#'
#' @return Shows a plot or, if given a folder argument, saves a .png version of the plot
#' @export
#' @importFrom ggplot2 ggplot aes element_text geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate filter pull
#' @importFrom magrittr %>%
#'
#' @examples
#' PPV_heatmap(Min_Prevalence = 1, 
#' Max_Prevalence = 1000, 
#' Sensitivity = 100, 
#' Max_FP = 2, 
#' Language = "en")
PPV_heatmap <-
  function(Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Min_FP = 0,
           Max_FP,
           overlay = "no",
           overlay_labels = "",
           overlay_position_FP = 1,
           overlay_position_FN = 1,
           overlay_prevalence_1 = 1,
           overlay_prevalence_2 = 100,
           uncertainty_prevalence = "high",
           label_title = "",
           label_subtitle = "",
           Language = "en",
           folder = "",
           PPV_NPV = "PPV",
           DEBUG = 0) {
    

  # Check dimensions -----------------------------------------------------------
    
    
    # If the dimensions of the overlay are bigger, adjust Max_FP and Max_Prevalence

    if (overlay == "area") {
      
      if (overlay_position_FP > Max_FP) {
      # if (overlay_position_FP_FN > Max_FP) {
          message("Changing Max_FP to overlay_position_FP to fit overlay")
        # Max_FP = overlay_position_FP_FN
        Max_FP = overlay_position_FP 
      }
      
      if (overlay_prevalence_2 > Max_Prevalence) {
        message("Changing Max_Prevalence to overlay_prevalence_2 to fit overlay")
        Max_Prevalence = overlay_prevalence_2 
      }
      
      # if(overlay_position_FP_FN > (100 - Sensitivity)) {
      if(overlay_position_FN > (100 - Sensitivity)) {
        Sensitivity = 100 - overlay_position_FN
      }
      
      
      if (overlay_prevalence_1/overlay_prevalence_2 < Min_Prevalence/Max_Prevalence) {
        
        message("Overlay impossible to fit in plot: overlay_prevalence_1/overlay_prevalence_2 < Min_Prevalence/Max_Prevalence: Changing Min_Prevalence to (overlay_prevalence_1/overlay_prevalence_2) * Max_Prevalence to fit overlay")
        
        # Min Prevalence adjusted to fit overlay
        Min_Prevalence = (overlay_prevalence_1/overlay_prevalence_2) * Max_Prevalence
        
      }
      
    }

    
    

# Check overlay prevalence ------------------------------------------------

    if (length(overlay_prevalence_1) == 1) {
      if (overlay_prevalence_1 > overlay_prevalence_2) {
        
        message(overlay_prevalence_1, " is > than ", overlay_prevalence_2)
        overlay_prevalence_1 = overlay_prevalence_2
        
      }
    } else if (length(overlay_prevalence_1) > 1) {
      if (DEBUG != 0) message("> 1 overlay")
    }
    

  # SYSTEM parameters -------------------------------------------------------

      #GRAPHIC Parameters *************

      # modifier_text_overlay_position = (Max_Prevalence/75)
      if (overlay != "no") {
        filename_overlay = paste0("_", overlay)
      } else {
        filename_overlay = ""
      }



  # Create PPV matrix -------------------------------------------------------

      PPV_melted = .createPPVmatrix(
        Min_Prevalence = Min_Prevalence,
        Max_Prevalence = Max_Prevalence,
        Sensitivity = Sensitivity,
        Min_FP = Min_FP,
        Max_FP = Max_FP
      )


  # PLOT --------------------------------------------------------------------

    # Create plot labels in Language
      translated_labels_list = .translate_labels(Language = Language,
                        Sensitivity = Sensitivity,
                        Max_FP = Max_FP,
                        PPV_NPV = PPV_NPV)
      
        label_caption <- translated_labels_list$label_caption
        x_axis_label <- translated_labels_list$x_axis_label
        y_axis_label <- translated_labels_list$y_axis_label
        prevalence_label <- translated_labels_list$prevalence_label
        legend_label <- translated_labels_list$legend_label
      


    # Max_FN & Min_FN are created in .createPPVmatrix()
      Max_FN <- (100 - Sensitivity)
      Min_FN <- 0
      decimals = .number_decimals_plot_axis(PPV_NPV = PPV_NPV,
                                            Min_FP = Min_FP,
                                            Max_FP = Max_FP,
                                            Min_FN = Min_FN,
                                            Max_FN = Max_FN,
                                            Min_Prevalence = Min_Prevalence,
                                            Max_Prevalence = Max_Prevalence)

      decimals_x <- decimals$decimals_x
      decimals_y <- decimals$decimals_y
      
      
    # Choose function depending on the type of overlay

     if (overlay == "line") {

       p = .plot_overlay_line(
          PPV_melted = PPV_melted,
          Min_Prevalence = Min_Prevalence,
          Max_Prevalence = Max_Prevalence,
          Max_FP = Max_FP,
          Sensitivity = Sensitivity,
          
          overlay_prevalence_2 = overlay_prevalence_2,
          
          # overlay_position_FP_FN = overlay_position_FP_FN,
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          
          
          overlay_labels = overlay_labels,
          
          decimals_x = decimals_x,
          decimals_y = decimals_y,
          
          prevalence_label = prevalence_label,
          legend_label = legend_label,
          label_title = label_title,
          label_subtitle = label_subtitle,
          x_axis_label = x_axis_label,
          y_axis_label = y_axis_label,
          PPV_NPV = PPV_NPV)


      } else if (overlay == "area") {
        p = .plot_overlay_area(
          PPV_melted,
          uncertainty_prevalence = uncertainty_prevalence,
          Min_Prevalence = Min_Prevalence,
          Max_Prevalence = Max_Prevalence,
          Sensitivity = Sensitivity,
          Min_FP = Min_FP,
          Max_FP = Max_FP,
          # Step_size_FP = Step_size_FP,
          overlay_labels = overlay_labels,
          overlay_prevalence_1 = overlay_prevalence_1,
          overlay_prevalence_2 = overlay_prevalence_2,
          
          # overlay_position_FP_FN = overlay_position_FP_FN,
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          
          decimals_x = decimals_x,
          decimals_y = decimals_y,
          
          prevalence_label = prevalence_label,
          legend_label = legend_label,
          label_title = label_title,
          label_subtitle = label_subtitle,
          x_axis_label = x_axis_label,
          y_axis_label = y_axis_label,
          
          PPV_NPV = PPV_NPV
          
        )

      } else {
        # if (overlay == "no") {
          
          p = .plot_creation(
            PPV_melted = PPV_melted,
            Min_Prevalence = Min_Prevalence,
            Sensitivity = Sensitivity,
            Min_FP = Min_FP,
            Max_FP = Max_FP,
            # Step_size_FP = Step_size_FP,
            decimals_x = decimals_x,
            decimals_y = decimals_y,
            
            prevalence_label = prevalence_label,
            legend_label = legend_label,
            label_title = label_title,
            label_subtitle = label_subtitle,
            x_axis_label = x_axis_label,
            y_axis_label = y_axis_label,
            
            PPV_NPV = PPV_NPV
            
          )
          
          
      }


    # Save plot ---------------------------------------------------------------

      if (folder != "") {

        print(p)
        # plot_name = here::here(paste0("outputs/PPV_heatmap/", PPV_NPV, "_", Min_Prevalence, "_", Max_Prevalence, "_", Sensitivity, "_", Max_FP, filename_overlay, "_", Language, ".png"))
        plot_name = paste0(folder, "/", PPV_NPV, "_", Min_Prevalence, "_", Max_Prevalence, "_", Sensitivity, "_", Max_FP, filename_overlay, "_", Language, ".png")
        ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
        message("\n Plot created in: ", plot_name, "\n")

      } else {

        print(p)

      }

}
