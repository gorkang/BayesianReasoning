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
#' @param overlay_position_FP_FN FP value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param uncertainty_prevalence How much certainty we have about the prevalence ["high"/"low"]
#' @param overlay_prevalence_2 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: "sp" / "en"
#' @param save_plot Should save the plot as .png or just show it?  [TRUE/FALSE]
#' @param PPV_NPV Should show PPV or NPV [PPV/NPV]
#' @param DEBUG Shows debug warnings [0/1]
#'
#' @return A .png plot in the /output folder, or shows a plot
#' @export
#' @importFrom ggplot2 ggplot aes element_text geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate filter pull
#' @importFrom magrittr %>%
#' 
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
#'             overlay_position_FP_FN = c(6.5, 7, 8, 9, 12, 14, 14),
#'             overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667))
#'
#' # Another plot with an overlay
#' PPV_heatmap(Max_Prevalence = 1200, Sensitivity = 81, Max_FP = 5,
#'             label_subtitle = "Prenatal screening for Down Syndrome by Age",
#'             save_plot = FALSE, Language = "en",
#'             overlay = "line",
#'             overlay_labels = c("40 y.o.", "35 y.o.", "30 y.o.", "25 y.o.", "20 y.o."),
#'             overlay_position_FP_FN = c(4.8, 4.8, 4.8, 4.8, 4.8),
#'             overlay_prevalence_2 = c(68, 249, 626, 946, 1068))

#' # A plot with a point overlay
#' PPV_heatmap(Max_Prevalence = 1200, Sensitivity = 81, Max_FP = 5,
#'             label_subtitle = "Prenatal screening for Down Syndrome by Age",
#'             overlay = "area",
#'             overlay_labels = "40 y.o.",
#'             overlay_position_FP_FN = 4.8,
#'             overlay_prevalence_2 = "1 out of 68")
PPV_heatmap <- function(Min_Prevalence = 1, Max_Prevalence, Sensitivity, Min_FP = 0, Max_FP,
                        overlay = "no", overlay_labels = "", overlay_position_FP_FN = 1, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100, uncertainty_prevalence = "high",
                        label_title = "", 
                        label_subtitle = "",
                        Language = "en", save_plot = FALSE,
                        PPV_NPV = "PPV",
                        DEBUG = 0) {

  # DEBUG -------------------------------------------------------------------

    # DEBUG <- 1
    # Sensitivity <<- Sensitivity
  
  # Libraries ---------------------------------------------------------------

  # Absolute paths
  # source("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_BayesianReasoning/BayesianReasoning/R/helper_functions.R", local = TRUE)
  # source(here::here("R/helper_functions.R"), local = TRUE)



  # Check dimensions -----------------------------------------------------------
    # If the dimensions of the overlay are bigger, adjust Max_FP and Max_Prevalence
  
  if (overlay == "area") {
    
    if (overlay_position_FP_FN > Max_FP) {
      message("Changing Max_FP to overlay_position_FP_FN to fit overlay")
      Max_FP = overlay_position_FP_FN 
    }
    
    if (overlay_prevalence_2 > Max_Prevalence) {
      message("Changing Max_Prevalence to overlay_prevalence_2 to fit overlay")
      Max_Prevalence = overlay_prevalence_2 
    }
    
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
                        Max_FP = Max_FP)
      
        label_caption <<- translated_labels_list$label_caption
        x_axis_label <<- translated_labels_list$x_axis_label
        y_axis_label <<- translated_labels_list$y_axis_label
        prevalence_label <<- translated_labels_list$prevalence_label
        legend_label <<- translated_labels_list$legend_label
      

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

      decimals_x <<- decimals$decimals_x
      decimals_y <<- decimals$decimals_y
      
      
    # Choose function depending on the type of overlay

     if (overlay == "line") {

       p = .plot_overlay_line(
          PPV_melted = PPV_melted,
          Max_Prevalence = Max_Prevalence,
          Max_FP = Max_FP,
          Sensitivity = Sensitivity,
          
          overlay_prevalence_2 = overlay_prevalence_2,
          overlay_position_FP_FN = overlay_position_FP_FN,
          overlay_labels = overlay_labels,
          label_title = label_title,
          label_subtitle = label_subtitle)


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
          PPV_NPV = PPV_NPV,
          overlay_prevalence_1 = overlay_prevalence_1,
          overlay_prevalence_2 = overlay_prevalence_2,
          overlay_position_FP_FN = overlay_position_FP_FN,
          decimals_x = decimals_x,
          decimals_y = decimals_y,
          prevalence_label = prevalence_label,
          label_title = label_title,
          label_subtitle = label_subtitle)

      } else {
        # if (overlay == "no") {
          
          p = .plot_creation(
            PPV_melted = PPV_melted,
            Sensitivity = Sensitivity,
            Min_FP = Min_FP,
            Max_FP = Max_FP,
            # Step_size_FP = Step_size_FP,
            decimals_x = decimals_x,
            decimals_y = decimals_y,
            prevalence_label = prevalence_label,
            label_title = label_title,
            label_subtitle = label_subtitle
          )
          
          
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
# PPV_heatmap(Max_Prevalence = 20, Sensitivity = 2.5, Max_FP = .1)
