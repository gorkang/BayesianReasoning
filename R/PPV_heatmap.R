#' Plot PPV and NPV heatmaps
#' 
#' Plot heatmaps showing the PPV for a given Sensitivity and a range of Prevalences and False Positive values or NPV values for a given Specificity and a range of Prevalences and True Positive values
#' 
#' @param min_Prevalence x in the "x out of y" prevalence (y-axis): [1-Inf]
#' @param max_Prevalence y in the "x out of y" prevalence (y-axis): [1-Inf]
#' @param Sensitivity Sensitivity of the test: [0-100]
#' @param Specificity Specificity of the test: [0-100]
#' @param limits_Sensitivity c(min Sensitivity, max Sensitivity) 
#' @param limits_Specificity c(min Specificity, max Specificity)
#' @param overlay Type of overlay: ["line", "area"]
#' @param overlay_labels Labels for each point in the overlay. For example: c("80", "70", "60", "50", "40", "30", "20  y.o.")
#' @param overlay_extra_info show extra info in overlay? [TRUE/FALSE]
#' @param overlay_position_FP FP value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param overlay_position_FN FN value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param uncertainty_prevalence How much certainty we have about the prevalence ["high"/"low"]
#' @param overlay_prevalence_1 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(1, 1, 1, 2, 1, 1)
#' @param overlay_prevalence_2 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: ["sp", "en"]
#' @param PPV_NPV Should show PPV or NPV ["PPV", "NPV"]
#' @param DEBUG Shows debug warnings [TRUE/FALSE]
#' @param folder Where to save the plot (the filename would be automatically created using the plot parameters)
#' @param one_out_of Show y scale as 1 out of x [TRUE, FALSE] FALSE by default
#' @param steps_matrix with of PPV/NPV matrix. 100 by default
#' @param ... .
#'
#' @return Shows a plot or, if given a folder argument, saves a .png version of the plot
#' @export
#' @importFrom ggplot2 ggplot aes element_text geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate filter pull
#' @importFrom magrittr %>%
#'
#' @examples
#' PPV_heatmap(min_Prevalence = 1, 
#' max_Prevalence = 1000, 
#' Sensitivity = 100, 
#' Specificity = 98, 
#' Language = "en")
PPV_heatmap <-
  function(min_Prevalence = 1,
           max_Prevalence = 1000,
           
           Sensitivity = NULL,
           Specificity = NULL,

           limits_Sensitivity = NULL, 
           limits_Specificity = NULL, 
           
           one_out_of = FALSE, 

           overlay = "no",
           overlay_labels = "",
           overlay_extra_info = FALSE,
           overlay_position_FP = NULL,
           overlay_position_FN = NULL,
           overlay_prevalence_1 = NULL,
           overlay_prevalence_2 = NULL,
           uncertainty_prevalence = "high",
           label_title = "",
           label_subtitle = "",
           Language = "en",
           folder = "",
           PPV_NPV = "PPV",
           
           steps_matrix = 100,
           DEBUG = FALSE,
           ...) {

    
    # PROCESS VARIABLES -------------------------------------------------------
    
    # Get ... vars    
    dots <- list(...)
    
    # CHECKS variables and sets defaults
    main_variables = process_variables(
      min_Prevalence = min_Prevalence,
      max_Prevalence = max_Prevalence,
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      limits_Sensitivity = limits_Sensitivity,
      limits_Specificity = limits_Specificity,
      overlay_labels = overlay_labels,
      overlay_position_FP = overlay_position_FP,
      overlay_position_FN = overlay_position_FN,
      overlay_prevalence_1 = overlay_prevalence_1,
      overlay_prevalence_2 = overlay_prevalence_2,
      PPV_NPV = PPV_NPV,
      one_out_of = one_out_of,
      overlay = overlay,
      steps_matrix = steps_matrix)
    
    
    if (DEBUG == TRUE) {
      message("\nDEBUG: ", "min_Sensitivity: ", main_variables$min_Sensitivity, " max_FN: ", main_variables$max_FN, " | max_Sensitivity: ", main_variables$max_Sensitivity, " min_FN: ",  main_variables$min_FN)
      message("DEBUG: ", "min_Specificity: ", main_variables$min_Specificity, " max_FP: ", main_variables$max_FP, " | max_Specificity: ", main_variables$max_Specificity, " min_FP: ",  main_variables$min_FP)
    }
    
        
 

  # SYSTEM parameters -------------------------------------------------------

    if (overlay != "no") {
      overlay_tag =  paste0("_", overlay)
    } else {
      overlay_tag = ""
    }

    if (overlay_extra_info == TRUE) {
      overlay_extra_info_tag = paste0(overlay_extra_info, "_")
    } else {
      overlay_extra_info_tag = ""
    }
    

  # Create PPV matrix -------------------------------------------------------

      PPV_melted <- .createPPVmatrix(
        min_Prevalence = main_variables$min_Prevalence,
        max_Prevalence = main_variables$max_Prevalence,
        Sensitivity = main_variables$Sensitivity,
        Specificity = main_variables$Specificity,
        min_FP = main_variables$min_FP,
        max_FP = main_variables$max_FP,
        max_FN = main_variables$max_FN,
        min_FN = main_variables$min_FN,
        
        one_out_of = one_out_of,
        PPV_NPV = PPV_NPV
      )


  # PLOT --------------------------------------------------------------------

    # Create plot labels in Language
      translated_labels = .translate_labels(Language = Language,
                                            Sensitivity = main_variables$Sensitivity,
                                            Specificity = main_variables$Specificity,
                                            PPV_NPV = PPV_NPV)
  

    # Number of decimals depends on the range
      decimals = .number_decimals_plot_axis(PPV_NPV = PPV_NPV,
                                            min_FP = main_variables$min_FP,
                                            max_FP = main_variables$max_FP,
                                            min_FN = main_variables$min_FN,
                                            max_FN = main_variables$max_FN,
                                            min_Prevalence = main_variables$min_Prevalence,
                                            max_Prevalence = main_variables$max_Prevalence)

      
    # Choose function depending on the type of overlay

     if (overlay == "line") {

       p = .plot_overlay_line(
          PPV_melted = PPV_melted,
          uncertainty_prevalence = uncertainty_prevalence,
          min_Prevalence = main_variables$min_Prevalence,
          max_Prevalence = main_variables$max_Prevalence,
          min_FP = main_variables$min_FP,
          max_FP = main_variables$max_FP,
          max_FN = main_variables$max_FN,
          min_FN = main_variables$min_FN,
          
          one_out_of = one_out_of, 
          
          overlay_prevalence_1 = main_variables$overlay_prevalence_1,
          overlay_prevalence_2 = main_variables$overlay_prevalence_2,
          
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          
          overlay_labels = overlay_labels,
          
          decimals_x = decimals$decimals_x,
          decimals_y = decimals$decimals_y,
          
          label_title = label_title,
          label_subtitle = label_subtitle,
          
          translated_labels = translated_labels,

          PPV_NPV = PPV_NPV)


      } else if (overlay == "area") {
        
        p = .plot_overlay_area(
          
          PPV_NPV = PPV_NPV,
          one_out_of = one_out_of,
          
          min_Prevalence = main_variables$min_Prevalence,
          max_Prevalence = main_variables$max_Prevalence,
          
          min_FP = main_variables$min_FP,
          max_FP = main_variables$max_FP,
          max_FN = main_variables$max_FN,
          min_FN = main_variables$min_FN,
          
          PPV_melted = PPV_melted,
          steps_matrix = steps_matrix,
          decimals_x = decimals$decimals_x,
          decimals_y = decimals$decimals_y,
          
          label_title = label_title,
          label_subtitle = label_subtitle,
          translated_labels = translated_labels,
          
          # Overlay area specific parameters
          Language = Language,
          
          Sensitivity = main_variables$Sensitivity,
          Specificity = main_variables$Specificity,
          
          uncertainty_prevalence = uncertainty_prevalence,
          overlay_prevalence_1 = main_variables$overlay_prevalence_1,
          overlay_prevalence_2 = main_variables$overlay_prevalence_2,
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          overlay_labels = overlay_labels,
          overlay_extra_info = overlay_extra_info,
          
          # Ellipsis
          DEBUG = DEBUG
          )

      } else {
        
          p = .plot_creation(
            PPV_melted = PPV_melted,
            min_Prevalence = main_variables$min_Prevalence,
            max_Prevalence = main_variables$max_Prevalence,
            
            min_FP = main_variables$min_FP,
            max_FP = main_variables$max_FP,
            max_FN = main_variables$max_FN,
            min_FN = main_variables$min_FN,
            
            one_out_of = one_out_of, 
            
            decimals_x = decimals$decimals_x,
            decimals_y = decimals$decimals_y,

            label_title = label_title,
            label_subtitle = label_subtitle,

            translated_labels = translated_labels,

            PPV_NPV = PPV_NPV
          )

      }


    # Show and Save plot -----------------------------------------------------

      if (folder != "") {
        
        if (is.null(dots$dpi)) dots$dpi = 150
        if (is.null(dots$width)) dots$width = 14
        if (is.null(dots$height)) dots$height = 10
          
        
        # PPV/NPV defines what we use for filename
        if (PPV_NPV == "PPV") {
          Sensitivity_Specificity_tag = main_variables$Sensitivity
          range_tag <- paste(c(main_variables$min_FP, main_variables$max_FP), collapse = "_")
        } else if (PPV_NPV == "NPV") {
          Sensitivity_Specificity_tag = main_variables$Specificity
          range_tag <- paste(c(main_variables$min_FN, main_variables$max_FN), collapse = "_")
        }

        # Name and save
        plot_name = paste0(folder, "/", PPV_NPV, "_", main_variables$min_Prevalence, "_", main_variables$max_Prevalence, "_", Sensitivity_Specificity_tag, "_", range_tag, overlay_tag, "_", overlay_extra_info_tag, Language, ".png")
        ggsave(plot_name, p, dpi = dots$dpi, width = dots$width, height = dots$height)
        message("\n Plot created in: ", plot_name, "\n")
        
      }
      
      print(p)
      

}
