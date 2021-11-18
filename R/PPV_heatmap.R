#' Plot PPV and NPV heatmaps
#' 
#' Plot heatmaps showing the PPV for a given Sensitivity and a range of Prevalences and False Positive values or NPV values for a given Specificity and a range of Prevalences and True Positive values
#' 
#' @param min_Prevalence x in the "x out of y" prevalence (y-axis): [1-Inf]
#' @param max_Prevalence y in the "x out of y" prevalence (y-axis): [1-Inf]
#' @param Sensitivity Sensitivity of the test: [0-100]
#' @param overlay Show overlay: [TRUE, FALSE]
#' @param overlay_labels Labels for each point in the overlay. For example: c("80", "70", "60", "50", "40", "30", "20  y.o.")
#' @param overlay_position_FP FP value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param overlay_position_FN FN value (position in the x-axis) for each point in the overlay. For example: c(7, 8, 9, 12, 14, 14)
#' @param uncertainty_prevalence How much certainty we have about the prevalence ["high"/"low"]
#' @param overlay_prevalence_1 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(1, 1, 1, 2, 1, 1)
#' @param overlay_prevalence_2 Prevalence value (position in the y-axis) for each point in the overlay. For example: c(26, 29, 44, 69, 227, 1667)
#' @param label_title Title for the plot
#' @param label_subtitle Subtitle for the plot
#' @param Language Language for the plot labels: ["sp", "en"]
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
#' PPV_heatmap(min_Prevalence = 1, 
#' max_Prevalence = 1000, 
#' Sensitivity = 100, 
#' max_FP = 2, 
#' Language = "en")
PPV_heatmap <-
  function(min_Prevalence = 1,
           max_Prevalence = 1000,
           
           Sensitivity = 95,
           Specificity = 95,
           width_Sensitivity = 10, 
           width_Specificity = 10,
           limits_Sensitivity = NULL, 
           limits_Specificity = NULL, 
           
           one_out_of = TRUE, 
           
           
           # min_FP = 0,
           # max_FP = 10,
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
    
    
    # PROCESS VARIABLES -------------------------------------------------------
    
    
    main_variables = process_variables(
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      width_Sensitivity = width_Sensitivity,
      width_Specificity = width_Specificity,
      limits_Sensitivity = limits_Sensitivity,
      limits_Specificity = limits_Specificity)
    
    
    # Min_FN = (100 - main_variables$min_Sensitivity)
    # Max_FN = (100 - main_variables$max_Sensitivity)
    # min_FP = (100 - main_variables$min_Specificity)
    # max_FP = (100 - main_variables$max_Specificity)
    
    
    Max_FN = (100 - main_variables$min_Sensitivity)
    Min_FN = (100 - main_variables$max_Sensitivity)

    # In
    max_FP = (100 - main_variables$min_Specificity)
    min_FP = (100 - main_variables$max_Specificity)

    message("\nDEBUG: ", "min_Sensitivity: ", main_variables$min_Sensitivity, " Max_FN: ", Max_FN, " | max_Sensitivity: ", main_variables$max_Sensitivity, " Min_FN: ",  Min_FN)
    message("DEBUG: ", "min_Specificity: ", main_variables$min_Specificity, " max_FP: ", max_FP, " | max_Specificity: ", main_variables$max_Specificity, " min_FP: ",  min_FP)
    
  # Check dimensions -----------------------------------------------------------


    # CHECKS
    if (Sensitivity > 100 | Sensitivity < 0) stop("* Sensitivity sould be a value 0-100")
    if (Specificity > 100 | Specificity < 0) stop("* Specificity sould be a value 0-100")
    if (overlay == "area" & length(overlay_position_FN) > 1) stop("* overlay_position_FN has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
    if (overlay == "area" & length(overlay_position_FN) > 1) stop("* overlay_position_FN has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
    if (overlay == "area" & length(overlay_prevalence_1) > 1) stop("* overlay_prevalence_1 has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")

    if (min_Prevalence < 1) {
      message("\n[WARNING]: min_Prevalence (", min_Prevalence , ") is < 1. \n[EXPECTED]: min_Prevalence should be an integer > 0.\n[CHANGED]: min_Prevalence = 1")
      min_Prevalence = 1
    }
    
    if (min_Prevalence > max_Prevalence) {
      message("\n[WARNING]: min_Prevalence (", min_Prevalence , ") is > than max_Prevalence (", max_Prevalence, ").\n[EXPECTED]: min_Prevalence should be smaller than max_Prevalence.\n[CHANGED]: min_Prevalence = max_Prevalence/2")
      min_Prevalence = max_Prevalence/2
    }
    # If the dimensions of the overlay are bigger, adjust max_FP and max_Prevalence

    if (overlay == "area") {
      
      if (exists("overlay_position_FP")) {
        if (overlay_position_FP > max_FP & PPV_NPV == "PPV") {
          message("\n[WARNING]: overlay_position_FP (", overlay_position_FP , ") is > than max_FP (", max_FP, ").\n[EXPECTED]: overlay_position_FP should be smaller than max_FP\n[CHANGED]: max_FP = overlay_position_FP")
          max_FP = overlay_position_FP
        }
      }
    }
    
    if (overlay == "area" | overlay == "line") {
        
      # CHECK overlay_prevalence_1/overlay_prevalence_2 fits into min_Prevalence/max_Prevalence
      if (any(min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2)) {
        
        index_issue = which(min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2)
        message("\n[WARNING]: min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2\n[EXPECTED]: min_Prevalence/max_Prevalence should be <= overlay_prevalence_1/overlay_prevalence_2")
        
        if (max_Prevalence == overlay_prevalence_2[index_issue] & min_Prevalence != overlay_prevalence_1[index_issue]) {
          message("[CONDITION]: max_Prevalence == overlay_prevalence_2\n[CHANGED]: Changing min_Prevalence = overlay_prevalence_1")
          min_Prevalence = overlay_prevalence_1[index_issue]
        } else if (min_Prevalence == overlay_prevalence_1[index_issue] & max_Prevalence != overlay_prevalence_2[index_issue]) {
          message("[CONDITION]: min_Prevalence == overlay_prevalence_1\n[CHANGED]: Changing max_Prevalence = overlay_prevalence_2")
          max_Prevalence = overlay_prevalence_2[index_issue]
        } else {
          message("[CONDITION]: min_Prevalence != overlay_prevalence_1\n\t     max_Prevalence != overlay_prevalence_2\n[CHANGED]: Changing max_Prevalence = overlay_prevalence_2 & min_Prevalence = overlay_prevalence_1")
          min_Prevalence = overlay_prevalence_1[index_issue]
          max_Prevalence = overlay_prevalence_2[index_issue]
        }
      }
      
    
      if (PPV_NPV == "PPV"){
      
        if (exists("overlay_position_FP")) {
          if (min(overlay_position_FP) < min_FP) {
            message("\n[WARNING]: overlay_position_FP (", min(overlay_position_FP) , ") is < min_FP (", min_FP, ").\n[EXPECTED]: overlay_position_FP should be >= min_FP.\n[CHANGED]: min_FP = 0")
            min_FP = 0
          }
          
          if (max(overlay_position_FP) > max_FP) {
            message("\n[WARNING]: overlay_position_FP (", max(overlay_position_FP) , ") is > min_FP (", min_FP, ").\n[EXPECTED]: overlay_position_FP should be <= max_FP.\n[CHANGED]: max_FP = overlay_position_FP + 10%")
            max_FP = max(overlay_position_FP) + (max(overlay_position_FP) * .1)
          }
        }
        
      } else if (PPV_NPV == "NPV") {
        
        if (exists("overlay_position_FN")) {
          if (max(overlay_position_FN) > Max_FN) {
            message("\n[WARNING]: overlay_position_FN (", max(overlay_position_FN) , ") is > Max_FN (", Max_FN, ")\n[EXPECTED]: overlay_position_FN should be <= Max_FN\n[CHANGED]: Max_FN = overlay_position_FN + 10%")
            Max_FN = max(overlay_position_FN) + (max(overlay_position_FN) * .1)
          }
          
          if (min(overlay_position_FN) < Min_FN) {
            message("\n[WARNING]: overlay_position_FN (", min(overlay_position_FN) , ") is > Min_FN (", Min_FN, ")\n[EXPECTED]: overlay_position_FN should be <= Min_FN  [CHANGED]: Min_FN = 0")
            Min_FN = 0
          }
          
        }  
      }
    }
      
      # Only needed in "line" because in area we calculate the position of the individual point using prevalence_PCT 
    if (overlay == "line") {  
      if (any(overlay_prevalence_1 > min_Prevalence)) {
        ratio_x = (overlay_prevalence_1 / min_Prevalence) 
        message("\n[WARNING]: Some of the overlay_prevalence_1 (", min(overlay_prevalence_1) , ") are > min_Prevalence (", min_Prevalence, ").\n[EXPECTED]: overlay_prevalence_1 should be >= min_Prevalence\n[CHANGED]: overlay_prevalence_1 and overlay_prevalence_2 to ", paste(overlay_prevalence_1 * ratio_x, collapse = ", "), " and ", paste(overlay_prevalence_2 * ratio_x, collapse = ", "))
        overlay_prevalence_1 = overlay_prevalence_1/ratio_x
        overlay_prevalence_2 = overlay_prevalence_2/ratio_x
      }
    }


  # Check overlay prevalence ------------------------------------------------

    if (length(overlay_prevalence_1) == 1) {
      if (overlay_prevalence_1 > overlay_prevalence_2) {
        message("[WARNING]: overlay_prevalence_1 (", overlay_prevalence_1 , ") is > than overlay_prevalence_2 (", overlay_prevalence_2, "). [EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2 [CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2")
        overlay_prevalence_1 = overlay_prevalence_2/2
      }
    } else if (length(overlay_prevalence_1) > 1) {
      if (DEBUG != 0) message("> 1 overlay")
    }
    

  # SYSTEM parameters -------------------------------------------------------

      if (overlay != "no") {
        filename_overlay = paste0("_", overlay)
      } else {
        filename_overlay = ""
      }

    
  


  # Create PPV matrix -------------------------------------------------------

      PPV_melted = .createPPVmatrix(
        min_Prevalence = min_Prevalence,
        max_Prevalence = max_Prevalence,
        Sensitivity = Sensitivity,
        min_FP = min_FP,
        max_FP = max_FP,
        Max_FN = Max_FN,
        Min_FN = Min_FN,
        
        one_out_of = one_out_of
      )


  # PLOT --------------------------------------------------------------------

    # Create plot labels in Language
      translated_labels = .translate_labels(Language = Language,
                        Sensitivity = Sensitivity,
                        Specificity = Specificity,
                        # max_FP = max_FP,
                        PPV_NPV = PPV_NPV)
  

    # Number of decimals depends on the range
      decimals = .number_decimals_plot_axis(PPV_NPV = PPV_NPV,
                                            min_FP = min_FP,
                                            max_FP = max_FP,
                                            Min_FN = Min_FN,
                                            Max_FN = Max_FN,
                                            min_Prevalence = min_Prevalence,
                                            max_Prevalence = max_Prevalence)

      
    # Choose function depending on the type of overlay

     if (overlay == "line") {

       p = .plot_overlay_line(
          PPV_melted = PPV_melted,
          uncertainty_prevalence = uncertainty_prevalence,
          min_Prevalence = min_Prevalence,
          max_Prevalence = max_Prevalence,
          min_FP = min_FP,
          max_FP = max_FP,
          Max_FN = Max_FN,
          Min_FN = Min_FN,
          
          one_out_of = one_out_of, 
          
          
          Sensitivity = Sensitivity,
          Specificity = Specificity,
          
          overlay_prevalence_2 = overlay_prevalence_2,
          
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          
          overlay_labels = overlay_labels,
          
          decimals_x = decimals$decimals_x,
          decimals_y = decimals$decimals_y,
          
          label_title = label_title,
          label_subtitle = label_subtitle,
          
          label_caption = translated_labels$label_caption,
          x_axis_label = translated_labels$x_axis_label,
          y_axis_label = translated_labels$y_axis_label,
          prevalence_label = translated_labels$prevalence_label,
          legend_label = translated_labels$legend_label,
          
          PPV_NPV = PPV_NPV)


      } else if (overlay == "area") {
        
        p = .plot_overlay_area(
          PPV_melted,
          uncertainty_prevalence = uncertainty_prevalence,
          min_Prevalence = min_Prevalence,
          max_Prevalence = max_Prevalence,
          Sensitivity = Sensitivity,
          Specificity = Specificity,
          
          min_FP = min_FP,
          max_FP = max_FP,
          Max_FN = Max_FN,
          Min_FN = Min_FN,
          
          one_out_of = one_out_of, 
          
          
          overlay_labels = overlay_labels,
          overlay_prevalence_1 = overlay_prevalence_1,
          overlay_prevalence_2 = overlay_prevalence_2,
          
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          
          decimals_x = decimals$decimals_x,
          decimals_y = decimals$decimals_y,

          label_title = label_title,
          label_subtitle = label_subtitle,
          
          label_caption = translated_labels$label_caption,
          label_caption_name = translated_labels$label_caption_name,
          x_axis_label = translated_labels$x_axis_label,
          y_axis_label = translated_labels$y_axis_label,
          prevalence_label = translated_labels$prevalence_label,
          legend_label = translated_labels$legend_label,
          PPV_NPV_label = translated_labels$PPV_NPV_label,
          
          PPV_NPV = PPV_NPV
        )

      } else {
        
          p = .plot_creation(
            PPV_melted = PPV_melted,
            min_Prevalence = min_Prevalence,
            max_Prevalence = max_Prevalence,
            Sensitivity = Sensitivity,
            
            min_FP = min_FP,
            max_FP = max_FP,
            Max_FN = Max_FN,
            Min_FN = Min_FN,
            
            one_out_of = one_out_of, 
            
            decimals_x = decimals$decimals_x,
            decimals_y = decimals$decimals_y,

            label_title = label_title,
            label_subtitle = label_subtitle,

            label_caption = translated_labels$label_caption,
            x_axis_label = translated_labels$x_axis_label,
            y_axis_label = translated_labels$y_axis_label,
            prevalence_label = translated_labels$prevalence_label,
            legend_label = translated_labels$legend_label,
            
            PPV_NPV = PPV_NPV
          )

      }


    # Save plot ---------------------------------------------------------------

      if (folder != "") {

        plot_name = paste0(folder, "/", PPV_NPV, "_", min_Prevalence, "_", max_Prevalence, "_", Sensitivity, "_", max_FP, filename_overlay, "_", Language, ".png")
        ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
        message("\n Plot created in: ", plot_name, "\n")
        print(p)

      } else {

        print(p)

      }

}
