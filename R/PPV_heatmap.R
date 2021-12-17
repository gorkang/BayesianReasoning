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
#' @param DEBUG Shows debug warnings [TRUE/FALSE]
#' @param folder Where to save the plot (the filename would be automatically created using the plot parameters)
#' @param one_out_of Show y scale as 1 out of x [TRUE, FALSE] FALSE by default
#' @param steps_matrix with of PPV/NPV matrix. 100 by default
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
           
           Sensitivity = 95,
           Specificity = 95,

           limits_Sensitivity = NULL, 
           limits_Specificity = NULL, 
           
           one_out_of = FALSE, 

           overlay = "no",
           overlay_labels = "",
           overlay_position_FP = NULL,
           overlay_position_FN = NULL,
           overlay_prevalence_1 = 1,
           overlay_prevalence_2 = 100,
           uncertainty_prevalence = "high",
           label_title = "",
           label_subtitle = "",
           Language = "en",
           folder = "",
           PPV_NPV = "PPV",
           
           steps_matrix = 100,
           DEBUG = FALSE) {
    


# DEBUG -------------------------------------------------------------------

    # min_Prevalence = 1
    # max_Prevalence = 1800
    # Sensitivity = 90
    # Specificity = 85
    # PPV_NPV = "PPV"
    # label_subtitle = "PPV of Mammogram for Breast Cancer by Age"
    # overlay = "line"
    # overlay_labels = c("80 y.o.", "70 y.o.", "60 y.o.", "50 y.o.", "40 y.o.", "30 y.o.", "20  y.o.")
    # overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14)
    # overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1)
    # overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667)
    # overlay_position_FN = NULL
    # limits_Sensitivity = NULL
    # limits_Specificity = NULL 
    # one_out_of = FALSE
    # DEBUG = TRUE
    # Language = "es"
    # uncertainty_prevalence = "high"
    # label_title = ""
    # label_subtitle = ""
    # 

  
    
  # CHECK variables ---------------------------------------------------------
    
  if (PPV_NPV == "PPV") {
    
    if (is.null(Sensitivity)) stop("\n* Sensitivity is needed in PPV_NPV == 'PPV'")
    
    if (overlay == "area" | overlay == "line") {
    
      if (is.null(overlay_position_FP)) stop("\n* overlay_position_FP needs a value")
      if (!is.null(overlay_position_FN)) warning("\n* overlay_position_FN should only be used for NPV plots")
    }
      
  } else if (PPV_NPV == "NPV") {
    
    if (is.null(Specificity)) stop("\n* Specificity is needed in PPV_NPV == 'NPV'")
    
    if (overlay == "area" | overlay == "line") {
    
      if (is.null(overlay_position_FN)) stop("\n* overlay_position_FN needs a value")
      if (!is.null(overlay_position_FP)) warning("\n*  overlay_position_FP should only be used for PPV plots")
      
    }
    
  }
  
    

    
    # PROCESS VARIABLES -------------------------------------------------------
    
    
    main_variables = process_variables(
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      limits_Sensitivity = limits_Sensitivity,
      limits_Specificity = limits_Specificity,
      PPV_NPV = PPV_NPV)
    
    if (DEBUG == TRUE) {
      message("\nDEBUG: ", "min_Sensitivity: ", main_variables$min_Sensitivity, " max_FN: ", main_variables$max_FN, " | max_Sensitivity: ", main_variables$max_Sensitivity, " min_FN: ",  main_variables$min_FN)
      message("DEBUG: ", "min_Specificity: ", main_variables$min_Specificity, " max_FP: ", main_variables$max_FP, " | max_Specificity: ", main_variables$max_Specificity, " min_FP: ",  main_variables$min_FP)
    }
    
        
  # Check dimensions -----------------------------------------------------------

    # CHECKS
    if (min_Prevalence < 1) {
      message("\n[WARNING]: min_Prevalence (", min_Prevalence , ") is < 1. \n[EXPECTED]: min_Prevalence should be an integer > 0.\n[CHANGED]: min_Prevalence = 1")
      min_Prevalence = 1
    }
    
    if (min_Prevalence > max_Prevalence) {
      message("\n[WARNING]: min_Prevalence (", min_Prevalence , ") is > than max_Prevalence (", max_Prevalence, ").\n[EXPECTED]: min_Prevalence should be smaller than max_Prevalence.\n[CHANGED]: min_Prevalence = max_Prevalence/2")
      min_Prevalence = max_Prevalence/2
    }
    
    # If the dimensions of the overlay are bigger, adjust max_FP and max_Prevalence

    if (overlay == "area" | overlay == "line") {

      if (overlay == "area" & length(overlay_prevalence_1) > 1) stop("* overlay_prevalence_1 has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
      
      
      # CHECK overlay_prevalence_1/overlay_prevalence_2 fits into min_Prevalence/max_Prevalence
      if (any(min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2)) {
        
        index_issue = which(min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2)
        message("\n[WARNING]: min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2\n[EXPECTED]: min_Prevalence/max_Prevalence should be <= overlay_prevalence_1/overlay_prevalence_2")
        
        if (max_Prevalence == overlay_prevalence_2[index_issue] & min_Prevalence != overlay_prevalence_1[index_issue]) {
          message("\n[WARNING]: max_Prevalence == overlay_prevalence_2\n[CHANGED]: Changing min_Prevalence = overlay_prevalence_1")
          min_Prevalence = overlay_prevalence_1[index_issue]
        } else if (min_Prevalence == overlay_prevalence_1[index_issue] & max_Prevalence != overlay_prevalence_2[index_issue]) {
          message("\n[WARNING]: min_Prevalence == overlay_prevalence_1\n[CHANGED]: Changing max_Prevalence = overlay_prevalence_2")
          max_Prevalence = overlay_prevalence_2[index_issue]
        } else {
          message("\n[WARNING]: min_Prevalence != overlay_prevalence_1\n\t     max_Prevalence != overlay_prevalence_2\n[CHANGED]: Changing max_Prevalence = overlay_prevalence_2 & min_Prevalence = overlay_prevalence_1")
          min_Prevalence = overlay_prevalence_1[index_issue]
          max_Prevalence = overlay_prevalence_2[index_issue]
        }
        
      }
      

      if (PPV_NPV == "PPV"){

        if (overlay == "area" & length(overlay_position_FP) > 1) stop("* overlay_position_FP has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
        
        if (overlay == "area") {
          
          if (exists("overlay_position_FP")) {
            if (overlay_position_FP > main_variables$max_FP & PPV_NPV == "PPV") {
              message("\n[WARNING]: overlay_position_FP (", overlay_position_FP , ") is > than max_FP (", main_variables$max_FP, ").\n[EXPECTED]: overlay_position_FP should be smaller than max_FP\n[CHANGED]: max_FP = overlay_position_FP")
              main_variables$max_FP = overlay_position_FP
            }
          }
        }
        
        if (exists("overlay_position_FP")) {
          if (min(overlay_position_FP) < main_variables$min_FP) {
            message("\n[WARNING]: overlay_position_FP (", min(overlay_position_FP) , ") is < min_FP (", main_variables$min_FP, ").\n[EXPECTED]: overlay_position_FP should be >= min_FP.\n[CHANGED]: min_FP = 0")
            main_variables$min_FP = 0
          }
          
          if (max(overlay_position_FP) > main_variables$max_FP) {
            message("\n[WARNING]: overlay_position_FP (", max(overlay_position_FP) , ") is > min_FP (", main_variables$min_FP, ").\n[EXPECTED]: overlay_position_FP should be <= max_FP.\n[CHANGED]: max_FP = overlay_position_FP + 10%")
            main_variables$max_FP = max(overlay_position_FP) + (max(overlay_position_FP) * .1)
          }
        }
        
      } else if (PPV_NPV == "NPV") {
        
        if (overlay == "area" & length(overlay_position_FN) > 1) stop("* overlay_position_FN has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
        
        if (exists("overlay_position_FN")) {
          if (max(overlay_position_FN) > main_variables$max_FN) {
            message("\n[WARNING]: overlay_position_FN (", max(overlay_position_FN) , ") is > max_FN (", main_variables$max_FN, ")\n[EXPECTED]: overlay_position_FN should be <= max_FN.\n[CHANGED]: max_FN = overlay_position_FN + 10%")
            main_variables$max_FN = max(overlay_position_FN) + (max(overlay_position_FN) * .1)
          }
          
          if (min(overlay_position_FN) < main_variables$min_FN) {
            message("\n[WARNING]: overlay_position_FN (", min(overlay_position_FN) , ") is < min_FN (", main_variables$min_FN, ")\n[EXPECTED]: overlay_position_FN should be <= min_FN.\n[CHANGED]: min_FN = 0")
            main_variables$min_FN = 0
          }
          
        }  
      }
    }
      
    # Only needed in "line" because in area we calculate the position of the individual point using prevalence_PCT 
    if (overlay == "line") {  
      if (any(overlay_prevalence_1 > min_Prevalence)) {
        ratio_x = (overlay_prevalence_1 / min_Prevalence) 
        message("\n[WARNING]: Some of the overlay_prevalence_1 (", min(overlay_prevalence_1) , ") are > min_Prevalence (", min_Prevalence, ").\n[EXPECTED]: overlay_prevalence_1 should be >= min_Prevalence.\n[CHANGED]: overlay_prevalence_1 and overlay_prevalence_2 to ", paste(overlay_prevalence_1 * ratio_x, collapse = ", "), " and ", paste(overlay_prevalence_2 * ratio_x, collapse = ", "))
        overlay_prevalence_1 = overlay_prevalence_1/ratio_x
        overlay_prevalence_2 = overlay_prevalence_2/ratio_x
      }
    }



  # Check overlay prevalence ------------------------------------------------

    if (length(overlay_prevalence_1) == 1) {
      if (overlay_prevalence_1 > overlay_prevalence_2) {
        message("\n[WARNING]: overlay_prevalence_1 (", overlay_prevalence_1 , ") is > than overlay_prevalence_2 (", overlay_prevalence_2, ").\n[EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2.\n[CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2")
        overlay_prevalence_1 = overlay_prevalence_2/2
      }
    } else if (length(overlay_prevalence_1) > 1) {
      if (DEBUG == TRUE) message("> 1 overlay")
    }

    
    # If the overlay prevalence is very high and we have one_out_of = TRUE, sometimes the closest row in the PPV matrix is the first one, which distorts the NPV calculation
    if (one_out_of == TRUE & PPV_NPV == "NPV" & overlay == "area") {
      
      overlay_P = overlay_prevalence_1/overlay_prevalence_2
      # steps_matrix = 100
      prevalence_temp <- seq(min_Prevalence, max_Prevalence, length.out = steps_matrix + 1) # *prevalence_2* x out of [y] (min_Prevalence out of max_Prevalence)
      prevalence_P = prevalence_temp[1]/prevalence_temp[2]
      
      if ((overlay_P - prevalence_P) > (1 - overlay_P)) {
        message("\n[WARNING]: overlay_prevalence_1/overlay_prevalence_2 closer to 1 than to the first prevalence row\n[CHANGED]: Changing max_Prevalence = (overlay_prevalence_2-overlay_prevalence_1) * 3")
        max_Prevalence = (overlay_prevalence_2 - overlay_prevalence_1) * 3
      }
      
    }    

  # SYSTEM parameters -------------------------------------------------------

      if (overlay != "no") {
        filename_overlay = paste0("_", overlay)
      } else {
        filename_overlay = ""
      }



  # Create PPV matrix -------------------------------------------------------

      PPV_melted <- .createPPVmatrix(
        min_Prevalence = min_Prevalence,
        max_Prevalence = max_Prevalence,
        Sensitivity = Sensitivity,
        Specificity = Specificity,
        min_FP = main_variables$min_FP,
        max_FP = main_variables$max_FP,
        max_FN = main_variables$max_FN,
        min_FN = main_variables$min_FN,
        
        one_out_of = one_out_of
      )


  # PLOT --------------------------------------------------------------------

    # Create plot labels in Language
      translated_labels = .translate_labels(Language = Language,
                                            Sensitivity = Sensitivity,
                                            Specificity = Specificity,
                                            PPV_NPV = PPV_NPV)
  

    # Number of decimals depends on the range
      decimals = .number_decimals_plot_axis(PPV_NPV = PPV_NPV,
                                            min_FP = main_variables$min_FP,
                                            max_FP = main_variables$max_FP,
                                            min_FN = main_variables$min_FN,
                                            max_FN = main_variables$max_FN,
                                            min_Prevalence = min_Prevalence,
                                            max_Prevalence = max_Prevalence)

      
    # Choose function depending on the type of overlay

     if (overlay == "line") {

       p = .plot_overlay_line(
          PPV_melted = PPV_melted,
          uncertainty_prevalence = uncertainty_prevalence,
          min_Prevalence = min_Prevalence,
          max_Prevalence = max_Prevalence,
          min_FP = main_variables$min_FP,
          max_FP = main_variables$max_FP,
          max_FN = main_variables$max_FN,
          min_FN = main_variables$min_FN,
          
          one_out_of = one_out_of, 
          
          
          # Sensitivity = Sensitivity,
          # Specificity = Specificity,
          
          overlay_prevalence_1 = overlay_prevalence_1,
          overlay_prevalence_2 = overlay_prevalence_2,
          
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
          
          min_Prevalence = min_Prevalence,
          max_Prevalence = max_Prevalence,
          
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
          
          # DEBUG_MESSAGE = DEBUG_MESSAGE,
          
          # Overlay area specific parameters
          Language = Language,
          
          Sensitivity = Sensitivity,
          Specificity = Specificity,
          
          uncertainty_prevalence = uncertainty_prevalence,
          overlay_prevalence_1 = overlay_prevalence_1,
          overlay_prevalence_2 = overlay_prevalence_2,
          overlay_position_FP = overlay_position_FP,
          overlay_position_FN = overlay_position_FN,
          overlay_labels = overlay_labels,
          
          # Ellipsis
          DEBUG = DEBUG
          
          
          # PPV_melted,
          # uncertainty_prevalence = uncertainty_prevalence,
          # min_Prevalence = min_Prevalence,
          # max_Prevalence = max_Prevalence,
          # Sensitivity = Sensitivity,
          # Specificity = Specificity,
          # 
          # min_FP = main_variables$min_FP,
          # max_FP = main_variables$max_FP,
          # max_FN = main_variables$max_FN,
          # min_FN = main_variables$min_FN,
          # 
          # one_out_of = one_out_of, 
          # 
          # 
          # overlay_labels = overlay_labels,
          # overlay_prevalence_1 = overlay_prevalence_1,
          # overlay_prevalence_2 = overlay_prevalence_2,
          # 
          # overlay_position_FP = overlay_position_FP,
          # overlay_position_FN = overlay_position_FN,
          # 
          # decimals_x = decimals$decimals_x,
          # decimals_y = decimals$decimals_y,
          # 
          # label_title = label_title,
          # label_subtitle = label_subtitle,
          # 
          # translated_labels = translated_labels,
          # 
          # PPV_NPV = PPV_NPV,
          # Language = Language,
          # 
          # # Ellipsis
          # DEBUG = DEBUG
        )

      } else {
        
          p = .plot_creation(
            PPV_melted = PPV_melted,
            min_Prevalence = min_Prevalence,
            max_Prevalence = max_Prevalence,
            
            # Sensitivity = Sensitivity,
            # Specificity = Specificity,
            
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


    # Save plot ---------------------------------------------------------------

      if (folder != "") {

        plot_name = paste0(folder, "/", PPV_NPV, "_", min_Prevalence, "_", max_Prevalence, "_", Sensitivity, "_", main_variables$max_FP, filename_overlay, "_", Language, ".png")
        ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
        message("\n Plot created in: ", plot_name, "\n")
        print(p)

      } else {

        print(p)

      }

}
