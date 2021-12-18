#' process_variables
#' Checks and process main variables, checks for errors, creates defaults
#'
#' @param Sensitivity .
#' @param Specificity .
#' @param limits_Sensitivity .
#' @param limits_Specificity .
#' @param PPV_NPV .
process_variables <- function(min_Prevalence = NULL,
                              max_Prevalence = NULL,
                              Sensitivity = NULL, 
                             Specificity = NULL, 
                             limits_Sensitivity = NULL,
                             limits_Specificity = NULL,
                             overlay_position_FP = NULL,
                             overlay_position_FN = NULL,
                             overlay_prevalence_1 = NULL,
                             overlay_prevalence_2 = NULL,
                             PPV_NPV = "PPV",
                             one_out_of = one_out_of,
                             overlay = "") {

  
  # DEBUG
  # Sensitivity = NULL
  # Specificity = NULL
  # limits_Sensitivity = NULL
  # limits_Specificity = NULL
  # PPV_NPV = "NPV"
  # overlay = ""
  # 
  # Specificity = 90
  
  
  # CHECK variables ---------------------------------------------------------

  if (PPV_NPV == "PPV") {
    
    # Sensitivity
    if (is.null(Sensitivity)) stop("\n* Sensitivity is needed in PPV_NPV == 'PPV'")
    
    # Specificity
    if (is.null(Specificity) & is.null(limits_Specificity)) {
      warning("Specificity is NULL. Setting to 95")
      Specificity = 95
    } else if (is.null(Specificity) & !is.null(limits_Specificity)) {
      warning("Specificity is NULL. Setting mean(limits_Specificity)")
      Specificity = mean(limits_Specificity)
    }
    
    # limits_Specificity
    if (is.null(limits_Specificity)) {
      warning("limits_Specificity is NULL. Setting to c(0,0)")
      limits_Specificity = c(0, 0)
    }
    
    # CHECK limits and correct
    # By default we show a range of Specificities of 10% (+-5%)
    if (Specificity + 5 > 100) limits_Specificity[2] = 100
    if (Specificity - 5 < 0) limits_Specificity[1] = 0
    if (Specificity + 5 <= 100) limits_Specificity[2] = c(Specificity + 5)
    if (Specificity - 5 >= 0) limits_Specificity[1] = c(Specificity - 5)
    
    
    if (overlay == "area" | overlay == "line") {
      if (is.null(overlay_position_FP)) stop("\n* overlay_position_FP needs a value")
      if (!is.null(overlay_position_FN)) warning("\n* overlay_position_FN should only be used for NPV plots")
    }
    
    if (length(limits_Specificity) != 2) stop("* limits_Specificity sould be a vector of length 2, now is (", limits_Specificity, "). e.g.: limits_Specificity = c(90, 95)")
    if (any(limits_Specificity > 100 | limits_Specificity < 0)) stop("* limits_Specificity sould be values 0-100")
    
    
  } else if (PPV_NPV == "NPV") {
    
    # Specificity
    if (is.null(Specificity)) stop("\n* Specificity is needed in PPV_NPV == 'NPV'")
    
    # Sensitivity
    if (is.null(Sensitivity) & is.null(limits_Sensitivity)) {
      warning("Sensitivity is NULL. Setting to 95")
      Sensitivity = 95
    } else if (is.null(Sensitivity) & !is.null(limits_Sensitivity)) {
      warning("Sensitivity is NULL. Setting mean(limits_Sensitivity)")
      Sensitivity = mean(limits_Sensitivity)
    }
    
    # limits_Sensitivity
    if (is.null(limits_Sensitivity)) {
      warning("limits_Sensitivity is NULL. Setting to c(0,0)")
      limits_Sensitivity = c(0, 0)
    }
    
    # CHECK limits and correct
    # By default we show a range of Sensitivities of 10% (+-5%)
    if (Sensitivity + 5 > 100) limits_Sensitivity[2] = 100
    if (Sensitivity - 5 < 0) limits_Sensitivity[1] = 0
    if (Sensitivity + 5 <= 100) limits_Sensitivity[2] = c(Sensitivity + 5)
    if (Sensitivity - 5 >= 0) limits_Sensitivity[1] = c(Sensitivity - 5)
    
    if (overlay == "area" | overlay == "line") {
      if (is.null(overlay_position_FN)) stop("\n* overlay_position_FN needs a value")
      if (!is.null(overlay_position_FP)) warning("\n*  overlay_position_FP should only be used for PPV plots")
    }
    
    if (length(limits_Sensitivity) != 2) stop("* limits_Sensitivity sould be a vector of length 2, now is (", limits_Sensitivity, "). e.g.: limits_Sensitivity = c(90, 95)")
    if (any(limits_Sensitivity > 100 | limits_Sensitivity < 0)) stop("* limits_Sensitivity sould be values 0-100")
    
  }


  
  # General CHECKS
  if (Sensitivity > 100 | Sensitivity < 0) stop("* Sensitivity should be a value 0-100")
  if (Specificity > 100 | Specificity < 0) stop("* Specificity should be a value 0-100")
  
  
  

  # Translate ---------------------------------------------------------------
  

  # Translate limits
  max_Sensitivity = limits_Sensitivity[2]
  min_Sensitivity = limits_Sensitivity[1]
  
  max_Specificity = limits_Specificity[2]
  min_Specificity = limits_Specificity[1]
  
  # Translate to FN and FP
  max_FN = (100 - min_Sensitivity)
  min_FN = (100 - max_Sensitivity)
  
  max_FP = (100 - min_Specificity)
  min_FP = (100 - max_Specificity)
  
  
  
  
  
  
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
          if (overlay_position_FP > max_FP & PPV_NPV == "PPV") {
            message("\n[WARNING]: overlay_position_FP (", overlay_position_FP , ") is > than max_FP (", max_FP, ").\n[EXPECTED]: overlay_position_FP should be smaller than max_FP\n[CHANGED]: max_FP = overlay_position_FP")
            max_FP = overlay_position_FP
          }
        }
      }
      
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
      
      if (overlay == "area" & length(overlay_position_FN) > 1) stop("* overlay_position_FN has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'? ")
      
      if (exists("overlay_position_FN")) {
        if (max(overlay_position_FN) > max_FN) {
          message("\n[WARNING]: overlay_position_FN (", max(overlay_position_FN) , ") is > max_FN (", max_FN, ")\n[EXPECTED]: overlay_position_FN should be <= max_FN.\n[CHANGED]: max_FN = overlay_position_FN + 10%")
          max_FN = max(overlay_position_FN) + (max(overlay_position_FN) * .1)
        }
        
        if (min(overlay_position_FN) < min_FN) {
          message("\n[WARNING]: overlay_position_FN (", min(overlay_position_FN) , ") is < min_FN (", min_FN, ")\n[EXPECTED]: overlay_position_FN should be <= min_FN.\n[CHANGED]: min_FN = 0")
          min_FN = 0
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
    # if (DEBUG == TRUE) message("> 1 overlay")
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
  
  
  # Output -------------------------------------------
  main_variables = 
    list(
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      
      max_Sensitivity = max_Sensitivity,
      min_Sensitivity = min_Sensitivity,
      max_Specificity = max_Specificity,
      min_Specificity = min_Specificity,
      
      # Redundant
      max_FN = max_FN,
      min_FN = min_FN,
      max_FP = max_FP,
      min_FP = min_FP
    )
  
  return(main_variables)

}



#' .createPPVmatrix
#'
#' Create a PPV matrix helper function
#' 
#' @param min_Prevalence [x] out of y prevalence of disease
#' @param max_Prevalence x out of [y] prevalence of disease
#' @param Sensitivity Sensitivity of test
#' @param Specificity Specificity of test
#' @param max_FP Maximum False Positive ratio
#' @param min_FP Minimum False Positive ratio
#' @param steps_matrix How big the matrix should be (probably better to leave as it is: 100)
#' @param one_out_of .
#' @param max_FN .
#' @param min_FN .
#'
#' @return A DF called PPV_melted
#' @importFrom reshape2 melt
#'
.createPPVmatrix <-
  function(min_Prevalence = 1,
           max_Prevalence = 1000,
           Sensitivity = 100,
           Specificity = 99,

           one_out_of = TRUE,
           PPV_NPV = "PPV",
           
           min_FP = 0,
           max_FP = 10,
           max_FN = 0,
           min_FN = 10,
           
           steps_matrix = 100) {
    
    
    
    # DEBUG *********************************************
    # ***************************************************

    # Sensitivity = 81
    # Specificity = 99
    # min_FP = 0
    # max_FP = 10
    # max_FN = 24
    # min_FN = 0

    # min_Prevalence = 1
    # max_Prevalence = 100    
    # steps_matrix = 100
    # one_out_of = TRUE
    
    # PPV_NPV = "PPV"
    
    # ***************************************************
    # ***************************************************
    

    # Parameters ---

    # Sensitivity range (False Negatives)
    if (PPV_NPV == "NPV") {
      range_FN = (max_FN - min_FN)
      step_size_FN <- range_FN/steps_matrix
      FN_array = seq(min_FN, max_FN, step_size_FN)
      if(length(FN_array) == 1) FN_array = rep(FN_array, steps_matrix + 1) # CATCH FN = 0
    }
    
    # Specificity range (False Positives)
    if (PPV_NPV == "PPV") {
      range_FP = (max_FP - min_FP)
      step_size_FP <- range_FP/steps_matrix
      FP_array = seq(min_FP, max_FP, step_size_FP)
      if(length(FP_array) == 1) FP_array = rep(FP_array, steps_matrix + 1) # CATCH FP = 0
    }

    # Prevalence
      if (one_out_of == FALSE) {
        prevalence_2 <- exp(seq(log(min_Prevalence), log(max_Prevalence), length.out = steps_matrix + 1))# *prevalence_2* x out of [y] (min_Prevalence out of max_Prevalence)
        # prevalence_2 <- pracma::logseq(min_Prevalence, max_Prevalence,  steps_matrix + 1) # GENERATES IDENTICAL SEQUENCE...
      } else {
        prevalence_2 <- seq(min_Prevalence, max_Prevalence, length.out = steps_matrix + 1) # *prevalence_2* x out of [y] (min_Prevalence out of max_Prevalence)
      }
      sick_array = rep(min_Prevalence, steps_matrix + 1)
      healthy_array = prevalence_2 - min_Prevalence
    
      
    # PPV 
      if (PPV_NPV == "PPV") {
        
        sensitivity_array = rep(Sensitivity/100, steps_matrix + 1) # Sensitivity is fixed when calculating PPV
        specificity_array = (100 - FP_array) / 100
        
        TRUE_positives = (sick_array %o% sensitivity_array)
        FALSE_positives = (healthy_array %o% (1 - specificity_array))
        
        # PPV Calculation ---
        PPV <- round(TRUE_positives / (TRUE_positives + FALSE_positives), 2)
        
        #Label columns and rows of matrix
        colnames(PPV) = FP_array
        rownames(PPV) = prevalence_2
        
        # Long format para ggplot Heatmap
        PPV_melted = reshape2::melt(PPV)
        
        names(PPV_melted) = c("prevalence_2", "FP", "PPV") 
        
        
      }
    
    # NPV
      if (PPV_NPV == "NPV") {
        
        sensitivity_array = (100 - FN_array) / 100
        specificity_array = rep(Specificity / 100, steps_matrix + 1) # Specificity is fixed when calculating PPV
        
        # The order of this %o% multiplications is critical (in NPV they have to be reversed)
        TRUE_negatives = (healthy_array %o% specificity_array)
        FALSE_negatives = (sick_array %o% (1 - sensitivity_array))
        
        # NPV Calculation ---
        NPV <- round(TRUE_negatives / (TRUE_negatives + FALSE_negatives), 2)
        
        #Label columns and rows of matrix
        colnames(NPV) = FN_array
        rownames(NPV) = prevalence_2
        
        # Long format para ggplot Heatmap
        PPV_melted = reshape2::melt(NPV) #%>% dplyr::select(-"Var1") # Var1 is prevalence_2, which we have from PPV
        
        names(PPV_melted) = c("prevalence_2", "FN", "NPV") 
        
      }
    
    
    # PPV & NPV Calculation ---
    # We calculate a 100x100 PPV matrix using %o% (outer)
      # With the old system, min_Prevalence == sick people, and prevalence_2 - min_Prevalence is healthy people (min_Prevalence is set to 1 by default)
      # PPV <- round((Sensitivity * min_Prevalence) / ((Sensitivity * min_Prevalence) + ((prevalence_2 - min_Prevalence) %o% FP_array)), 2)
      # NPV <- round(((prevalence_2 - min_Prevalence) * (100 - max_FP)) / (((prevalence_2 - min_Prevalence) * (100 - max_FP)) + (prevalence_2  %o% FN_array)), 2)
      # (Healthy * Specificity) / (Healthy * Specificity) + (Sick * FN)
    
    
    
    # PPV_melted = PPV_melted_PPV %>% cbind(PPV_melted_NPV)
    
    # Give names to variables
    # names(PPV_melted) = c("prevalence_2", "FP", "PPV", "prevalence_2_rep", "FN", "NPV")
    # names(PPV_melted) = c("prevalence_2", "FP", "PPV", "FN", "NPV") 
    
    PPV_melted = PPV_melted %>% 
      dplyr::mutate(prevalence_1 = min_Prevalence) %>% 
      # dplyr::select(-prevalence_2_rep) %>% 
      dplyr::select(prevalence_1, dplyr::everything()) %>% 
      dplyr::mutate(prevalence_pct = prevalence_1/prevalence_2) %>% 
      dplyr::as_tibble()
    
    return(PPV_melted)
    
  }




#' .get_point_ppv_npv
#' 
#' Get PPV or NPV for the overlay
#'
#' @param PPV_melted DF out of .createPPVmatrix()
#' @param PPV_NPV Should calculate PPV or NPV?
#' @param Language ["en" / "es"]
#' @param Sensitivity Sensitivity of the test
#' @param Specificity Specificity of the test
#' @param overlay_prevalence_1 [x] out of y prevalence of disease
#' @param overlay_prevalence_2 x out of [y] prevalence of disease
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param overlay_labels .
#' @param point_Prevalence .
#' @param decimals_x .
#' @param translated_labels .
#' @param decimals_y .

.get_point_ppv_npv <- function(
  PPV_melted, 
  PPV_NPV = "PPV",
  Language,
  Sensitivity,
  Specificity,

  overlay_prevalence_1,
  overlay_prevalence_2,
  overlay_labels,
  
  overlay_position_FP,
  overlay_position_FN,
  
  point_Prevalence, 
  
  translated_labels,
  
  decimals_x,
  decimals_y) {


  # message("\n", PPV_NPV,": Sensitivity: ", Sensitivity, " Specificity: ", Specificity, " overlay_position_FP: ", overlay_position_FP, " overlay_position_FN: ", overlay_position_FN, "\n")

  
  # Common vars
  TRUE_positives = (overlay_prevalence_1 * Sensitivity) /100
  healthy_n = (overlay_prevalence_2 - overlay_prevalence_1)
  PCT_prevalence_overlay = overlay_prevalence_1/overlay_prevalence_2
  
  
  # X The variable that defines axis position depends on PPV_NPV
  if (PPV_NPV == "PPV") {
    Specificity = (100 - overlay_position_FP)
  } else if (PPV_NPV == "NPV") {
    Sensitivity = (100 - overlay_position_FN)
  }
  

  # Process labels for area overlay    
  sick_positive = (overlay_prevalence_1 * Sensitivity) / 100
  healthy_positive = (healthy_n * (100 - Specificity)) / 100
  
  
  # Make sure we use the proper plural when needed (e.g. in Spanish)
  if (Language == "sp" | Language == "es") {
    translated_labels$label_sick = ifelse(sick_positive > 1, paste0(translated_labels$label_sick, "s"), translated_labels$label_sick)
    translated_labels$label_healthy = ifelse(healthy_n > 1, paste0(translated_labels$label_healthy, "s"), translated_labels$label_healthy)
  } else {
    translated_labels$label_sick = translated_labels$label_sick
    translated_labels$label_healthy = translated_labels$label_healthy
  }
  
  
  # Get PPV or NPV value ---
  decimals_overlay = 2
  
  if (PPV_NPV == "NPV")  {

    DF_point_PPV_NPV = PPV_melted %>%
      dplyr::filter(
        # Closest value to PCT_prevalence_overlay & overlay_position_FP_FN
        abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay)) &
          abs(FN - overlay_position_FN) == min(abs(FN - overlay_position_FN)))
    
    DF_point_PPV_NPV = DF_point_PPV_NPV[1,]
    
    # Manually calculate NPV
    calculated_NPV = 
      round(
        ((Specificity) * (healthy_n)) / 
          (((Specificity) * (healthy_n)) + (overlay_prevalence_1 * overlay_position_FN))
        , 2) 
    
    DEBUG_MESSAGE = paste0("Specificity: ", Specificity, " | overlay_position_FN: ", overlay_position_FN, " | overlay_prevalence_1: ", overlay_prevalence_1, " | overlay_prevalence_2: ", overlay_prevalence_2, "\n",
      "calculated_NPV: ", calculated_NPV * 100, "%", " | NPV in PPV_melted: ", DF_point_PPV_NPV$NPV * 100, "%", " | DIFF: ", round(calculated_NPV - DF_point_PPV_NPV$NPV, 2) * 100, "%")
    
    # Overlay message
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", translated_labels$label_y_axis, ": ", overlay_prevalence_1, " ", translated_labels$label_prevalence, " ", overlay_prevalence_2,
      "\n", translated_labels$label_caption_name, ": ", Specificity, "%",
      "\n", translated_labels$label_x_axis, ": ", overlay_position_FN, "%",
      "\n ---------------------------------------------",
      "\n", overlay_prevalence_1, " ", translated_labels$label_sick, ": ", round(TRUE_positives, decimals_overlay), " (+) ", round(overlay_prevalence_1 - TRUE_positives, decimals_overlay), " (-)",
      "\n", (healthy_n), " ", translated_labels$label_healthy, ": ", round((healthy_n) - ((healthy_n) * (100 - Specificity))/100, decimals_overlay), " (-) ", round(((healthy_n) * (100 - Specificity))/100, decimals_overlay), " (+) "
      
      )

    point_PPV_NPV = calculated_NPV * 100

  } else if (PPV_NPV == "PPV"){

    DF_point_PPV_NPV = PPV_melted %>%
      dplyr::filter(
        # Closest value to PCT_prevalence_overlay & overlay_position_FP_FN
        abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay)) &
          abs(FP - overlay_position_FP) == min(abs(FP - overlay_position_FP)))
    
    DF_point_PPV_NPV = DF_point_PPV_NPV[1,]
    
    # Manually calculate PPV
    calculated_PPV = 
      round(
        (Sensitivity * overlay_prevalence_1) / ((Sensitivity * overlay_prevalence_1) + (healthy_n) * overlay_position_FP),
        2) 
    
    DEBUG_MESSAGE = paste0("Sensitivity: ", Sensitivity, " | overlay_position_FP: ", overlay_position_FP, " | overlay_prevalence_1: ", overlay_prevalence_1, " | overlay_prevalence_2: ", overlay_prevalence_2, "\n",
                           "calculated_PPV: ", calculated_PPV * 100, "%", " | PPV in PPV_melted: ", DF_point_PPV_NPV$PPV * 100, "%", " | DIFF: ", round(calculated_PPV - DF_point_PPV_NPV$PPV, 2) * 100, "%")

    # Should use "~"?
    # decimals_sick_positive = sick_positive %% 1
    # label_sick_aprox = ifelse(decimals_sick_positive >= .99 | decimals_sick_positive <= 0.01, "", "~")
    # decimals_healthy_positive = healthy_positive %% 1
    # label_healthy_aprox = ifelse(decimals_healthy_positive >= .99 | decimals_healthy_positive <= 0.01, "", "~")
    
    # overlay
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", translated_labels$label_y_axis, ": ", overlay_prevalence_1, " ", translated_labels$label_prevalence, " ", overlay_prevalence_2,
      "\n", translated_labels$label_caption_name, ": ", Sensitivity, "%", 
      "\n", translated_labels$label_x_axis, ": ", paste0(round((100 - Specificity), decimals_overlay), "% "),
      "\n ---------------------------------------------",
      "\n", overlay_prevalence_1, " ", translated_labels$label_sick, ": ", round(TRUE_positives, decimals_overlay), " (+) ", round(overlay_prevalence_1 - TRUE_positives, decimals_overlay), " (-)",
      "\n", healthy_n, " ", translated_labels$label_healthy, ": ", round((healthy_n) - ((healthy_n) * (100 - Specificity))/100, decimals_overlay), " (-) ", round(((healthy_n) * (100 - Specificity))/100, decimals_overlay), " (+) "
      )
    

    # point_PPV_NPV = DF_point_PPV_NPV %>% dplyr::mutate(PPV = round(PPV * 100, 2))  %>% dplyr::pull(PPV)
    point_PPV_NPV = calculated_PPV * 100
  }

  # Function outputs
  list(
    Details_point_PPV_NPV = Details_point_PPV_NPV,
    point_PPV_NPV = point_PPV_NPV,
    # size_overlay_text = nchar(paste0(overlay_prevalence_1, " ", translated_labels$label_prevalence, " ", overlay_prevalence_2)),
    DEBUG_MESSAGE = DEBUG_MESSAGE
  )
}



#' .number_decimals_plot_axis
#' 
#' The number of decimal places in the x and y axis label depends on how wide the range is.
#'
#' @param PPV_NPV .
#' @param min_FP .
#' @param max_FP .
#' @param min_FN .
#' @param max_FN .
#' @param min_Prevalence [x] out of y prevalence of disease
#' @param max_Prevalence x out of [y] prevalence of disease

.number_decimals_plot_axis <- function(PPV_NPV = "PPV", min_FP = 0, max_FP, min_FN, max_FN, min_Prevalence, max_Prevalence) {
  
  # The vars to calculate range depend on PPV NPV
  if (PPV_NPV == "PPV") {
    max_FP_FN = max_FP
    min_FP_FN = min_FP
  } else if (PPV_NPV == "NPV") {
    max_FP_FN = max_FN
    min_FP_FN = min_FN
  }
  
  # Number of decimals x AXIS
  if (max_FP_FN - min_FP_FN < 1) {
    decimals_x = 2
  } else if (max_FP_FN - min_FP_FN <= 5) {
    decimals_x = 1
  } else if (max_FP_FN - min_FP_FN > 5) {
    decimals_x = 0
  } else {
    decimals_x = 0
  }
  
  
  # Number of decimals y AXIS
  if (max_Prevalence - min_Prevalence < 2) {
    decimals_y = 2
  } else if (max_Prevalence - min_Prevalence <= 64) {
    decimals_y = 1
  } else if (max_Prevalence - min_Prevalence > 64) {
    decimals_y = 0
  } else {
    decimals_y = 0
  }
  
  
  # Output vars ---
  
  list("decimals_x" = decimals_x, 
       "decimals_y" = decimals_y)
}



#' .plot_creation
#' 
#' Function to create the main heatmap plot
#' 
#' @param PPV_melted .
#' @param PPV_NPV .
#' @param min_Prevalence .
#' @param min_FP .
#' @param max_FP .
#' @param steps_matrix .
#' @param decimals_x .
#' @param decimals_y .
#' @param label_title .
#' @param label_subtitle .
#' @param translated_labels .
#' @param max_Prevalence .
#' @param max_FN .
#' @param min_FN .
#' @param one_out_of .
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin element_text
#'
.plot_creation <-
  function(PPV_NPV = "PPV",
           one_out_of = TRUE,
           
           min_Prevalence,
           max_Prevalence,
           min_FP = 0,
           max_FP,
           max_FN,
           min_FN,
           
           PPV_melted,
           steps_matrix = 100, 
           decimals_x,
           decimals_y,
           
           label_title = "",
           label_subtitle = "",
           translated_labels = translated_labels
           
           # DEBUG_MESSAGE = ""
           ) {
    

    # DEBUG
    # min_FP <<- min_FP
    # max_FP <<- max_FP
    # decimals_x <<- decimals_x
    # max_Prevalence <<- max_Prevalence
    # min_Prevalence <<- min_Prevalence
    
    
    # Global variables ---
    
    # Colors PPV
    # https://www.google.com/search?q=color+picker
    # Palettes: 0%, 25%, 2550%, 75%, 100%
    if (PPV_NPV == "PPV") {
      
      Paleta_legend <- c("white", "grey", "black", "yellowgreen", "chartreuse4") #Original
      
    } else if (PPV_NPV == "NPV") {
      
      # Paleta_legend = c("white", "grey", "black","#bd7afa", "#420080") # Violet
      Paleta_legend = c("white", "grey", "black","#f7d479", "#ffb300") # Orange
      
    }
    
    # Breaks and labels for PPV/NPV legend
    breaks_legend <- c(0, 0.25, 0.5, 0.75, 1)
    labels_legend <- c(0, 25, 50, 75, 100)
    
    # False Positives (x axis) 
    Steps_FP <- steps_matrix
    range_FP <- (max_FP - min_FP)
    step_size_FP <- range_FP/Steps_FP
    
    # Sensitivity (For NPV plot)
    Steps_FN <- steps_matrix
    # min_FN <- 0
    # max_FN <- (100 - Sensitivity)
    range_FN <- (max_FN - min_FN)
    step_size_FN <- range_FN/Steps_FN

    
    # PPV ---------------------------------------------------------------------
    
    if (PPV_NPV == "PPV") {
      
      # Create plot
      if (one_out_of == TRUE) {
        p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (prevalence_2)))  
      } else {
        p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (prevalence_pct)))    
      }
      
      
      # BREAKS X # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x)
      
        # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
        if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x + 1)

      # PPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = PPV), colour = "white")
      
      
    # NPV ---------------------------------------------------------------------
      
    } else if (PPV_NPV == "NPV") {
      
      # Create plot
      if (one_out_of == TRUE) {
        p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (prevalence_2)))  
      } else {
        p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (prevalence_pct)))    
      }
      
      # BREAKS X # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(min_FN, max_FN, step_size_FN * 10), decimals_x)
      
        # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
        if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = min_FN, to = max_FN, by = step_size_FN * 10), decimals_x + 1)

      # NPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = NPV), colour = "white")
      
    }

    labels_x = paste0(breaks_x, "%")
    
    p <- p + 
      ggplot2::scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) +
      ggplot2::scale_fill_gradientn(colours = Paleta_legend, na.value = "transparent", breaks = breaks_legend, labels = labels_legend, limits = c(0,1), name = translated_labels$label_legend) +
      ggplot2::theme(text = ggplot2::element_text(size = 16),
                     panel.background = ggplot2::element_rect(fill = "transparent"),
                     plot.caption = ggplot2::element_text(size = 16, color = "darkgrey"),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,10,0,0)), 
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10,0,0,0)),
                     legend.position = c(0.94, 0.85), # horizontal, vertical
                     legend.direction = "vertical",
                     # legend.direction = 'horizontal',
                     legend.margin =  margin(5, 5, 10, 5)
                     ) +
      ggplot2::labs(title = label_title,
                    subtitle = label_subtitle, 
                    caption = translated_labels$label_caption,
                    x = paste(translated_labels$label_x_axis, translated_labels$label_x_axis_extra), 
                    y = translated_labels$label_y_axis)
    
    

  # Y axis breaks ---

    min_prevalence_pct = min(PPV_melted$prevalence_pct)
    max_prevalence_pct = max(PPV_melted$prevalence_pct)

    if (max_Prevalence - min_Prevalence < 20) {
      num_breaks = 15
    } else {
      num_breaks = 10  
    }
    
    
    # BREAKS Y
    if (one_out_of == TRUE) {
      
      # breaks_y = unique(PPV_melted$prevalence_pct)[c(seq(1, steps_matrix, 10), 101)]
      breaks_y <- seq(min_Prevalence, max_Prevalence, length.out = num_breaks)

      # 1 out of 1
      labels_y = paste(min_Prevalence, translated_labels$label_prevalence, round(breaks_y, decimals_y))
      
      # 1 out of MAX
      # labels_y <- rev(paste(round(breaks_y, decimals_y), "out of", max_Prevalence))
      
      
    } else {
      
      # breaks_y <- pracma::logseq(min_prevalence_pct, max_prevalence_pct,  num_breaks) # IDENTICAL SEQUENCE
      breaks_y <- exp(seq(log(min_prevalence_pct), log(max_prevalence_pct), length.out = num_breaks))
      labels_y <- paste(round(breaks_y * max_Prevalence, decimals_y), translated_labels$label_prevalence, max_Prevalence) # breaks_y * max_Prevalence
      
      # OLD way, unequal space between labels
      # breaks_y <- round(exp(seq(log(min_Prevalence), log(max_Prevalence), length.out = 10)), decimals_y)
      # labels_y <- rev(paste(breaks_y, translated_labels$label_prevalence, max_Prevalence))
    }
    
    
    # Change scale depending on one_out_of
    if (one_out_of == TRUE) {
      p = p + ggplot2::scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0))
    } else {
      # p = p + ggplot2::scale_y_log10(breaks = breaks_y, labels = labels_y, expand = c(0,0))
      # trans_reverser() to reverse a log scale
      p = p +  ggplot2::scale_y_continuous(trans = ggforce::trans_reverser('log10'), breaks = breaks_y, labels = labels_y, expand = c(0,0)) 
      

    }
    
    # Output vars ---
    return(p)
    
  }



#' .plot_overlay_area
#'
#' Add area overlay to PPV_heatmap plot 
#'
#' @param PPV_melted .
#' @param uncertainty_prevalence .
#' @param min_Prevalence [x] out of y prevalence of disease
#' @param max_Prevalence x out of [y] prevalence of disease
#' @param Sensitivity .
#' @param min_FP .
#' @param max_FP .
#' @param overlay_labels .
#' @param PPV_NPV .
#' @param Language .
#' @param overlay_prevalence_1 [x] out of y prevalence of disease for the overlay
#' @param overlay_prevalence_2 x out of [y] prevalence of disease for the overlay
#' @param decimals_x .
#' @param decimals_y .
#' @param label_title .
#' @param label_subtitle .
#' @param translated_labels .
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param Specificity .
#' @param max_FN .
#' @param min_FN .
#' @param one_out_of .
#' @param steps_matrix .
#' @param ... .
#' 
#' @importFrom ggplot2 annotate

.plot_overlay_area <-
  function(PPV_NPV = "PPV",
           one_out_of = TRUE,
           
           min_Prevalence,
           max_Prevalence,
           min_FP = 0,
           max_FP,
           max_FN,
           min_FN,
           
           PPV_melted,
           steps_matrix = 100, 
           decimals_x,
           decimals_y,
           
           label_title = "",
           label_subtitle = "",
           translated_labels = translated_labels,
           
           # DEBUG_MESSAGE = "",
           
           # Overlay area specific parameters
           Language = "en",
           
           Sensitivity,
           Specificity,
           
           uncertainty_prevalence = "low",
           overlay_prevalence_1,
           overlay_prevalence_2,
           overlay_position_FP,
           overlay_position_FN,
           overlay_labels = "",
           ...
           ) {

    # Get ... vars    
    dots <- list(...)

    
    # Calculate point prevalence ---

    # # Use overlay prevalence as a pct
    PCT_prevalence_overlay = overlay_prevalence_1/overlay_prevalence_2
    # 
    # # Looks for closer value of prevalence_2 (prevalence_2) using the prevalence_pct
      # Sets the y axis position of overlay
    point_Prevalence_DF = PPV_melted %>%
      dplyr::filter(abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay))) %>%
      dplyr::sample_n(1)
    
    if (one_out_of == TRUE) {
      point_Prevalence <- point_Prevalence_DF %>% dplyr::pull(prevalence_2) 
    } else {
      point_Prevalence <- point_Prevalence_DF %>% dplyr::pull(prevalence_pct) 
    }
    
    
    # Get PPV or NPV value ---

    list_point_PPV = .get_point_ppv_npv(
      PPV_melted = PPV_melted,
      
      PPV_NPV = PPV_NPV,
      Language = Language,
      
      Sensitivity = Sensitivity,
      Specificity = Specificity,

      overlay_prevalence_1 = overlay_prevalence_1,
      overlay_prevalence_2 = overlay_prevalence_2,
      overlay_labels = overlay_labels,
      
      overlay_position_FP = overlay_position_FP,
      overlay_position_FN = overlay_position_FN,
      
      point_Prevalence = point_Prevalence,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      
      translated_labels = translated_labels
    ) 
  

    # dots list processed through ...
    if (dots$DEBUG == TRUE) message("\nDEBUG ", PPV_NPV, ": ", list_point_PPV$DEBUG_MESSAGE)
      
    
    # Add overlay ---
    
    # Size of geom_mark_rect()
    if (uncertainty_prevalence == "high") {
      uncertainty_prevalence_num = .05
    } else {
      uncertainty_prevalence_num = .02
    }  
    
      
    # X The variable that defines axis position depends on PPV_NPV
    if (PPV_NPV == "PPV") {
      x_axis_position = overlay_position_FP
    } else if (PPV_NPV == "NPV") {
      x_axis_position = overlay_position_FN
    }
        
    p = .plot_creation(
      PPV_melted = PPV_melted,
      min_Prevalence = min_Prevalence,
      max_Prevalence = max_Prevalence,
      min_FP = min_FP,
      max_FP = max_FP,
      max_FN = max_FN,
      min_FN = min_FN,
      one_out_of = one_out_of,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      
      translated_labels = translated_labels,
      
      PPV_NPV = PPV_NPV
      
      # DEBUG_MESSAGE = list_point_PPV$DEBUG_MESSAGE
      )
    
    
    p = p +
      
      # Overlay center (red dot)
      ggplot2::annotate("point", color = "red", alpha = 1, size = 1.5,
                        x = x_axis_position,
                        y = point_Prevalence) +
      
      # Text + rectangle
      ggforce::geom_mark_rect(
        
        # Uncertainty square
        aes(label = paste0(translated_labels$label_PPV_NPV, ": ", list_point_PPV$point_PPV_NPV, "%"), # BOLD title white rectangle
            x = x_axis_position,
            y = point_Prevalence),
        alpha = .04,
        expand = uncertainty_prevalence_num,
        fill = "red", 
        color = "black",
        
        # Description white rectangle
        label.colour = "black",
        description = paste0(list_point_PPV$Details_point_PPV_NPV),
        label.width = 85, # Adjust to fit label
        label.minwidth = 35, 
        
        # Connector (line)
        con.size = .2
        )
    
    
    # Output vars ---
    
    return(p)
    
  }



#' .plot_overlay_line
#' 
#' Add line overlay to PPV_heatmap plot
#'
#' @param PPV_melted DF 
#' @param min_Prevalence [x] out of y prevalence of disease
#' @param max_Prevalence x out of [y] prevalence of disease
#' @param overlay_prevalence_1 vector with [x] out of y prevalence of disease
#' @param overlay_prevalence_2 vector with x out of [y] prevalence of disease
#' @param overlay_labels vector with labels for each overlay point
#' @param max_FP .
#' @param decimals_x .
#' @param decimals_y .
#' @param label_title .
#' @param label_subtitle .
#' @param translated_labels .
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param PPV_NPV .
#' @param uncertainty_prevalence How big the uncertainty area should be: ["low" or "high"]
#' @param min_FP .
#' @param max_FN .
#' @param min_FN .
#' @param one_out_of .
#' @param steps_matrix .
#'
#' @importFrom ggplot2 annotate

.plot_overlay_line <-
  function(PPV_NPV = "PPV",
           one_out_of = TRUE,
           
           min_Prevalence,
           max_Prevalence,
           min_FP = 0,
           max_FP,
           max_FN,
           min_FN,
           
           PPV_melted,
           steps_matrix = 100, 
           decimals_x,
           decimals_y,
           
           label_title = "",
           label_subtitle = "",
           translated_labels = translated_labels,
           
           # DEBUG_MESSAGE = "",
           
           # Overlay line specific parameters
           uncertainty_prevalence = "low",
           overlay_prevalence_1,
           overlay_prevalence_2,
           overlay_position_FP,
           overlay_position_FN,
           overlay_labels = ""
           ) {
    
    
    
    
    # DEBUG
    # min_FP = main_variables$min_FP
    # max_FP = main_variables$max_FP
    # max_FN = main_variables$max_FN
    # min_FN = main_variables$min_FN
    # decimals_x = 1
    # decimals_y = 1
    
  # Size of geom_mark_rect()
  if (uncertainty_prevalence == "high") {
    uncertainty_prevalence_num = .02
  } else if (uncertainty_prevalence == "low") {
    uncertainty_prevalence_num = .01
  }  
    
  
  # Create plot after adjusting overlay dimensions
  p = .plot_creation(
    PPV_melted = PPV_melted,
    min_Prevalence = min_Prevalence,
    max_Prevalence = max_Prevalence,
    min_FP = min_FP,
    max_FP = max_FP,
    max_FN = max_FN,
    min_FN = min_FN,
    one_out_of = one_out_of,
    decimals_x = decimals_x,
    decimals_y = decimals_y,
    translated_labels = translated_labels,
    label_subtitle = label_subtitle,
    label_title = label_title,
    PPV_NPV = PPV_NPV)  
  
  
  # X The variable that defines axis position depends on PPV_NPV
  if (PPV_NPV == "PPV") {
    x_axis_position = overlay_position_FP
    overlay_position_FN = NA
  } else {
    x_axis_position = overlay_position_FN
    overlay_position_FP = NA
  }
  
  # Use the equivalent of prevalence_pct when one_out_of == FALSE
  if (one_out_of == FALSE) {
    overlay_prevalence_2 = overlay_prevalence_1/overlay_prevalence_2
  }
  
  overlay_position_x_end = c(x_axis_position[1], x_axis_position[-length(x_axis_position)])
  overlay_position_y_end = c(overlay_prevalence_2[1], overlay_prevalence_2[-length(overlay_prevalence_2)])
  
  
  
  # Plot Overlay ---
  
  # DF for ggforce::geom_mark_rect()
  DF_X = data.frame(x_axis_position = x_axis_position,
             overlay_prevalence_2 = overlay_prevalence_2,
             overlay_labels = overlay_labels)
  
   p = p + ggplot2::annotate("segment", 
                            x = x_axis_position, 
                            xend = overlay_position_x_end, 
                            y = overlay_prevalence_2, 
                            yend = overlay_position_y_end,
                            color = "red", alpha = .1, size = 3) +
    
    ggplot2::annotate("point", color = "red", alpha = .5, size = .8,
                      x = x_axis_position,
                      y = overlay_prevalence_2) +
    
     ggforce::geom_mark_rect(data = DF_X,
                             label.colour = "black",
                             alpha = .04,
                             expand = uncertainty_prevalence_num,
                             aes(
                               x = x_axis_position,
                               y = overlay_prevalence_2,
                               group = overlay_labels,
                               label = overlay_labels),
                             fill = "red", 
                             # con.border = "none", 
                             con.size = .2)

  # Output vars 
  return(p)
  
}



#' .translate_labels
#' 
#' Supports showing plot labels in Spanish (sp) or English (default)
#'
#' @param Language Can be Spanish "sp" or English (default)
#' @param Sensitivity . 
#' @param Specificity .
#' @param PPV_NPV .
#'
#' @return A list with labels

.translate_labels <- function(Language, Sensitivity, Specificity, PPV_NPV = "PPV") {
  
  # General ---
  
  if (Language == "sp" | Language == "es") {
    
    label_sick = "enferma"
    label_healthy = "sana"
    
  } else {
    
    label_sick = "sick"
    label_healthy = "healthy"
    
  }
  
  # PPV ---
  
  if (PPV_NPV == "PPV") {
    
    #Labels 
    if (Language == "sp" | Language == "es") {
      
      label_caption_name = "Sensibilidad"
      label_caption = paste0("Sensibilidad = ", Sensitivity, "%")
      label_x_axis = "Falsos +"
      label_x_axis_extra = "(1 - Especificidad)"
      label_y_axis = "Prevalencia"
      label_prevalence = "de"
      label_legend = "Valor\nPredictivo\nPositivo (%)\n "
      label_PPV_NPV = "Valor Predictivo Positivo"
      
    } else {
      
      label_caption_name = "Sensitivity"
      label_caption = paste0("Sensitivity = ", Sensitivity, "%")
      label_x_axis = "False +"
      label_x_axis_extra = "(1 - Specificity)"
      label_y_axis = "Prevalence"
      label_prevalence = "out of"
      label_legend = "Positive\nPredictive\nValue (%)\n "
      label_PPV_NPV = "Positive Predictive Value"
    }
    
    
  # NPV ---
    
  } else if (PPV_NPV == "NPV") {
    
    
    #Labels 
    if (Language == "sp" | Language == "es") {
      
      label_caption_name = "Especificidad"
      label_caption = paste0("Especificidad = ", Specificity, "%") #Tasa de Verdaderos Negativos
      label_x_axis = "Falsos Negativos"
      label_x_axis_extra = "(1 - Sensibilidad)"
      label_y_axis = "Prevalencia"
      label_prevalence = "de"
      label_legend = "Valor\nPredictivo\nNegativo (%)\n "
      label_PPV_NPV = "Valor Predictivo Negativo"
      
    } else {
      
      label_caption_name = "Specificity"
      label_caption = paste0("Specificity = ", Specificity, "%") #True Negative Rate
      label_x_axis = "False Negatives"
      label_x_axis_extra = "(1 - Sensitivity)"
      label_y_axis = "Prevalence"
      label_prevalence = "out of"
      label_legend = "Negative\nPredictive\nValue (%)\n "
      label_PPV_NPV = "Negative Predictive Value"
      
    }
    
  }
  
  
  # Output vars ---
  
  list(
    label_sick = label_sick,
    label_healthy = label_healthy,
    label_caption = label_caption,
    label_caption_name = label_caption_name,
    label_x_axis = label_x_axis,
    label_x_axis_extra = label_x_axis_extra,
    label_y_axis = label_y_axis,
    label_prevalence = label_prevalence,
    label_legend = label_legend,
    label_PPV_NPV = label_PPV_NPV
  )

}
