#' process_variables
#' Process main variables, checks for errors, creates ranges...
#'
#' @param Sensitivity 
#' @param Specificity 
#' @param width_Sensitivity 
#' @param width_Specificity 
process_variables <- function(Sensitivity, 
                             Specificity, 
                             width_Sensitivity = 10, 
                             width_Specificity = 10) {
  
  # CHECKS
  if (Sensitivity > 100 | Sensitivity < 0) stop("* Sensitivity sould be a value 0-100")
  if (Specificity > 100 | Specificity < 0) stop("* Specificity sould be a value 0-100")

  # Sensitivity range
  if (Sensitivity + (width_Sensitivity / 2) > 100) {
    max_Sensitivity = 100
    min_Sensitivity = max_Sensitivity - width_Sensitivity - (100 - max_Sensitivity)
  } else if (Sensitivity - (width_Sensitivity / 2) < 0) {
    min_Sensitivity = 0
    max_Sensitivity = min_Sensitivity + width_Sensitivity + (0 + min_Sensitivity)
  } else {
    min_Sensitivity = Sensitivity - (width_Sensitivity / 2)
    max_Sensitivity = Sensitivity + (width_Sensitivity / 2)
  }
  
  # Specificity range
  if (Specificity + (width_Specificity / 2) > 100) {
    max_Specificity = 100
    min_Specificity = max_Specificity - width_Specificity - (100 - max_Specificity)
  } else if (Specificity - (width_Specificity / 2) < 0) {
    min_Specificity = 0
    max_Specificity = min_Specificity + width_Specificity + (0 + min_Specificity)
  } else {
    min_Specificity = Specificity - (width_Specificity / 2)
    max_Specificity = Specificity + (width_Specificity / 2)
  }
  
  
  main_variables = 
    list(
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      max_Sensitivity = max_Sensitivity,
      min_Sensitivity = min_Sensitivity,
      max_Specificity = max_Specificity,
      min_Specificity = min_Specificity
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
#' @param max_FP Maximum False Positive ratio
#' @param min_FP Minimum False Positive ratio
#' @param steps_matrix How big the matrix should be (probably better to leave as it is: 100)
#'
#' @return A DF called PPV_melted
#' @importFrom reshape2 melt
#'
.createPPVmatrix <-
  function(min_Prevalence = 1,
           max_Prevalence = 1000,
           Sensitivity = 100,
           # Specificity = 100,
           
           #TESTING
           # width_Sensitivity = 10,
           # width_Specificity = 10,
           
           
           min_FP = 0,
           max_FP = 10,
           
           Max_FN = 0,
           Min_FN = 10,
           
           steps_matrix = 100) {
    
    
    
    # DEBUG *********************************************
    # ***************************************************
    # PPV_NPV = "NPV"
    # min_Prevalence = 50 
    # max_Prevalence = 100
    # Sensitivity = 100
    # max_FP = 1
    # steps_matrix = 10
    # 
    # min_FP = 0
    
    # ***************************************************
    # ***************************************************
    
    
    # Parameters ---
    
    # False Positives (x axis) 
    range_FP = (max_FP - min_FP)
    step_size_FP <- range_FP/steps_matrix
    FP = seq(min_FP, max_FP, step_size_FP)
    if(length(FP) == 1) FP = rep(FP, steps_matrix + 1) # CATCH FP = 0
    
    # Sensitivity
    # Min_FN <- 0
    # Max_FN <- (100 - Sensitivity)
    range_FN = (Max_FN - Min_FN)
    Step_size_FN <- range_FN/steps_matrix
    Sensitivity_range = seq(Min_FN, Max_FN, Step_size_FN)
    if(length(Sensitivity_range) == 1) Sensitivity_range = rep(Sensitivity_range, steps_matrix + 1) # CATCH Sensitivity = 100
    
    # prevalence_2
    range_prevalence = (max_Prevalence - min_Prevalence)
    Step_size_Prevalence <- range_prevalence / steps_matrix
    prevalence_2 <- round(seq(min_Prevalence, max_Prevalence, Step_size_Prevalence), 4) # *prevalence_2* x out of [y] (min_Prevalence out of max_Prevalence)
    
    
    # NEW SYSTEM ---
    
    # PPV 
    sick = rep(min_Prevalence, steps_matrix + 1)
    healthy = prevalence_2 - min_Prevalence
    
    sensitivity = rep(Sensitivity/100, steps_matrix + 1) # sensitivity is fixed when calculating PPV
    specificity = (100 - FP) / 100
    
    TRUE_positives = (sick %o% sensitivity)
    FALSE_positives = (healthy %o% (1 - specificity))

    
    # NPV
    sensitivity = (100 - Sensitivity_range) / 100
    specificity = rep((100 - max_FP) / 100, steps_matrix + 1) # specificity is fixed when calculating PPV
    
    # The order of this  %o% multiplications is critical (in NPV they have to be reversed) ¯\_(ツ)_/¯
    TRUE_negatives = (healthy %o% specificity)
    FALSE_negatives = (sick %o% (1 - sensitivity))
    
    
    # PPV Calculation -------------------------------------------------------------
    
    # We calculate a 100x100 PPV matrix using %o% (outer)
    # With the old system, min_Prevalence == sick people, and prevalence_2 - min_Prevalence is healthy people (min_Prevalence is set to 1 by default)
    # PPV <- round((Sensitivity * min_Prevalence) / ((Sensitivity * min_Prevalence) + ((prevalence_2 - min_Prevalence) %o% FP)), 2)
    # NPV <- round(((prevalence_2 - min_Prevalence) * (100 - max_FP)) / (((prevalence_2 - min_Prevalence) * (100 - max_FP)) + (prevalence_2  %o% Sensitivity_range)), 2)
    # (Healthy * Specificity) / (Healthy * Specificity) + (Sick * FN)
    
    PPV <- round(TRUE_positives / (TRUE_positives + FALSE_positives), 2)
    NPV <- round(TRUE_negatives / (TRUE_negatives + FALSE_negatives), 2)
    
    #Label columns and rows of matrix
    colnames(PPV) = FP
    rownames(PPV) = prevalence_2
    
    colnames(NPV) = Sensitivity_range
    rownames(NPV) = prevalence_2
    
    
    # Long format para ggplot Heatmap
    PPV_melted_PPV = reshape2::melt(PPV)
    PPV_melted_NPV = reshape2::melt(NPV)
    PPV_melted = PPV_melted_PPV %>% 
      cbind(PPV_melted_NPV)
    
    # Give names to variables
    names(PPV_melted) = c("prevalence_2", "FP", "PPV", "Prevalence2", "FN", "NPV") 
    
    PPV_melted = PPV_melted %>% 
      dplyr::mutate(prevalence_1 = min_Prevalence) %>% 
      dplyr::select(-Prevalence2) %>% 
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
#' @param Sensitivity Sensitivity of the test
#' @param Specificity Specificity of the test
#' @param overlay_prevalence_1 [x] out of y prevalence of disease
#' @param overlay_prevalence_2 x out of [y] prevalence of disease
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param overlay_labels .
#' @param point_Prevalence .
#' @param prevalence_label .
#' @param x_axis_label .
#' @param y_axis_label .
#' @param decimals_x .
#' @param decimals_y .

.get_point_ppv_npv <- function(
  PPV_melted, 
  PPV_NPV = "PPV", 
  Sensitivity,
  Specificity,
  # max_FP,
  overlay_prevalence_1,
  overlay_prevalence_2,
  overlay_labels,
  
  overlay_position_FP,
  overlay_position_FN,
  
  point_Prevalence, 
  prevalence_label,
  x_axis_label,
  y_axis_label,
  decimals_x,
  decimals_y) {


  # X The variable that defines axis position depends on PPV_NPV
  if (PPV_NPV == "PPV") {
    overlay_FP_FN = overlay_position_FP
  } else if (PPV_NPV == "NPV") {
    overlay_FP_FN = overlay_position_FN
  }
  
  PCT_prevalence_overlay = overlay_prevalence_1/overlay_prevalence_2
  
  
  # Get PPV or NPV value ----------------------------------------------------

  if (PPV_NPV == "NPV")  {

    DF_point_PPV_NPV = PPV_melted %>%
      dplyr::filter(
        # Closest value to PCT_prevalence_overlay & overlay_position_FP_FN
        abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay)) &
          abs(FN - overlay_position_FN) == min(abs(FN - overlay_position_FN)))
    
    DF_point_PPV_NPV = DF_point_PPV_NPV[1,]
    
    # REVIEW
    calculated_NPV = 
      round(
        ((Specificity) * (overlay_prevalence_2 - overlay_prevalence_1)) / 
          (((Specificity) * (overlay_prevalence_2 - overlay_prevalence_1)) + (overlay_prevalence_1 * overlay_FP_FN))
        , 2) 
    
    DEBUG_MESSAGE = paste0("calculated_NPV: ", calculated_NPV * 100, "%", " | NPV in PPV_melted: ", DF_point_PPV_NPV$NPV * 100, "%", " | DIFF: ", round(calculated_NPV - DF_point_PPV_NPV$NPV, 2) * 100, "%")
    
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", y_axis_label, ": ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\nSpecificity : ", Specificity, "%",
      "\n", x_axis_label, ": ", overlay_FP_FN, "%"
      )

    # point_PPV_NPV = DF_point_PPV_NPV %>% mutate(NPV = round(NPV * 100, 2))  %>% dplyr::pull(NPV)
    point_PPV_NPV = calculated_NPV * 100

  } else if (PPV_NPV == "PPV"){

    DF_point_PPV_NPV = PPV_melted %>%
      dplyr::filter(
        # Closest value to PCT_prevalence_overlay & overlay_position_FP_FN
        abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay)) &
          abs(FP - overlay_position_FP) == min(abs(FP - overlay_position_FP)))
    
    DF_point_PPV_NPV = DF_point_PPV_NPV[1,]
    
    # REVIEW
    calculated_PPV = 
      round(
        (Sensitivity * overlay_prevalence_1) / ((Sensitivity * overlay_prevalence_1) + (overlay_prevalence_2 - overlay_prevalence_1) * overlay_FP_FN),
        2) 
    
    DEBUG_MESSAGE = paste0("calculated_PPV: ", calculated_PPV * 100, "%", " | PPV in PPV_melted: ", DF_point_PPV_NPV$PPV * 100, "%", " | DIFF: ", round(calculated_PPV - DF_point_PPV_NPV$PPV, 2) * 100, "%")

    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", y_axis_label, ": ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\nSensitivity : ", Sensitivity, "%",
      "\n", x_axis_label, ": ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%")
      )

    # point_PPV_NPV = DF_point_PPV_NPV %>% dplyr::mutate(PPV = round(PPV * 100, 2))  %>% dplyr::pull(PPV)
    point_PPV_NPV = calculated_PPV * 100
  }

  # Function outputs
  list(
  Details_point_PPV_NPV = Details_point_PPV_NPV,
  point_PPV_NPV = point_PPV_NPV,
  size_overlay_text = nchar(paste0(overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2)),
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
#' @param Min_FN .
#' @param Max_FN .
#' @param min_Prevalence [x] out of y prevalence of disease
#' @param max_Prevalence x out of [y] prevalence of disease

.number_decimals_plot_axis <- function(PPV_NPV = "PPV", min_FP = 0, max_FP, Min_FN, Max_FN, min_Prevalence, max_Prevalence) {
  
  # The vars to calculate range depend on PPV NPV
  if (PPV_NPV == "PPV") {
    Max_FP_FN = max_FP
    min_FP_FN = min_FP
  } else if (PPV_NPV == "NPV") {
    Max_FP_FN = Max_FN
    min_FP_FN = Min_FN
  }
  
  # Number of decimals x AXIS
  if (Max_FP_FN - min_FP_FN < 1) {
    decimals_x = 2
  } else if  (max_FP - min_FP <= 5) {
    decimals_x = 1
  } else if  (Max_FP_FN - min_FP_FN > 5) {
    decimals_x = 0
  } else {
    decimals_x = 0
  }
  
  
  # Number of decimals y AXIS
  if (max_Prevalence - min_Prevalence < 9) {
    decimals_y = 1
  } else if  (max_Prevalence - min_Prevalence >= 9) {
    decimals_y = 0
  } else {
    decimals_y = 0
  }
  
  
  # Output vars -------------------------------------------------------------
  
  list("decimals_x" = decimals_x, 
       "decimals_y" = decimals_y)
}



#' .plot_creation
#' 
#' Function to create the main heatmap plot
#' 
#' @param PPV_melted .
#' @param Sensitivity .
#' @param PPV_NPV .
#' @param min_Prevalence .
#' @param min_FP .
#' @param max_FP .
#' @param steps_matrix .
#' @param decimals_x .
#' @param decimals_y .
#' @param label_title .
#' @param label_subtitle .
#' @param label_caption .
#' @param prevalence_label .
#' @param legend_label .
#' @param x_axis_label .
#' @param y_axis_label .
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin element_text
#'
.plot_creation <-
  function(PPV_melted,
           min_Prevalence,
           max_Prevalence,
           Sensitivity,
           PPV_NPV = "PPV",
           min_FP = 0,
           max_FP,
           Max_FN,
           Min_FN,
           
           steps_matrix = 100, 
           decimals_x,
           decimals_y,
           label_title = "",
           label_subtitle = "",
           label_caption = "",           
           prevalence_label = "",
           legend_label = "",
           x_axis_label,
           y_axis_label,
           
           DEBUG_MESSAGE = "") {
    

    # Global variables -------------------------------------------------------
    
    # Colors PPV
    # https://www.google.com/search?q=color+picker
    if (PPV_NPV == "PPV") {
      
      Paleta_DV = c("white", "grey", "1b2610", "yellowgreen", "chartreuse4") #Original
      
    } else if (PPV_NPV == "NPV") {
      
      Paleta_DV = c("#ffffff", "grey", "#190d24","#bd7afa", "#420080") # Violet
      
    }
    
    # Breaks and labels for PPV/NPV legend
    breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
    labels_DV = c(0, 25, 50, 75, 100)
    
    # False Positives (x axis) 
    Steps_FP <- steps_matrix
    range_FP = (max_FP - min_FP)
    step_size_FP <- range_FP/Steps_FP
    
    # Sensitivity (For NPV plot)
    Steps_FN <- steps_matrix
    # Min_FN <- 0
    # Max_FN <- (100 - Sensitivity)
    range_FN = (Max_FN - Min_FN)
    Step_size_FN <- range_FN/Steps_FN

    
    # PPV ---------------------------------------------------------------------
    
    if (PPV_NPV == "PPV") {
      
      # label_caption = paste0("Sensitivity = ", Sensitivity, "%")
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (prevalence_2)))  
      
      # BREAKS X # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x)
      # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
      if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x + 1)
      
      labels_x = paste0(breaks_x, "%")
      
      # BREAKS Y
      breaks_y = round(unique(PPV_melted$prevalence_2)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(min_Prevalence, prevalence_label, breaks_y)
      
      # PPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = PPV), colour = "white")
      
      
    # NPV ---------------------------------------------------------------------
      
    } else if (PPV_NPV == "NPV") {
      
      # label_caption = paste0("Specificity = ", 100 - max_FP, "%")
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (prevalence_2)))  
      
      # BREAKS X # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
      # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
      if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = Min_FN, to = Max_FN, by = Step_size_FN * 10), decimals_x + 1)
      
      labels_x = paste0(breaks_x, "%")
      
      # BREAKS Y
      breaks_y = round(unique(PPV_melted$prevalence_2)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(min_Prevalence, prevalence_label, breaks_y)
      

      # NPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = NPV), colour = "white")
      
    }

    p = p + 
      ggplot2::scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) +
      ggplot2::scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) + 
      ggplot2::scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
      ggplot2::theme(text = ggplot2::element_text(size = 20),
                     plot.caption = ggplot2::element_text(size = 16, color = "darkgrey"),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,10,0,0)), 
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10,0,0,0))) +
      ggplot2::labs(title = label_title,
                    subtitle = label_subtitle, 
                    caption = label_caption,
                    x = x_axis_label, 
                    y = y_axis_label)
    
    
      # Output vars -------------------------------------------------------------
      
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
#' @param overlay_prevalence_1 [x] out of y prevalence of disease for the overlay
#' @param overlay_prevalence_2 x out of [y] prevalence of disease for the overlay
#' @param decimals_x .
#' @param decimals_y .
#' @param prevalence_label .
#' @param label_title .
#' @param label_subtitle .
#' @param x_axis_label .
#' @param y_axis_label .
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param legend_label .
#' 
#' @importFrom ggplot2 annotate

.plot_overlay_area <-
  function(PPV_melted,
           uncertainty_prevalence = "low",
           min_Prevalence,
           max_Prevalence,
           Sensitivity,
           Specificity,
           min_FP = 0,
           max_FP,
           Max_FN,
           Min_FN,
           overlay_labels = "",
           PPV_NPV = "PPV",
           overlay_prevalence_1,
           overlay_prevalence_2,
           
           overlay_position_FP,
           overlay_position_FN,
           
           decimals_x,
           decimals_y,
           prevalence_label,
           legend_label,
           PPV_NPV_label,
           label_title,
           label_subtitle,
           label_caption,
           
           x_axis_label,
           y_axis_label
           ) {
    
    
    # Calculate point prevalence ----------------------------------------------

    # Use overlay prevalence as a pct
    PCT_prevalence_overlay = overlay_prevalence_1/overlay_prevalence_2
  
    # Looks for closer value of prevalence_2 (prevalence_2) using the prevalence_pct
      # Sets the y axis position of overlay
    point_Prevalence = PPV_melted %>%
      dplyr::filter(abs(prevalence_pct - PCT_prevalence_overlay) == min(abs(prevalence_pct - PCT_prevalence_overlay))) %>% 
      dplyr::sample_n(1) %>% 
      dplyr::pull(prevalence_2) # OLD Style
      # dplyr::pull(prevalence_1) # NEW Style
    
    
    # Get PPV or NPV value ----------------------------------------------------

    list_point_PPV = .get_point_ppv_npv(
      PPV_melted = PPV_melted,
      PPV_NPV = PPV_NPV,
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      # max_FP = max_FP,
      overlay_prevalence_1 = overlay_prevalence_1,
      overlay_prevalence_2 = overlay_prevalence_2,
      overlay_labels = overlay_labels,
      
      overlay_position_FP = overlay_position_FP,
      overlay_position_FN = overlay_position_FN,
      
      point_Prevalence = point_Prevalence,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      
      prevalence_label = prevalence_label,
      x_axis_label = x_axis_label,
      y_axis_label = y_axis_label
    ) 
  
      Details_point_PPV_NPV = list_point_PPV$Details_point_PPV_NPV
      point_PPV_NPV = list_point_PPV$point_PPV_NPV
      size_overlay_text = list_point_PPV$size_overlay_text
      
      # TODO: DEBUG - COMMENT OUT
      cat(PPV_NPV, ": ", list_point_PPV$DEBUG_MESSAGE)
      
      
      
    # Add overlay -------------------------------------------------------------
    
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
      Sensitivity = Sensitivity,
      min_FP = min_FP,
      max_FP = max_FP,
      Max_FN = Max_FN,
      Min_FN = Min_FN,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      prevalence_label = prevalence_label,
      legend_label = legend_label,
      label_subtitle = label_subtitle,
      label_title = label_title,
      label_caption = label_caption,
      
      x_axis_label = x_axis_label,
      y_axis_label = y_axis_label,
      
      PPV_NPV = PPV_NPV,
      
      DEBUG_MESSAGE = list_point_PPV$DEBUG_MESSAGE)
    
    
    p = p +
      
      # Overlay center (red dot)
      ggplot2::annotate("point", color = "red", alpha = 1, size = 1.5,
                        x = x_axis_position,
                        y = point_Prevalence) +
      
      # Text + rectangle
      ggforce::geom_mark_rect(
        
        # Uncertainty square
        aes(label = paste0(PPV_NPV_label, ": ", point_PPV_NPV, "%"), # BOLD title white rectangle
            x = x_axis_position,
            y = point_Prevalence),
        alpha = .04,
        expand = uncertainty_prevalence_num,
        fill = "red", 
        color = "black",
        
        # Description white rectangle
        label.colour = "black",
        description = paste0(Details_point_PPV_NPV),
        label.width = 82,
        label.minwidth = 35, 
        
        # Connector (line)
        con.size = .2
        )
    
    
    # Output vars -------------------------------------------------------------
    
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
#' @param Sensitivity .
#' @param label_title .
#' @param label_subtitle .
#' @param decimals_x .
#' @param decimals_y .
#' @param prevalence_label .
#' @param x_axis_label .
#' @param y_axis_label .
#' @param overlay_position_FP .
#' @param overlay_position_FN .
#' @param PPV_NPV .
#' @param legend_label .
#' @param uncertainty_prevalence How big the uncertainty area should be: ["low" or "high"]
#'
#' @importFrom ggplot2 annotate

.plot_overlay_line <-
  function(PPV_melted,
           uncertainty_prevalence = "low",
           PPV_NPV = "PPV",
           min_Prevalence,
           max_Prevalence,
           min_FP = 0,
           max_FP,
           Max_FN,
           Min_FN,
           Sensitivity,
           Specificity,
           
           overlay_prevalence_1,
           overlay_prevalence_2,
           
           overlay_position_FP,
           overlay_position_FN,
           
           overlay_labels,
           label_title,
           label_subtitle,
           label_caption,
           legend_label,

           decimals_x,
           decimals_y,
           
           prevalence_label,
           
           x_axis_label,
           y_axis_label) {
    
    

    
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
    Max_FN = Max_FN,
    Min_FN = Min_FN,
    Sensitivity = Sensitivity,
    decimals_x = decimals_x,
    decimals_y = decimals_y,
    prevalence_label = prevalence_label,
    
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label,
    
    legend_label = legend_label,
    label_subtitle = label_subtitle,
    label_title = label_title,
    label_caption = label_caption,
    
    PPV_NPV = PPV_NPV)  
  
  
  # X The variable that defines axis position depends on PPV_NPV
  if (PPV_NPV == "PPV") {
    x_axis_position = overlay_position_FP
    overlay_position_FN = NA
  } else {
    x_axis_position = overlay_position_FN
    overlay_position_FP = NA
  }
  
  overlay_position_x_end = c(x_axis_position[1], x_axis_position[-length(x_axis_position)])
  overlay_position_y_end = c(overlay_prevalence_2[1], overlay_prevalence_2[-length(overlay_prevalence_2)])
  
  
  
  # Plot Overlay ------------------------------------------------------------
  
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
#' @param max_FP .
#' @param PPV_NPV .
#'
#' @return A list with labels

.translate_labels <- function(Language, Sensitivity, Specificity, PPV_NPV = "PPV") {
  
  
  # PPV ---------------------------------------------------------------------
  
  if (PPV_NPV == "PPV") {
    
    #Labels 
    if (Language == "sp" | Language == "es") {
      
      label_caption = paste0("Sensibilidad = ", Sensitivity, "%")
      x_axis_label = "Tasa de Falsos Positivos"
      y_axis_label = "Prevalencia"
      prevalence_label = "de"
      legend_label = "Valor\nPredictivo\nPositivo (%)\n "
      PPV_NPV_label = "Valor Predictivo Positivo"
      
    } else {
      
      label_caption = paste0("Sensitivity = ", Sensitivity, "%")
      x_axis_label = "False Positive rate"
      y_axis_label = "Prevalence"
      prevalence_label = "out of"
      legend_label = "Positive\nPredictive\nValue (%)\n "
      PPV_NPV_label = "Positive Predictive Value"
    }
    
    
  } else if (PPV_NPV == "NPV") {
    
    
    #Labels 
    if (Language == "sp" | Language == "es") {
      
      label_caption = paste0("Especificidad = ", Specificity, "%") #Tasa de Verdaderos Negativos
      x_axis_label = "Tasa de Falsos Negativos"
      y_axis_label = "Prevalencia"
      prevalence_label = "de"
      legend_label = "Valor\nPredictivo\nNegativo (%)\n "
      PPV_NPV_label = "Valor Predictivo Negativo"
      
    } else {
      
      label_caption = paste0("Specificity = ", Specificity, "%") #True Negative Rate
      x_axis_label = "False Negative rate"
      y_axis_label = "Prevalence"
      prevalence_label = "out of"
      legend_label = "Negative\nPredictive\nValue (%)\n "
      PPV_NPV_label = "Negative Predictive Value"
      
    }
    
  }
  
  
  # Output vars -------------------------------------------------------------
  
  list(
    label_caption = label_caption,
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label,
    prevalence_label = prevalence_label,
    legend_label = legend_label,
    PPV_NPV_label = PPV_NPV_label
  )
  
}







#' 
#' 
#' #' .createPPVmatrix
#' #'
#' #' Create a PPV matrix helper function
#' #' 
#' #' @param min_Prevalence [x] out of y prevalence of disease
#' #' @param max_Prevalence x out of [y] prevalence of disease
#' #' @param Sensitivity Sensitivity of test
#' #' @param max_FP Maximum False Positive ratio
#' #' @param min_FP Minimum False Positive ratio
#' #' @param steps_matrix How big the matrix should be (probably better to leave as it is: 100)
#' #'
#' #' @return A DF called PPV
#' #' @importFrom reshape2 melt
#' #'
#' .createPPVmatrix <-
#'   function(min_Prevalence = 1,
#'            max_Prevalence = 1000,
#'            Sensitivity = 100,
#'            min_FP = 0,
#'            max_FP = 10,
#'            steps_matrix = 100) {
#'     
#'     
#'     
#'     # DEBUG *********************************************
#'     # ***************************************************
#'     # PPV_NPV = "NPV"
#'     # min_Prevalence = 1 
#'     # max_Prevalence = 500
#'     # Sensitivity = 85
#'     # max_FP = 1
#'     # steps_matrix = 100
#'     # 
#'     # min_FP = 0
#'     
#'     # ***************************************************
#'     # ***************************************************
#'     
#'     
#'     # False Positives (x axis) 
#'     Steps_FP <- steps_matrix
#'     range_FP = (max_FP - min_FP)
#'     step_size_FP <- range_FP/Steps_FP
#'     FP = seq(min_FP, max_FP, step_size_FP)
#'     
#'     
#'     # Sensitivity
#'     Steps_FN <- steps_matrix
#'     Min_FN <- 0
#'     Max_FN <- (100 - Sensitivity)
#'     range_FN = (Max_FN - Min_FN)
#'     Step_size_FN <- range_FN/Steps_FN
#'     Sensitivity_range = seq(Min_FN, Max_FN, Step_size_FN)
#'     
#'     
#'     # prevalence_2
#'     Steps_Prevalence <- steps_matrix
#'     range_prevalence = (max_Prevalence - min_Prevalence)
#'     Step_size_Prevalence <- range_prevalence / Steps_Prevalence
#'     # prevalence_2 <- round(seq(min_Prevalence, (max_Prevalence), Step_size_Prevalence), 4) # *prevalence_2* of sick [x] out of y
#'     
#'     
#'     prevalence_1 <- round(seq(min_Prevalence, (max_Prevalence), Step_size_Prevalence), 4) # *prevalence_2* of sick [x] out of y
#'     prevalence_2 <- max_Prevalence
#'     
#'     
#'     # PPV Calculation -------------------------------------------------------------
#'     
#'     # We calculate a 100x100 PPV matrix using %o% (outer)
#'     PPV <- round((Sensitivity * prevalence_1) / ((Sensitivity * prevalence_1) + ((rep(prevalence_2, steps_matrix + 1) - 1) %o% FP)), 2)
#'     
#'     NPV <- round(((prevalence_2 - prevalence_1) * (100 - max_FP)) / (((prevalence_2 - prevalence_1) * (100 - max_FP)) + (rep(prevalence_2, steps_matrix + 1)  %o% Sensitivity_range)), 2)
#'     # (Healthy * Specificity) / (Healthy * Specificity) + (Sick * FN)
#'     
#'     #Label columns and rows of matrix
#'     colnames(PPV) = FP
#'     rownames(PPV) = rep(prevalence_2, steps_matrix + 1)
#'     
#'     colnames(NPV) = Sensitivity_range
#'     rownames(NPV) = rep(prevalence_2, steps_matrix + 1)
#'     
#'     
#'     # Long format para ggplot Heatmap
#'     PPV_melted_PPV = reshape2::melt(PPV)
#'     PPV_melted_NPV = reshape2::melt(NPV)
#'     PPV_melted = PPV_melted_PPV %>% 
#'       cbind(PPV_melted_NPV)
#'     
#'     # Give names to variables
#'     names(PPV_melted) = c("prevalence_2", "FP", "PPV", "Prevalence2", "FN", "NPV") 
#'     
#'     PPV_melted = 
#'       PPV_melted %>% 
#'       dplyr::mutate(prevalence_1 = rep(prevalence_1, steps_matrix + 1),
#'                     prevalence_2 = as.double(prevalence_2)
#'                     ) %>% 
#'       dplyr::select(-Prevalence2) %>% 
#'       dplyr::select(prevalence_1, dplyr::everything()) %>% 
#'       dplyr::mutate(prevalence_pct = prevalence_1/prevalence_2) %>% 
#'       dplyr::as_tibble()
#'     
#'     return(PPV_melted)
#'     
#'   }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #' .plot_creation
#' #' 
#' #' Function to create the main heatmap plot
#' #' 
#' #' @param PPV_melted .
#' #' @param Sensitivity .
#' #' @param PPV_NPV .
#' #' @param min_Prevalence .
#' #' @param min_FP .
#' #' @param max_FP .
#' #' @param steps_matrix .
#' #' @param decimals_x .
#' #' @param decimals_y .
#' #' @param label_title .
#' #' @param label_subtitle .
#' #' @param label_caption .
#' #' @param prevalence_label .
#' #' @param legend_label .
#' #' @param x_axis_label .
#' #' @param y_axis_label .
#' #'
#' #' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin element_text
#' #'
#' .plot_creation <-
#'   function(PPV_melted,
#'            min_Prevalence,
#'            max_Prevalence,
#'            Sensitivity,
#'            PPV_NPV = "PPV",
#'            min_FP = 0,
#'            max_FP,
#'            steps_matrix = 100, 
#'            decimals_x,
#'            decimals_y,
#'            label_title = "",
#'            label_subtitle = "",
#'            label_caption = "",           
#'            prevalence_label = "",
#'            legend_label = "",
#'            x_axis_label,
#'            y_axis_label,
#'            
#'            DEBUG_MESSAGE = "") {
#'     
#'     
#'     
#'     
#'     # PPV_melted <<- PPV_melted
#'     # min_Prevalence <<- min_Prevalence
#'     # max_Prevalence <<- max_Prevalence
#'     # Sensitivity <<- Sensitivity
#'     # PPV_NPV <<- PPV_NPV
#'     # min_FP <<- min_FP
#'     # max_FP <<- max_FP
#'     # steps_matrix <<- steps_matrix
#'     # decimals_x <<- decimals_x
#'     # decimals_y <<- decimals_y
#'     # label_title <<- label_title
#'     # label_subtitle <<- label_subtitle
#'     # label_caption <<- label_caption
#'     # prevalence_label <<- prevalence_label
#'     # legend_label <<- legend_label
#'     # x_axis_label <<- x_axis_label
#'     # y_axis_label <<- y_axis_label
#'     
#'     
#'     
#'     # Global variables -------------------------------------------------------
#'     
#'     # Colors PPV
#'     # https://www.google.com/search?q=color+picker
#'     if (PPV_NPV == "PPV") {
#'       
#'       Paleta_DV = c("white", "grey", "1b2610", "yellowgreen", "chartreuse4") #Original
#'       
#'     } else if (PPV_NPV == "NPV") {
#'       
#'       Paleta_DV = c("#ffffff", "grey", "#190d24","#bd7afa", "#420080") # Violet
#'       
#'     }
#'     
#'     # Breaks and labels for PPV/NPV legend
#'     breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
#'     labels_DV = c(0, 25, 50, 75, 100)
#'     
#'     # False Positives (x axis) 
#'     Steps_FP <- steps_matrix
#'     range_FP = (max_FP - min_FP)
#'     step_size_FP <- range_FP/Steps_FP
#'     
#'     # Sensitivity (For NPV plot)
#'     Steps_FN <- steps_matrix
#'     Min_FN <- 0
#'     Max_FN <- (100 - Sensitivity)
#'     range_FN = (Max_FN - Min_FN)
#'     Step_size_FN <- range_FN/Steps_FN
#'     
#'     
#'     # PPV ---------------------------------------------------------------------
#'     
#'     if (PPV_NPV == "PPV") {
#'       
#'       # label_caption = paste0("Sensitivity = ", Sensitivity, "%")
#'       
#'       # Create plot
#'       p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (prevalence_1)))  
#'       
#'       # BREAKS X # [TODO] Can USE PPV_melted to get this?
#'       breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x)
#'       # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
#'       if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = min_FP, to = max_FP, by = step_size_FP * 10), decimals_x + 1)
#'       
#'       labels_x = paste0(breaks_x, "%")
#'       
#'       # BREAKS Y
#'       # breaks_y = round(unique(PPV_melted$prevalence_2)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
#'       # labels_y = paste(min_Prevalence, prevalence_label, breaks_y)
#'       
#'       breaks_y = round(unique(PPV_melted$prevalence_1)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
#'       labels_y = paste(breaks_y, prevalence_label, max_Prevalence)
#'       
#'       # PPV tiles
#'       p = p + ggplot2::geom_tile(ggplot2::aes(fill = PPV), colour = "white")
#'       
#'       
#'       # NPV ---------------------------------------------------------------------
#'       
#'     } else if (PPV_NPV == "NPV") {
#'       
#'       # label_caption = paste0("Specificity = ", 100 - max_FP, "%")
#'       
#'       # Create plot
#'       p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (prevalence_1)))  
#'       
#'       # BREAKS X # [TODO] Can USE PPV_melted to get this?
#'       breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
#'       # With no decimals sometimes the breaks are not equidistant. This is a hacky way to solve it
#'       if (length(unique(diff(breaks_x))) > 1) breaks_x = round(seq(from = Min_FN, to = Max_FN, by = Step_size_FN * 10), decimals_x + 1)
#'       
#'       labels_x = paste0(breaks_x, "%")
#'       
#'       # BREAKS Y
#'       # breaks_y = round(unique(PPV_melted$prevalence_2)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
#'       # labels_y = paste(min_Prevalence, prevalence_label, breaks_y)
#'       breaks_y = round(unique(PPV_melted$prevalence_1)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
#'       labels_y = paste(breaks_y, prevalence_label, max_Prevalence)
#'       
#'       
#'       # NPV tiles
#'       p = p + ggplot2::geom_tile(ggplot2::aes(fill = NPV), colour = "white")
#'       
#'     }
#'     
#'     p = p + 
#'       ggplot2::scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) +
#'       ggplot2::scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) + 
#'       ggplot2::scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
#'       ggplot2::theme(text = ggplot2::element_text(size = 20),
#'                      plot.caption = ggplot2::element_text(size = 16, color = "darkgrey"),
#'                      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,10,0,0)), 
#'                      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10,0,0,0))) +
#'       ggplot2::labs(title = label_title,
#'                     subtitle = label_subtitle, 
#'                     caption = label_caption,
#'                     x = x_axis_label, 
#'                     y = y_axis_label)
#'     
#'     
#'     # Output vars -------------------------------------------------------------
#'     
#'     return(p)
#'     
#'   }
#' 
