
#' .createPPVmatrix
#'
#' Create a PPV matrix helper function
#' 
#' @param Max_Prevalence Max prevalence of disease
#' @param Sensitivity Sensitivity of test
#' @param Max_FP Maximum False Positives ratio
#'
#' @return A DF called PPV
#' @export
#' @importFrom reshape2 melt
#'
#' @examples
#' .createPPVmatrix(1, 1000, 100, 2)
.createPPVmatrix <- function(Min_Prevalence = 1, Max_Prevalence, Sensitivity, Max_FP, PPV_NPV = "PPV") {
  
  # library(reshape2)
  
  # DEBUG -------------------------------------------------------------------
  
  # Max_Prevalence = 500
  # Sensitivity = 90
  # Max_FP = 5
  # library(tidyverse)
  # PPV_NPV = "NPV" # NPV/PPV
  # label_title = ""
  # label_subtitle = ""
  
  #TEST Parameters **************
  
  steps_matrix <<- 100
  
  # False Positives (x axis) 
  Steps_FP <<- steps_matrix
  Step_size_FP <<- Max_FP/Steps_FP
  Min_FP <<- 0 
  FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
  
  # Sentisitivy
  Max_FN <<- (100 - Sensitivity)
  Steps_FN <<- steps_matrix
  Step_size_FN <<- Max_FN/Steps_FN
  Min_FN <<- 0
  
  Sensitivity_range = seq(Min_FN, Max_FN, Step_size_FN)
  
  
  #CONDITION Parameters ***********
  
  Min_Prevalence <<- Min_Prevalence
  Prevalence_x <<- Min_Prevalence
  Max_Prevalence <<- Max_Prevalence
  Steps_Prevalence <<- steps_matrix
  range_prevalence = (Max_Prevalence - Min_Prevalence)
  Step_size_Prevalence <<- range_prevalence / Steps_Prevalence
  Prevalence <<- round(seq(Min_Prevalence, (Max_Prevalence), Step_size_Prevalence), 4)  #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
  
  # Prevalence_percent = Prevalence/Max_Prevalence
  # Prevalence_percent * 1000
  
  
  # PPV Calculation -------------------------------------------------------------
  
  # We calculate the 100x100 PPV matrix using %o% (outer)
  PPV <- round((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP)), 2)
  
  NPV <- round(((Prevalence - Min_Prevalence) * (100 - Max_FP)) / (((Prevalence - Min_Prevalence) * (100 - Max_FP)) + (Prevalence  %o% Sensitivity_range)), 2)
  # (       Healty               * Specificity)   /  (          Healty               * Specificity)    + Sick       *     FN
  
  #Label columns and rows of matrix
  colnames(PPV) = FP
  rownames(PPV) = Prevalence
  
  colnames(NPV) = Sensitivity_range
  rownames(NPV) = Prevalence
  
  
  # Long format para ggplot Heatmap
  PPV_melted_PPV = reshape2::melt(PPV)
  PPV_melted_NPV = reshape2::melt(NPV)
  PPV_melted = PPV_melted_PPV %>% 
    cbind(PPV_melted_NPV)
  
  # Give names to variables
  # names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV", "melted_Prevalence2", "melted_XXX", "melted_NPV") 
  names(PPV_melted) = c("Prevalence", "FP", "PPV", "Prevalence2", "FN", "NPV") 
  
  PPV_melted %>% 
    dplyr::mutate(prevalence_1 = Min_Prevalence) %>% 
    dplyr::select(-Prevalence2) %>% 
    dplyr::select(prevalence_1, dplyr::everything()) %>% 
    dplyr::mutate(prevalence_pct = prevalence_1/Prevalence) %>% 
    dplyr::as_tibble()
  
  # hist(PPV_melted$prevalence_pct, breaks = 100)
  
}





#' Get point ppv-npv
#'
#' @param PPV_melted 
#' @param PPV_NPV 
#' @param overlay_position_FP_FN 
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#' @examples
.get_point_ppv_npv <- function(PPV_melted, PPV_NPV = "PPV", overlay_position_FP_FN) {
  
  # BUG: THIS SHOULDN'T BE HERE. SHOULD READ FROM THE APPROPR
  # prevalence_label = " / "
  # decimals_x = 1
  if (exists("overlay_labels") == FALSE) { overlay_labels = ""}
  if (exists("prevalence_label") == FALSE) { prevalence_label = ""}
  if (exists("decimals_x") == FALSE) { decimals_x = 1}
  
  
  # Get PPV or NPV value ----------------------------------------------------
  DF_point_PPV_NPV = PPV_melted %>%
    dplyr::filter(
      # Closest value to overlay_prevalence_2 & overlay_position_FP_FN
      abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
        abs(FP - overlay_position_FP_FN) == min(abs(FP - overlay_position_FP_FN))) 
  
  DF_point_PPV_NPV = DF_point_PPV_NPV[1,]
  
  
  if (PPV_NPV == "NPV")  {
    
    calculated_NPV = paste0(round( ( (100 - overlay_position_FP_FN) * overlay_prevalence_2) / (((100 - overlay_position_FP_FN) * overlay_prevalence_2) + (overlay_prevalence_1) * Sensitivity), 2) * 100, "%")
    # NPV <<- round(((Prevalence - Min_Prevalence) * (100 - Max_FP)) / (((Prevalence - Min_Prevalence) * (100 - Max_FP)) + (Prevalence  %o% Sensitivity_range)), 2)
    
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      # "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
      "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\n FN = ", paste0(overlay_position_FP_FN,
                         # round(DF_point_PPV_NPV$FN, decimals_x) # BUG
                         "%"),
      "\n NPV = ", calculated_NPV
      # paste0(round(DF_point_PPV_NPV$NPV, 2) * 100, "%")
    )
    
    point_PPV_NPV = DF_point_PPV_NPV %>% mutate(NPV = round(NPV * 100, 2))  %>% dplyr::pull(NPV)
    
  } else {
    
    calculated_PPV = paste0(
      round( 
        (Sensitivity * overlay_prevalence_1) / ((Sensitivity * overlay_prevalence_1) + (overlay_prevalence_2 - overlay_prevalence_1) * overlay_position_FP_FN),
        2) * 100, "%")
    
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      # "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
      "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\n FP = ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%"),
      "\n PPV = ", calculated_PPV
      # paste0(round(DF_point_PPV_NPV$PPV, 2) * 100, "%")
    )
    
    point_PPV_NPV = DF_point_PPV_NPV %>% dplyr::mutate(PPV = round(PPV * 100, 2))  %>% dplyr::pull(PPV)
    
  }
  
  # Function outputs
  Details_point_PPV_NPV <<- Details_point_PPV_NPV
  point_PPV_NPV <<- point_PPV_NPV
  size_overlay_text <<- nchar(paste0(overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2))
  
}











#' Title
#'
#' @param PPV_NPV 
#' @param Min_FP 
#' @param Max_FP 
#' @param Min_FN 
#' @param Max_FN 
#' @param Min_Prevalence 
#' @param Max_Prevalence 
#'
#' @return
#' @export
#'
#' @examples
.number_decimals_plot_axis <- function(PPV_NPV, Min_FP, Max_FP, Min_FN, Max_FN, Min_Prevalence, Max_Prevalence) {
  
  # The number of decimal places in the x and y axis label depends on how wide the range is
  
  # The vars to calculate range depend on PPV NPV
  if (PPV_NPV == "PPV") {
    Max_FP_FN = Max_FP
    Min_FP_FN = Min_FP
  } else if (PPV_NPV == "NPV") {
    Max_FP_FN = Max_FN
    Min_FP_FN = Min_FN
  }
  
  # Number of decimals x AXIS
  if (Max_FP_FN - Min_FP_FN < 1) {
    decimals_x = 2
  } else if  (Max_FP - Min_FP <= 5) {
    decimals_x = 1
  } else if  (Max_FP_FN - Min_FP_FN > 5) {
    decimals_x = 0
  }
  
  
  # Number of decimals y AXIS
  if (Max_Prevalence - Min_Prevalence < 9) {
    decimals_y = 1
  } else if  (Max_Prevalence - Min_Prevalence >= 9) {
    decimals_y = 0
  } 
  
  
  # Output vars -------------------------------------------------------------
  
  decimals_x <<- decimals_x
  decimals_y <<- decimals_y
  
  # decimals = c(decimals_x, decimals_y)
  # decimals
}










#' .plot_creation
#'
#' @param PPV_melted 
#' @param label_title 
#' @param label_subtitle 
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin element_text
#'
#' @examples
.plot_creation <-
  function(PPV_melted,
           Max_FP,
           Step_size_FP,
           label_title = "",
           label_subtitle = "",
           label_caption = "",
           decimals_x,
           decimals_y,
           prevalence_label) {
    
    # DEBUG -------------------------------------------------------------------
    
    # label_title = ""
    # label_subtitle = ""
    # label_caption = ""
    
    # # DEBUG -------------------------------------------------------------------
    # if (exists("DEBUG") == FALSE) {DEBUG = 0}
    # 
    # if (DEBUG == 1) {
    #   message("\n*** .plot_creation() *** ")
    #   message("Max_FP: ", Max_FP)
    #   message("Step_size_FP: ", Step_size_FP)
    #   message("decimals_x: ", decimals_x)
    #   message("prevalence_label: ", prevalence_label)
    #   message("\n*** END *** ")
    #   
    # }
    # # **************************************************************************
    
    
    # Global variables -------------------------------------------------------
    
    # Colors, breaks and steps for Prevalence and FP axis
    Paleta_DV = c("white", "grey", "gray30", "yellowgreen", "chartreuse4")
    breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
    labels_DV = c(0, 25, 50, 75, 100)
    
    
    
    # PPV ---------------------------------------------------------------------
    
    if (PPV_NPV == "PPV") {
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (Prevalence)))  
      
      # USE PPV_melted to get this!!!!
      breaks_x = round(seq(from = Min_FP, to = Max_FP, by = Step_size_FP * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")
      
      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      # breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), max(PPV_melted$Prevalence))], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
      # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
      
      # PPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = PPV), colour = "white")
      
      
      # NPV ---------------------------------------------------------------------
      
    } else if (PPV_NPV == "NPV") {
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (Prevalence)))  
      
      # USE PPV_melted to get this!!!!
      breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")
      
      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
      # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
      
      # NPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = NPV), colour = "white")
      
    }
    
    # add 
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




#' Plot overlay area
#'
#' @param PPV_melted 
#' @param uncertainty_prevalence 
#' @param overlay_labels 
#' @param PPV_NPV 
#' @param overlay_position_FP_FN 
#'
#' @return
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
.plot_overlay_area <-
  function(PPV_melted,
           uncertainty_prevalence = "low",
           Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Max_FP,
           overlay_labels = "",
           PPV_NPV = "PPV",
           overlay_prevalence_1,
           overlay_prevalence_2,
           overlay_position_FP_FN,
           decimals_x,
           decimals_y,
           prevalence_label) {
    
    # DEBUG -------------------------------------------------------------------
    # uncertainty_prevalence = "high"
    # overlay_labels = ""
    

    # Calculate point prevalence ----------------------------------------------

    # Calculates y as in Min_Prevalence out of y
    point_Prevalence_temp = Min_Prevalence / (overlay_prevalence_1 / overlay_prevalence_2)
    
    # Looks for closer value in the Prevalence column
    point_Prevalence <<-  PPV_melted %>%
      dplyr::filter(
        # Closest value to overlay_prevalence_2
        abs(Prevalence - point_Prevalence_temp) == min(abs(Prevalence - point_Prevalence_temp))) %>% 
      dplyr::sample_n(1) %>% 
      dplyr::pull(Prevalence)     
    
    
    # Get PPV or NPV value ----------------------------------------------------
    
    # SHOULD THIS GO INSIDE .calculate_area_overlay_coordinates()???? ####
    .get_point_ppv_npv(PPV_melted, PPV_NPV = PPV_NPV, overlay_position_FP_FN = overlay_position_FP_FN) #, overlay_labels = overlay_labels, decimals_x = decimals_x, prevalence_label = prevalence_label
    
    
    
    # Add overlay -------------------------------------------------------------
    
    ## If overlay outside old matrix, we need to do this
    # if (DEBUG == 1) warning("\n\n  *Recalculate PPVMatrix: ", Min_Prevalence, " ", Max_Prevalence, " ", Sensitivity, " ", Max_FP)
    if (uncertainty_prevalence == "high") {
      uncertainty_prevalence_num = .05
    } else {
      uncertainty_prevalence_num = .02
    }  
    

    
    p = .plot_creation(
      PPV_melted = PPV_melted,
      Max_FP = Max_FP,
      Step_size_FP = Step_size_FP,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      prevalence_label = prevalence_label,
      label_subtitle = label_subtitle,
      label_title = label_title,
      label_caption = paste0("Sensitivity = ", Sensitivity, "%"))  
    
    p = p +
      
      # Overlay center
      ggplot2::annotate("point", color = "red", alpha = .5, size = 1,
                        x = overlay_position_FP_FN,
                        y = point_Prevalence) +
      
      # Text + rectangle
      ggforce::geom_mark_rect(
        label.colour = "black",
        alpha = .04,
        expand = uncertainty_prevalence_num,
        aes(x = overlay_position_FP_FN, 
            y = point_Prevalence),
        # label = plot_text,
        fill = "red",
        description = paste0(Details_point_PPV_NPV))

    
    
    # Output vars -------------------------------------------------------------
    
    return(p)
    
  }




#' Title
#'
#' @param PPV_melted 
#' @param Max_Prevalence 
#' @param overlay_prevalence_1
#' @param overlay_prevalence_2 
#' @param overlay_position_FP_FN 
#' @param overlay_labels 
#'
#' @return
#' @export
#' @importFrom ggplot2 annotate
#'
#' @examples
.plot_overlay_line <- function(PPV_melted, Max_Prevalence, overlay_prevalence_1, overlay_prevalence_2, overlay_position_FP_FN, overlay_labels) {
  
  # We made the modifiers proportional to the parameters (Max_Prevalence, Max_FP)
  if (exists("size_uncertainty_area") == FALSE) {size_uncertainty_area = 0}
  
  if (abs(Max_Prevalence - max(overlay_prevalence_2)) > 10) {
    
    modifier_text_overlay_position = (Max_Prevalence * size_uncertainty_area  + 1)
    
  } else {
    
    modifier_text_overlay_position = -(Max_Prevalence * size_uncertainty_area + 1)
    
  }
  
  # overlay_labels = c("80", "70", "60", "50", "40", "30 y.o.")
  # overlay_position_FP_FN = c(7, 8, 9, 12, 14)
  # overlay_prevalence_2 = c(26, 29, 44, 69, 227)
  
  overlay_position_x_end = c(overlay_position_FP_FN[1], overlay_position_FP_FN[-length(overlay_position_FP_FN)])
  overlay_position_y_end = c(overlay_prevalence_2[1], overlay_prevalence_2[-length(overlay_prevalence_2)])
  
  
  # Create plot after adjusting overlay dimensions
  # Should re-create ppv/npv matrix first?
  # .plot_creation(PPV_melted)   
  p = .plot_creation(
    PPV_melted = PPV_melted,
    Max_FP = Max_FP,
    Step_size_FP = Step_size_FP,
    decimals_x = decimals_x,
    decimals_y = decimals_y,
    prevalence_label = prevalence_label,
    label_subtitle = label_subtitle,
    label_title = label_title,
    label_caption = paste0("Sensitivity = ", Sensitivity, "%"))  
  
  
  
  # Plot Overlay ------------------------------------------------------------
  p = p + ggplot2::annotate("segment", x = overlay_position_FP_FN, xend = overlay_position_x_end, 
                              y = overlay_prevalence_2, yend = overlay_position_y_end,
                              color = "red", alpha = .1, size = 3) +
    ggplot2::annotate("text", x = overlay_position_FP_FN, y = overlay_prevalence_2, label = overlay_labels, size = 4) 
  
  return(p)
}





#' .translate_labels
#'
#' @param Language 
#' @param Sensitivity 
#' @param Max_FP 
#'
#' @return
#' @export
#'
#' @examples
.translate_labels <- function(Language, Sensitivity, Max_FP) {
  
  
  # PPV ---------------------------------------------------------------------
  
  if (PPV_NPV == "PPV") {
    
    #Labels 
    if (Language == "sp") {
      
      label_caption = paste("Sensibilidad =", Sensitivity, "%")
      x_axis_label = "Tasa de Falsos Positivos"
      y_axis_label = "Prevalencia"
      prevalence_label = "de cada"
      legend_label = "VPP (%)\n"
      
    } else {
      
      label_caption = paste("Sensitivity =", Sensitivity, "%")
      x_axis_label = "False Positive rate"
      y_axis_label = "Prevalence"
      prevalence_label = "out of"
      legend_label = "PPV (%)\n"
      
    }
    
    
  } else if (PPV_NPV == "NPV") {
    
    
    #Labels 
    if (Language == "sp") {
      
      label_caption = paste("Tasa de Verdaderos Negativos =", (100 - Max_FP), "%")
      x_axis_label = "Tasa de Falsos Negativos"
      y_axis_label = "Prevalencia"
      prevalence_label = "de cada"
      legend_label = "VPN (%)\n"
      
    } else {
      
      label_caption = paste("True Negative Rate =", (100 - Max_FP), "%")
      x_axis_label = "False Negative rate"
      y_axis_label = "Prevalence"
      prevalence_label = "out of"
      legend_label = "NPV (%)\n"
      
    }
    
  }
  
  
  # Output vars -------------------------------------------------------------
  
  label_caption <<- label_caption
  x_axis_label <<- x_axis_label
  y_axis_label <<- y_axis_label
  prevalence_label <<- prevalence_label 
  legend_label <<- legend_label
  
}

