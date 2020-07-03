#' .createPPVmatrix
#'
#' Create a PPV matrix helper function
#' 
#' @param Min_Prevalence [x] out of y prevalence of disease
#' @param Max_Prevalence x out of [y] prevalence of disease
#' @param Sensitivity Sensitivity of test
#' @param Max_FP Maximum False Positive ratio
#' @param Min_FP Minimum False Positive ratio
#' @param steps_matrix How big the matrix should be (probably better to leave as it is: 100)
#'
#' @return A DF called PPV
#' @importFrom reshape2 melt
#'
.createPPVmatrix <-
  function(Min_Prevalence = 1,
           Max_Prevalence = 1000,
           Sensitivity = 100,
           Min_FP = 0,
           Max_FP = 10,
           steps_matrix = 100) {
    
    
  # TEST Parameters ---
  
  # False Positives (x axis) 
  Steps_FP <- steps_matrix
  Step_size_FP <- Max_FP/Steps_FP
  
  FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
  
  # Sensitivity
  Max_FN <- (100 - Sensitivity)
  Steps_FN <- steps_matrix
  Step_size_FN <- Max_FN/Steps_FN
  Min_FN <- 0
  
  Sensitivity_range = seq(Min_FN, Max_FN, Step_size_FN)
  
  
  # CONDITION Parameters ---
  
  Min_Prevalence <- Min_Prevalence
  Prevalence_x <- Min_Prevalence
  Max_Prevalence <- Max_Prevalence
  Steps_Prevalence <- steps_matrix
  range_prevalence = (Max_Prevalence - Min_Prevalence)
  Step_size_Prevalence <- range_prevalence / Steps_Prevalence
  Prevalence <- round(seq(Min_Prevalence, (Max_Prevalence), Step_size_Prevalence), 4)  #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100

  
  # PPV Calculation -------------------------------------------------------------
  
  # We calculate a 100x100 PPV matrix using %o% (outer)
  PPV <- round((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP)), 2)
  
  NPV <- round(((Prevalence - Min_Prevalence) * (100 - Max_FP)) / (((Prevalence - Min_Prevalence) * (100 - Max_FP)) + (Prevalence  %o% Sensitivity_range)), 2)
    # (Healthy * Specificity) / (Healthy * Specificity) + (Sick * FN)
  
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
  names(PPV_melted) = c("Prevalence", "FP", "PPV", "Prevalence2", "FN", "NPV") 
  
  PPV_melted %>% 
    dplyr::mutate(prevalence_1 = Min_Prevalence) %>% 
    dplyr::select(-Prevalence2) %>% 
    dplyr::select(prevalence_1, dplyr::everything()) %>% 
    dplyr::mutate(prevalence_pct = prevalence_1/Prevalence) %>% 
    dplyr::as_tibble()
  
}



#' .get_point_ppv_npv
#' 
#' Get PPV or NPV for the overlay
#'
#' @param PPV_melted DF out of .createPPVmatrix()
#' @param PPV_NPV Should calculate PPV or NPV?
#' @param Sensitivity Sensitivity of the test
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
  
  
  # Get PPV or NPV value ----------------------------------------------------
  DF_point_PPV_NPV = PPV_melted %>%
    dplyr::filter(
      # Closest value to overlay_prevalence_2 & overlay_position_FP_FN
      abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
        abs(FP - overlay_position_FP) == min(abs(FP - overlay_position_FP)))
        
  DF_point_PPV_NPV = DF_point_PPV_NPV[1,]


  if (PPV_NPV == "NPV")  {

    calculated_NPV = paste0(
      round(
        ((100 - overlay_position_FP) * (overlay_prevalence_2 - overlay_prevalence_1)) / 
          
          (((100 - overlay_position_FP) * (overlay_prevalence_2 - overlay_prevalence_1)) + (overlay_prevalence_1 * overlay_position_FN))
        
        , 2) * 100, "%")

    
    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\nFN = ", paste0(overlay_FP_FN, "%"),
      "\nNPV = ", calculated_NPV
      )

    point_PPV_NPV = DF_point_PPV_NPV %>% mutate(NPV = round(NPV * 100, 2))  %>% dplyr::pull(NPV)

  } else if (PPV_NPV == "PPV"){

    calculated_PPV = paste0(
      round(
        (Sensitivity * overlay_prevalence_1) / ((Sensitivity * overlay_prevalence_1) + (overlay_prevalence_2 - overlay_prevalence_1) * overlay_FP_FN),
        2) * 100, "%")

    Details_point_PPV_NPV = paste0(
      overlay_labels,
      "\n", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
      "\nFP = ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%"),
      "\nPPV = ", calculated_PPV
      )

    point_PPV_NPV = DF_point_PPV_NPV %>% dplyr::mutate(PPV = round(PPV * 100, 2))  %>% dplyr::pull(PPV)

  }

  # Function outputs
  list(
  Details_point_PPV_NPV = Details_point_PPV_NPV,
  point_PPV_NPV = point_PPV_NPV,
  size_overlay_text = nchar(paste0(overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2))
  )
}



#' .number_decimals_plot_axis
#' 
#' The number of decimal places in the x and y axis label depends on how wide the range is.
#'
#' @param PPV_NPV .
#' @param Min_FP .
#' @param Max_FP .
#' @param Min_FN .
#' @param Max_FN .
#' @param Min_Prevalence [x] out of y prevalence of disease
#' @param Max_Prevalence x out of [y] prevalence of disease

.number_decimals_plot_axis <- function(PPV_NPV = "PPV", Min_FP = 0, Max_FP, Min_FN, Max_FN, Min_Prevalence, Max_Prevalence) {
  
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
#' @param Min_Prevalence .
#' @param Min_FP .
#' @param Max_FP .
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
           Min_Prevalence,
           Sensitivity,
           PPV_NPV = "PPV",
           Min_FP = 0,
           Max_FP,
           steps_matrix = 100, 
           decimals_x,
           decimals_y,
           label_title = "",
           label_subtitle = "",
           label_caption = "",           
           prevalence_label = "",
           legend_label = "",
           x_axis_label,
           y_axis_label) {
    
    
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
    Step_size_FP <- Max_FP/Steps_FP
    
    # Sensitivity (For NPV plot)
    Max_FN <- (100 - Sensitivity)
    Steps_FN <- steps_matrix
    Step_size_FN <- Max_FN/Steps_FN
    Min_FN <- 0
    
    
    # PPV ---------------------------------------------------------------------
    
    if (PPV_NPV == "PPV") {
      
      label_caption = paste0("Sensitivity = ", Sensitivity, "%")
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FP, (Prevalence)))  
      
      # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(from = Min_FP, to = Max_FP, by = Step_size_FP * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")
      
      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)

      
      # PPV tiles
      p = p + ggplot2::geom_tile(ggplot2::aes(fill = PPV), colour = "white")
      
      
    # NPV ---------------------------------------------------------------------
      
    } else if (PPV_NPV == "NPV") {
      
      label_caption = paste0("Specificity = ", 100 - Max_FP, "%")
      
      # Create plot
      p = ggplot2::ggplot(PPV_melted, ggplot2::aes(FN, (Prevalence)))  
      
      # [TODO] Can USE PPV_melted to get this?
      breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")
      
      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)

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
#' @param Min_Prevalence [x] out of y prevalence of disease
#' @param Max_Prevalence x out of [y] prevalence of disease
#' @param Sensitivity .
#' @param Min_FP .
#' @param Max_FP .
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
           Min_Prevalence,
           Max_Prevalence,
           Sensitivity,
           Min_FP = 0,
           Max_FP,
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
           label_title,
           label_subtitle,
           
           x_axis_label,
           y_axis_label
           ) {
    

    # Calculate point prevalence ----------------------------------------------

    # Calculates y as in Min_Prevalence out of y
    point_Prevalence_temp = Min_Prevalence / (overlay_prevalence_1 / overlay_prevalence_2)
    
    # Looks for closer value in the Prevalence column
    point_Prevalence <-  PPV_melted %>%
      dplyr::filter(
        # Closest value to overlay_prevalence_2
        abs(Prevalence - point_Prevalence_temp) == min(abs(Prevalence - point_Prevalence_temp))) %>% 
      dplyr::sample_n(1) %>% 
      dplyr::pull(Prevalence)     
    
    
    # Get PPV or NPV value ----------------------------------------------------

    list_point_PPV = .get_point_ppv_npv(
      PPV_melted = PPV_melted,
      PPV_NPV = PPV_NPV,
      Sensitivity = Sensitivity,
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
      Min_Prevalence = Min_Prevalence,
      Sensitivity = Sensitivity,
      Max_FP = Max_FP,
      decimals_x = decimals_x,
      decimals_y = decimals_y,
      prevalence_label = prevalence_label,
      legend_label = legend_label,
      label_subtitle = label_subtitle,
      label_title = label_title,
      label_caption = paste0("Sensitivity = ", Sensitivity, "%"),
      
      x_axis_label = x_axis_label,
      y_axis_label = y_axis_label,
      
      PPV_NPV = PPV_NPV)
    
    
    p = p +
      
      # Overlay center
      ggplot2::annotate("point", color = "red", alpha = .5, size = 1,
                        x = x_axis_position,
                        y = point_Prevalence) +
      
      # Text + rectangle
      ggforce::geom_mark_rect(label.colour = "black",
                              alpha = .04,
                              expand = uncertainty_prevalence_num,
                              aes(
                                x = x_axis_position,
                                y = point_Prevalence),
                              fill = "red", 
                              description = paste0(Details_point_PPV_NPV),
                              # con.border = "none", 
                              con.size = .2)

    
    
    # Output vars -------------------------------------------------------------
    
    return(p)
    
  }



#' .plot_overlay_line
#' 
#' Add line overlay to PPV_heatmap plot
#'
#' @param PPV_melted DF 
#' @param Min_Prevalence [x] out of y prevalence of disease
#' @param Max_Prevalence x out of [y] prevalence of disease
#' @param overlay_prevalence_1 vector with [x] out of y prevalence of disease
#' @param overlay_prevalence_2 vector with x out of [y] prevalence of disease
#' @param overlay_labels vector with labels for each overlay point
#' @param Max_FP .
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
           PPV_NPV,
           Min_Prevalence,
           Max_Prevalence,
           Max_FP,
           Sensitivity,
           overlay_prevalence_1,
           overlay_prevalence_2,
           
           overlay_position_FP,
           overlay_position_FN,
           
           overlay_labels,
           label_title,
           label_subtitle,
           legend_label,

           decimals_x,
           decimals_y,
           
           prevalence_label,
           
           x_axis_label,
           y_axis_label) {
    
    

    
  # Size of geom_mark_rect()
  if (uncertainty_prevalence == "high") {
    uncertainty_prevalence_num = .02
  } else if (uncertainty_prevalence == "low"){
    uncertainty_prevalence_num = .01
  }  
    
  
  # Create plot after adjusting overlay dimensions
  p = .plot_creation(
    PPV_melted = PPV_melted,
    Min_Prevalence = Min_Prevalence,
    Max_FP = Max_FP,
    Sensitivity = Sensitivity,
    decimals_x = decimals_x,
    decimals_y = decimals_y,
    prevalence_label = prevalence_label,
    
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label,
    
    legend_label = legend_label,
    label_subtitle = label_subtitle,
    label_title = label_title,
    label_caption = paste0("Sensitivity = ", Sensitivity, "%"))  
  
  
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
                               description = overlay_labels),
                             fill = "red", 
                             # con.border = "none", 
                             con.size = .2)

  
  # Output vars -------------------------------------------------------------
  
  return(p)
  
}



#' .translate_labels
#' 
#' Supports showing plot labels in Spanish (sp) or English (default)
#'
#' @param Language Can be Spanish "sp" or English (default)
#' @param Sensitivity . 
#' @param Max_FP .
#' @param PPV_NPV .
#'
#' @return A list with labels

.translate_labels <- function(Language, Sensitivity, Max_FP, PPV_NPV = "PPV") {
  
  
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
  
  list(
    label_caption = label_caption,
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label,
    prevalence_label = prevalence_label,
    legend_label = legend_label
  )
  
}
