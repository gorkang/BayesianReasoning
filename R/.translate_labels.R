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
