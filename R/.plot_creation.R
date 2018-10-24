.plot_creation <- function(PPV_melted, label_title = "", label_subtitle = "") {

# DEBUG -------------------------------------------------------------------

    # label_title = ""
    # label_subtitle = ""

  
  # Global variables -------------------------------------------------------
  
  # Colors, breaks and steps for Prevalence and FP axis
  Paleta_DV = c( "white", "grey", "gray30", "yellowgreen", "chartreuse4")
  breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
  labels_DV = c(0, 25, 50, 75, 100)
  
  
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
    decimals_x <<- 2
  } else if  (Max_FP - Min_FP <= 5) {
    decimals_x <<- 1
  } else if  (Max_FP_FN - Min_FP_FN > 5) {
    decimals_x <<- 0
  }
  
  
      # Number of decimals y AXIS
      if (Max_Prevalence - Min_Prevalence < 9) {
        decimals_y = 1
      } else if  (Max_Prevalence - Min_Prevalence >= 9) {
        decimals_y = 0
      } 
      
  
# PPV ---------------------------------------------------------------------
  
  if (PPV_NPV == "PPV") {
      
      #Labels 
      if (Language == "sp") {
        
        label_caption = paste("Sensibilidad =", Sensitivity, "%")
        x_axis_label = "Tasa de Falsos Positivos"
        y_axis_label = "Prevalencia"
        prevalence_label <<- "de cada"
        legend_label = "VPP (%)\n"
        
      } else {
        
        label_caption = paste("Sensitivity =", Sensitivity, "%")
        x_axis_label = "False Positive rate"
        y_axis_label = "Prevalence"
        prevalence_label <<- "out of"
        legend_label = "PPV (%)\n"
        
      }
      
    # USE PPV_melted to get this!!!!
      # breaks_x = round(seq(0, Max_FP, Step_size_FP * 10), 2)
      # labels_x = paste0(round(seq(Min_FP, Max_FP, Step_size_FP * 10), 2), "%")
      breaks_x = round(seq(Min_FP, Max_FP, Step_size_FP * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")
      
      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
      # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
      
      # Create plot
        p = ggplot(PPV_melted, aes(FP, Prevalence)) + 
          geom_tile(aes(fill = PPV), colour = "white")


# NPV ---------------------------------------------------------------------

        
    } else if (PPV_NPV == "NPV") {

      
        #Labels 
        if (Language == "sp") {
          
          label_caption = paste("Tasa de Verdaderos Negativos =", (100 - Max_FP), "%")
          x_axis_label = "Tasa de Falsos Negativos"
          y_axis_label = "Prevalencia"
          prevalence_label <<- "de cada"
          legend_label = "VPN (%)\n"
          
        } else {
          
          label_caption = paste("True Negative Rate =", (100 - Max_FP), "%")
          x_axis_label = "False Negative rate"
          y_axis_label = "Prevalence"
          prevalence_label <<- "out of"
          legend_label = "NPV (%)\n"
          
        }
        
      # USE PPV_melted to get this!!!!
      # breaks_x = round(seq(0, Max_FN, Step_size_FN * 10), 2)
      # labels_x = paste0(round(seq(Min_FP, Max_FN, Step_size_FN * 10), 2), "%")
      breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
      labels_x = paste0(breaks_x, "%")

      breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
      labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
      # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
      
      # Create plot
      p = ggplot(PPV_melted, aes(FN, Prevalence)) + 
        geom_tile(aes(fill = NPV), colour = "white")
      
    }
    
  p <<- p + 
    scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) + 
    scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) +
    scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
    theme(text = element_text(size = 20),
          plot.caption = element_text(size = 16, color = "darkgrey"),
          axis.title.y = element_text(margin = margin(0,10,0,0)), 
          axis.title.x = element_text(margin = margin(10,0,0,0))) +
    labs(title = label_title,
         subtitle = label_subtitle, 
         caption = label_caption,
         x = x_axis_label, 
         y = y_axis_label) 

}
