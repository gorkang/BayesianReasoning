#' .plot_creation
#'
#' @param PPV_melted 
#' @param label_title 
#' @param label_subtitle 
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin
#'
#' @examples
.plot_creation <- function(PPV_melted, label_title = "", label_subtitle = "") {

  # DEBUG -------------------------------------------------------------------
  
      # label_title = ""
      # label_subtitle = ""
  
    
    # Global variables -------------------------------------------------------
    
    # Colors, breaks and steps for Prevalence and FP axis
    Paleta_DV = c( "white", "grey", "gray30", "yellowgreen", "chartreuse4")
    breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
    labels_DV = c(0, 25, 50, 75, 100)
    
    
    
  # PPV ---------------------------------------------------------------------
     
    if (PPV_NPV == "PPV") {
         
      # USE PPV_melted to get this!!!!
        # breaks_x = round(seq(0, Max_FP, Step_size_FP * 10), 2)
        # labels_x = paste0(round(seq(Min_FP, Max_FP, Step_size_FP * 10), 2), "%")
        breaks_x = round(seq(Min_FP, Max_FP, Step_size_FP * 10), decimals_x)
        labels_x = paste0(breaks_x, "%")
        
        breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
        labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
        # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
        
        # Create plot
          p = ggplot2::ggplot(PPV_melted, aes(FP, Prevalence)) + 
            ggplot2::geom_tile(aes(fill = PPV), colour = "white")
  
  
  # NPV ---------------------------------------------------------------------
  
          
      } else if (PPV_NPV == "NPV") {
  

        # USE PPV_melted to get this!!!!
        # breaks_x = round(seq(0, Max_FN, Step_size_FN * 10), 2)
        # labels_x = paste0(round(seq(Min_FP, Max_FN, Step_size_FN * 10), 2), "%")
        breaks_x = round(seq(Min_FN, Max_FN, Step_size_FN * 10), decimals_x)
        labels_x = paste0(breaks_x, "%")
  
        breaks_y = round(unique(PPV_melted$Prevalence)[c(seq(1, steps_matrix, 10), 101)], decimals_y)
        labels_y = paste(Min_Prevalence, prevalence_label, breaks_y)
        # labels_y = paste(Min_Prevalence, prevalence_label, round(unique(PPV_melted$Prevalence)[c(seq(1, 100, 10), 101)], 0))
        
        # Create plot
        p = ggplot2::ggplot(PPV_melted, aes(FN, Prevalence)) + 
          ggplot2::geom_tile(aes(fill = NPV), colour = "white")
        
      }
      
    p = p + 
      ggplot2::scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) + 
      ggplot2::scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) +
      ggplot2::scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
      ggplot2::theme(text = element_text(size = 20),
            plot.caption = element_text(size = 16, color = "darkgrey"),
            axis.title.y = element_text(margin = margin(0,10,0,0)), 
            axis.title.x = element_text(margin = margin(10,0,0,0))) +
      ggplot2::labs(title = label_title,
           subtitle = label_subtitle, 
           caption = label_caption,
           x = x_axis_label, 
           y = y_axis_label) 
  


  # Output vars -------------------------------------------------------------
    
    p <<- p

}
