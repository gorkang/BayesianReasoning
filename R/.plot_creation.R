.plot_creation <- function(PPV_melted) {
  
  # Colors, breaks and steps for Prevalence and FP axis
  Paleta_DV = c( "white", "grey", "gray30", "yellowgreen", "chartreuse4")
  breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
  labels_DV = c(0, 25, 50, 75, 100)
  
  breaks_x = seq(0, Max_FP, Step_size_FP * 10)
  labels_x = paste0(seq(Min_FP, Max_FP, Step_size_FP * 10), "%")
  breaks_y = seq(0, Max_Prevalence, Step_size_Prevalence * 10)
  labels_y = paste(prevalence_label, round(seq(Min_Prevalence - 1, Max_Prevalence, Step_size_Prevalence * 10),0))[-1]
  labels_y = c(paste(prevalence_label, "1"), labels_y) #We want the legend to start on 1 out of 1
  
  # Create plot
    p <<- ggplot(PPV_melted, aes(melted_FP, melted_Prevalence)) + 
      geom_tile(aes(fill = melted_PPV), colour = "white") +
      scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) + 
      scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) +
      scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
      theme(text = element_text(size = 20), axis.title.y = element_text(margin = margin(0,10,0,0)), axis.title.x = element_text(margin = margin(10,0,0,0))) +
      labs(title = label_title,
           subtitle = label_subtitle, 
           caption = label_caption,
           x = x_axis_label, 
           y = y_axis_label) 

}
