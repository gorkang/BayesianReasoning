# PPV_Heatmap_FUN(2000, 100, 1, Language = "en", overlay = F) 

# TODO --------------------------------------------------------------------
# Add parameter to save

PPV_Heatmap_FUN <- function(Max_Prevalence, Sensitivity, Max_FP, Language = "en", overlay = F) {
  
  # Libraries ---------------------------------------------------------------
  
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(tidyverse, reshape2)
  
  
  
  # TODO --------------------------------------------------------------------
  
  # En la matriz, FP tiene 101 y Prev 100
  # Convertir en f(x), con parametros (Sensitivity, FP y Prev_y)? Ademas de idioma de labels (en / sp?) Ademas de presencia y parametros de overlay?
  # OVERLAYs via function
  
  # ***************************************************
  
  
  # Parameters --------------------------------------------------------------
  
  
  # USER DEFINED parameters -------------------------------------------------
  
  #Should we use overlays?
  overlay = overlay # F # If you activate the overlay, you have to set its parameters in the section # "OVERLAY INFO ABOUT A SPECIFIC TEST"
  
  #Language: (sp / en)
  Language = Language #"en"
  
  #Sensitivity: (0-100)
  Sensitivity = Sensitivity #80 # CHANGE ME
  
  #FP (1-Specificity): (0-100)
  Max_FP = Max_FP #5 # CHANGE ME
  
  # Prevalence (1 out of X): (1-Inf?)
  Max_Prevalence = Max_Prevalence #2000 # CHANGE ME
  
  
  # SYSTEM parameters (Do not modify) -------------------------------------------------
      
      #TEST Parameters **************
      
      # FP 
      Steps_FP = 100
      Step_size_FP = Max_FP/Steps_FP
      Min_FP = 0 #Step_size_FP #0
      #Step_size_FP = (Max_FP - Min_FP) / Steps_FP
      FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
      
      #CONDITION Parameters ***********
      
      #Prevalence_y - x out of y
      Prevalence_x = 1
      
      Min_Prevalence = 1
      Steps_Prevalence = 100
      Step_size_Prevalence = Max_Prevalence/Steps_Prevalence
      #Step_size_Prevalence = round((Max_Prevalence - Min_Prevalence) / Steps_Prevalence)
      Prevalence = seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence) #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
      
      
      #GRAPHIC Parameters *************
      
      #Labels 
      if (Language == "sp") {
        
        Title_label = paste("Sensibilidad =", Sensitivity, "%")
        x_axis_label = "Tasa de falsos positivos"
        y_axis_label = "Prevalencia"
        prevalence_label = "1 de cada"
        legend_label = "VPP (%)\n"
        
      } else {
        
        Title_label = paste("Sensitivity =", Sensitivity, "%")
        x_axis_label = "False Positive rate"
        y_axis_label = "Prevalence"
        prevalence_label = "1 out of"
        legend_label = "PPV (%)\n"
        
      }
      
      
  
  # Calculation -------------------------------------------------------------
  
      # We calculate the 100x100 PPV matrix
      # REMEMBER: Check the calculation ####
      PPV = (Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP) )
      
      
      #Label columns and rows of matrix
      colnames(PPV) = FP
      rownames(PPV) = Prevalence
      
      # Long format para ggplot Heatmap
      PPV_melted = melt(PPV)
      
      # Give names to variables
      names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV") 
      
      
  
  # PLOT --------------------------------------------------------------------
  
      #HEATMAP
      Paleta_DV = c( "white", "grey", "gray30", "yellowgreen", "chartreuse4")
      breaks_DV = c(0, 0.25, 0.5, 0.75, 1)
      labels_DV = c(0, 25, 50, 75, 100)
        
      #OLD
      # breaks_x = seq(0,100,10)
      # labels_x = paste0(seq(Min_FP, Max_FP, Step_size_FP*10), "%")
      # 
      # breaks_y = seq(0,100,10)
      # labels_y = paste(prevalence_label, seq(Min_Prevalence-1, Max_Prevalence, Step_size_Prevalence*10))
      
      # NEW
      breaks_x = seq(0, Max_FP, Step_size_FP * 10)
      labels_x = paste0(seq(Min_FP, Max_FP, Step_size_FP * 10), "%")
      
      breaks_y = seq(0, Max_Prevalence, Step_size_Prevalence * 10)
      labels_y = paste(prevalence_label, seq(Min_Prevalence - 1, Max_Prevalence, Step_size_Prevalence * 10))[-1]
      labels_y = c(paste(prevalence_label, "1"), labels_y) #We want the legend to start on 1 out of 1
      
      
      # PLOT
      p <<- ggplot(PPV_melted, aes(melted_FP, melted_Prevalence)) + geom_tile(aes(fill = melted_PPV), colour = "white") +
        ggtitle(Title_label) +  labs(x = x_axis_label, y = y_axis_label) + 
        scale_x_continuous(breaks = breaks_x, labels = labels_x, expand = c(0,0)) + 
        scale_y_continuous(breaks = breaks_y, labels = labels_y, expand = c(0,0)) +
        scale_fill_gradientn(colours = Paleta_DV, na.value = "transparent", breaks = breaks_DV, labels = labels_DV, limits = c(0,1), name = legend_label) +
        theme(text = element_text(size = 20), axis.title.y = element_text(margin = margin(0,10,0,0)), axis.title.x = element_text(margin = margin(10,0,0,0)))
      
      if (overlay == TRUE) {
        print(p)
        ggsave(paste0("outputs/", Max_Prevalence, "_", Sensitivity, "_", Max_FP, "_", Language, ".png"), p, dpi = 300, width = 14, height = 10)
        
      # p + annotate("segment", x = 14, xend = 14, y = 1667, yend = 227 , color = "red", alpha = .1, size=3) + 
      #   annotate("segment", x = 14, xend = 12, y = 227, yend = 69 , color = "red", alpha = .1, size=3) + 
      #   
      #   annotate("text", x = 14, y = 1667, label = "20 años", size = 5) + 
      #   annotate("text", x = 14, y = 227, label = "30 a.", size = 5) + 
      #   annotate("rect", 
      #            xmin = Min_FP_desired_in_overlay - (0.5 * Step_size_FP), 
      #            xmax = Max_FP_desired_in_overlay + (0.5 * Step_size_FP), 
      #            ymin = Min_Prevalence_desired_in_overlay, 
      #            ymax = Max_Prevalence_desired_in_overlay, fill = "red", alpha = .2)
      } else {
        print(p)
        ggsave(paste0("outputs/PPV_heatmap/", Max_Prevalence, "_", Sensitivity, "_", Max_FP, "_", Language, ".png"), p, dpi = 300, width = 14, height = 10)
      }
      
      
}

PPV_Heatmap_FUN(1000, 100, .2, Language = "en", overlay = F) 


# OVERLAY -----------------------------------------------------------------

