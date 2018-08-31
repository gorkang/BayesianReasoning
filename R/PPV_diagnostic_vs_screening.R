# TODO ---------------------

# Add points where the real and study PPV are base on a parameter for the actual FP (include number!)


PPV_diagnostic_vs_screening <- function(Max_FP = 10, Sensitivity = 100, prevalence_screening_group = 100,  prevalence_diagnostic_group = 2, 
                                        labels_prevalence = c("Screening", "Diagnostic"),
                                        save_plot = TRUE) {
  # Prevalences (1 out of x)  
  
  library(tidyverse)
  
  # PARAMETERS --------------------------------------------------------------
  # Max_FP = 10
  # Sensitivity = 100
  # prevalence_screening_group = 100
  # prevalence_diagnostic_group = 2
  
  
  # FIXED parameters --------------------------------------------------------
  
  Prevalence_x = 1
  
  # FP 
  Steps_FP = 100
  Step_size_FP = Max_FP/Steps_FP
  Min_FP = 0 #Step_size_FP #0
  FP = seq(Min_FP, Max_FP, Step_size_FP)
  
  # Calculate PPVs ----------------------------------------------------------
  
  Real_Prevalence_PPV = list()
  Real_Prevalence_PPV = ((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((prevalence_screening_group - 1) * FP) )) * 100
  
  Study_Prevalence_PPV = list()
  Study_Prevalence_PPV = ((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((prevalence_diagnostic_group - 1) * FP) )) * 100
  
  
  # Build DF ----------------------------------------------------------------
  
  FINAL = FP %>% as_tibble() %>% 
    mutate(Real_Prevalence = Real_Prevalence_PPV,
           Study_Prevalence = Study_Prevalence_PPV) %>% 
    rename(FP = value) %>% 
    gather(Prevalence, PPV, 2:3) %>% 
    mutate(Prevalence = as.factor(Prevalence))
  
  
  # Plot --------------------------------------------------------------------
  
  # Labels_plot = c(paste0("Real: ", (1/prevalence_screening_group) * 100, "%"), paste0("Study: ", (1/(prevalence_diagnostic_group) * 100), "%"))
  Labels_plot = c(paste0(labels_prevalence[1], " prevalence: 1 out of ", prevalence_screening_group), paste0(labels_prevalence[2], " prevalence: 1 out of ", prevalence_diagnostic_group))
  
  p = ggplot(data = FINAL, aes(x = FP, y = PPV, colour = Prevalence)) + 
    geom_line(size = 1.5) + 
    scale_colour_hue(l = 50, labels = Labels_plot) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(name = "Positive Predictive Value", limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    theme(legend.position = "bottom") +  
    labs(title = "",
         subtitle = paste0("Sensitivity = ", Sensitivity, "%" ), 
         x = "False Positive rate", 
         color = "") 
  # labs(caption = "(based on data from ...)") + 
  # theme(plot.caption = element_text(size = 10))
  # guides(color=guide_legend("Prevalence of the "))
  
  print(p)
  
  # Parameters = paste0("FP", Max_FP, "_Sens", Sensitivity, "_PReal", prevalence_screening_group, "_PStudy", prevalence_diagnostic_group)
  # ggsave(paste0("outputs/diagnostic_vs_screening/", Parameters, ".svg"), Plot_PPV, dpi = 300, width = 14, height = 10)
  # ggsave(paste0("outputs/diagnostic_vs_screening/", Parameters, ".png"), Plot_PPV, dpi = 300, width = 14, height = 10)
 
  if (save_plot == TRUE) {
    
    print(p)
    plot_name = paste0("outputs/diagnostic_vs_screening/", "FP_", Max_FP, "_sens_", Sensitivity, "_screening_", prevalence_screening_group, "_diagnostic_", prevalence_diagnostic_group, ".png")
    ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
    cat("\n Plot created in: ", plot_name, "\n")
    
  } else {
    
    print(p)
    
  }
   
}

# EXAMPLE
# PPV_diagnostic_vs_screening(Max_FP = 10, Sensitivity = 100, prevalence_screening_group = 1667, prevalence_diagnostic_group = 44, labels_prevalence = c("20 y.o.", "50 y.o."))

# PPV_diagnostic_vs_screening(Max_FP = 10, Sensitivity = 100, prevalence_screening_group = 1000, prevalence_diagnostic_group = 3)
