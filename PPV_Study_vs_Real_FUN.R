# TODO ---------------------

# Add poiints where the real and study PPV are base on a parameter for the actual FP (include number!)


library(tidyverse)
PPV_Study_vs_Real(10, 90, 25, 2)
#AD, Prevalence at 65 yo = ~10%

PPV_Study_vs_Real <- function(Max_FP = 10, Sensitivity = 100, Prevalence_Real = 100,  Prevalence_Study = 2) {
# Prevalences (1 out of x)  
  
# PARAMETERS --------------------------------------------------------------
  # Max_FP = 10
  # Sensitivity = 100
  # Prevalence_Real = 100
  # Prevalence_Study = 2


# FIXED parameters --------------------------------------------------------

  Prevalence_x = 1
  
  # FP 
  Steps_FP = 100
  Step_size_FP = Max_FP/Steps_FP
  Min_FP = 0 #Step_size_FP #0
  FP = seq(Min_FP, Max_FP, Step_size_FP)

# Calculate PPVs ----------------------------------------------------------
  
  Real_Prevalence_PPV = list()
  Real_Prevalence_PPV = ((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence_Real - 1) * FP) )) * 100
  
  Study_Prevalence_PPV = list()
  Study_Prevalence_PPV = ((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence_Study - 1) * FP) )) * 100


# Build DF ----------------------------------------------------------------

  FINAL = FP %>% as_tibble() %>% 
    mutate(Real_Prevalence = Real_Prevalence_PPV,
           Study_Prevalence = Study_Prevalence_PPV) %>% 
    rename(FP = value) %>% 
    gather(Prevalence, PPV, 2:3) %>% 
    mutate(Prevalence = as.factor(Prevalence))

  
# Plot --------------------------------------------------------------------
  
  Labels_plot = c(paste0("Real: ", (1/Prevalence_Real)*100, "%"), paste0("Study: ", (1/(Prevalence_Study)*100), "%"))
  
  apatheme = theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          text = element_text(family = 'Times'))
  
  Plot_PPV = ggplot(data = FINAL, aes(x = FP, y = PPV, colour = Prevalence)) + 
    geom_line(size = 1) + 
    scale_colour_hue(l = 50, labels = Labels_plot) +
    theme(legend.position = "top") +
    scale_y_continuous(name = "Positive Predictive Value", limits = c(0, 100)) +
    labs(x = "False Positive rate (%)") + apatheme
    # ggtitle("")
    # guides(color=guide_legend("Prevalence of the "))
  print(Plot_PPV)
  
  Parameters = paste0("FP", Max_FP, "_Sens", Sensitivity, "_PReal", Prevalence_Real, "_PStudy", Prevalence_Study)
  ggsave(paste0("Plots/", Parameters, ".svg"), Plot_PPV, width = 10, height = 10)
  ggsave(paste0("Plots/", Parameters, ".png"), Plot_PPV, width = 10, height = 10)
  
  cat("Sensitivity of test = ", Sensitivity)
}
