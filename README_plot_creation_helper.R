# To create man pages:

    # 1. Add Roxygen skeleton to the functions
    # 2. RUN: roxygen2::roxygenise()

# To build bundle package:devtools::build()

# To install from Github: devtools::install_github("gorkang/R_PPV_Plot") 


# MAN: # http://r-pkgs.had.co.nz/package.html

# README plot creation helper

        # PPV_diagnostic_vs_screening ---------------------------------------------
        
            source("PPV_diagnostic_vs_screening.R")
            
            PPV_diagnostic_vs_screening(Max_FP = 10, Sensitivity = 100, prevalence_screening_group = 1000, prevalence_diagnostic_group = 3)
        
        
        
        # PPV_heatmap -------------------------------------------------------------
        
            
            source("PPV_heatmap.R")
            
            # No overlay example
            PPV_heatmap(Max_Prevalence = 1000, Sensitivity = 100, Max_FP = 2, Language = "en", overlay = FALSE) 
            
            
            # Overlay example
            PPV_heatmap(Max_Prevalence = 1800, Sensitivity = 90, Max_FP = 15, 
                        label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
                        save_plot = TRUE, Language = "en", 
                        overlay = TRUE, 
                        overlay_labels = c("80", "70", "60", "50", "40", "30", "20  y.o."),
                        overlay_position_FP = c(7, 8, 9, 12, 14, 14),
                        overlay_position_Prevalence = c(26, 29, 44, 69, 227, 1667))
