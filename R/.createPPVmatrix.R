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
#' .createPPVmatrix(1000, 100, 2)
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
  
  # False Positives (x axis) 
  Steps_FP <<- 100
  Step_size_FP <<- Max_FP/Steps_FP
  Min_FP <<- 0 
  FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
  
  # Sentisitivy
  Max_FN <<- (100 - Sensitivity)
  Steps_FN <<- 100
  Step_size_FN <<- Max_FN/Steps_FN
  Min_FN <<- 0
  
  Sensitivity_range = seq(Min_FN, Max_FN, Step_size_FN)
  
  
  #CONDITION Parameters ***********
  
  Min_Prevalence <<- Min_Prevalence
  Prevalence_x <<- Min_Prevalence
  Max_Prevalence <<- Max_Prevalence
  Steps_Prevalence <<- 100
  Step_size_Prevalence <<- (Max_Prevalence - Min_Prevalence)/Steps_Prevalence
  Prevalence <<- round(seq(Min_Prevalence, (Max_Prevalence), Step_size_Prevalence), 0)  #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
  
  # Prevalence_percent = Prevalence/Max_Prevalence
  # Prevalence_percent * 1000
  
  
  # PPV Calculation -------------------------------------------------------------
  
  # We calculate the 100x100 PPV matrix using %o% (outer)
  PPV <<- round((Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP)), 2)
  
  NPV <<- round(((Prevalence - Min_Prevalence) * (100 - Max_FP)) / (((Prevalence - Min_Prevalence) * (100 - Max_FP)) + (Prevalence  %o% Sensitivity_range)), 2)
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