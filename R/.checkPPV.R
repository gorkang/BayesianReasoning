.checkPPV <- function(round_decimals = 10) {
  
library(tidyverse)
# source("R/.createPPVmatrix.R")

Max_Prevalence = 9
Sensitivity = 90
Max_FP = 10

PPV_melted = .createPPVmatrix(Max_Prevalence = Max_Prevalence, Sensitivity = Sensitivity, Max_FP = Max_FP)
round_decimals = 2 # If round_decimals == 4,5,6... there is 1 or 2 fake FALSEs


# PPV ---------------------------------------------------------------------
    calculated_PPV_DF = PPV_melted %>% as_tibble() %>% 
      mutate(calc_PPV = ((Sensitivity * 1) / ((Sensitivity * 1) + (FP * (Prevalence - 1))))) %>% 
      mutate(TF = round(PPV, round_decimals) == round(calc_PPV, round_decimals))
    
    sum(calculated_PPV_DF$TF)
  
}
