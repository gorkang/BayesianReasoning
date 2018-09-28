.checkPPV <- function(round_decimals = 10) {
  
library(tidyverse)
# source("R/.createPPVmatrix.R")

# Max_Prevalence = 9
# Sensitivity = 90
# Max_FP = 10

.createPPVmatrix(Max_Prevalence = 9, Sensitivity = 90, Max_FP = 10)
# round_decimals = 10 # If round_decimals == 4,5,6... there is 1 or 2 fake FALSEs  


# PPV ---------------------------------------------------------------------
    xxx2 = PPV_melted %>% as_tibble() %>% 
      mutate(calc_PPV = ((Sensitivity * 1) / ((Sensitivity * 1) + (melted_FP * (melted_Prevalence - 1))))) %>% 
      mutate(TF = round(melted_PPV, round_decimals) == round(calc_PPV, round_decimals))
    
    # summary(xxx2)
    sum(xxx2$TF)
    
    
    
    # ALL TRUES
    # expect_equivalent(sum(xxx2$TF), 10201)
    
    
    # ggplot(xxx2, aes(melted_FP, melted_Prevalence, color = calc_PPV)) +
    #   geom_point()
    # 
    # ggplot(xxx2, aes(melted_FP, melted_Prevalence, color = melted_PPV)) +
    #   geom_point()


# NPV ---------------------------------------------------------------------
    # xxx2 = NPV_melted %>% as_tibble() %>% 
    #   mutate(TRUE_NEG = ((100 - melted_FP) * (melted_Prevalence - 1))) %>% 
    #   mutate(
    #     calc_NPV = 
    #            (
    #              ((100 - melted_FP) * (melted_Prevalence - 1)) / 
    #            (((100 - melted_FP) * (melted_Prevalence - 1)) + ((100 - Sensitivity) * 1))
    #            )) %>% 
    #   mutate(TF = round(melted_NPV, round_decimals) == round(calc_NPV, round_decimals))
    # 
    # summary(xxx2)
    # 
    # 
    # 
    # ggplot(xxx2, aes(melted_FP, melted_Prevalence, color = calc_NPV)) +
    #   geom_point()
    # 
    # ggplot(xxx2, aes(melted_FP, melted_Prevalence, color = melted_NPV)) +
    #   geom_point()
    # 
    
}
