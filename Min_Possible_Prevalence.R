
# Libraries ---------------------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(tidyverse, reshape2)


# FUNCTION : Given a FP and a desired PPV, what is the Maximum Prevalence of a Condition ####
Min_Possible_Prevalence <- function(Sensitivity, FP_test, min_PPV_desired) {

  #TEST Parameters **************
    
    #Sensitivity
    # Sensitivity = 100 # CHANGE ME
    
    #FP
    Max_FP = 100 # CHANGE ME
    
    Steps_FP = 1000
    Step_size_FP = Max_FP/Steps_FP
    Min_FP = 0 #Step_size_FP #0
    #Step_size_FP = (Max_FP - Min_FP) / Steps_FP
    FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
    
    #CONDITION Parameters ***********
    
    #Prevalence_y - x out of y
    Prevalence_x = 1
    
    Min_Prevalence = 1
    Max_Prevalence = 10000 # CHANGE ME
    Steps_Prevalence = 10000
    Step_size_Prevalence = Max_Prevalence/Steps_Prevalence
    #Step_size_Prevalence = round((Max_Prevalence - Min_Prevalence) / Steps_Prevalence)
    Prevalence = seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence) #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
    
    # ****************************************************************************************
    
    # We calculate the 100x100 PPV matrix
    PPV = (Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP) )
    #Label columns and rows of matrix
    colnames(PPV) = FP
    rownames(PPV) = Prevalence
    
    # Long format para ggplot Heatmap
    PPV_melted = melt(PPV)
    #Por algun motivo, melt a veces cambia nombres de variables. De este modo los fijamos
    names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV") 
    
    # Function output!
    cat("To get a PPV of", min_PPV_desired, "with a test with", Sensitivity, "% Sensitivity and", FP_test, "% False Positive Rate, you need a prevalence of at least 1 out of", max(PPV_melted$melted_Prevalence[PPV_melted$melted_PPV > min_PPV_desired & PPV_melted$melted_FP == FP_test]))

}      

#EXAMPLE
Min_Possible_Prevalence(100, 1, 0.75)
# Min_Possible_Prevalence(100, 0.1, 0.98)
