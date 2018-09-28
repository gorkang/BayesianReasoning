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
.createPPVmatrix <- function(Max_Prevalence, Sensitivity, Max_FP) {
    
  # library(reshape2)
  
    # DEBUG -------------------------------------------------------------------
  
    # Max_Prevalence = 4
    # Sensitivity = 90
    # Max_FP = 10
    # PPV_NPV = "PPV" # NPV/PPV
  
    #TEST Parameters **************
    
    # False Positives (x axis) 
    Steps_FP <<- 100
    Step_size_FP <<- Max_FP/Steps_FP
    Min_FP <<- 0 
    FP = seq(Min_FP, Max_FP, Step_size_FP) #With (Max_FP-Step_size_FP) we get 100 FPs. If we use Max_FP instead we have 101 (because we start at 0!)
    
    #CONDITION Parameters ***********
    
    #Prevalence_y - x out of y
    Prevalence_x <<- 1
    Min_Prevalence <<- 1
    Steps_Prevalence <<- 100
    Step_size_Prevalence <<- Max_Prevalence/Steps_Prevalence
    Prevalence = seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence) #With (1 + Max_Prevalence) we get 101. If we use Max_Prevalence we get 100
    
    
    # PPV Calculation -------------------------------------------------------------
        
        # We calculate the 100x100 PPV matrix using %o% (outer)
        PPV <<- (Sensitivity * Prevalence_x) / ((Sensitivity * Prevalence_x) + ((Prevalence - 1) %o% FP) )
        
        #Label columns and rows of matrix
        colnames(PPV) = FP
        rownames(PPV) = Prevalence
        
        # Long format para ggplot Heatmap
        PPV_melted = reshape2::melt(PPV)
        
        # Give names to variables
        names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV") 
        
        PPV_melted <<- PPV_melted
}