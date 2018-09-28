#' Title
#'
#' @param Max_Prevalence 
#' @param Sensitivity 
#' @param Max_FP 
#'
#' @return
#' @export
#' @importFrom reshape2 melt
#'
#' @examples
.createPPVmatrix <- function(Max_Prevalence, Sensitivity, Max_FP) {
    
  library(reshape2)
  
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
    
    # NPV Calculation -------------------------------------------------------------
        
        # As NPV is more dependent on Sensitivity, the x axis should show Sensitivity, and FP should be fixed
        NPV <<- ((Prevalence - 1) %o% (100 - FP)) / (((Prevalence - 1) %o% (100 - FP)) + (Prevalence_x * (100 - Sensitivity)))
    
        #Label columns and rows of matrix
        colnames(NPV) = FP
        rownames(NPV) = Prevalence
        
        # Long format para ggplot Heatmap
        NPV_melted = melt(NPV)
        
        # Give names to variables
        names(NPV_melted) = c("melted_Prevalence", "melted_FP", "melted_NPV") 
  
        NPV_melted <<- NPV_melted

}