#' Title
#'
#' @param PPV_NPV 
#' @param Min_FP 
#' @param Max_FP 
#' @param Min_FN 
#' @param Max_FN 
#' @param Min_Prevalence 
#' @param Max_Prevalence 
#'
#' @return
#' @export
#'
#' @examples
.number_decimals_plot_axis <- function(PPV_NPV, Min_FP, Max_FP, Min_FN, Max_FN, Min_Prevalence, Max_Prevalence) {
  
    # The number of decimal places in the x and y axis label depends on how wide the range is
    
    # The vars to calculate range depend on PPV NPV
    if (PPV_NPV == "PPV") {
      Max_FP_FN = Max_FP
      Min_FP_FN = Min_FP
    } else if (PPV_NPV == "NPV") {
      Max_FP_FN = Max_FN
      Min_FP_FN = Min_FN
    }
    
    # Number of decimals x AXIS
    if (Max_FP_FN - Min_FP_FN < 1) {
      decimals_x = 2
    } else if  (Max_FP - Min_FP <= 5) {
      decimals_x = 1
    } else if  (Max_FP_FN - Min_FP_FN > 5) {
      decimals_x = 0
    }
    
    
    # Number of decimals y AXIS
    if (Max_Prevalence - Min_Prevalence < 9) {
      decimals_y = 1
    } else if  (Max_Prevalence - Min_Prevalence >= 9) {
      decimals_y = 0
    } 
    

  # Output vars -------------------------------------------------------------
  
    decimals_x <<- decimals_x
    decimals_y <<- decimals_y
  
    # decimals = c(decimals_x, decimals_y)
    # decimals
}