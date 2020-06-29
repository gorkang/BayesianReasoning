#' Show minimum possible prevalence given the test characteristics
#'
#' Given a FP and a desired PPV, what is the Minimum Prevalence of a Condition 
#'
#' @param Sensitivity Sensitivity of the test: [0-100]
#' @param FP_test False positive rate (1-Specificity): [0-100]
#' @param min_PPV_desired Which PPV is what you consider the minimum to trust a positive result in the test: [0-100]
#'
#' @return A description showing the minimum necessary prevalence.
#' @export
#' @importFrom reshape2 melt
#'
#' @examples
#' 
#' # Example 1
#' min_possible_prevalence(Sensitivity = 99.9, FP_test = .1, min_PPV_desired = 70)
#' "To reach a PPV of 70 when using a test with 99.9 % Sensitivity and 0.1 % False Positive Rate, 
#' you need a prevalence of at least 1 out of 429"
#' 
#' # Example 2
#' min_possible_prevalence(100, 0.1, 98)
#' "To reach a PPV of 98 when using a test with 100 % Sensitivity and 0.1 % False Positive Rate, 
#' you need a prevalence of at least 1 out of 21"
min_possible_prevalence <- function(Sensitivity, FP_test, min_PPV_desired) {

  #TEST Parameters **************
    #FP
    Max_FP = 100
    Steps_FP = 1000
    Step_size_FP = Max_FP / Steps_FP
    Min_FP = 0
    FP = seq(Min_FP, Max_FP, Step_size_FP)

    #CONDITION Parameters ***********

    #Prevalence_y - x out of y
    Prevalence_x = 1
    Min_Prevalence = 1
    Max_Prevalence = 10000 # CHANGE ME
    Steps_Prevalence = 10000
    Step_size_Prevalence = Max_Prevalence / Steps_Prevalence
    Prevalence = seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence)

    # ****************************************************************************************

    # We calculate the 100x100 PPV matrix
    PPV = (Sensitivity * Prevalence_x) / ( (Sensitivity * Prevalence_x) + ( (Prevalence - 1) %o% FP) )
    #Label columns and rows of matrix
    colnames(PPV) = FP
    rownames(PPV) = Prevalence

    # Long format para ggplot Heatmap
    PPV_melted = reshape2::melt(PPV)
    
    # Rename columns
    names(PPV_melted) = c("melted_Prevalence", "melted_FP", "melted_PPV")

    # Calculate prevalence
    output_prevalence = max(PPV_melted$melted_Prevalence[PPV_melted$melted_PPV > (min_PPV_desired / 100) & PPV_melted$melted_FP == FP_test])

    # Function output!
    message("To reach a PPV of ", min_PPV_desired, "% when using a test with ", Sensitivity, "% Sensitivity and ", FP_test, "% False Positive Rate, you need a prevalence of at least 1 out of ", output_prevalence)
}
