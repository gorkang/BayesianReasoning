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
min_possible_prevalence <- function(Sensitivity = 95, FP_test = 1, min_PPV_desired = 90) {


  # Fixed parameters --------------------------------------------------------

  Min_Prevalence <- 1
  Max_Prevalence <- 10000 # CHANGE ME
  Steps_Prevalence <- 10000
  Step_size_Prevalence <- Max_Prevalence / Steps_Prevalence
  Prevalence <- seq(Min_Prevalence, (1 + Max_Prevalence), Step_size_Prevalence)


  # Calculation -------------------------------------------------------------

  # We calculate the 100x100 PPV matrix
  PPV <- (Sensitivity * Min_Prevalence) / ((Sensitivity * Min_Prevalence) + ((Prevalence - 1) * FP_test))

  # Long format
  PPV_melted <- data.frame(melted_PPV = PPV, melted_Prevalence = seq_along(PPV))
  

  # Calculate prevalence
  output_prevalence <- max(PPV_melted$melted_Prevalence[PPV_melted$melted_PPV > (min_PPV_desired / 100)])
  # PPV_melted %>% filter(abs(melted_PPV - (min_PPV_desired / 100)) == min(abs(melted_PPV - (min_PPV_desired / 100)))) # Keep closest value to min_PPV_desired


  # Function output --------------------------------------------------------
  message("To reach a PPV of ", min_PPV_desired, "% when using a test with ", Sensitivity, "% Sensitivity and ", FP_test, "% False Positive Rate, you need a prevalence of at least 1 out of ", output_prevalence)
}
