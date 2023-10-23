#' Plot PPV values for a diagnostic and a screening group
#'
#' Plot PPV associated to different levels of FP and a specific Sensitivity, for two different Prevalence groups.
#'
#' @param max_FP False positive rate (1-Specificity) [0-100].
#' @param Sensitivity Sensitivity of the test [0-100].
#' @param prevalence_screening_group Prevalence of the screening group, 1 out of x [1-Inf].
#' @param prevalence_diagnostic_group Prevalence of the diagnostic group, 1 out of x [1-Inf].
#' @param folder Where to save the plot (the filename would be automatically created using the plot parameters)
#' @param labels_prevalence Labels to use for both groups.
#'
#' @return Shows a plot or, if given a folder argument, saves a .png version of the plot
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line scale_colour_hue theme_minimal theme element_text scale_x_continuous scale_y_continuous labs
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rename
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#' # Example 1
#' PPV_diagnostic_vs_screening(
#'   max_FP = 10, Sensitivity = 100,
#'   prevalence_screening_group = 1500,
#'   prevalence_diagnostic_group = 3
#' )
#'
#' # Example 2. QWith custom labels
#' PPV_diagnostic_vs_screening(
#'   max_FP = 10, Sensitivity = 100,
#'   prevalence_screening_group = 1667,
#'   prevalence_diagnostic_group = 44,
#'   labels_prevalence = c("20 y.o.", "50 y.o.")
#' )
PPV_diagnostic_vs_screening <- function(max_FP = 10, Sensitivity = 100, prevalence_screening_group = 100, prevalence_diagnostic_group = 2,
                                        labels_prevalence = c("Screening", "Diagnostic"),
                                        # save_plot = FALSE,
                                        folder = "") {


  # FIXED parameters --------------------------------------------------------

  min_Prevalence <- 1

  # FP
  Steps_FP <- 100
  Step_size_FP <- max_FP / Steps_FP
  min_FP <- 0 # Step_size_FP #0
  FP <- seq(min_FP, max_FP, Step_size_FP)


  # Calculate PPVs ----------------------------------------------------------

  Real_Prevalence_PPV <- list()
  Real_Prevalence_PPV <- ((Sensitivity * min_Prevalence) / ((Sensitivity * min_Prevalence) + ((prevalence_screening_group - 1) * FP))) * 100

  Study_Prevalence_PPV <- list()
  Study_Prevalence_PPV <- ((Sensitivity * min_Prevalence) / ((Sensitivity * min_Prevalence) + ((prevalence_diagnostic_group - 1) * FP))) * 100


  # Build DF ----------------------------------------------------------------

  FINAL <- FP %>%
    as_tibble() %>%
    mutate(
      Real_Prevalence = Real_Prevalence_PPV,
      Study_Prevalence = Study_Prevalence_PPV
    ) %>%
    rename(FP = value) %>%
    gather(prevalence, PPV, 2:3) %>%
    mutate(prevalence = as.factor(prevalence))


  # Plot --------------------------------------------------------------------

  Labels_plot <- c(paste0(labels_prevalence[1], " prevalence: 1 out of ", prevalence_screening_group), paste0(labels_prevalence[2], " prevalence: 1 out of ", prevalence_diagnostic_group))

  p <- ggplot(data = FINAL, aes(x = FP, y = PPV, colour = prevalence)) +
    geom_line(linewidth = 1.5) +
    scale_colour_hue(l = 50, labels = Labels_plot) +
    theme_minimal() +
    theme(text = ggplot2::element_text(size = 20)) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(name = "Positive Predictive Value", limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    theme(legend.position = "bottom") +
    labs(
      title = "",
      subtitle = paste0("Sensitivity = ", Sensitivity, "%"),
      x = "False Positive rate",
      color = ""
    )


  if (folder != "") {
    print(p)
    plot_name <- paste0(folder, "/FP_", max_FP, "_sens_", Sensitivity, "_screening_", prevalence_screening_group, "_diagnostic_", prevalence_diagnostic_group, ".png")
    ggsave(plot_name, p, dpi = 300, width = 14, height = 10)
    message("\n Plot created in: ", plot_name, "\n")
  } else {
    print(p)
  }
}
