#' BayesianReasoning
#'
#' Description
#'
#' @docType package
#' @name BayesianReasoning
#' @keywords internal
NULL
## FROM janitor: quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
# Avoid NOTE about "no visible binding for global variable":
  # PPV_diagnostic_vs_screening.R
  # if (getRversion() >= "2.15.1") utils::globalVariables(c("value", "prevalence", "PPV"))
  # PPV_heatmap.R
  # if (getRversion() >= "2.15.1") utils::globalVariables(c(".process_overlay_position_prevalence", "overlay_prevalence_2", ".createPPVmatrix", ".translate_labels", ".number_decimals_plot_axis", "Min_FP", "Min_FN", "Max_FN", ".plot_overlay_line", ".plot_overlay_area", "decimals_x", "decimals_y", "prevalence_label", ".plot_creation", "Step_size_FP", "p"))