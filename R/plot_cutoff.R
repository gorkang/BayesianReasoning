#' plot_cutoff
#' Create a cutoff plot, showing the healthy and sick distributions, and the 
#' consequences of different cutoff points
#'
#' @param prevalence prevalence of the disease
#' @param cutoff_point cutoff point to use
#' @param mean_sick mean for the sick people distribution
#' @param mean_healthy mean for the healthy people distribution
#' @param sd_sick sd for the sick people distribution
#' @param sd_healthy sd for the healthy people distribution
#' @param n_people number of people to use
#' @param add_table FALSE/TRUE: add gt table with Sensitivity, Specificity, etc.
#' @param output_filename NULL. If a filename, will save the plot
#'
#' @return A list with plots and table
#' @export
#' @importFrom stats rnorm
#' @importFrom ggtext element_markdown
#' @importFrom gt gt cell_text cells_column_labels cols_align cols_label fmt_markdown tab_style
#' @importFrom png readPNG
#' @importFrom scales comma
#' @importFrom ggplot2 ggplot_build
#'
#' @examples
#' \dontrun{
#' plot_cutoff(prevalence = 0.2)
#' }
plot_cutoff <- function(prevalence = 0.1, 
                        cutoff_point = 30, 
                        mean_sick = 35, 
                        mean_healthy = 20, 
                        sd_sick = 3, 
                        sd_healthy = 5,
                        n_people = 100000,
                        add_table = FALSE,
                        output_filename = NULL) {
  
  
  # DEBUG
  # prevalence = 0.1
  # cutoff_point = 40
  # mean_sick = 35
  # mean_healthy = 20
  # sd_sick = 3
  # sd_healthy = 5
  # n_people = 100000
  # output_filename = NULL
  
  SEED = 10
  
  # How many sick & healthy
  n_healthy = (1 - prevalence) * n_people
  n_sick = prevalence * n_people
  
  # Checks
  if (n_people >= 10^7) cli::cli_alert_warning("Lots of observations. Will take a few seconds to create the plot. Lower the n_people ({n_people})")
  if (prevalence < 0.005) cli::cli_alert_warning("With prevalence this low, you will hardly see the sick distribution ({prevalence})")
  
  
  
  # Colors ------------------------------------------------------------------
  
  # Base 16 (0123456789abcdef), from 00 (lower) to ff (higher) 
  DF_colors = tibble::tibble(
    classification = c("TN", "FP", "FN", "TP"), 
    fill_str = c("#00000040", "#990ff7ff", "#000000ee", "#990ff740")
  )
  
  color_TN = DF_colors[DF_colors$classification == "TN",]$fill_str
  color_FP = DF_colors[DF_colors$classification == "FP",]$fill_str
  color_FN = DF_colors[DF_colors$classification == "FN",]$fill_str
  color_TP = DF_colors[DF_colors$classification == "TP",]$fill_str
  
  # Formatted names for title
  TP_title = paste0("<span style = 'color: ", color_TP, ";'>True Positives</span>")
  FN_title = paste0("<span style = 'color: ", color_FN, ";'>False Negatives</span>")
  FP_title = paste0("<span style = 'color: ", color_FP, ";'>False Positives</span>")
  TN_title = paste0("<span style = 'color: ", color_TN, ";'>True Negatives</span>")
  
  
  
  
  # Create distributions ----------------------------------------------------
  
  set.seed(SEED)
  data_sick = round(rnorm(n_sick, mean = mean_sick , sd = sd_sick))
  set.seed(SEED)
  data_healthy = round(rnorm(n_healthy, mean = mean_healthy , sd = sd_healthy))
  
  
  DF = tibble::tibble(type = c(rep("healthy", n_healthy), rep("sick", n_sick)),
                      test_result = c(data_healthy, data_sick)) |> 
    dplyr::mutate(test_result_dico = ifelse(test_result >= cutoff_point, "positive", "negative")) |> 
    dplyr::mutate(classification = 
                    dplyr::case_when(
                      type == "healthy" & test_result_dico == "negative" ~ "TN",
                      type == "healthy" & test_result_dico == "positive" ~ "FP",
                      type == "sick" & test_result_dico == "negative" ~ "FN",
                      type == "sick" & test_result_dico == "positive" ~ "TP",
                    ),
                  fill_str = 
                    dplyr::case_when(
                      classification == "TN" ~ color_TN,
                      classification == "FP" ~ color_FP,
                      classification == "FN" ~ color_FN,
                      classification == "TP" ~ color_TP,
                    ))
  
  
  # Table -------------------------------------------------------------------
  
  DF_table_raw1 = DF |> 
    dplyr::group_by(classification) |> 
    dplyr::summarise(N = dplyr::n(), .groups = "drop") 
  
  table_template = tibble::tibble(TN = 0, FP = 0, FN = 0, TP = 0)
  
  DF_table_raw2 = 
    DF_table_raw1 |> 
    tidyr::pivot_wider(names_from = classification, values_from = N)
  
  DF_table = 
    DF_table_raw2 |> 
    # Add columns if they do not exist (e.g. when 0 FP)
    tibble::add_column(!!!table_template[setdiff(names(table_template), names(DF_table_raw2))]) |> 
    # Calculate
    dplyr::mutate(Sensitivity = TP/ (TP + FN),
                  Specificity = TN / (TN + FP),
                  PPV = TP / (TP + FP),
                  NPV = TN / (TN + FN),
                  Prevalence = prevalence)
  
  
  # Formatted cells for table
  TP = paste0("<span style = 'color: ", color_TP, ";'>TP", "</span><BR>", format(DF_table$TP, big.mark = ",", scientific = FALSE))
  FN = paste0("<span style = 'color: ", color_FN, ";'>FN", "</span><BR>", format(DF_table$FN, big.mark = ",", scientific = FALSE))
  FP = paste0("<span style = 'color: ", color_FP, ";'>FP", "</span><BR>", format(DF_table$FP, big.mark = ",", scientific = FALSE))
  TN = paste0("<span style = 'color: ", color_TN, ";'>TN", "</span><BR>", format(DF_table$TN, big.mark = ",", scientific = FALSE))
  Sensitivity = paste0("**Sensitivity**<BR>", round(DF_table$Sensitivity, 3) * 100, "%")
  Specificity = paste0("**Specificity**<BR>", round(DF_table$Specificity, 3) * 100, "%")
  PPV = paste0("**PPV**<BR>", round(DF_table$PPV, 3) * 100, "%")
  NPV = paste0("**NPV**<BR>", round(DF_table$NPV, 3) * 100, "%")
  
  
  TABLE_raw = tibble::tibble(X = c("**test +**", "**test -**", ""),
                             `Sick` = c(TP, FN, Sensitivity), 
                             `Healthy` = c(FP, TN, Specificity), 
                             Y = c(PPV, NPV, ""))
  
  TABLE_gt =
    TABLE_raw |> 
    gt::gt() |> 
    gt::fmt_markdown(dplyr::everything()) |> 
    gt::cols_align(align = "center") |> 
    gt::cols_label(X = "", Y= "") |> 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )
  
  
  
  # Initial Plot ---------------------------------------------------------------
  
  # Number of individual bins
  bins_histogram = max(DF$test_result) - min(DF$test_result)
  binwidth = 1
  
  # Base plot
  plot =
    DF |> 
    ggplot2::ggplot(ggplot2::aes(test_result)) +
    
    # Histograms
    ggplot2::geom_histogram(data = subset(DF, classification == 'TN'), ggplot2::aes(fill = fill_str), bins = bins_histogram, binwidth = 1, show.legend = FALSE)  +
    ggplot2::geom_histogram(data = subset(DF, classification == 'TP'), ggplot2::aes(fill = fill_str), bins = bins_histogram, binwidth = 1, show.legend = FALSE)  +
    ggplot2::geom_histogram(data = subset(DF, classification == 'FN'), ggplot2::aes(fill = fill_str), bins = bins_histogram, binwidth = 1, show.legend = FALSE)  +
    ggplot2::geom_histogram(data = subset(DF, classification == 'FP'), ggplot2::aes(fill = fill_str), bins = bins_histogram, binwidth = 1, show.legend = FALSE)  +
    
    # Outlines (healthy, sick)
    # stat_bin(data = subset(DF, classification %in% c('FN', 'TP')), geom = "step", direction = "mid", aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    # stat_bin(data = subset(DF, classification %in% c('TN', 'FP')), geom = "step", direction = "mid", aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    ggplot2::stat_bin(data = subset(DF, classification %in% c('TN')), geom = "step", direction = "mid", ggplot2::aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    ggplot2::stat_bin(data = subset(DF, classification %in% c('TP')), geom = "step", direction = "mid", ggplot2::aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    ggplot2::stat_bin(data = subset(DF, classification %in% c('FN')), geom = "step", direction = "mid", ggplot2::aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    ggplot2::stat_bin(data = subset(DF, classification %in% c('FP')), geom = "step", direction = "mid", ggplot2::aes(linetype = type), binwidth = binwidth, show.legend = FALSE) +
    
    
    ggplot2::geom_vline(xintercept = cutoff_point - 0.5, linetype = "dashed") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(caption = paste0(format(n_people, big.mark = ",", scientific = FALSE), " people, ", "prevalence 1 out of ", 1/DF_table$Prevalence, "\n",
                                   format(n_sick, big.mark = ",", scientific = FALSE), " sick (M = ", mean_sick, ", SD = ", sd_sick, ")\n",
                                   format(n_healthy, big.mark = ",", scientific = FALSE) , " healthy (M = ", mean_healthy, ", SD = ", sd_healthy, ")"
                                   # "Sensitivity = ", round(DF_table$Sensitivity * 100, 0), "% Specificity = ", round(DF_table$Specificity * 100, 0), "% \n",
                                   # "PPV = ", round(DF_table$PPV * 100, 0), "% NPV = ", round(DF_table$NPV * 100, 0), "%"
    )) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_fill_identity() 
  
  
  # Max values for healthy and sick -----------------------------------------
  
  ggplot_layout = ggplot2::ggplot_build(plot)$layout
  ggplot_data = ggplot2::ggplot_build(plot)$data
  
  data_1 = ggplot_data[[1]][which.max(ggplot_data[[1]]$count),]
  data_2 = ggplot_data[[2]][which.max(ggplot_data[[2]]$count),]
  data_3 = ggplot_data[[3]][which.max(ggplot_data[[3]]$count),]
  data_4 = ggplot_data[[4]][which.max(ggplot_data[[4]]$count),]
  
  max_counts = rbind(data_1, data_2, data_3, data_4) |> 
    dplyr::left_join(DF_colors, by = c("fill" = "fill_str"))
  
  range_x = ggplot_layout$panel_params[[1]]$x.range
  
  # Looking for the max value in the healthy or sick histograms
  max_count_healthy = max(max_counts[max_counts$classification %in% c("TN", "FP"), "count"])
  max_count_sick = max(max_counts[max_counts$classification %in% c("TP", "FN"), "count"])
  
  TN_x = max(max_counts[max_counts$classification %in% c("TN"), "x"])
  FN_x = max(max_counts[max_counts$classification %in% c("FN"), "x"])
  FP_x = max(max_counts[max_counts$classification %in% c("FP"), "x"])
  TP_x = max(max_counts[max_counts$classification %in% c("TP"), "x"])
  
  
  if (FP_x == TP_x) TP_x = TP_x * 1.05
  if (FN_x == TN_x) TN_x = TN_x * .95
  
  
  # Plot annotations --------------------------------------------------------
  
  final_plot =
    plot +
    ggplot2::annotate(x = cutoff_point -.5, y = max_count_healthy *.95, label = "Cutoff point", vjust = 2, geom = "text", angle = 90) +
    # annotate(x = mean_healthy, y = max_count_healthy / 2, label = "Healthy", vjust = 2, geom = "text") +
    # annotate(x = mean_sick, y = max_count_sick / 2, label = "Sick", vjust = 2, geom = "text", color = "white") +
    
    # Sick / Healthy
    ggplot2::annotate(x = mean_healthy, y = max_count_healthy, label = "Healthy", vjust = -1, geom = "text", color = "#000000") +
    ggplot2::annotate(x = mean_sick, y = max_count_sick, label = "Sick", vjust = -1, geom = "text", color = "#222222") +
    
    # TN / FN / FP / TP
    ggplot2::annotate(x = TN_x, y = 0, label = "TN", vjust = 2, geom = "text", color = color_TN) +
    ggplot2::annotate(x = FN_x, y = 0, label = "FN", vjust = 2, geom = "text", color = color_FN) +
    ggplot2::annotate(x = FP_x, y = 0, label = "FP", vjust = 2, geom = "text", color = color_FP) +
    ggplot2::annotate(x = TP_x, y = 0, label = "TP", vjust = 2, geom = "text", color = color_TP) +
    
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = paste0(TP_title, ", ", FN_title, ", ", FP_title, ", ",TN_title),
                  subtitle = "Depending on a Cutoff point<BR><BR>") +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown()) +
    ggplot2::scale_y_continuous(labels = scales::comma)
  
  
  # Combine table and plot --------------------------------------------------
  
  if (add_table == TRUE) {
  
    # Save and read image
    name_file = paste0(tempfile(), ".png")
    TABLE_gt |> gt::gtsave(name_file, quiet = TRUE)
    grob_table <- grid::rasterGrob(png::readPNG(name_file), interpolate=TRUE)
    
    # Add image of gt table to plot
    final_plot = 
      final_plot + 
      ggplot2::annotation_custom(
        grob_table,
        # xmin = min(range_x),
        xmax = max(range_x) / 4 + min(range_x),
        ymin = max_count_healthy * .8
        # ymax = max_count_healthy * .5
      )
    
  }
  
  # Save plot
  if (!is.null(output_filename)) ggplot2::ggsave(output_filename, final_plot,  bg = "white", width = 16, height = 12)
  
  
  # Output ------------------------------------------------------------------
  
  OUTPUT = list(TABLE_gt = TABLE_gt,
                final_plot = final_plot)
  
  return(OUTPUT)
  
}




#' remove_layers_cutoff_plot
#' Remove layers from a cutoff plot. This is useful to show how different things
#' are calculated (e.g. Sensitivity)
#'
#' @param cutoff_plot A plot_cutoff() plot
#' @param delete_what Elements to delete (i.e. FP, FN, TP, TN)
#' @param silent TRUE do not show debug info
#'
#' @return a cutoff plot without the elements deleted
#' @export
#'
#' @examples
#' \dontrun{
#' PLOT = plot_cutoff(prevalence = 0.2)
#' remove_layers_cutoff_plot(PLOT$final_plot, delete_what = c("FN", "TP")) + 
#' ggplot2::labs(subtitle = "Specificity = TN/(TN+FP)")
#' }
remove_layers_cutoff_plot <- function(cutoff_plot, delete_what, silent = TRUE) {
  
  layers <- lapply(cutoff_plot$layers, function(x) {
    
    # GeomStep, GeomBar
    if (class(x$geom)[1] %in% c("GeomStep", "GeomBar")) {
      
      if (unique(x$data$classification)[1] %in% delete_what) {
        if (silent == FALSE) cli::cli_alert_info("DELETE: {class(x$geom)[1]} | {unique(x$data$classification)[1]}")
        NULL
      } else {
        if (silent == FALSE) cli::cli_alert_info("{class(x$geom)[1]} | {unique(x$data$classification)[1]}")
        x
      }
      
      # GeomText  
    } else if (class(x$geom)[1] %in% c("GeomText")) {
      
      if (x$aes_params$label %in% delete_what) {
        if (silent == FALSE) cli::cli_alert_info("DELETE: {class(x$geom)[1]} | {x$aes_params$label}")
        NULL
        
      } else if (x$aes_params$label == "Cutoff point") {
        if (silent == FALSE) cli::cli_alert_info("DELETE: {class(x$geom)[1]} | {x$aes_params$label}")
        NULL
        # Delete the Healthy text when we delete TN
      } else if (x$aes_params$label == "Healthy" & all(c("TN", "FP") %in% delete_what)) {
        if (silent == FALSE) cli::cli_alert_info("DELETE: {class(x$geom)[1]} | {x$aes_params$label} | delete_what: {c('TN', 'FP') %in% delete_what}")
        NULL
      } else if (x$aes_params$label == "Sick" & all(c("TP", "FN") %in% delete_what)) {
        if (silent == FALSE) cli::cli_alert_info("DELETE: {class(x$geom)[1]} | {x$aes_params$label} | delete_what: {c('TP', 'FN') %in% delete_what}")
        NULL
      } else {
        if (silent == FALSE) cli::cli_alert_info("{class(x$geom)[1]} | {x$aes_params$label}")
        x
      }
      
    } else {
      if (silent == FALSE) cli::cli_alert_info("{class(x$geom)[1]} | {unique(x$data$classification)[1]} | {x$aes_params$label}")
      x
    }
  })
  layers <- layers[!sapply(layers, is.null)]
  cutoff_plot$layers <- layers
  
  return(cutoff_plot)
  
}
