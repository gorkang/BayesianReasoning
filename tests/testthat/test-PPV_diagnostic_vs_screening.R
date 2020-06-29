library(ggplot2)


testthat::test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_diagnostic_vs_screening(
    Max_FP = 10,
    Sensitivity = 100,
    prevalence_screening_group = 1667,
    prevalence_diagnostic_group = 44,
    labels_prevalence = c("20 y.o.", "50 y.o."),
    save_plot = FALSE
  )
  testthat::expect_true(ggplot2::is.ggplot(p))
  testthat::expect_identical(p$labels$y, "PPV")
  testthat::expect_identical(p$labels$x, "False Positive rate")

})


testthat::test_that("Plot is type GeomLine", {
  p <-
    BayesianReasoning::PPV_diagnostic_vs_screening(
      Max_FP = 10,
      Sensitivity = 100,
      prevalence_screening_group = 1667,
      prevalence_diagnostic_group = 44,
      labels_prevalence = c("20 y.o.", "50 y.o.")
    )
  
  testthat::expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]), "GeomLine")
  
})


# test_that("Saving plot",{
#
#   random_sensitivity = round(runif(1) * 100, 0)
#   p <- BayesianReasoning::PPV_diagnostic_vs_screening(Max_FP = 10, Sensitivity = random_sensitivity,
#                                                       prevalence_screening_group = 1667,
#                                                       prevalence_diagnostic_group = 44,
#                                                       labels_prevalence = c("20 y.o.", "50 y.o."),
#                                                       save_plot = TRUE)
#
#   file_name_expected = paste0("man/figures/diagnostic_vs_screening/FP_10_sens_", random_sensitivity, "_screening_1667_diagnostic_44.png")
#   testthat::expect_true(file.exists(file_name_expected))
#
# })
