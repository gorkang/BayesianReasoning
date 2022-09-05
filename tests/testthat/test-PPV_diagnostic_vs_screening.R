library(ggplot2)

testthat::test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_diagnostic_vs_screening(
    max_FP = 10,
    Sensitivity = 100,
    prevalence_screening_group = 1667,
    prevalence_diagnostic_group = 44,
    labels_prevalence = c("20 y.o.", "50 y.o.")
  )
  testthat::expect_true(ggplot2::is.ggplot(p))
  testthat::expect_identical(p$labels$y, "PPV")
  testthat::expect_identical(p$labels$x, "False Positive rate")
})


testthat::test_that("Plot is type GeomLine", {
  p <-
    BayesianReasoning::PPV_diagnostic_vs_screening(
      max_FP = 10,
      Sensitivity = 100,
      prevalence_screening_group = 1667,
      prevalence_diagnostic_group = 44,
      labels_prevalence = c("20 y.o.", "50 y.o.")
    )

  testthat::expect_identical(
    vapply(p$layers, function(x) {class(x$geom)[1]}, ""),
    "GeomLine")
})



testthat::test_that("Plot saved", {
  p <-
    BayesianReasoning::PPV_diagnostic_vs_screening(
      max_FP = 10,
      Sensitivity = 100,
      prevalence_screening_group = 1667,
      prevalence_diagnostic_group = 44,
      labels_prevalence = c("20 y.o.", "50 y.o."),
      folder = "."
    )

  file_name_test <- "FP_10_sens_100_screening_1667_diagnostic_44.png"
  expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
})
