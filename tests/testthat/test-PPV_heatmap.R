library(ggplot2)


testthat::test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en",
    PPV_NPV = "PPV"
  )
  testthat::expect_true(ggplot2::is.ggplot(p))
  testthat::expect_identical(p$labels$y, "Prevalence")
  testthat::expect_identical(p$labels$caption, "Sensitivity = 100%")
  testthat::expect_identical(p$labels$fill, "PPV")
  
})


testthat::test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en",
    PPV_NPV = "NPV"
  )
  testthat::expect_true(ggplot2::is.ggplot(p))
  testthat::expect_identical(p$labels$caption, "Specificity = 98%")
  testthat::expect_identical(p$labels$fill, "NPV")
  
})


testthat::test_that("Spanish translation works'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "sp"
  )
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Tasa de Falsos Positivos")
  
})


testthat::test_that("Spanish NPV'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    PPV_NPV = "NPV",
    Language = "sp"
  )
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Tasa de Falsos Negativos")
  
})


testthat::test_that("Plot is type GeomTile", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en"
  )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]), 
    "GeomTile")
  
})


testthat::test_that("Plot with area overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 1200,
      Sensitivity = 81,
      Max_FP = 5,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 1,
      overlay_prevalence_2 = 68
    )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  
})

testthat::test_that("PPV calculation with area overlay, low uncertainty, and decimals in y axis", {
  p <-
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 8,
      Sensitivity = 81,
      Max_FP = 5,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 2,
      overlay_prevalence_2 = 8,
      uncertainty_prevalence = "low",
      PPV_NPV = "PPV"
    )
  testthat::expect_identical(
    p$layers[[3]]$computed_geom_params$description,
    "40 y.o.\n2 out of 8\nFP = 4.8%\nPPV = 85%")

  # Decimal breaks y axis
  testthat::expect_identical(
    p$plot_env$breaks_y,
    c(1.0, 1.7, 2.4, 3.1, 3.8, 4.5, 5.2, 5.9, 6.6, 7.3, 8.0))

})



testthat::test_that("NPV calculation with area overlay and low uncertainty", {
  p <-
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 1200,
      Sensitivity = 81,
      Max_FP = 5,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 67,
      overlay_prevalence_2 = 68,
      uncertainty_prevalence = "low",
      PPV_NPV = "NPV"
    )
  testthat::expect_identical(
    p$layers[[3]]$computed_geom_params$description,
    "40 y.o.\n67 out of 68\nFN = 1%\nNPV = 59%")

})


# Line overlay ------------------------------------------------------------

testthat::test_that("Plot with line overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 1800,
      Sensitivity = 90,
      Max_FP = 15,
      label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
      overlay = "line",
      uncertainty_prevalence = "low",
      overlay_labels = c(
        "80 y.o.",
        "70 y.o.",
        "60 y.o.",
        "50 y.o.",
        "40 y.o.",
        "30 y.o.",
        "20  y.o."
      ),
      overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667)
    )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect"))
  
})



testthat::test_that("Plot with line overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      uncertainty_prevalence = "high",
      DEBUG = 1,
      Min_Prevalence = 1,
      Max_Prevalence = 1800, 
      Sensitivity = 90,
      Max_FP = 15,
      label_subtitle = "PPV of Mammogram for Breast Cancer by Age",
      overlay = "line",
      overlay_labels = c(
        "80 y.o.",
        "70 y.o.",
        "60 y.o.",
        "50 y.o.",
        "40 y.o.",
        "30 y.o.",
        "20  y.o.",
        "X  y.o."
      ),
      overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667, 1798)
    )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect"))
  
})



# Messages ----------------------------------------------------------------

testthat::test_that("WARNING Min_Prevalence > Max_Prevalence", {
  
  res <- evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 2000,
      Max_Prevalence = 1000,
      Sensitivity = 81,
      Max_FP = 5))
  
  testthat::expect_equal(
    res$messages,
    "[WARNING]: Min_Prevalence (2000) is > than Max_Prevalence (1000). [EXPECTED]: Min_Prevalence should be smaller than Max_Prevalence. [CHANGED]: Min_Prevalence = Max_Prevalence/2\n")
})


testthat::test_that("WARNING overlay_prevalence_1/overlay_prevalence_2", {
  
  res <- evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 2,
      Max_Prevalence = 1000,
      Sensitivity = 81,
      Max_FP = 5,
      overlay = "area",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 1,
      overlay_prevalence_2 = 1000))
  
  testthat::expect_equal(
    res$messages,
    "[WARNING]: overlay_prevalence_1/overlay_prevalence_2 (0.001) is > than Min_Prevalence/Max_Prevalence (0.002). [EXPECTED]: Prevalence for overlay should be smaller than Prevalence [CHANGED]: Changing Min_Prevalence to (overlay_prevalence_1/overlay_prevalence_2) * Max_Prevalence to fit overlay\n")
})

testthat::test_that("WARNINGS overlay_position_FP, overlay_prevalence_2", {
  
  res <- evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 2,
      Max_Prevalence = 1000,
      Sensitivity = 81,
      Max_FP = 5,
      overlay = "area",
      overlay_position_FP = 6,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100))
  
  testthat::expect_equal(
    res$messages,
    c("[WARNING]: overlay_position_FP (6) is > than Max_FP (5). [EXPECTED]: overlay_position_FP should be smaller than Max_FP [CHANGED]: Max_FP = overlay_position_FP\n",
      "[WARNING]: overlay_prevalence_2 (1100) is > than Max_Prevalence (1000). [EXPECTED]: overlay_prevalence_2 should be smaller than Max_Prevalence [CHANGED]: Max_Prevalence = overlay_prevalence_2\n",
      "[WARNING]: overlay_prevalence_1 (1110) is > than overlay_prevalence_2 (1100). [EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2 [CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2\n"))
})

testthat::test_that("WARNINGS - NPV", {
  
  res <- evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      DEBUG = 1,
      Min_Prevalence = 2,
      Max_Prevalence = 1000,
      Sensitivity = 90,
      Max_FP = 5,
      overlay = "area",
      overlay_position_FN = 20,
      # overlay_position_FP = 6,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100))
  
  testthat::expect_equal(
    res$messages,
    c("[WARNING]: overlay_prevalence_2 (1100) is > than Max_Prevalence (1000). [EXPECTED]: overlay_prevalence_2 should be smaller than Max_Prevalence [CHANGED]: Max_Prevalence = overlay_prevalence_2\n", 
      "[WARNING]: overlay_position_FN (20) is > than (100 - Sensitivity) (10). [EXPECTED]: overlay_position_FN should be smaller than (100 - Sensitivity) [CHANGED]: Sensitivity = 100 - overlay_position_FN\n", 
      "[WARNING]: overlay_prevalence_1 (1110) is > than overlay_prevalence_2 (1100). [EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2 [CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2\n"))
})



# Save plot ---------------------------------------------------------------

testthat::test_that("Plot saved", {
  
    p <- BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 1000,
      Sensitivity = 100,
      Max_FP = 2,
      folder = "."
    )
  
  file_name_test = "PPV_1_1000_100_2_en.png"
  expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
})

