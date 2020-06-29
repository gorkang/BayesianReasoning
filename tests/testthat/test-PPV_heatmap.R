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
  testthat::expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]), "GeomTile")
  
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
  testthat::expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  
})


testthat::test_that("Plot with line overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
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
        "20  y.o."
      ),
      overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667)
    )
  testthat::expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]),
    c("GeomTile", "GeomSegment", "GeomText"))
  
})


testthat::test_that("Warning message", {
  
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
    "Overlay impossible to fit in plot: overlay_prevalence_1/overlay_prevalence_2 < Min_Prevalence/Max_Prevalence: Changing Min_Prevalence to (overlay_prevalence_1/overlay_prevalence_2) * Max_Prevalence to fit overlay\n")
})
