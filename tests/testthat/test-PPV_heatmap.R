library(ggplot2)


test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en",
    PPV_NPV = "PPV"
  )
  expect_true(is.ggplot(p))
  expect_identical(p$labels$y, "Prevalence")
  expect_identical(p$labels$caption, "Sensitivity = 100%")
  expect_identical(p$labels$fill, "PPV")
  
})


test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en",
    PPV_NPV = "NPV"
  )
  expect_true(is.ggplot(p))
  expect_identical(p$labels$caption, "Specificity = 98%")
  expect_identical(p$labels$fill, "NPV")
  
})


test_that("Spanish translation works'", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "sp"
  )
  expect_identical(p$labels$y, "Prevalencia")
  expect_identical(p$labels$x, "Tasa de Falsos Positivos")
  
})


test_that("Plot is type GeomTile", {
  p <- BayesianReasoning::PPV_heatmap(
    Min_Prevalence = 1,
    Max_Prevalence = 1000,
    Sensitivity = 100,
    Max_FP = 2,
    Language = "en"
  )
  expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]), "GeomTile")
  
})


test_that("Plot with area overlay", {
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
  expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  
})


test_that("Plot with line overlay", {
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
  expect_identical(sapply(p$layers, function(x)
    class(x$geom)[1]),
    c("GeomTile", "GeomSegment", "GeomText"))
  
})
