test_that("simple plot PPV", {
  p <-
    PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1500,
      Sensitivity = 92,
      limits_Specificity = c(90, 100)
    )
  vdiffr::expect_doppelganger("simple plot PPV", p)
})


test_that("simple plot NPV", {
  p <-
    PPV_heatmap(
      min_Prevalence = 50,
      max_Prevalence = 100,
      Specificity = 92,
      limits_Sensitivity = c(90, 100),
      PPV_NPV = "NPV"
    )
  vdiffr::expect_doppelganger("simple plot NPV", p)
})

test_that("line plot", {
  p <-
    BayesianReasoning::PPV_heatmap(
      Min_Prevalence = 1,
      Max_Prevalence = 1200,
      Sensitivity = 81,
      limits_Specificity = c(0, 100),
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "line",
      overlay_labels = c("40 y.o.", "35 y.o."),
      overlay_position_FP = c(4.8, 4.8),
      overlay_prevalence_1 = c(1, 1),
      overlay_prevalence_2 = c(68, 249)
    )
  
      vdiffr::expect_doppelganger("line plot", p)
})


test_that("area plot", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1000,
      Sensitivity = 100,
      limits_Specificity = c(90, 100),
      label_title = "Title plot",
      label_subtitle = "Subtitle plot",
      Language = "sp",
      overlay = "area",
      overlay_position_FP = 1,
      overlay_prevalence_1 = 1,
      overlay_prevalence_2 = 100
    )
  
  vdiffr::expect_doppelganger("area plot", p)
})




