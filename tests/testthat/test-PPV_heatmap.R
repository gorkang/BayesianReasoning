library(ggplot2)
library(dplyr)


testthat::test_that("Scale is labelled 'Prevalence'", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 98,
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
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 98,
    Language = "en",
    PPV_NPV = "NPV"
  )
  testthat::expect_true(ggplot2::is.ggplot(p))
  testthat::expect_identical(p$labels$caption, "Specificity = 98%")
  testthat::expect_identical(p$labels$fill, "NPV")
  
})


testthat::test_that("Spanish translation works'", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 98,
    Language = "sp"
  )
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Falsos + (1 - Especificidad)")
  
})

testthat::test_that("Spanish translation with overlay works'", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1, max_Prevalence = 1000,
    Sensitivity = 100,
    Language = "sp",
    overlay = "area",
    overlay_position_FP = 1
  )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  
})


testthat::test_that("Spanish NPV'", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 98,
    PPV_NPV = "NPV",
    Language = "sp"
  )
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Falsos Negativos (1 - Sensibilidad)")
  
})


testthat::test_that("Plot is type GeomTile", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 98,
    Language = "en"
  )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]), 
    "GeomTile")
  
})


testthat::test_that("Plot with area overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1200,
      Sensitivity = 81,
      Specificity = 95,
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


testthat::test_that("one_out_of PPV", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 2,
      Sensitivity = 3,
      limits_Specificity = c(97, 98),
      overlay = "area",
      overlay_position_FP = 3,
      overlay_prevalence_1 = 1, overlay_prevalence_2 = 1.5,
      PPV_NPV = "PPV",
      one_out_of = TRUE
    )
  
  testthat::expect_true(p$plot_env$one_out_of)
})


testthat::test_that("one_out_of NPV", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1,
      Sensitivity = 3,
      overlay = "area",
      overlay_position_FN = 3,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
      PPV_NPV = "NPV",
      one_out_of = TRUE
    )
  
  testthat::expect_true(p$plot_env$one_out_of)
})


testthat::test_that("Wrong parameters", {
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = NULL))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = NULL, PPV_NPV = "NPV"))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Specificity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Sensitivity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Specificity = c(0, 200)))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Sensitivity = c(0, 200)))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area"))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", PPV_NPV = "NPV"))
  
  res1 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(overlay = "area", overlay_position_FP = 1, overlay_position_FN = 2))
  testthat::expect_equal(res1$warnings[1], "\n* overlay_position_FN should only be used for NPV plots")
  
  res2 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(overlay = "area", PPV_NPV = "NPV", overlay_position_FP = 1, overlay_position_FN = 2))
  testthat::expect_equal(res2$warnings[1], "\n*  overlay_position_FP should only be used for PPV plots")
  
  res3 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(min_Prevalence = 0))
  1:length(res3$messages) %>% purrr::walk(~ if(grepl("WARNING", res3$messages[.x])) {testthat::expect_match(res3$messages[.x], "min_Prevalence \\(0\\) is < 1")})

  res4 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(overlay = "line", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 2))
  1:length(res4$messages) %>% purrr::walk(~ if(grepl("WARNING", res4$messages[.x])) {testthat::expect_match(res4$messages[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence != overlay_prevalence_1")})
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = c(1, 2)))
  
  res5 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(overlay = "area", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 1))
  1:length(res5$messages) %>% purrr::walk(~ if(grepl("WARNING", res5$messages[.x])) {testthat::expect_match(res5$messages[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence == overlay_prevalence_1")})

})



testthat::test_that("PPV calculation with area overlay, low uncertainty, and decimals in y axis", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 8,
      Sensitivity = 81,
      Specificity = 95,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 2,
      overlay_prevalence_2 = 8,
      uncertainty_prevalence = "low",
      PPV_NPV = "PPV"
    )
  
  
  # Overlay description
  testthat::expect_identical(
  p$layers[[3]]$computed_geom_params$description,
  "40 y.o.\nPrevalence: 2 out of 8\nSensitivity: 81%\nFalse +: 4.8% \n ---------------------------------------------\n2 sick: 1.62 (+) 0.38 (-)\n6 healthy: 5.71 (-) 0.29 (+) ")
    
  # Decimal breaks y axis
  testthat::expect_equal(
    p$plot_env$breaks_y,
    c(0.1250000, 0.1450162, 0.1682375, 0.195177295538928, 0.226430916065977, 0.262689159663305, 0.304753413551119, 0.353553390593274, 0.410167678003819, 0.47584757650531, 0.552044756836906, 0.640443344882136, 0.742997144568474, 0.861972821246978, 1.0000000))

})


testthat::test_that("Range of data is the same as the x axis range", {
  p <-
    PPV_heatmap(PPV_NPV = "NPV",
                min_Prevalence = 800,
                max_Prevalence = 1000,
                Sensitivity = 80,
                Specificity = 95)

  # Decimal breaks y axis
  testthat::expect_identical(
    c(p$plot_env$min_FN, p$plot_env$max_FN), 
    range(p$data$FN)
    )
    

})



testthat::test_that("NPV calculation with area overlay and low uncertainty", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1200,
      Sensitivity = 81,
      Specificity = 95,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FN = 4.8,
      overlay_prevalence_1 = 67,
      overlay_prevalence_2 = 68,
      uncertainty_prevalence = "low",
      PPV_NPV = "NPV"
    )
  
  testthat::expect_identical(
    p$layers[[3]]$computed_geom_params$description,
    "40 y.o.\nPrevalence: 67 out of 68\nSpecificity: 95%\nFalse Negatives: 4.8%\n ---------------------------------------------\n67 sick: 54.27 (+) 12.73 (-)\n1 healthy: 0.95 (-) 0.05 (+) ")
  
})


# Line overlay ------------------------------------------------------------

testthat::test_that("Plot with line overlay", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1800,
      Sensitivity = 90,
      Specificity = 85,
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
      min_Prevalence = 1,
      max_Prevalence = 1800, 
      Sensitivity = 90,
      Specificity = 85,
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
      overlay_position_FN = c(6.5, 7, 8, 9, 12, 14, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667, 1798)
    )
  testthat::expect_identical(
    sapply(p$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect"))
  
})



# Messages ----------------------------------------------------------------

testthat::test_that("WARNING min_Prevalence > max_Prevalence", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2000,
      max_Prevalence = 1000,
      Sensitivity = 81,
      Specificity = 95))

  # CHECKS ONLY THE WARNING messages  
  1:length(res$messages) %>% purrr::walk(~ if(grepl("WARNING", res$messages[.x])) testthat::expect_match(res$messages[.x], "min_Prevalence \\(2000\\) is > than max_Prevalence \\(1000\\)"))
  # testthat::expect_match(
  #   res$messages,
  #   "min_Prevalence \\(2000\\) is > than max_Prevalence \\(1000\\)"
  #   )
})


testthat::test_that("WARNING overlay_prevalence_1/overlay_prevalence_2", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2,
      max_Prevalence = 1000,
      Sensitivity = 81,
      Specificity = 95,
      overlay = "area",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 1,
      overlay_prevalence_2 = 1000)
    )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$messages) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$messages[.x])) {
                    testthat::expect_match(
                      res$messages[.x], 
                      "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|max_Prevalence == overlay_prevalence_2")
                    }
                )
  
  # testthat::expect_equal(
  #   res$messages,
  #   c("\n[WARNING]: min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2\n[EXPECTED]: min_Prevalence/max_Prevalence should be <= overlay_prevalence_1/overlay_prevalence_2\n",
  #     "[CONDITION]: max_Prevalence == overlay_prevalence_2\n[CHANGED]: Changing min_Prevalence = overlay_prevalence_1\n")
  #   )
})

testthat::test_that("WARNINGS overlay_position_FP, overlay_prevalence_2", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2,
      max_Prevalence = 1000,
      Sensitivity = 81,
      Specificity = 95,
      overlay = "area",
      overlay_position_FP = 6,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100)
    )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$messages) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$messages[.x])) {
                    testthat::expect_match(
                      res$messages[.x], 
                      "overlay_prevalence_1 \\(1110\\) is > than overlay_prevalence_2 \\(1100\\)")
                  }
    )
  # testthat::expect_equal(
  #   res$messages,
  #   c(
  #     # "[WARNING]: overlay_position_FP (6) is > than max_FP (5). [EXPECTED]: overlay_position_FP should be smaller than max_FP [CHANGED]: max_FP = overlay_position_FP\n",
  #     "[WARNING]: overlay_prevalence_1 (1110) is > than overlay_prevalence_2 (1100). [EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2 [CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2\n"))
})

testthat::test_that("WARNINGS - NPV", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      DEBUG = 1,
      min_Prevalence = 2,
      max_Prevalence = 1000,
      Sensitivity = 90,
      Specificity = 95,
      overlay = "area",
      overlay_position_FN = 20,
      # overlay_position_FP = 6,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100)
    )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$messages) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$messages[.x])) {
                    testthat::expect_match(
                      res$messages[.x], 
                      "overlay_position_FN \\(20\\) is > max_FN \\(15\\)|overlay_prevalence_1 \\(1110\\) is > than overlay_prevalence_2 \\(1100\\)")
                  }
    )
  # testthat::expect_equal(
  #   res$messages,
  #   c(
  #     "[WARNING]: overlay_position_FN (20) is > Max_FN (15) [EXPECTED]: overlay_position_FN should be <= Max_FN  [CHANGED]: Max_FN = overlay_position_FN + 10%\n",
  #     "[WARNING]: overlay_prevalence_1 (1110) is > than overlay_prevalence_2 (1100). [EXPECTED]: overlay_prevalence_1 should be smaller than overlay_prevalence_2 [CHANGED]: overlay_prevalence_1 = overlay_prevalence_2/2\n"
  #     # "[WARNING]: overlay_prevalence_2 (1100) is > than max_Prevalence (1000). [EXPECTED]: overlay_prevalence_2 should be smaller than max_Prevalence [CHANGED]: max_Prevalence = overlay_prevalence_2\n"
  #     # "[WARNING]: overlay_position_FN (20) is > than (100 - Sensitivity) (10). [EXPECTED]: overlay_position_FN should be smaller than (100 - Sensitivity) [CHANGED]: Sensitivity = 100 - overlay_position_FN\n", 
  #     
  #     ))
})



# Save plot ---------------------------------------------------------------

testthat::test_that("Plot saved", {
  
    p <- BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 100,
      folder = "."
    )
  
  file_name_test = "PPV_1_1000_100_10_en.png"
  expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
})
