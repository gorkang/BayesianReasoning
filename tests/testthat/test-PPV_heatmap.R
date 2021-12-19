library(ggplot2)
library(dplyr)


testthat::test_that("PPV Plot", {
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    limits_Specificity = c(95, 100),
    Language = "en",
    PPV_NPV = "PPV"
  ))
  
  testthat::expect_true(ggplot2::is.ggplot(p$result))
  testthat::expect_identical(p$result$labels$x, "False + (1 - Specificity)")
  testthat::expect_identical(p$result$labels$y, "Prevalence")
  testthat::expect_identical(p$result$labels$caption, "Sensitivity = 100%")
  testthat::expect_equal(p$result$plot_env$breaks_y, c(0.001000000, 0.002154435, 0.004641589, 0.010000000, 0.021544347, 0.046415888, 0.100000000, 0.215443469, 0.464158883, 1.000000000))
  testthat::expect_equal(p$result$plot_env$breaks_x, c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0))
  testthat::expect_equal(p$result$plot_env$labels_x, c("0%",   "0.5%", "1%",   "1.5%", "2%",   "2.5%", "3%",   "3.5%", "4%",   "4.5%", "5%"))
  testthat::expect_equal(p$result$plot_env$labels_y, c("1 out of 1000", "2 out of 1000", "5 out of 1000", "10 out of 1000", "22 out of 1000", "46 out of 1000", "100 out of 1000", "215 out of 1000", "464 out of 1000", "1000 out of 1000"))
  testthat::expect_equal(p$result$plot_env$breaks_legend, c(0.00, 0.25, 0.50, 0.75, 1.00))
  testthat::expect_identical(p$result$labels$fill, "PPV")
  testthat::expect_equal(length(p$result$plot_env$max_FP), 1)
  testthat::expect_equal(p$result$plot_env$max_FP, 5)
  testthat::expect_equal(length(p$result$plot_env$max_FN), 0)
  
})


testthat::test_that("PPV Plot", {
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1000,
      Specificity = 100,
      limits_Sensitivity = c(95, 100),
      Language = "en",
      PPV_NPV = "NPV"
    ))
  testthat::expect_true(ggplot2::is.ggplot(p$result))
  testthat::expect_identical(p$result$labels$x, "False - (1 - Sensitivity)")
  testthat::expect_identical(p$result$labels$y, "Prevalence")
  testthat::expect_identical(p$result$labels$caption, "Specificity = 100%")
  testthat::expect_equal(p$result$plot_env$breaks_y, c(0.001000000, 0.002154435, 0.004641589, 0.010000000, 0.021544347, 0.046415888, 0.100000000, 0.215443469, 0.464158883, 1.000000000))
  testthat::expect_equal(p$result$plot_env$breaks_x, c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0))
  testthat::expect_equal(p$result$plot_env$labels_x, c("0%",   "0.5%", "1%",   "1.5%", "2%",   "2.5%", "3%",   "3.5%", "4%",   "4.5%", "5%"))
  testthat::expect_equal(p$result$plot_env$labels_y, c("1 out of 1000", "2 out of 1000", "5 out of 1000", "10 out of 1000", "22 out of 1000", "46 out of 1000", "100 out of 1000", "215 out of 1000", "464 out of 1000", "1000 out of 1000"))
  testthat::expect_equal(p$result$plot_env$breaks_legend, c(0.00, 0.25, 0.50, 0.75, 1.00))
  testthat::expect_identical(p$result$labels$fill, "NPV")
  testthat::expect_equal(length(p$result$plot_env$max_FP), 0)
  testthat::expect_equal(length(p$result$plot_env$max_FN), 1)
  testthat::expect_equal(p$result$plot_env$max_FN, 5)
  
})



testthat::test_that("Spanish translation works'", {
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1,
    max_Prevalence = 1000,
    Sensitivity = 100,
    limits_Specificity = c(93, 103),
    Language = "sp"
  )
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Falsos + (1 - Especificidad)")
  
})

testthat::test_that("Spanish translation with overlay works'", {
  p <-  testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1, max_Prevalence = 1000,
    Sensitivity = 100,
    Specificity = 90,
    limits_Specificity = c(90, 100),
    Language = "sp",
    overlay = "area",
    overlay_position_FP = 1)
  )
  testthat::expect_identical(
    sapply(p$result$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  
})

testthat::test_that("Spanish translation with overlay works NPV", {
  p <-  testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 100,
      Specificity = 90,
      limits_Sensitivity = c(90, 100),
      Language = "sp",
      overlay = "area",
      overlay_position_FN = 1)
  )
  testthat::expect_identical(
    sapply(p$result$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomPoint", "GeomMarkRect"))
  testthat::expect_identical(p$result$labels$y, "Prevalencia")
  testthat::expect_identical(p$result$labels$x, "Falsos - (1 - Sensibilidad)")
  
})


testthat::test_that("one_out_of PPV", {
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 2,
      Sensitivity = 3,
      Specificity = 1,
      overlay = "area",
      overlay_position_FP = 3,
      overlay_prevalence_1 = 1, overlay_prevalence_2 = 1.5,
      PPV_NPV = "PPV",
      one_out_of = TRUE)
    )
  
  testthat::expect_true(p$result$plot_env$one_out_of)
  testthat::expect_equal(range(p$result$plot_env$breaks_x), c(0,100))
  
})


testthat::test_that("one_out_of NPV", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1,
      Specificity = 3,
      limits_Sensitivity = c(90, 100),
      overlay = "area",
      overlay_position_FN = 3,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
      PPV_NPV = "NPV",
      one_out_of = TRUE
    )
  
  testthat::expect_true(p$plot_env$one_out_of)
})

testthat::test_that("Error NPV", {
  
  testthat::expect_error(
    testthat::evaluate_promise(
      PPV_heatmap(
        PPV_NPV = "NPV",
        min_Prevalence = 1, max_Prevalence = 1000,
        Sensitivity = 100,
        Specificity = 90,
        limits_Specificity = c(90, 100),
        Language = "sp",
        overlay = "area",
        overlay_position_FP = 1)
      )
    )

})

testthat::test_that("Wrong parameters", {
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = NULL))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = NULL, PPV_NPV = "NPV"))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 200, limits_Specificity = c(90, 100)))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Specificity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Sensitivity = 200))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Specificity = c(0, 200)))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(limits_Sensitivity = c(0, 200)))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area"))
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", PPV_NPV = "NPV"))
  
  res1 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1, overlay_position_FN = 2))
  testthat::expect_equal(res1$warnings[1], "\n* overlay_position_FN should only be used for NPV plots")
  
  res2 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", PPV_NPV = "NPV", overlay_position_FP = 1, overlay_position_FN = 2))
  testthat::expect_equal(res2$warnings[1], "\n*  overlay_position_FP should only be used for PPV plots")
  
  res3 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(min_Prevalence = 0, Sensitivity = 99, limits_Specificity = c(90, 100)))
  1:length(res3$messages) %>% purrr::walk(~ if(grepl("WARNING", res3$messages[.x])) {testthat::expect_match(res3$messages[.x], "min_Prevalence \\(0\\) is < 1")})

  res4 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "line", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 2))
  1:length(res4$messages) %>% purrr::walk(~ if(grepl("WARNING", res4$messages[.x])) {testthat::expect_match(res4$messages[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence != overlay_prevalence_1")})
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = c(1, 2)))
  
  res5 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 1))
  1:length(res5$messages) %>% purrr::walk(~ if(grepl("WARNING", res5$messages[.x])) {testthat::expect_match(res5$messages[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence == overlay_prevalence_1")})

})



testthat::test_that("PPV calculation with area overlay, low uncertainty, and decimals in y axis", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 8,
      Sensitivity = 81,
      limits_Specificity = c(90, 100),
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
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
                limits_Sensitivity = c(90, 100),
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
      min_Prevalence = 1, max_Prevalence = 1200,
      limits_Sensitivity = c(76, 86),
      Specificity = 95,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.",
      overlay_position_FN = 4.8,
      overlay_prevalence_1 = 67, overlay_prevalence_2 = 68,
      uncertainty_prevalence = "low",
      PPV_NPV = "NPV", DEBUG = TRUE
    )
  
  testthat::expect_identical(
    p$layers[[3]]$computed_geom_params$description,
    "40 y.o.\nPrevalence: 67 out of 68\nSpecificity: 95%\nFalse -: 4.8%\n ---------------------------------------------\n67 sick: 54.27 (+) 12.73 (-)\n1 healthy: 0.95 (-) 0.05 (+) ")
  
})


testthat::test_that("Multiple overlays in overlay area", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1200,
      limits_Sensitivity = c(76, 86),
      Specificity = 95,
      overlay = "area",
      overlay_position_FN = 4.8,
      overlay_prevalence_1 = c(67, 100), overlay_prevalence_2 = c(68, 1500),
      uncertainty_prevalence = "low",
      PPV_NPV = "PPV"
    ))
  
})


# Line overlay ------------------------------------------------------------

testthat::test_that("Plot with line overlay PPV", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1800,
      Sensitivity = 90,
      limits_Specificity = c(80, 90),
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



testthat::test_that("Plot with line overlay NPV", {
  p <-
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      uncertainty_prevalence = "high",
      DEBUG = 1,
      min_Prevalence = 1,
      max_Prevalence = 1800, 
      limits_Sensitivity = c(85, 95),
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
})



# Save plot ---------------------------------------------------------------

testthat::test_that("Plot saved", {
  
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1, max_Prevalence = 1000,
    Sensitivity = 100,
    limits_Specificity = c(90, 100),
    folder = "."
  )
  
  file_name_test = "PPV_1_1000_100_0_10_en.png"
  testthat::expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
})

testthat::test_that("Plot saved", {
  
  p <- BayesianReasoning::PPV_heatmap(
    PPV_NPV = "NPV",
    min_Prevalence = 1, max_Prevalence = 1000,
    Specificity = 100,
    limits_Sensitivity = c(90, 100),
    folder = "."
  )
  
  file_name_test = "NPV_1_1000_100_0_10_en.png"
  testthat::expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
})

