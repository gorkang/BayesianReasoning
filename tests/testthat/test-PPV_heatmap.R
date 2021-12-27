library(ggplot2)
library(dplyr)


# PPV / NPV calculation ---------------------------------------------------

testthat::test_that("Clasical breast cancer problem with hardcoded expected results", {
  
  p <-  testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 200,
      Sensitivity = 80, limits_Specificity = c(80, 100),
      overlay = "area",
      overlay_prevalence_1 = 1, overlay_prevalence_2 = 100,
      overlay_position_FP = 9.6)
  )
  
  p_build <- ggplot2::ggplot_build(p$result)
  PPV_PLOT = as.numeric(gsub(".*: ([0-9]{1,2})%", "\\1", p_build$data[[3]]$label))/100
  
  PPV_tibble = p$result$data %>% 
    filter(sensitivity == 80/100) %>% 
    filter(FP == 9.6) %>%
    filter(abs(prevalence_2 - 100) == min(abs(prevalence_2 - 100)))
  
  # Expect difference between the PPV calculated and the PPV shown in plot < 1% [expect_lt = expect less than]
  testthat::expect_lt(PPV_tibble$PPV - PPV_PLOT, 0.01)
  testthat::expect_lt(PPV_tibble$PPV_calc - PPV_PLOT, 0.01)
  
  testthat::expect_equal(PPV_PLOT, 0.08)
  testthat::expect_equal(PPV_tibble$PPV, 0.08)
  testthat::expect_equal(PPV_tibble$PPV_calc, 0.07732454)
})



testthat::test_that("calculated PPV in area plot", {
  
  # En PPV_melted, FP esta en % y sensitivity y specificity en x/1
  # specificity    FP
  # 0.999         0.1/100 = 0.001 
  Sensitivity = 90
  overlay_position_FP = 1
  Specificity = 100 - overlay_position_FP
  overlay_prevalence_2 = 1000
  
  p <-  testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = Sensitivity, limits_Specificity = c(90, 100),
      overlay = "area",
      overlay_prevalence_1 = 1, overlay_prevalence_2 = overlay_prevalence_2,
      overlay_position_FP = overlay_position_FP)
  )
  
  p_build <- ggplot2::ggplot_build(p$result)
  PPV_PLOT = as.numeric(gsub(".*: ([0-9]{1,2})%", "\\1", p_build$data[[3]]$label))/100
  
  PPV_tibble = p$result$data %>% 
    filter(sensitivity == Sensitivity/100) %>% 
    filter(FP == overlay_position_FP) %>% # filter(specificity == Specificity/100) %>%
    # Closest prevalence_2 
    filter(abs(prevalence_2 - overlay_prevalence_2) == min(abs(prevalence_2 - overlay_prevalence_2)))
    
  # Expect difference between the PPV calculated and the PPV shown in plot < 1% [expect_lt = expect less than]
  testthat::expect_lt(PPV_tibble$PPV - PPV_PLOT, 0.01)
  testthat::expect_lt(PPV_tibble$PPV_calc - PPV_PLOT, 0.01)
  
  # Manual calculation
  # dplyr::mutate(PPV_calc = (prevalence_1 * sensitivity) / ((prevalence_1 * sensitivity) + ((prevalence_2 - prevalence_1) * (1-specificity))),
  #               DIFF = PPV_calc - PPV) 
  
})


# ERRORS ------------------------------------------------------------------

testthat::test_that("* limits_Specificity need two different numbers: limits_Specificity = c(min, max)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 100, limits_Specificity = c(100, 100),
      PPV_NPV = "PPV")
    )
})

testthat::test_that("* limits_Specificity need two different numbers: limits_Specificity = c(min, max)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 100, Specificity = 100, limits_Specificity = c(100, 100),
      PPV_NPV = "PPV")
    )
})

testthat::test_that("* limits_Sensitivity need two different numbers: limits_Sensitivity = c(min, max)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Specificity = 100, limits_Sensitivity = c(100, 100),
      PPV_NPV = "NPV")
    )
})

testthat::test_that("* limits_Sensitivity need two different numbers: limits_Sensitivity = c(min, max)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Specificity = 100, Sensitivity = 100, limits_Sensitivity = c(100, 100),
      PPV_NPV = "NPV")
    )
})

testthat::test_that("* limits_Specificity sould be a vector of length 2, now is (90, 90, 100). e.g.: limits_Specificity = c(90, 95)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 90, limits_Specificity = c(80, 90, 100),
      PPV_NPV = "PPV")
    )
})

testthat::test_that("* limits_Sensitivity sould be a vector of length 2, now is (90, 90, 100). e.g.: limits_Sensitivity = c(90, 95)", {
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Specificity = 90, limits_Sensitivity = c(80, 90, 100),
      PPV_NPV = "NPV")
    )
})


testthat::test_that("ERRORS because of Wrong parameters", {

  testthat::expect_error(testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 200, PPV_NPV = "PPV"))) # * Sensitivity should be a value 0-100
  testthat::expect_error(testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Specificity = 200, PPV_NPV = "NPV"))) # * Specificity should be a value 0-100
  testthat::expect_error(testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 95, limits_Specificity = 200, PPV_NPV = "PPV"))) # * limits_Specificity sould be a vector of length 2, now is (200). e.g.: limits_Specificity = c(90, 95)
  testthat::expect_error(testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Specificity = 95, limits_Sensitivity = 200, PPV_NPV = "NPV"))) # * limits_Sensitivity sould be a vector of length 2, now is (200). e.g.: limits_Sensitivity = c(90, 95)
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 95, limits_Specificity = c(0, 200), PPV_NPV = "PPV")) # * limits_Specificity should be between 0 and 100, now are (0, 200). e.g.: limits_Specificity = c(90, 95)
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = 95, limits_Sensitivity = c(0, 200), PPV_NPV = "NPV")) # * limits_Sensitivity should be between 0 and 100, now are (0, 200). e.g.: limits_Sensitivity = c(90, 95)
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", PPV_NPV = "PPV")) # * Sensitivity is needed in PPV_NPV == 'PPV'
  testthat::expect_error(BayesianReasoning::PPV_heatmap(overlay = "area", PPV_NPV = "NPV")) # * Specificity is needed in PPV_NPV == 'NPV'
  
  # Missing overlay_prevalence_ parameters
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = NULL, overlay_prevalence_2 = 1, PPV_NPV = "PPV")) # * Missing the overlay_prevalence_1 parameter for the overlay's prevalence (overlay_prevalence_1 out of overlay_prevalence_2)
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = 1, overlay_prevalence_2 = NULL, PPV_NPV = "PPV")) # * Missing the overlay_prevalence_2 parameter for the overlay's prevalence (overlay_prevalence_1 out of overlay_prevalence_2)
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1)) # * Need a prevalence for the overlay. Use the overlay_prevalence_1 and overlay_prevalence_2 parameters (overlay_prevalence_1 out of overlay_prevalence_2)

  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = NULL, PPV_NPV = "PPV")) # * overlay_position_FP needs a value
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", overlay_position_FN = NULL, PPV_NPV = "NPV")) # * overlay_position_FN needs a value
  
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Sensitivity = 2, Specificity = 1, limits_Specificity = c(5, 8), PPV_NPV = "PPV")) # * Both Specificity (6.5) and limits_Specificity (5, 8) have values. Ignoring Sensitivity and using limits_Specificity
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Sensitivity = 2, Specificity = 1, limits_Sensitivity = c(5, 8), PPV_NPV = "PPV")) # * limits_Specificity is NULL. Setting limits_Specificity = c(-4, 6)
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Sensitivity = 2, Specificity = 1, limits_Specificity = c(5, 8), PPV_NPV = "NPV")) # * limits_Sensitivity is NULL. Setting limits_Sensitivity = c(-3, 7)
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Sensitivity = 2, Specificity = 1, limits_Sensitivity = c(5, 8), PPV_NPV = "NPV")) # * Both Sensitivity (6.5) and limits_Sensitivity (5, 8) have values. Ignoring Sensitivity and using limits_Sensitivity
  
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = c(1, 1), overlay_prevalence_2 = 100)) # * overlay_prevalence_1 has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'?
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = c(1, 1), overlay_prevalence_1 = 1, overlay_prevalence_2 = 100, PPV_NPV = "PPV")) # * overlay_position_FP has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'?
  testthat::expect_error(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", overlay_position_FN = c(1, 1), overlay_prevalence_1 = 1, overlay_prevalence_2 = 100, PPV_NPV = "NPV")) # * overlay_position_FN has > 1 value. Not allowed in overlay = 'area'. Did you meant overlay = 'line'?
  
  

})

# WARNINGS ------------------------------------------------------------------

testthat::test_that("WARNINGS because of Wrong parameters", {
  
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 90, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100, PPV_NPV = "PPV")) # [WARNING]: overlay_position_FP (90) is > than max_FP (10).
  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", overlay_position_FN = 90, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100, PPV_NPV = "NPV")) # [WARNING]: overlay_position_FN (90) is > max_FN (10)

  testthat::expect_warning(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", overlay_position_FN = 1, overlay_prevalence_1 = 1, overlay_prevalence_2 = 1, PPV_NPV = "NPV", one_out_of = TRUE)) 
  
  
  res1 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", overlay_position_FP = 1, overlay_position_FN = 2, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100))
  testthat::expect_equal(res1$warnings[1], "\n* overlay_position_FN should only be used for NPV plots")
  
  res2 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Specificity = 99, limits_Sensitivity = c(90, 100), overlay = "area", PPV_NPV = "NPV", overlay_position_FP = 1, overlay_position_FN = 2, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100))
  testthat::expect_equal(res2$warnings[1], "\n*  overlay_position_FP should only be used for PPV plots")
  
  res3 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(min_Prevalence = 0, Sensitivity = 99, limits_Specificity = c(90, 100)))
  1:length(res3$warnings) %>% purrr::walk(~ if(grepl("WARNING", res3$warnings[.x])) {testthat::expect_match(res3$warnings[.x], "min_Prevalence \\(0\\) is < 1")})
  
  res4 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "line", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 2, overlay_prevalence_2 = 100))
  1:length(res4$warnings) %>% purrr::walk(~ if(grepl("WARNING", res4$warnings[.x])) {testthat::expect_match(res4$warnings[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence != overlay_prevalence_1")})
  
  testthat::expect_error(testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(overlay = "area", overlay_position_FP = 1, overlay_prevalence_1 = c(1, 2))))
  
  res5 = testthat::evaluate_promise(BayesianReasoning::PPV_heatmap(Sensitivity = 99, limits_Specificity = c(90, 100), overlay = "area", min_Prevalence = 1, max_Prevalence = 2, overlay_position_FP = 1, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100))
  1:length(res5$warnings) %>% purrr::walk(~ if(grepl("WARNING", res5$warnings[.x])) {testthat::expect_match(res5$warnings[.x], "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|min_Prevalence == overlay_prevalence_1")})
  
    
})


testthat::test_that("WARNING min_Prevalence > max_Prevalence", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2000,
      max_Prevalence = 1000,
      Sensitivity = 81,
      Specificity = 95))
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$warnings) %>% purrr::walk(~ if(grepl("WARNING", res$warnings[.x])) testthat::expect_match(res$warnings[.x], "min_Prevalence \\(2000\\) is > than max_Prevalence \\(1000\\)"))
  
})


testthat::test_that("WARNING overlay_prevalence_1/overlay_prevalence_2", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2,
      max_Prevalence = 1000,
      Sensitivity = 81,
      limits_Specificity = c(90, 95),
      overlay = "area",
      overlay_position_FP = 4,
      overlay_prevalence_1 = 1,
      overlay_prevalence_2 = 1000)
  )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$warnings) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$warnings[.x])) {
                    testthat::expect_match(
                      res$warnings[.x], 
                      "min_Prevalence/max_Prevalence > overlay_prevalence_1/overlay_prevalence_2|max_Prevalence == overlay_prevalence_2|overlay_position_FP \\(4\\) is < min_FP \\(5\\)")
                  }
    )
})

testthat::test_that("WARNINGS overlay_position_FP, overlay_prevalence_2", {
  
  res <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 2,
      max_Prevalence = 1000,
      Sensitivity = 81,
      limits_Specificity = c(89, 99),
      overlay = "area",
      overlay_position_FP = 6,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100)
  )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$warnings) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$warnings[.x])) {
                    testthat::expect_match(
                      res$warnings[.x], 
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
      limits_Sensitivity = c(85, 95),
      Specificity = 95,
      overlay = "area",
      overlay_position_FN = 20,
      overlay_prevalence_1 = 1110,
      overlay_prevalence_2 = 1100)
  )
  
  # CHECKS ONLY THE WARNING messages  
  1:length(res$warnings) %>% 
    purrr::walk(~ 
                  if(grepl("WARNING", res$warnings[.x])) {
                    testthat::expect_match(
                      res$warnings[.x], 
                      "overlay_position_FN \\(20\\) is > max_FN \\(15\\)|overlay_prevalence_1 \\(1110\\) is > than overlay_prevalence_2 \\(1100\\)")
                  }
    )
})


# PARAMETERS --------------------------------------------------------------

testthat::test_that("Extra decimals when axis are tight", {
  
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1.1,
      Sensitivity = 100, limits_Specificity = c(99.9, 100))
  )
  
  testthat::expect_equal(p$result$plot_env$breaks_x, c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10))
  
})


testthat::test_that("PPV Plot", {
  
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1000,
      Sensitivity = 100,
      limits_Specificity = c(95, 100),
      label_title = "Title plot", 
      label_subtitle = "Subtitle plot",
      Language = "en",
      PPV_NPV = "PPV")
    )
  
  testthat::expect_true(ggplot2::is.ggplot(p$result))
  testthat::expect_identical(p$result$labels$x, "False + (1 - Specificity)")
  testthat::expect_identical(p$result$labels$y, "Prevalence")
  testthat::expect_identical(p$result$labels$title, "Title plot")
  testthat::expect_identical(p$result$labels$subtitle, "Subtitle plot")
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


testthat::test_that("NPV Plot", {
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1,
      max_Prevalence = 1000,
      Specificity = 100,
      limits_Sensitivity = c(95, 100),
      label_title = "Title plot", 
      label_subtitle = "Subtitle plot",
      Language = "en",
      PPV_NPV = "NPV"
    ))
  testthat::expect_true(ggplot2::is.ggplot(p$result))
  testthat::expect_identical(p$result$labels$x, "False - (1 - Sensitivity)")
  testthat::expect_identical(p$result$labels$y, "Prevalence")
  testthat::expect_identical(p$result$labels$title, "Title plot")
  testthat::expect_identical(p$result$labels$subtitle, "Subtitle plot")
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




# Area plot ---------------------------------------------------------------


testthat::test_that("Spanish translation", {
  
  p <- BayesianReasoning::PPV_heatmap(
        min_Prevalence = 1, max_Prevalence = 1000,
        Sensitivity = 100, limits_Specificity = c(93, 100),
        Language = "sp")
  
  testthat::expect_identical(p$labels$y, "Prevalencia")
  testthat::expect_identical(p$labels$x, "Falsos + (1 - Especificidad)")
  
  p <-  testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1000,
      Sensitivity = 100, limits_Specificity = c(90, 100),
      label_title = "Title plot", 
      label_subtitle = "Subtitle plot",
      Language = "sp",
      overlay = "area",
      overlay_position_FP = 1, overlay_prevalence_1 = 1, overlay_prevalence_2 = 100)
  )
  
  testthat::expect_identical(sapply(p$result$layers, function(x) class(x$geom)[1]), c("GeomTile", "GeomPoint", "GeomMarkRect"))
  testthat::expect_identical(p$result$labels$title, "Title plot")
  testthat::expect_identical(p$result$labels$subtitle, "Subtitle plot")
  
  
  p <-  testthat::evaluate_promise(
      BayesianReasoning::PPV_heatmap(
        PPV_NPV = "NPV",
        min_Prevalence = 1, max_Prevalence = 1000,
        Specificity = 90,
        limits_Sensitivity = c(90, 100),
        Language = "sp",
        overlay = "area",
        overlay_position_FN = 1,
        overlay_prevalence_1 = 1, overlay_prevalence_2 = 100)
      )
  testthat::expect_identical(sapply(p$result$layers, function(x) class(x$geom)[1]), c("GeomTile", "GeomPoint", "GeomMarkRect"))
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
  
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1,
      Specificity = 3,
      limits_Sensitivity = c(90, 100),
      overlay = "area",
      overlay_position_FN = 3,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
      PPV_NPV = "NPV",
      one_out_of = TRUE)
    )
  
  testthat::expect_true(p$result$plot_env$one_out_of)
})


testthat::test_that("Area overlay, low uncertainty, and decimals in y axis. overlay_extra_info TRUE/FALSE", {
  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 8,
      Sensitivity = 81, limits_Specificity = c(90, 100),
      overlay = "area",
      overlay_labels = "40 y.o.", overlay_extra_info = TRUE,
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
      PPV_NPV = "PPV"
    )
  
  # Overlay description
  testthat::expect_identical(
  p$layers[[3]]$computed_geom_params$description, "40 y.o.\nPrevalence: 2 out of 8\nSensitivity: 81%\nFalse +: 4.8% \n ---------------------------------------------\n2 sick: 1.62 (+) 0.38 (-)\n6 healthy: 5.71 (-) 0.29 (+) ")
    
  # Decimal breaks y axis
  testthat::expect_equal(p$plot_env$breaks_y, c(0.1250000, 0.1450162, 0.1682375, 0.195177295538928, 0.226430916065977, 0.262689159663305, 0.304753413551119, 0.353553390593274, 0.410167678003819, 0.47584757650531, 0.552044756836906, 0.640443344882136, 0.742997144568474, 0.861972821246978, 1.0000000))

  p <-
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 8,
      Sensitivity = 81, limits_Specificity = c(90, 100),
      overlay = "area",
      overlay_labels = "40 y.o.", overlay_extra_info = FALSE,
      overlay_position_FP = 4.8,
      overlay_prevalence_1 = 2, overlay_prevalence_2 = 8,
      PPV_NPV = "PPV"
    )
  
  # Overlay description
  testthat::expect_identical(p$layers[[3]]$computed_geom_params$description, "40 y.o.\nPrevalence: 2 out of 8\nSensitivity: 81%\nFalse +: 4.8% ")
  
  # Decimal breaks y axis
  testthat::expect_equal(p$plot_env$breaks_y, c(0.1250000, 0.1450162, 0.1682375, 0.195177295538928, 0.226430916065977, 0.262689159663305, 0.304753413551119, 0.353553390593274, 0.410167678003819, 0.47584757650531, 0.552044756836906, 0.640443344882136, 0.742997144568474, 0.861972821246978, 1.0000000))
  
})


testthat::test_that("Range of data is the same as the x axis range", {
  p <-
    PPV_heatmap(PPV_NPV = "NPV",
                min_Prevalence = 800,
                max_Prevalence = 1000,
                limits_Sensitivity = c(90, 100),
                Specificity = 95)

  # Decimal breaks y axis
  testthat::expect_identical(c(p$plot_env$min_FN, p$plot_env$max_FN), range(p$data$FN))
    

})


# TODO: Should check uncertainty_prevalence consequences
testthat::test_that("NPV calculation with area overlay and low uncertainty", {
  
  p <- testthat::evaluate_promise(
    
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1200,
      limits_Sensitivity = c(76, 86),
      Specificity = 95,
      label_subtitle = "Prenatal screening for Down Syndrome by Age",
      overlay = "area",
      overlay_labels = "40 y.o.", overlay_extra_info = TRUE,
      overlay_position_FN = 4.8,
      overlay_prevalence_1 = 67, overlay_prevalence_2 = 68,
      uncertainty_prevalence = "low",
      PPV_NPV = "NPV", DEBUG = TRUE)
    
    )
  
  testthat::expect_identical(
    p$result$layers[[3]]$computed_geom_params$description,
    "40 y.o.\nPrevalence: 67 out of 68\nSpecificity: 95%\nFalse -: 4.8%\n ---------------------------------------------\n67 sick: 63.78 (+) 3.22 (-)\n1 healthy: 0.95 (-) 0.05 (+) ")
  
})



#   
# } else if (!is.null(overlay_position_FN) & !is.null(Sensitivity) & !is.null(limits_Sensitivity)) {
#   


testthat::test_that("all parameters", {
  
  testthat::expect_warning(
    
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 2,
      Sensitivity = 3,
      Specificity = 99,
      limits_Specificity = c(90, 99),
      overlay = "area",
      overlay_position_FP = 3,
      overlay_prevalence_1 = 1, overlay_prevalence_2 = 2,
      PPV_NPV = "PPV",
      one_out_of = TRUE)
  )
  
  testthat::expect_warning(
    
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 100,
      Specificity = 3,
      Sensitivity = 90,
      limits_Sensitivity = c(90, 100),
      overlay = "area",
      overlay_position_FN = 3,
      overlay_prevalence_1 = 1, overlay_prevalence_2 = 8,
      PPV_NPV = "NPV",
      one_out_of = TRUE)
  )
  
})




# Line overlay ------------------------------------------------------------


testthat::test_that("Plot with line overlay", {
  
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(min_Prevalence = 1, max_Prevalence = 1800,
                                   Sensitivity = 90,
                                   limits_Specificity = c(84, 100),
                                   overlay = "line",
                                   overlay_labels = c("80 y.o.", "70 y.o."),
                                   overlay_position_FP = c(6.5, 7),
                                   overlay_prevalence_1 = c(1, 1),
                                   overlay_prevalence_2 = c(22, 26)
    )
  )
  
  testthat::expect_length(p$warnings, 0)
    
  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1800,
      Sensitivity = 90, limits_Specificity = c(80, 90),
      label_title = "Title plot", 
      label_subtitle = "Subtitle plot",
      overlay = "line",
      uncertainty_prevalence = "low",
      overlay_labels = c("80 y.o.", "70 y.o.","60 y.o.", "50 y.o.", "40 y.o.", "30 y.o.", "20  y.o."),
      overlay_position_FP = c(6.5, 7, 8, 9, 12, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 2),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667))
    )
  testthat::expect_identical(sapply(p$result$layers, function(x) class(x$geom)[1]), c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect"))
  testthat::expect_identical(p$result$labels$title, "Title plot")
  testthat::expect_identical(p$result$labels$subtitle, "Subtitle plot")

  p <- testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      uncertainty_prevalence = "high",
      min_Prevalence = 1, max_Prevalence = 1800, 
      Specificity = 85, limits_Sensitivity = c(85, 95),
      label_title = "Title plot", 
      label_subtitle = "Subtitle plot",
      overlay = "line",
      overlay_labels = c("80 y.o.", "70 y.o.","60 y.o.", "50 y.o.", "40 y.o.", "30 y.o.", "20  y.o.", "X yo"),
      overlay_position_FN = c(6.5, 7, 8, 9, 12, 14, 14, 14),
      overlay_prevalence_1 = c(1, 1, 1, 1, 1, 1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 29, 44, 69, 227, 1667, 1798))
  )
  
  testthat::expect_identical(sapply(p$result$layers, function(x) class(x$geom)[1]), c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect"))
  testthat::expect_identical(p$result$labels$title, "Title plot")
  testthat::expect_identical(p$result$labels$subtitle, "Subtitle plot")
  
})


testthat::test_that("Unequal overlay number of parameters", {
  
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      min_Prevalence = 1, max_Prevalence = 1800, 
      Specificity = 85, limits_Sensitivity = c(85, 95),
      overlay = "line",
      overlay_labels = c("80 y.o."),
      overlay_position_FN = c(6.5, 7),
      overlay_prevalence_1 = c(1, 1, 1),
      overlay_prevalence_2 = c(22, 26, 23, 278))
    )
  
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "PPV",
      min_Prevalence = 1, max_Prevalence = 1800, 
      Sensitivity = 85, limits_Specificity = c(85, 95),
      overlay = "line",
      overlay_labels = c("80 y.o.", "asd", "asd", "asd"),
      overlay_position_FP = c(6.5, 7),
      overlay_prevalence_1 = c(1, 1, 1, 3),
      overlay_prevalence_2 = c(22, 26, 23, 278))
  )
  
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = "NPV",
      min_Prevalence = 1, max_Prevalence = 1800, 
      Specificity = 85, limits_Sensitivity = c(85, 95),
      overlay = "line",
      overlay_labels = c("80 y.o.", "asd", "asd", "asd"),
      overlay_position_FN = c(6.5, 7),
      overlay_prevalence_1 = c(1, 1, 1, 3),
      overlay_prevalence_2 = c(22, 26, 23, 278))
  )
  
  p = testthat::evaluate_promise(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1200,
      limits_Specificity = c(80, 100),
      Sensitivity = 95,
      overlay = "line",
      overlay_labels = c("tag 1", "", "tag 3"),
      overlay_position_FP = c(5, 10, 20),
      overlay_prevalence_1 = c(1, 1, 5), overlay_prevalence_2 = c(8, 55, 176),
      uncertainty_prevalence = "low",
      PPV_NPV = "PPV")
  )
  
  testthat::expect_length(p$warnings, 1)
  # "\n[WARNING]: Some of the overlay_prevalence_1 (1) are > min_Prevalence (1).\n[EXPECTED]: overlay_prevalence_1 should be >= min_Prevalence.\n[CHANGED]: overlay_prevalence_1 and overlay_prevalence_2 to 1, 1, 25 and 8, 55, 880"
  
  # TODO: accept NULL values in overlay line?
  testthat::expect_error(
    BayesianReasoning::PPV_heatmap(
      min_Prevalence = 1, max_Prevalence = 1200,
      limits_Specificity = c(76, 86),
      Sensitivity = 95,
      overlay = "line",
      overlay_labels = c("tag 1", NULL, "tag 3"),
      overlay_position_FP = c(4.8, 15, 20),
      overlay_prevalence_1 = c(1, 2, 1), overlay_prevalence_2 = c(8, 55, 176),
      uncertainty_prevalence = "low",
      PPV_NPV = "PPV")
  )
  
})




# Save plot ---------------------------------------------------------------

testthat::test_that("Save plots", {
  
  p <- BayesianReasoning::PPV_heatmap(
    min_Prevalence = 1, max_Prevalence = 1000,
    Sensitivity = 100,
    limits_Specificity = c(90, 100),
    folder = "."
  )
  
  file_name_test = "PPV_1_1000_100_0_10_en.png"
  testthat::expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
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
  
  p <- BayesianReasoning::PPV_heatmap(
    PPV_NPV = "NPV",
    min_Prevalence = 1, max_Prevalence = 1000,
    Specificity = 100,
    limits_Sensitivity = c(90, 100),
    overlay = "area",
    overlay_extra_info = TRUE,
    overlay_position_FN = 2,
    overlay_prevalence_1 = 1,
    overlay_prevalence_2 = 10,
    folder = "."
  )
  
  file_name_test = "NPV_1_1000_100_0_10_area_TRUE_en.png"
  testthat::expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
  
})
