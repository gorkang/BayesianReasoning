
test_that("min_possible_prevalence Output", {
  res <- evaluate_promise(BayesianReasoning::min_possible_prevalence(Sensitivity = 99, min_PPV_desired = 99, FP_test = .1))
  expect_equal(
    res$messages,
    "To reach a PPV of 99% when using a test with 99% Sensitivity and 0.1% False Positive Rate, you need a prevalence of at least 1 out of 10\n"
  )
})
