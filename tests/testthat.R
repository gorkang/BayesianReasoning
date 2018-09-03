library(testthat)
library(BayesianReasoning)

test_check("BayesianReasoning")

# The above fails with "Error: No tests found for BayesianReasoning", but then, this works:
devtools::test()
