library(testthat)
library(BayesianReasoning)

path_1 = ("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_BayesianReasoning/BayesianReasoning/R/.checkPPV.R")
path_2 = ("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/R_BayesianReasoning/BayesianReasoning/R/.createPPVmatrix.R")
source(path_1) # ../../ ?
source(path_2) # ../../ ?


test_check("BayesianReasoning")

# The above fails with "Error: No tests found for BayesianReasoning", but then, this works:
# This works manually, and the lines above automatically?
# devtools::test()
