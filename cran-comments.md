## Test environments
* local Ubuntu 20.04, R 4.1.2
* win-builder (devel and release)
* R-hub
  + Windows Server 2022, R-devel, 64 bit
  + Ubuntu Linux 20.04.1 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 0 note


## Minor fixes

* Fixing issues with URL's (https://cran.r-project.org/web/checks/check_results_BayesianReasoning.html)

  + Added function so the internet resources in the introduction vignette have a fallback condition with an informative message. 

* Add more checks and tests to catch corner cases