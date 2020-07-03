## Test environments
* local Ubuntu 18.04, R 3.6.3
* win-builder (devel and release)
* R-hub
  + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  + Ubuntu Linux 16.04 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 3

## New minor version

This is new minor version to correct a few NOTES that appeared in the CRAN checks.

I also added more tests, cleaned up old comments and improved a couple functions:

* 100% code coverage
* min_possible_prevalence() is now much more efficient
* Improvements to overlay = "line" en PPV_heatmap(), now using {ggforce} for labels
* Changed color palette for NPV
* Tweaked color palette for PPV