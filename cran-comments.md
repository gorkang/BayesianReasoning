## Test environments
* local Ubuntu 18.04, R 3.6.3
* win-builder (devel and release)
* R-hub
  + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  + Ubuntu Linux 16.04 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. In this version I have:

Corrected issues raised in CRAN revision:

* Reduced title to less than 65 characters
* Do not capitalize things in the Description text
* Added references to Description explaining main concepts
* Replaced cat() with message()
* Aded folder parameter to PPV_diagnostic_vs_screening() and PPV_heatmap() functions

I also added tests for all main functions, deleted old comments and slightly improve the documentation