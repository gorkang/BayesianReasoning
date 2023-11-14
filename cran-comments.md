## Test environments
* local Ubuntu 22.04, R 4.3.2
* win-builder (devel and release)
* R-hub
  + Windows Server 2022, R-devel, 64 bit
  + Ubuntu Linux 20.04.1 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 0 note


## Major updates  

* New plot_cutoff()  
  + Shows healthy and sick distributions and shows FP, FN, TP and TN depending on a cutoff point  
* New remove_layers_cutoff_plot() functions  
  + Get's rid of layers of a cutoff_plot: FP, FN, TP or TN  

## Minor updates

* Fix for CRAN change in docType
* Use linewidth instead of size
* Faster implementation of a step in min_possible_prevalence
