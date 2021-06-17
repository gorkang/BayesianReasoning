## Test environments
* local Ubuntu 20.04, R 4.1
* win-builder (devel and release)
* R-hub
  + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  + Ubuntu Linux 20.04.1 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 0 notes


## New minor version

This is a new minor version updating two tests to work with the upcomming ggplot2.

I also fixed a couple minor bugs:

* x axis labels should always be equally spaced #37  
* Update tests to work with upcoming ggplot2 #42  
* Add default parameters to avoid errors #43  
* Fix LazyData NOTE  
