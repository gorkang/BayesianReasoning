## Test environments
* local Ubuntu 20.04, R 4.0.5
* win-builder (devel and release)
* R-hub
  + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  + Ubuntu Linux 16.04 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 3

## New minor version

This is new minor version to update some test to woek with the upcomming ggplot2.

I also fixed a couple minor bugs:

* x axis labels should always be equally spaced #37
* Update tests to work with upcoming ggplot2 #42 (comment out new version until next release)
* Add default parameters to avoid errors #43
* Fix LazyData NOTE
