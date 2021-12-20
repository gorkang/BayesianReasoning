## Test environments
* local Ubuntu 20.04, R 4.1.2
* win-builder (devel and release)
* R-hub
  + Windows Server 2022, R-devel, 64 bit
  + Ubuntu Linux 20.04.1 LTS, R-release, GCC
  + Fedora Linux, R-devel, clang, gfortran
  
## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: ‘Gorka Navarrete <gorkang@gmail.com>’

Size of tarball: 6242682 bytes


## New major version

This is a new version refactoring the way we calculate the PPV and NPV matrices.

I also added parameters to control the scales, and multiple checks and tests.
