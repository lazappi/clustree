## Test environments

* local OS X install, R 4.0.0
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on R-hub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (on R-hub)
* Fedora Linux, R-devel, clang, gfortran (on R-hub)

## R CMD check results

0 errors | 0 warnings | 0 note

* This release is primarily to fix the issue of datasets loading suggested
  packages as notified by CRAN. The sc_example dataset has been updated to
  contain a data.frame instead of a Bioconductor DataFrame object
* Additional minor bugs have been fixed and unit tests added
* One of the secondary author (NOT maintainer) emails has been updated
