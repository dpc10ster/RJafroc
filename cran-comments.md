# Reason for submission
* This is an update to CRAN version 2.0.1. This package is passing all checks on all platforms (as of 2022-07-20 15:50:17 CEST).
* This update (v2.1.0) includes improvements to the code, some as a result of user-reported bugs and new feature requests, and others discovered during ongoing testing and code simplification since the last successful submission. 

# Test environments

## OS X
* R version 4.2.1 (2022-06-23)
* Platform: x86_64-apple-darwin17.0 (64-bit)
* Running under: macOS Catalina 10.15.7
* iMac, 3.4GHz Quad-Core Intel Core i7

* `R CMD check` ran with no errors, warnings or notes

## GitHub Actions
* windows-latest (release): OK 
* macOS-latest (release):  OK
* ubuntu-20.04 (release): OK
* ubuntu-20.04 (devel): OK

## Windows portability
```
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
```

1.   R Under development (unstable) (2022-07-19 r82607 ucrt)  OK
1.   R version 4.2.1 (2022-06-23 ucrt)                        OK




## CRAN compatibility
CRAN compatibility was tested using `rhub::check_for_cran()`.

1.    Fedora Linux, R-devel, clang, gfortran          OK
1.    Windows Server 2008 R2 SP1, R-devel, 32/64 bit           OK
1.    Ubuntu Linux 20.04.1 LTS, R-release, GCC     NOTE (file size is 5.8Mb)
1.    Debian Linux, R-devel, GCC ASAN/UBSAN        NOTE (PREPERROR: dependency ‘openxlsx’ not available)

## Summary of checks in other environments
TBA Apart from failures on 2 environments which do not appear to be among the `flavors` tested on CRAN, and 1 on which required packages were not available, the package passed all checks on the remaining 18 environments. The check details on all 19 environments follows. 

## Details of checks in 18 environments implemented on `rhub`

1.  r-devel-linux-x86_64-debian-clang 	2.0.1 	       OK 	
1.  Debian Linux, R-devel, clang, ISO-8859-15 locale   OK
1.  Ubuntu Linux 20.04.1 LTS, R-devel, GCC             OK
1.  Ubuntu Linux 20.04.1 LTS, R-release, GCC           NOTE (installed size is  5.8Mb)
1.  Debian Linux, R-devel, GCC                         OK
1.  Debian Linux, R-devel, GCC, no long double         OK
1.  Debian Linux, R-release, GCC                       NOTE (installed size is  5.1Mb)
1.  Debian Linux, R-devel, GCC ASAN/UBSAN              NOTE (PREPERROR: dependency ‘openxlsx’ not available)
1.  Debian Linux, R-patched, GCC                       OK
1.  Fedora Linux, R-devel, clang, gfortran             OK
1.  Fedora Linux, R-devel, GCC                         NOTE (installed size is  5.6Mb)
1.  Windows Server 2022, R-devel, 64 bit               OK
1.  Windows Server 2022, R-patched, 32/64 bit          OK
1.  Windows Server 2022, R-release, 32/64 bit          OK
1.  Windows Server 2022, R-oldrel, 32/64 bit           NOTE (installed size is  5.3Mb)
1.  Apple Silicon (M1), macOS 11.6 Big Sur, R-release  OK
1.  macOS 10.13.6 High Sierra, R-release, CRAN's setup OK
1.  macOS 10.13.6 High Sierra, R-release, brew         OK



# FAILURE SUMMARY (from last attempted submission)
Not applicable, as previous version installed with 0 errors, 0 warnings and 0 notes.

# All revdep maintainers were notified of the release on release date
```
devtools::revdep()
```
Not applicable, as no reverse dependencies were found.

