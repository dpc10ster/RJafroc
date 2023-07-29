---
title: "CRAN comments"
---


# Reason for submission
* In response to an email from Kurt Hornik <Kurt.Hornik@wu.ac.at> requesting an update to the current CRAN version (v2.1.1) of `RJafroc` which is generating ERRORS all of which are due to failing tests in `tests/testthat.R` related to the recent update of `ggplot2` on which my package depends. 
* This update (v2.1.3) should fix the ERRORS. 


# Test environments

## OS X
* R version 4.2.2 (2022-10-31)
* Platform: x86_64-apple-darwin17.0 (64-bit)
* Running under: macOS Catalina 10.15.7
* iMac, 3.4GHz Quad-Core Intel Core i7

* `R CMD check` ran with no errors, warnings or notes


## GitHub Actions

* windows-latest (release): OK 
* macOS-latest (release):  OK
* ubuntu-20.04 (release): OK1
* ubuntu-20.04 (devel): OK1


## MAC portability

```
devtools::check_win_devel(): OK
devtools::check_win_release(): OK
devtools::check_win_oldrelease(): OK 
```



## Windows portability

```
devtools::check_win_devel(): OK
devtools::check_win_release(): OK
devtools::check_win_oldrelease(): OK 
```


## CRAN compatibility

CRAN compatibility was tested using `rhub::check_for_cran()`. 

## Summary of checks in other `rhub` implemented environments


| Platform                                           | Status                                                |
|:---------------------------------------------------|:------------------------------------------------------|
| macOS 10.13.6 High Sierra, R-release, brew         | OK1                                                    |
| macOS 10.13.6 High Sierra, R-release, CRAN's setup | OK1                                                    |
| Windows Server 2022, R-oldrel, 32/64 bit           | OK1                                                    |
| Windows Server 2022, R-release, 32/64 bit          | OK1                                                   |
| Windows Server 2022, R-patched, 32/64 bit          | OK1                                                   |
| Windows Server 2022, R-devel, 64 bit               | NOTE elapsed CPU time 6s                             |
| Debian Linux, R-devel, clang, ISO-8859-15 locale   | OK1                                                    |
| Debian Linux, R-devel, GCC                         | OK1                                                    |
| Debian Linux, R-devel, GCC ASAN/UBSAN              | PREPERROR \* (missing packages)             |
| Fedora Linux, R-devel, clang, gfortran             | NOTE elapsed CPU time 6s                   |
| Ubuntu Linux 20.04.1 LTS, R-devel, GCC             | OK1                                                    |
| Ubuntu Linux 20.04.1 LTS, R-release, GCC           | NOTE installed size 6Mb              |


`*` PREPERROR: dependencies ‘openxlsx’ and ‘readxl’ not available on this platform.

`**` I would have to remove a significant number of code, tests and datasets to meet the 5MB requirement on this platform. 

`***` I would have to remove or comment out a significant number of examples to meet the CPU time restrictions on this platform. 


# FAILURE1 SUMMARY (from last attempted submission)

The previous version installed with 0 errors, 0 warnings and 1 note (installed size is 5.2Mb). The size of the package has been reduced by moving all vignettesto my `RJafroc`-based online books on GitHub. 

# All revdep maintainers were notified of the release on release date

```
devtools::revdep()
```

No reverse dependencies were found.

