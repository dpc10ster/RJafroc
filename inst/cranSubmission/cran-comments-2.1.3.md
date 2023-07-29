---
title: "CRAN comments"
---


# Reason for submission
* In response to an email from Kurt Hornik <Kurt.Hornik@wu.ac.at> requesting an update to the current CRAN version (v2.1.1) of `RJafroc` which is generating ERRORS all of which are due to failing tests in `tests/testthat.R` related to the recent update of `ggplot2` on which my package depends. 
* This update (v2.1.3) should fix the ERRORS. 


# Test environments

## OS X
* R version 4.3.0 (2023-04-21)

* Platform: aarch64-apple-darwin20
* macOS Ventura 13.4.1 (c)
* Apple M2 Pro

`R CMD check` ran with no errors, warnings or notes


## GitHub Actions

* windows-latest (release): OK1 
* macOS-latest (release):  OK1
* ubuntu-20.04 (release): OK1
* ubuntu-20.04 (devel): OK1



## Windows portability

```
devtools::check_win_devel(): OK
devtools::check_win_release(): OK
devtools::check_win_oldrelease(): OK 
```

## MAC compatibility using `devtools::check_mac_release()`



## CRAN compatibility using `rhub::check_for_cran()`


| URL                                                                                  | Status                                                |
|:-------------------------------------------------------------------------------------|:------------------------------------------------------|
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-591107c09f17497da17f6c5921d42074 | NOTE Example CPU time 5.6s and \*see below            |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-baa2cb7423724eadadac7f22e7ce9a8c | NOTE CPU time 6s & package size 6 MB \*see below      |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-0ecbba6afcd146e1be20f9343eb1af37 | NOTE CPU time 6s and \*see below                      |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-30c816b4e9774a849aeada1e6071f967 | ERROR \**see below                                    |

`*` package ‘V8’ not available on this platform.

`**` Dependencies ‘openxlsx’, ‘readxl’ are not available for package ‘RJafroc’ on this platform.


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

