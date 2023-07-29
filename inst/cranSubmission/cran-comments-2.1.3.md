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


## Windows portability

```
devtools::check_win_devel(): OK
devtools::check_win_release(): OK
devtools::check_win_oldrelease(): OK 
```


## CRAN compatibility

CRAN compatibility was tested using `rhub::check_for_cran()`. 


| URL                                                                                  | Status                    |
|:-------------------------------------------------------------------------------------|:--------------------------|
|https://builder.r-hub.io/status/RJafroc_2.1.2.tar.gz-d304d3f79a0f4cfc978120a399369143 | OK1                        |
|https://builder.r-hub.io/status/RJafroc_2.1.2.tar.gz-dd7cfd00b60d4876a883d7a88ca8c095 | OK1                        |
|https://builder.r-hub.io/status/RJafroc_2.1.2.tar.gz-a314baaf4b4e415481c954855dd35c71 | OK1                        |
|https://builder.r-hub.io/status/RJafroc_2.1.2.tar.gz-a080796f2ecc48c4827153f483397d68 | ERROR (see below)         |

Dependency ‘openxlsx’ is not |available for package ‘RJafroc’; one of the packages that my package depends on is not available on this platform.

## Summary of checks in other rhub implemented environments


| Platform                                           | Status                                                |
|:---------------------------------------------------|:------------------------------------------------------|
| macOS 10.13.6 High Sierra, R-release, brew         | OK1                                                    |
| macOS 10.13.6 High Sierra, R-release, CRAN's setup | OK1                                                    |
| Windows Server 2022, R-oldrel, 32/64 bit           | OK1                                                    |
| Windows Server 2022, R-release, 32/64 bit          | OK1                                                   |
| Windows Server 2022, R-patched, 32/64 bit          | OK1                                                   |
| Windows Server 2022, R-devel, 64 bit               | OK1                                                    |
| Debian Linux, R-devel, clang, ISO-8859-15 locale   | OK1                                                    |
| Debian Linux, R-devel, GCC                         | OK1                                                    |
| Debian Linux, R-devel, GCC ASAN/UBSAN              | PREPERROR \* (missing package, see below)             |
| Fedora Linux, R-devel, clang, gfortran             | NOTE  \*\*\* (CPU time, see below)                    |
| Ubuntu Linux 20.04.1 LTS, R-devel, GCC             | OK1                                                    |
| Ubuntu Linux 20.04.1 LTS, R-release, GCC           | NOTE\*\* (file size is 5.4Mb, see below)              |


`*` PREPERROR: dependency ‘openxlsx’ not available; it appears one of the packages that my package depends on is not available on this platform.

`**` I would have to remove a significant number of code, tests and datasets to meet the 5MB requirement on this platform. 

`***` I would have to remove or comment out a significant number of examples to meet the CPU time restrictions on this platform. 


# FAILURE SUMMARY (from last attempted submission)

The previous version installed with 0 errors, 0 warnings and 1 note (installed size is 5.2Mb). The size of the package has been reduced by moving all vignettesto my `RJafroc`-based online books on GitHub. 

# All revdep maintainers were notified of the release on release date

```
devtools::revdep()
```

No reverse dependencies were found.

