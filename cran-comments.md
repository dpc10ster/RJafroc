---
title: "CRAN comments"
---

# Current post CRAN acceptance version is 2.1.0.9000
* When done, this version number will need to be bumped up
* All check below need to be then updated


# Reason for submission
* This is an update to CRAN version 2.0.1 which is passing all checks on all platforms (as of 2022-07-20 15:50:17 CEST).
* This update (v2.1.0) includes improvements to the code, some as a result of user-reported bugs and new feature requests, and others discovered during ongoing testing and code simplification. 


# Test environments

## OS X
* R version 4.2.1 (2022-06-23)
* Platform: x86_64-apple-darwin17.0 (64-bit)
* Running under: macOS Catalina 10.15.7
* iMac, 3.4GHz Quad-Core Intel Core i7

* `R CMD check` ran with no errors, warnings or notes


## OS X
* R version 4.2.1 (2022-06-23)
* Platform: x86_64-apple-darwin17.0 (64-bit)
* Running under: macOS Montery 12.4
* Macbook Pro, 2.5 GHz Quad-Core Intel Core i7

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

| R Version    | Platform                    | Status |
|:-------------|:----------------------------|:-------|
| R-devel      | x86_64-w64-mingw32 (64-bit) | OK     |
| R release    | x86_64-w64-mingw32 (64-bit) | OK     |
| R-oldrelease | x86_64-w64-mingw32 (64-bit) | OK     |


## CRAN compatibility

CRAN compatibility was tested using `rhub::check_for_cran()`.

| Platform                                 | Status                                     |
|:-----------------------------------------|:-------------------------------------------|
| Fedora Linux, R-devel, clang, gfortran   | OK                                         |
| Ubuntu Linux 20.04.1 LTS, R-release, GCC | NOTE\*\*\* (file size is 5.8Mb, see below) |
| Debian Linux, R-devel, GCC ASAN/UBSAN    | PREPERROR\* (see below)                    |
| Windows Server 2022, R-devel, 64 bit     | NOTE\*\* (see below)                       |

`***` I would have to remove a significant number of code, tests and datasets to meet the strict 5MB requirement on this platform. 

`*` PREPERROR: dependency ‘openxlsx’ not available; it appears one of the packages that my package depends on is not available on this platform.

`**` checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'


There is no such file/directory on my computer. This might be some issue with `LaTeX` that I cannot reproduce, [as others have observed](https://github.com/r-hub/rhub/issues/503). 


## Summary of checks in other environments implemented on `rhub`

| Platform                                           | Status                                                |
|:---------------------------------------------------|:------------------------------------------------------|
| r-devel-linux-x86_64-debian-clang 2.0.1            | OK                                                    |
| Debian Linux, R-devel, clang, ISO-8859-15 locale   | OK                                                    |
| Ubuntu Linux 20.04.1 LTS, R-devel, GCC             | OK                                                    |
| Ubuntu Linux 20.04.1 LTS, R-release, GCC           | NOTE\*\* (installed size is 5.8Mb, see below)         |
| Debian Linux, R-devel, GCC                         | OK                                                    |
| Debian Linux, R-devel, GCC, no long double         | OK                                                    |
| Debian Linux, R-release, GCC                       | NOTE\*\* (installed size is 5.1Mb, see below)         |
| Debian Linux, R-devel, GCC ASAN/UBSAN              | NOTE (PREPERROR: dependency ‘openxlsx’ not available) |
| Debian Linux, R-patched, GCC                       | OK                                                    |
| Fedora Linux, R-devel, clang, gfortran             | OK                                                    |
| Fedora Linux, R-devel, GCC                         | NOTE\*\* (installed size is 5.6Mb, see below)         |
| Windows Server 2022, R-devel, 64 bit               | NOTE\* (see below)                                    |
| Windows Server 2022, R-patched, 32/64 bit          | OK                                                    |
| Windows Server 2022, R-release, 32/64 bit          | OK                                                    |
| Windows Server 2022, R-oldrel, 32/64 bit           | NOTE\*\* (installed size is 5.3Mb, see below)         |
| Apple Silicon (M1), macOS 11.6 Big Sur, R-release  | OK                                                    |
| macOS 10.13.6 High Sierra, R-release, CRAN’s setup | OK                                                    |
| macOS 10.13.6 High Sierra, R-release, brew         | OK                                                    |


`*` checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'

There is no such file/directory on my computer. This might be some issue with `LaTeX` that I cannot reproduce, [as others have observed](https://github.com/r-hub/rhub/issues/503). 

  
`**` I would have to remove a significant number of code, tests and datasets to meet the strict 5MB requirement on this platform. 



# FAILURE SUMMARY (from last attempted submission)

Not applicable, as previous version installed with 0 errors, 0 warnings and 0 notes.

# All revdep maintainers were notified of the release on release date

```
devtools::revdep()
```

Not applicable, as no reverse dependencies were found.

