---
title: "CRAN comments"
---


# Reason for submission1
* This submission is in response to 2 user-reported bugs documented as Issues here:

    + https://github.com/dpc10ster/RJafroc/issues/89
    + https://github.com/dpc10ster/RJafroc/issues/90

* This update (v2.1.3) fixes these issues. 


# Test environments

## My machine
* R version 4.3.0 (2023-04-21)
* Platform: aarch64-apple-darwin20
* macOS Ventura 13.4.1 (c)
* Apple M2 Pro

## Checks on my machine

* `R CMD check` in RStudio ran with no errors, warnings or notes
* `devtools::check()` ran with no errors, warnings or notes


## Checks on GitHub Actions
* windows-latest (release): OK
* macOS-latest (release):  OK
* ubuntu-20.04 (release): OK
* ubuntu-20.04 (devel): OK


## Windows and MAC portability checked using `devtools`

`devtools::check_win_devel()`: OK
`devtools::check_win_release()`: OK
`devtools::check_win_oldrelease()`: OK 
`devtools::check_mac_release()`: OK


## CRAN compatibility checked using `rhub::check_for_cran()`


| URL                                                                                  | Status                                                    |
|:-------------------------------------------------------------------------------------|:----------------------------------------------------------|
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-591107c09f17497da17f6c5921d42074 | NOTE Example CPU time 5.6s and \*see below                |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-baa2cb7423724eadadac7f22e7ce9a8c | NOTE CPU time 6s & package size 6 MB and \*see below      |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-0ecbba6afcd146e1be20f9343eb1af37 | NOTE CPU time 6s and \*see below                          |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-30c816b4e9774a849aeada1e6071f967 | ERROR \**see below                                        |

`*` package ‘V8’ not available on this platform.
`**` package dependencies ‘openxlsx’ and ‘readxl’ are not available on this platform.


## Summary of checks in other `rhub` implemented environments using package `rhub`

```
platforms <- rhub::platforms()

indx_packages_cran <- c(1,2,6,7,12,8,15,13)  
  
packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.3.tar.gz"
if (!file.exists(packagePath))
  packagePath <- devtools::build()

for (indx in 1:length(indx_packages_cran)) { 
  cat(platforms[[1]][indx_packages_cran[indx]],"\n")
  chk1 <- rhub::check(packagePath, platforms = platforms[[1]][indx])
  next
}
```

| Platform                                                 | Status                                                |
|:---------------------------------------------------------|:------------------------------------------------------|
| Debian Linux, R-devel, clang, ISO-8859-15 locale         | OK                                                    |
| Debian Linux, R-devel, GCC                               | OK                                                    |
| Debian Linux, R-devel, GCC, no long double               | OK                                                    |
| Debian Linux, R-patched, GCC                             | OK                                                    |
| Fedora Linux, R-devel, clang, gfortran                   | OK                                                    |
| Fedora Linux, R-devel, GCC                               | NOTE installed size 5.1Mb                             |
| Windows Server 2022, R-devel, 64 bit                     | NOTE CPU time and installed size 5.1Mb                             |
| Debian Linux, R-release, GCC                             | NOTE installed size 5.1Mb                             |
| Debian Linux, R-devel, GCC ASAN/UBSAN                    | PREPERROR \* I could not track the reason             |



# FAILURE SUMMARY (from last attempted submission)

The previous version installed with 0 errors, 0 warnings and 0 notes. 


# All revdep maintainers were notified of the release on release date

```
devtools::revdep()
```

No reverse dependencies were found.

