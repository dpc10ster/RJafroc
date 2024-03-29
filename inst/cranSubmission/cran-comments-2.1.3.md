---
title: "CRAN comments"
---


# Reason for submission
* This submission is in response to two user-reported bugs documented as Issues here:

    + https://github.com/dpc10ster/RJafroc/issues/89
    + https://github.com/dpc10ster/RJafroc/issues/90

* This update (v2.1.3) fixes these issues. 


# Test environments

## My machines

### Apple M2 Pro

* macOS Ventura 13.4.1 (c)
* Platform: aarch64-apple-darwin20
* R version 4.3.0 (2023-04-21)

### Checks on this machine

* `R CMD check` in `RStudio`: OK


### Apple iMac

* macOS Catalina 10.15.7
* Platform: x86_64-apple-darwin17.0
* R version 4.2.3 (2023-03-15)

### Checks on this machine

* `R CMD check` in `RStudio`: OK


## Checks on GitHub Actions

* windows-latest (release): OK
* macOS-latest (release):  OK
* ubuntu-20.04 (release): OK
* ubuntu-20.04 (devel): OK


## Windows and MAC portability were checked

`devtools::check_win_devel()`: OK
`devtools::check_win_release()`: OK
`devtools::check_win_oldrelease()`: OK 
`devtools::check_mac_release()`: OK


## CRAN compatibility was checked using `rhub::check_for_cran()`


| URL                                                                                  | Status                                                    |
|:-------------------------------------------------------------------------------------|:----------------------------------------------------------|
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-591107c09f17497da17f6c5921d42074 | NOTE Example CPU time 5.6s and \*see below                |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-baa2cb7423724eadadac7f22e7ce9a8c | NOTE CPU time 6s & package size 6 MB and \*see below      |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-0ecbba6afcd146e1be20f9343eb1af37 | NOTE CPU time 6s and \*see below                          |
|https://builder.r-hub.io/status/RJafroc_2.1.3.tar.gz-30c816b4e9774a849aeada1e6071f967 | ERROR \**see below                                        |

`*` package ‘V8’ not available on this platform.

`**` package dependencies ‘openxlsx’ and ‘readxl’ are not available on this platform.


## Checks on other platform using package `rhub`

```
library(rhub)
rhub::validate_email()
rhub::check()
```



| Platform                                                 | Status                                                |
|:---------------------------------------------------------|:------------------------------------------------------|
| Debian Linux, R-devel, clang, ISO-8859-15 locale         | OK                                                    |
| Debian Linux, R-devel, GCC                               | OK                                                    |
| Fedora Linux, R-devel, clang, gfortran                   | OK                                                    |
| Fedora Linux, R-devel, GCC                               | NOTE installed size 5.7Mb                             |
| Windows Server 2022, R-devel, 64 bit                     | OK                                                    |
| Debian Linux, R-patched, GCC                             | OK                                                    |
| Debian Linux, R-release, GCC                             | NOTE Installed size 5.1Mb                             |
| Windows Server 2022, R-release, 32/64 bit                | OK                                                    |
| Windows Server 2022, R-oldrel, 32/64 bit                 | OK                                                    |



# FAILURE SUMMARY from last attempted submission

The previous version installed with 0 errors, 0 warnings and 0 notes. 


# All revdep maintainers were notified of the release on release date

```
devtools::revdep()
```

No reverse dependencies were found.

