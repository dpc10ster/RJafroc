# Reason for submission
* This is an update to CRAN version 1.3.2 which installed with no errors, warnings, or notes (2020-03-06) on all platforms. The package is still passing all checks on all platforms (as of 2020-12-10).
* This update (v2.0.1) includes includes many improvements to the code, some as a result of user-reported bugs and new feature requests, and others discovered during ongoing testing and code simplification since the last successful submission.

# Test environments

## OS X
* iMac, 3.4GHz Quad-Core Intel Core i7
* "x86_64-apple-darwin17.0", 
* "R version 4.0.3 (2020-10-10)" 
* `R CMD check` ran with no errors, warnings or notes

## GitHub Action
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
1. check_win_devel 
    + using log directory 'd:/RCompile/CRANguest/R-release/RJafroc.Rcheck'
    + using R version 4.0.3 (2020-10-10)
    + using platform: x86_64-w64-mingw32 (64-bit)
    + One Note_to_CRAN_maintainers: Maintainer: 'Dev Chakraborty <dpc10ster@gmail.com>', which is expected as I am the maintainer.
    + OK. 
1. check_win_release
    + using log directory 'd:/RCompile/CRANguest/R-devel/RJafroc.Rcheck'
    + using R Under development (unstable) (2020-12-09 r79601)
    + using platform: x86_64-w64-mingw32 (64-bit)
    + One Note_to_CRAN_maintainer: Maintainer: 'Dev Chakraborty <dpc10ster@gmail.com>', which is expected as I am the maintainer.
    + OK. 
1. check_win_oldrelease
    + using log directory 'd:/RCompile/CRANguest/R-oldrelease/RJafroc.Rcheck'
    + using R version 3.6.3 (2020-02-29)
    + using platform: x86_64-w64-mingw32 (64-bit)
    + One Note_to_CRAN_maintainers: Maintainer: 'Dev Chakraborty <dpc10ster@gmail.com>', which is expected as I am the maintainer.
    + OK. 

## CRAN compatibility
CRAN compatibility was tested using `rhub::check_for_cran()`.

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* OK

## Summary of checks in all environments
Apart from failures on 2 environments which do not appear to be among the `flavors` tested on CRAN, and 1 on which required packages were not available, the package passed all checks on the remaining 18 environments. The check details on all 21 environments follows. 

## Details of checks in 21 environments
```
paths <- rhub::platforms()
start <- 1; end <- 21; for (i in start:end) rhub::check(platform = paths[[1]][i]) 
```

1. "debian-clang-devel", 
    + Debian Linux, R-devel, clang, ISO-8859-15 locale: 
    + OK
1. "debian-gcc-devel", 
    + Debian Linux, R-devel, GCC: 
    + OK
1. "debian-gcc-devel-nold", 
    + Debian Linux, R-devel, GCC, no long double:     
    + OK
1. "debian-gcc-patched", 
    + Debian Linux, R-patched, GCC:        
    + OK
1. "debian-gcc-release", 
    + Debian Linux, R-release, GCC File size: 
    + OK
1. "fedora-clang-devel", 
    + Fedora Linux, R-devel, clang, gfortran:
    + OK
1. "fedora-gcc-devel", 
    + Fedora Linux, R-devel, GCC: 
    + OK
1. "linux-x86_64-centos6-epel", 
    + CentOS 6, stock R from EPEL (not on CRAN `flavors`): 
    + PREPERROR   
1. "linux-x86_64-centos6-epel-rdt", 
    + CentOS 6 with Redhat Developer Toolset, R from EPEL: 
    + OK
1. "linux-x86_64-rocker-gcc-san", 
    + Debian Linux, R-devel, GCC ASAN/UBSAN:
    + OK
1. "macos-highsierra-release", 
    + macOS 10.13.6 High Sierra, R-release, brew:   
    + OK
1. "macos-highsierra-release-cran", 
    + macOS 10.13.6 High Sierra, R-release, CRAN's setup: 
    + OK
1. "solaris-x86-patched", 
    + Oracle Solaris 10, x86, 32 bit, R-release:        
    + OK
1. "solaris-x86-patched-ods", 
    + Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6:  
    + OK
1. "ubuntu-gcc-devel", 
    + Ubuntu Linux 16.04 LTS, R-devel, GCC:     
    + OK
1. "ubuntu-gcc-release", 
    + Ubuntu Linux 16.04 LTS, R-release, GCC: 
    + OK
1. "ubuntu-rchk", 
    + Ubuntu Linux 16.04 LTS, R-devel with rchk  (not on CRAN `flavors`): 
    + ERROR too many states (abstraction error?) in function strptime_internal
1. "windows-x86_64-devel", 
    + Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
    + OK
1. "windows-x86_64-oldrel", 
    + Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit:    
    + OK
1. "windows-x86_64-patched", 
    + Windows Server 2008 R2 SP1, R-patched, 32/64 bit: 
    + OK
1. "windows-x86_64-release", 
    + Windows Server 2008 R2 SP1, R-release, 32/64 bit: 
    + ERROR Packages required but not available: 'readxl', 'stringr'


# FAILURE SUMMARY (from last attempted submission)
* A non-functioning website was removed from the version originally submitted as 2.0.0.


# All revdep maintainers were notified of the release on release date
```
devtools::revdep()
```
Not applicable, as no reverse dependencies were found.

