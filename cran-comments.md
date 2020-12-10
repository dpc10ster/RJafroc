# Reason for submission
* This is an update to CRAN version 1.3.2 which installed with no errors, warnings, or notes (2020-03-06) on all platforms.
* This update includes includes significant improvements to the code, some as a result of user-reported bugs and others discovered in independent testing.

# Test environments

## OS X
* iMac, 3.4GHz Quad-Core Intel Core i7
* "x86_64-apple-darwin17.0", 
* "R version 4.0.3 (2020-10-10)" 
* `R CMD check` ran with no errors, warnings or notes

## GitHub Action
R
windows-latest (release) OK 
macOS-latest (release)  OK
ubuntu-20.04 (release) OK
ubuntu-20.04 (devel) Fail
X No errors, warnings or notes on `oldrel`,`release` or `developer`. 

## Windows portability
X. This was tested using `devtools::check_win_devel`, `devtools::check_win_release` and `devtools::check_win_oldrelease`: each of these generated 1 note, namely that the maintainer is 'Dev Chakraborty <dpc10ster@gmail.com>', which is expected because I am the maintainer.


## CRAN compatibility
1. CRAN compatibility was tested using `rhub::check_for_cran()`. 
* This yielded 1 note:
* Installed size is  5.1Mb; sub-directories of 1Mb or more: libs   2.2Mb


## Further checks were conducted across all platforms implemented in `rhub::platforms()`

### `debian-clang-devel`: 
X  Debian Linux, R-devel, clang, ISO-8859-15 locale: This failed with following message: Error in loadNamespace(name) : there is no package called 'BiocManager'. My namespace does not contain `BiocManager`.
  
### `debian-gcc-devel`:
X  Debian Linux, R-devel, GCC: Generated 0 errors, 0 warnings and 0 notes.

### `debian-gcc-devel-nold`:
X  Debian Linux, R-devel, GCC, no long double: Generated 0 errors, 0 warnings and 0 notes.

### `debian-gcc-patched`:
X  Debian Linux, R-patched, GCC:  Generated 0 errors, 0 warnings and 0 notes.

### `debian-gcc-release`:
X  Debian Linux, R-release, GCC: Generated 0 errors, 0 warnings and 1 note, that the installed size is  5.4Mb.

### `fedora-clang-devel`:
X  Fedora Linux, R-devel, clang, gfortran: Generated 0 errors, 0 warnings and 0 notes.

### `fedora-gcc-devel`:
X  Fedora Linux, R-devel, GCC: Generated 0 errors, 0 warnings and 1 note, that the installed size is  5.4Mb.

### `linux-x86_64-centos6-epel`: 
X  CentOS 6, stock R from EPEL: This failed with followiing message: ERROR: dependency ‘ggplot2’ is not available for package ‘RJafroc’. My package needs `ggplot2`. 
  
`linux-x86_64-centos6-epel-rdt`:
X  CentOS 6 with Redhat Developer Toolset, R from EPEL: Generated 0 errors, 0 warnings and 0 notes. 

### `linux-x86_64-rocker-gcc-san`:
X  Debian Linux, R-devel, GCC ASAN/UBSAN: Generated 0 errors, 0 warnings and 0 notes. 
  
### `macos-elcapitan-release`:
X  macOS 10.11 El Capitan, R-release (experimental): Generated 0 errors, 0 warnings and 0 notes.
  
### `solaris-x86-patched`:
X  Oracle Solaris 10, x86, 32 bit, R-patched (experimental): Generated 0 errors, 0 warnings and 0 notes.
  
### `ubuntu-gcc-devel`:
X  Ubuntu Linux 16.04 LTS, R-devel, GCC: PREPERROR, test could not be conducted as packages could not be loaded: 404 Not Found.

### `ubuntu-gcc-release`:
X  Ubuntu Linux 16.04 LTS, R-release, GCC: Generated 0 errors, 0 warnings and 0 notes.

### `ubuntu-rchk`:
X  Ubuntu Linux 16.04 LTS, R-devel with rchk: Confusing output; email said ERROR but log said "Build step 'Send files or execute commands over SSH' changed build result to SUCCESS"

### `windows-x86_64-devel`: 
X  Windows Server 2008 R2 SP1, R-devel, 32/64 bit: Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-devel-rtools4`:
X  Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental): Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-oldrel`:
X  Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit: Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-patched`:
X  Windows Server 2008 R2 SP1, R-patched, 32/64 bit: Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-release`:
X  Windows Server 2008 R2 SP1, R-release, 32/64 bit: Generated 0 errors, 0 warnings and 0 notes

# Reverse dependencies #
No reverse dependencies were found.

# FAILURE SUMMARY (from last attempted submission) #
Not applicable, as previous version installed with 0 errors, 0 warnings and 0 notes.

# All revdep maintainers were notified of the release on RELEASE DATE. #
Not applicable, as no reverse dependencies were found.


# My Notes #
R = running
C = completed
X = not yet updated
