# Overall
* This is an update to CRAN version 1.2.0 which installed with no errors, warnings or notes (2019-07-31) on all platforms except `r-devel-windows-ix86+x86_64-gcc8`, which generated the following file size NOTE:
````
Version: 1.2.0 
Check: installed package size 
Result: NOTE 
     installed size is 5.2Mb
     sub-directories of 1Mb or more:
     libs 2.2Mb 
````
* This update corrects bugs noted by some users and introduces greater flexibility for future observer performance study designs (e.g., split-plot designs). 

* I have eliminated the file-size NOTE in all tested platforms. 

# Test environments

## OSX
* iMac, 3.4GHz Quad-Core Intel Core i7
* `"x86_64-apple-darwin15.6.0", "arch x86_64", "os darwin 15.6.0" "R version 3.6.2 (2019-12-12)"` 
* `R CMD check` ran with no errors, warnings or notes

## Travis
* No errors, warnings or notes on `oldrel`,`release` or `developer`. 

## Windows portability
1. This was tested using `devtools`
1. `devtools::check_win_devel`, `devtools::check_win_release` and `devtools::check_win_oldrelease`: each of these generated one note:  
* checking CRAN incoming feasibility
Maintainer: 'Dev Chakraborty <dpc10ster@gmail.com>'. This is expected because I am the maintainer.
1. Additionally, the checks cited possibly misspelled words in the DESCRIPTION file, all of which are false positives.

## Other platform portability
1. CRAN compatibility was tested using `rhub::check_for_cran()`: This yielded 2 NOTES:
1. The first note is that I am the maintainer, which I attest is true.
1. The second note states (for Windows Server 2008 R2 SP1, R-devel, 32/64 bit):
Found the following files/directories:
     'RJafroc-Ex_i386.Rout' 'RJafroc-Ex_x64.Rout' 'examples_i386'
     'examples_x64'
I have checked the installation directory carefully, and I believe the above files are absent. Moreover, the checks performed on this platform using `rhub::check(platform = "windows-x86_64-devel")` passed, see below.
1. Additionally, the checks cited possibly misspelled words in DESCRIPTION file, all of which are false positives.

## Further checks were conducted across all platforms implemented in `rhub::platforms()`

### `debian-clang-devel`: 
  Debian Linux, R-devel, clang, ISO-8859-15 locale: This failed with following message: Error in loadNamespace(name) : there is no package called 'BiocManager'. My namespace does not contain `BiocManager`.
  
### `debian-gcc-devel`:
  Debian Linux, R-devel, GCC: Generated 0 errors, 0 warnings and 0 notes.

### `debian-gcc-devel-nold`:
  Debian Linux, R-devel, GCC, no long double: OK.

### `debian-gcc-patched`:
  Debian Linux, R-patched, GCC:  OK.

### `debian-gcc-release`:
  Debian Linux, R-release, GCC: Generated 0 errors, 0 warnings and 1 NOTE.
1. The first note is that I am the maintainer, which is true.
  Maintainer: ‘Dev Chakraborty <dpc10ster@gmail.com>’

### `fedora-clang-devel`:
  Fedora Linux, R-devel, clang, gfortran: Generated 0 errors, 0 warnings and 1 NOTE.
1. The first note is that I am the maintainer, which is true.

### `fedora-gcc-devel`:
  Fedora Linux, R-devel, GCC: Generated 0 errors, 0 warnings and 1 NOTE.
1. The first note is that I am the maintainer, which is true.

### `linux-x86_64-centos6-epel`: 
  CentOS 6, stock R from EPEL: This failed with followiing message: 
1.  ERROR: dependency ‘ggplot2’ is not available for package ‘RJafroc’
It appears `ggplot2` is not available on this platform. My package needs `ggplot2`. 
  
`linux-x86_64-centos6-epel-rdt`:
  CentOS 6 with Redhat Developer Toolset, R from EPEL: Generated 0 errors, 0 warnings and 0 NOTES. 

### `linux-x86_64-rocker-gcc-san`:
  Debian Linux, R-devel, GCC ASAN/UBSAN: Generated 0 errors, 0 warnings and 0 NOTES. 
  
### `macos-elcapitan-release`:
  macOS 10.11 El Capitan, R-release (experimental): Generated 0 errors, 0 warnings and 0 NOTES.
  
### `solaris-x86-patched`:
  Oracle Solaris 10, x86, 32 bit, R-patched (experimental): Generated 0 errors, 0 warnings and 0 NOTES.
  
### `ubuntu-gcc-devel`:
  Ubuntu Linux 16.04 LTS, R-devel, GCC: OK

### `ubuntu-gcc-release`:
  Ubuntu Linux 16.04 LTS, R-release, GCC: Generated 0 errors, 0 warnings and 1 NOTE.
1. The maintainer note.  

### `ubuntu-rchk`:
  Ubuntu Linux 16.04 LTS, R-devel with rchk: Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-devel`: 
  Windows Server 2008 R2 SP1, R-devel, 32/64 bit: Generated 0 errors, 0 warnings and 0 notes

### `windows-x86_64-devel-rtools4`:
  Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental): build reported "success"

### `windows-x86_64-oldrel`:
  Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit: Generated 0 errors, 0 warnings and 1 NOTE
1. The maintainer note.  

### `windows-x86_64-patched`:
  Windows Server 2008 R2 SP1, R-patched, 32/64 bit: Generated 0 errors, 0 warnings and 1 NOTE
1. The maintainer note.  

### `windows-x86_64-release`:
  Windows Server 2008 R2 SP1, R-release, 32/64 bit: Generated 0 errors, 0 warnings and 1 NOTE
1. The maintainer note.  

# Reverse dependencies #
No reverse dependencies were found.

# FAILURE SUMMARY (from last attempted submission) #
* I have eliminated the file-size NOTE in all tested platforms.

# All revdep maintainers were notified of the release on RELEASE DATE. #
Not applicable as no reverse dependencies were found.
