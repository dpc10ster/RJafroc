# Overall #
* This is an update to CRAN version 1.2.0 which passed all tests (2019-07-31). This update corrects a few bugs noted by some users and introduces greater flexibility in future study design, as detailed in NEWS.md.

# Test environments #

## OSX ##
* iMac, 3.4GHz Quad-Core Intel Core i7
* `"x86_64-apple-darwin15.6.0", "arch x86_64", "os darwin 15.6.0" "R version 3.6.1 (2019-07-05)"` 
* `R CMD check` ran with no errors, warnings or notes

## Travis ##
* No errors, warnings or notes on `oldrel`,`release` or `developer`. 

## Windows portability ##
1. This was tested on https://win-builder.r-project.org 

## Other platform portability ##
1. This was checked using `devtools::check_rhub()`
1. The checks were conducted across 20 platforms implemented in `devtools`
1. Most relevant to the error which resulted in archiving of the previous version, the check on Solaris (Oracle Solaris 10, x86, 32 bit, R-patched (experimental)) resulted in no errors, warnings or notes.
### A summary of the checks follows ###
1. On 7 platforms the checks ran with no errors, warnings or notes.
1. One platform (`Ubuntu Linux 16.04 LTS, R-devel with rchk`) generated an error, which we believe is a false positive unrelated to our package, as it generated the following error message, suggesting the error is coming from package `Rcpp`: 

# Reverse dependencies #
No reverse dependencies were found.

# FAILURE SUMMARY (from last attempted submission) #
* Not applicable as the last submission did not generate any errors, warnings or notes.

# All revdep maintainers were notified of the release on RELEASE DATE. #
Not applicable as no reverse dependencies were found.
