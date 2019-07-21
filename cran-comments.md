## This is a new submission ##
A C++11 compilation error on Solaris resulted in archival of the previous version. Archived on 2019-01-17.
This error has been corrected.

## Test environments; OS and R.version.string ##
*  OSX "x86_64-apple-darwin15.6.0", "R version 3.6.1 (2019-07-05)"
── R CMD check results ────
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
# devtools::check_rhub()
   2 Notes (arch 'i386' and arch 'x64') CPU time > 5 s: these examples of maximum likelihood fitting from clinical
      datasets are as minimal as I can make them while illustrating the analytical techniques
   Similar note on Windows Server 2008 R2 SP1, R-devel, 32/64 bit
   In all cases time is still less than 7s
   1 Note on Ubuntu Linux 16.04 LTS, R-release, GCC regarding possibly misspelled words, all FPs
   Debian Linux, R-devel, GCC ASAN/UBSAN: OK
   1 Error on Fedora Linux, R-devel, clang, gfortran: 
      Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called lattice
      I believe this is a false positive as my package does not depend on *lattice*
      See independent check on R-hub builder below, which gives different error

* travis(https://travis-ci.org): 
   oldrel: OK
   release: OK
   devel: OK
   
*  win-builder ( https://win-builder.r-project.org)
   R-release: OK
   R-devel: OK
   R-oldrelease: 1 Note regarding 4 possibly mis-spelled words, all FPs
   
*  Independent checks on R-hub builder (https://builder.r-hub.io)   
   macOS 10.9 Mavericks, R-oldrel (experimental): OK
   macOS 10.11 El Capitan, R-release (experimental): OK
   Debian, Linux, R-devel, GCC: OK
   Debian Linux, R-release, GCC: OK
   Debian Linux, R-patched, GCC: OK
   Ubuntu Linux 16.04 LTS, R-release, GCC: OK
   
   Fedora Linux, R-devel, clang, gfortran: WARNING, unable to start device PNG, in 
      vignette that produces no plots.
   Fedora Linux, R-devel, do: 
      Both of these appear to be related to an unresolved issue on this platform (https://github.com/r-hub/rhub/issues/92)
   Ubuntu Linux 16.04 LTS, R-devel, GCC: PREPERROR
   Oracle Solaris 10, x86, 32 bit, R-patched (experimental): 1 Warning, Vignette
      builder function Pandoc not available on this platform; known issue (https://github.com/rstudio/DT/issues/395)


* This is a new release.
No. This is an update of a previous version.

## Reverse dependencies ##
None, see below.

---

* I have run R CMD check on the downstream dependencies.
Not applicable, no downstream dependencies, see below.


* FAILURE SUMMARY (from last attempted submission)
Previous version installed on every platform except Solaris resulting in archival of the package on CRAN
The c-code compilation error on Solaris has been corrected in this version


* All revdep maintainers were notified of the release on RELEASE DATE.
Not applicable.
revdep("rjafroc")
character(0)
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
No ERRORs or WARNINGs found :)