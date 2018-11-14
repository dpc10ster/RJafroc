## Test environments; OS and R.version.string
*  "local OS X 10.13.5", "R version 3.5.1 (2018-07-02)"
*  R CMD check results: 0 errors | 0 warnings | 0 notes
  
*  devtools::check(): 0 errors | 0 warnings | 0 notes

* travis(https://travis-ci.org): 
   oldrel: OK
   release: OK
   devel: OK
   
*  win-builder ( https://win-builder.r-project.org)
   R-release: OK
   R-devel: OK
   R-oldrelease: 1 Note regarding 4 possibly mis-spelled words, all FPs
   
*  R-hub builder (https://builder.r-hub.io)   
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

## Reverse dependencies
None, see below.

---

* I have run R CMD check on the downstream dependencies.
Not applicable, no downstream dependencies, see below.


* FAILURE SUMMARY (from last attempted submission)
Previous version (1.0.2) installed on every platform except
r-oldrel-windows-ix86+x86_64, on which it generated an ERROR
in namespaceExport(ns, exports): undefined exports: %s!=%, %s!==%, very long output involving stri_xxx. 
The new version passes all windows tests (see lines 5 - 8) including R-oldrelease.


* All revdep maintainers were notified of the release on RELEASE DATE.
Not applicable.
revdep("rjafroc")
character(0)
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
No ERRORs or WARNINGs found :)