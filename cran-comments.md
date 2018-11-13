## Test environments; OS and R.version.string
* "local OS X 10.13.5", "R version 3.5.1 (2018-07-02)"
  R CMD check results: 0 errors | 0 warnings | 0 notes
  
* devtools::check(): 0 errors | 0 warnings | 0 notes

* win-builder ( https://win-builder.r-project.org)
   R-release: OK
   R-devel: OK
   R-oldrelease: 1 Note regarding 4 possibly mis-spelled words, all FPs
* R-hub builder (https://builder.r-hub.io)   
   Debian, Linux, R-devel, GCC: OK
   Debian Linux, R-release, GCC: waiting
   Debian Linux, R-patched, GCC: OK1
  
   Oracle Solaris 10, x86, 32 bit, R-patched (experimental): 1 Warning, Vignette
   builder function Pandoc not available on this platform
  
* travis: 
  oldrel: OK
  release: OK
  devel: OK
   

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