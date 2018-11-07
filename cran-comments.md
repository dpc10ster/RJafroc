## Test environments; OS and R.version.string
* "local OS X 10.13.5", "R version 3.5.1 (2018-07-02)"
  R CMD check: 
   OK
* win-builder ( https://win-builder.r-project.org)
   R-release: OK
   R-devel: OK
   R-oldrelease: 1 Note related to 4 flagged misspelled words which are false positives 
* r-hub builder (https://builder.r-hub.io)
  Debian, Linux, R-devel, GCC: OK
  Oracle Solaris 10, x86, 32 bit, R-patched (experimental): OK
* devtools::check(): OK
* travis: 
  oldrel: OK
  release: OK
  devel: OK
   

## R CMD check resultsn (under RStudio)

0 errors | 0 warnings | 0 notes


* This is a new release.
Yes

## Reverse dependencies
None.

---

* I have run R CMD check on the downstream dependencies.
Not applicable

* FAILURE SUMMARY (from last attempted submission)
Previous version (1.0.2) installed on every platform.

Windows: <https://win-builder.r-project.org/incoming_pretest/RJafroc_1.0.2_20180527_202314/Windows/00check.log>
Status: OK

Debian: <https://win-builder.r-project.org/incoming_pretest/RJafroc_1.0.2_20180527_202314/Debian/00check.log>
Status: 1 NOTE

The note, relates to excessive cpu time, which has been corrected.


Last released version's CRAN status: ERROR: 1, OK: 11
See: <https://CRAN.R-project.org/web/checks/check_results_RJafroc.html>

Last released version's additional issues:
  noLD <https://www.stats.ox.ac.uk/pub/bdr/noLD/RJafroc.out>


CRAN Web: <https://cran.r-project.org/package=RJafroc>

The error relates to the last released version, 1.0.1, which has been corrected in the submitted version 1.0.2.


* All revdep maintainers were notified of the release on RELEASE DATE.
revdep("rjafroc")
character(0)
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
No ERRORs or WARNINGs found :)