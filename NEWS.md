Update History
==========================
## RJafroc 1.1.0
### Major changes
Added travis-ci testing after each push; and build passing badges, etc. Real geeky stuff;
Removed dependence on caTools package, which was not being supported; extracted function trapz() from it
   and inserted directly into gpfMyFOM.R - see comments in that file of what led to this
Removed dependence on xlsx package, which requires rJava and JAVA, replaced with dependence on
   openxlsx package. Was having difficulty installing rJava correctly after each OSX or R update.
Corrected errors in UtilOutputReport function.
Fixed bug in UtilOutputReport that was preventing overwriting of existing output file, even
   when the user keys "y" in response to prompt
Added correlated contaminated binormal model (CORCBM) fitting and related functions to
   make package current with 2017 CORCBM publication.
Fixed error in PlotEmpiricalCharacteristics.R that was giving incorrect plots for other than
   ROC and wAFROC plots
   
==========================
## RJafroc 1.0.2
### Minor bug
StSignificanceTestingCadVsRadiologists was not working for different numbers of readers.
   As noted by Alejandro, the number of readers was hard coded. Fixed this and extended
   DfExtractDataset to include LROC datasets.
Removed function SsFROCPowerGivenJK: FROC power is implemented in Online
  Appendix Chapter 19 (see email exchange with Kota Aoyagi)
This package installed on SOLARIS.
   
==========================
## RJafroc 1.0.1
### Minor bug
Package was not installing on Solaris - overloading errors. Changed sqrt(2) in 
   RsmFuncs.cpp to sqrt(2.0). However, Solaris is incompatible with ggplot2; 
   so will recommend that Solaris version not be distributed on CRAN.

Sorry, but I'm not sure what's different between the CRAN Solaris machine and 
   R-hub's Solaris machine. You could prepare a new package submission for CRAN 
   with the caveat that, since you do not have access to a Solaris machine, 
   your fix is speculative and may yet fail to compile on the CRAN Solaris machine.
The CRAN Repository Policy (https://cran.r-project.org/web/packages/policies.html) 
   also states:
   Package authors should make all reasonable efforts to provide cross-platform 
   portable code. Packages will not normally be accepted that do not run on at 
   least two of the major R platforms. Cases for Windows-only packages will be 
   considered, but CRAN may not be the most appropriate place to host them. So 
   you could in theory argue your case that your package does not support Solaris, 
   and request that CRAN not distribute your package on that platform. But given 
   that the issue you're bumping to is (not) documented explicitly in the R manuals, 
   I'm not sure how much success you would have.

==========================
## RJafroc 1.0.0
### Major changes
Renamed functions for better organization; 
Removed shiny GUI interface
Support for LROC datasets and cross-modality datasets
CAD vs. radiologist analysis, both single modality and dual modality

## RJafroc 0.1.1
### Bug fixes
* A critcal error in the *p* value calculation that gave incorrect *p* value (possibly exceeding one) when the first modality performed better than 2nd has been fixed. Thanks to Lucy D'Agostino McGowan for pointing out the error and the fix. This error, which does not occur in Windows version of JAFROC V 4.2.1, was not noticed as in all example files the 2nd modality performed better.

## RJafroc 0.1.0
### Major change
* A "shiny" based GUI has been added, accessed by the function RJafrocGui(). This allows a user only interested in
analyzing a data file to access the underlying code in a "user friendly way. The GUI is similar in functionality to
that of Windows JAFROC 4.2.1 software.

### Minor bug and aesthetic fixes
* For the curve plotting functions, legend position and direction are automatically decided if they are not explicityly specified. 
* The the output number of significant digits for statistical power in power table has been set to 3.
* Variance and covariance calculation error for ROI data has been fixed.
* A bug in the JAFROC data reading function that caused an error when encountering non-numeric values has been fixed.
* Floating point ratings are rounded to 6 significant digits when saving a dataset in JAFROC format. 
* A bug in the plotting routine that affected plots for a single rating FROC dataset has been fixed.
* A bug in the plotting of AFROC curves for a dataset containing only non-diseased cases has been fixed.

## RJafroc 0.0.1
Original version posted to CRAN website
